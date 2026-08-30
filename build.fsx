#!/usr/bin/env -S dotnet fsi
#r "nuget: Fun.Build, 1.1.16"
#r "nuget: Fake.IO.FileSystem, 6.1.4"

open System
open System.IO
open System.Text.Json
open Fake.IO
open Fake.IO.FileSystemOperators
open Fun.Build

let apiKey = Environment.GetEnvironmentVariable "NUGET_KEY"
let packageOutput = __SOURCE_DIRECTORY__ </> "artifacts" </> "package" </> "release"

/// The newest entry of CHANGELOG.md: its version, its date and its body, the sections as written.
/// The release on GitHub is that entry, so the two cannot come to say different things.
let latestChangelogEntry () : string * DateTime * string =
    let lines = File.ReadAllLines (__SOURCE_DIRECTORY__ </> "CHANGELOG.md")

    let heading =
        Text.RegularExpressions.Regex @"^## \[(?<version>[^\]]+)\] - (?<date>\d{4}-\d{2}-\d{2})"

    let headings =
        lines
        |> Array.indexed
        |> Array.choose (fun (index, line) ->
            let m = heading.Match line
            if m.Success then
                Some (index, m.Groups.["version"].Value, DateTime.Parse m.Groups.["date"].Value)
            else
                None
        )

    match Array.toList headings with
    | [] -> failwith "CHANGELOG.md has no release entry."
    | (start, version, date) :: rest ->
        let stop =
            match rest with
            | (next, _, _) :: _ -> next
            | [] -> lines.Length

        let body =
            lines.[start + 1 .. stop - 1] |> String.concat "\n" |> fun body -> body.Trim ()
        version, date, body

/// "August 30th Release", the title fantomas gives its releases as well.
let releaseTitle (date : DateTime) : string =
    let ordinal =
        match date.Day with
        | 11
        | 12
        | 13 -> "th"
        | day when day % 10 = 1 -> "st"
        | day when day % 10 = 2 -> "nd"
        | day when day % 10 = 3 -> "rd"
        | _ -> "th"

    let month = date.ToString "MMMM"
    $"%s{month} %d{date.Day}%s{ordinal} Release"

/// Create the GitHub release for the newest changelog entry, unless it exists already. A rerun of
/// the workflow, or a push that touches nothing in the changelog, then changes nothing.
let createGithubRelease (ctx : Internal.StageContext) : Async<int> =
    async {
        let version, date, body = latestChangelogEntry ()
        let tag = $"v%s{version}"

        let! existing = ctx.RunCommandCaptureOutput $"gh release view %s{tag} --json tagName"

        match existing with
        | Ok _ ->
            printfn $"Release %s{tag} already exists on GitHub, nothing to do."
            return 0
        | Error _ ->

        let notes =
            $"""# %s{version}

%s{body}

[https://www.nuget.org/packages/telplin/%s{version}](https://www.nuget.org/packages/telplin/%s{version})
"""

        let notesFile = Path.GetTempFileName ()
        File.WriteAllText (notesFile, notes)
        let package = packageOutput </> $"telplin.%s{version}.nupkg"
        let prerelease = if version.Contains '-' then " --prerelease" else ""

        let! result =
            ctx.RunCommand
                $"gh release create %s{tag} \"%s{package}\"%s{prerelease} --title \"%s{releaseTitle date}\" --notes-file \"%s{notesFile}\""

        File.Delete notesFile

        match result with
        | Ok () ->
            printfn $"Created GitHub release %s{tag}."
            return 0
        | Error error ->
            eprintfn $"Could not create GitHub release %s{tag}: %s{error}"
            return 1
    }

/// Compile the troubleshooting scripts in `scripts/` without running them, so a rename in `src/`
/// that one of them refers to is caught here and not the next time somebody reaches for it.
/// A script another one `#load`s is left out: it is compiled as part of what loads it.
/// They reference the debug build of Telplin.Core, which the stage builds first.
let checkScripts (ctx : Internal.StageContext) : Async<int> =
    async {
        let folder = __SOURCE_DIRECTORY__ </> "scripts"
        let scripts = Directory.EnumerateFiles (folder, "*.fsx") |> List.ofSeq
        let loadDirective = Text.RegularExpressions.Regex "^\\s*#load\\s+\"([^\"]+)\""

        let loaded =
            scripts
            |> List.collect (fun script ->
                File.ReadLines script
                |> Seq.choose (fun line ->
                    let m = loadDirective.Match line
                    if m.Success then
                        Some (Path.GetFullPath (folder </> m.Groups.[1].Value))
                    else
                        None
                )
                |> List.ofSeq
            )
            |> Set.ofList

        let mutable failed = 0

        for script in scripts do
            if not (loaded.Contains (Path.GetFullPath script)) then
                let name = Path.GetRelativePath (__SOURCE_DIRECTORY__, script)
                let! result = ctx.RunCommand $"dotnet fsi --typecheck-only --nologo \"%s{script}\""

                match result with
                | Ok () -> printfn "%s compiles." name
                | Error _ ->
                    printfn "%s does not compile." name
                    failed <- failed + 1

        return (if failed = 0 then 0 else 1)
    }

pipeline "Build" {
    workingDir __SOURCE_DIRECTORY__
    stage "clean" {
        run (fun _ ->
            async {
                let deleteIfExists folder =
                    if Directory.Exists folder then
                        Directory.Delete (folder, true)

                deleteIfExists packageOutput
                deleteIfExists (__SOURCE_DIRECTORY__ </> "output")
                deleteIfExists (__SOURCE_DIRECTORY__ </> "docs" </> ".tool" </> "dist")
                return 0
            }
        )
    }
    stage "lint" {
        run "dotnet tool restore"
        run "dotnet fantomas check"
    }
    stage "restore" { run "dotnet restore -tl" }
    stage "build" { run "dotnet build --no-restore -c Release ./telplin.sln -tl" }
    stage "test" { run "dotnet test --no-restore --no-build -c Release -tl" }
    stage "scripts" {
        run "dotnet build --no-restore -c Debug ./src/Telplin.Core/Telplin.Core.fsproj -tl"
        run checkScripts
    }
    stage "pack" { run "dotnet pack ./src/Telplin/Telplin.fsproj -c Release -tl" }
    stage "docs" {
        stage "client" {
            workingDir "tool/client"
            run "bun i --frozen-lockfile"
            run "bunx --bun vite build"
        }
        run (fun _ -> Shell.copyRecursive "./tool/client/dist" "./docs" true |> ignore)
        run "dotnet fsdocs build --noapidocs"
    }
    stage "lambda" {
        workingDir "tool/server"
        run "dotnet lambda package"
    }
    stage "push" {
        whenCmdArg "--push"
        workingDir packageOutput
        run
            $"dotnet nuget push telplin.*.nupkg --source https://api.nuget.org/v3/index.json --api-key {apiKey} --skip-duplicate"
    }
    stage "release" {
        whenCmdArg "--push"
        run createGithubRelease
    }
    runIfOnlySpecified false
}

pipeline "Watch" {
    workingDir __SOURCE_DIRECTORY__
    stage "main" {
        run "dotnet tool restore"
        paralle
        stage "client" {
            envVars [ "VITE_API_ROOT", "http://localhost:8906" ]
            workingDir "tool/client"
            run "bun i --frozen-lockfile"
            run "bunx --bun vite"
        }
        stage "server" {
            workingDir "tool/server"
            run "dotnet publish /p:ReadyToRun  --nologo -c Debug --ucr -p:PublishReadyToRun=true -o ./publish"
            run "dotnet ./publish/bootstrap.dll"
        }
        run "dotnet fsdocs watch --port 7890 --noapidocs --nolaunch"
    }
    runIfOnlySpecified true
}

/// Every project in the solution. Reading the solution rather than globbing keeps stray projects out.
let projectsToAnalyze : string list =
    File.ReadAllLines (__SOURCE_DIRECTORY__ </> "telplin.sln")
    |> Array.choose (fun line ->
        let m = Text.RegularExpressions.Regex.Match (line, "\"([^\"]+\.fsproj)\"")
        if m.Success then
            Some (m.Groups[1].Value.Replace('\\', '/'))
        else
            None
    )
    |> Array.toList

/// Where the analyzer packages were restored to. The two packages are ordinary package references,
/// so MSBuild already knows the path of each and the version lives in Directory.Packages.props only.
let analyzerPaths (ctx : Internal.StageContext) : Async<Result<string list, string>> =
    async {
        let! output =
            ctx.RunCommandCaptureOutput
                "dotnet msbuild src/Telplin/Telplin.fsproj -getProperty:PkgIonide_Analyzers -getProperty:PkgG-Research_FSharp_Analyzers"

        return
            output
            |> Result.bind (fun json ->
                use document = JsonDocument.Parse json

                document.RootElement.GetProperty("Properties").EnumerateObject()
                |> Seq.map (fun property ->
                    match property.Value.GetString () with
                    | null
                    | "" -> Error $"MSBuild has no value for {property.Name}. Run `dotnet restore` first."
                    | path -> Ok (path </> "analyzers" </> "dotnet" </> "fs")
                )
                |> Seq.fold
                    (fun acc next ->
                        match acc, next with
                        | Ok paths, Ok path -> Ok (path :: paths)
                        | Error e, _
                        | _, Error e -> Error e
                    )
                    (Ok [])
            )
    }

pipeline "Analyze" {
    workingDir __SOURCE_DIRECTORY__
    stage "restore" {
        run "dotnet tool restore"
        run "dotnet restore -tl"
    }
    stage "analyze" {
        run (fun ctx ->
            async {
                match! analyzerPaths ctx with
                | Error error -> return Error error
                | Ok analyzers ->
                    let report = __SOURCE_DIRECTORY__ </> "analysis.sarif"
                    File.delete report

                    let arguments =
                        [
                            "dotnet fsharp-analyzers"
                            for analyzer in analyzers do
                                $"--analyzers-path \"{analyzer}\""
                            // Generated sources, not ours: the test SDK entry point and the per-project AssemblyInfo.
                            "--exclude-files **/Microsoft.NET.Test.Sdk.Program.fs **/*.AssemblyInfo.fs"
                            "--configuration Release"
                            "--verbosity d"
                            $"--code-root \"{__SOURCE_DIRECTORY__}\""
                            $"--report \"{report}\""
                            for project in projectsToAnalyze do
                                $"--project \"{__SOURCE_DIRECTORY__ </> project}\""
                        ]

                    return! ctx.RunCommand (String.concat " " arguments)
            }
        )
    }
    runIfOnlySpecified true
}

pipeline "Publish" {
    workingDir __SOURCE_DIRECTORY__
    stage "publish" {
        run "dotnet publish --nologo -c Release --ucr -p:PublishReadyToRun=true ./src/Telplin/Telplin.fsproj"
    }
    runIfOnlySpecified true
}

tryPrintPipelineCommandHelp ()
