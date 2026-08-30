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
