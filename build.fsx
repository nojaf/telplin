#r "nuget: Fun.Build, 0.3.8"
#r "nuget: Fake.IO.FileSystem, 6.0.0"
#r "nuget: CliWrap, 3.6.0"

open System
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open Fun.Build
open CliWrap

let apiKey = Environment.GetEnvironmentVariable "TELPLIN_NUGET_KEY"
let fsharpCompiler = Path.Combine (__SOURCE_DIRECTORY__, ".fsharp") |> DirectoryInfo
let fsharpCompilerRemote = "https://github.com/nojaf/fsharp.git"
let fsharpCompilerCommit = "43ec6a35a81738947c3fe6dc2ec1d9734a4a024d" // See https://github.com/dotnet/fsharp/compare/main...nojaf:fsharp:hash-contraint-seq-fix

let runCommand (file : string) (arguments : string) =
    task {
        let! _result =
            Cli
                .Wrap(file)
                .WithWorkingDirectory(fsharpCompiler.FullName)
                .WithArguments(arguments)
                .WithStandardOutputPipe(PipeTarget.ToDelegate (printfn "%s"))
                .WithStandardErrorPipe(PipeTarget.ToDelegate (printfn "%s"))
                .ExecuteAsync()
                .Task

        return ()
    }

let git = runCommand "git"
let dotnet = runCommand "dotnet"

pipeline "Init" {
    workingDir __SOURCE_DIRECTORY__
    stage "clone local compiler" {
        run (fun _ ->
            task {
                if not fsharpCompiler.Exists then
                    fsharpCompiler.Create ()
                    do! git "init"
                    do! git $"remote add origin {fsharpCompilerRemote}"
                    do! git "fetch origin"
                do! git $"reset --hard {fsharpCompilerCommit}"

                let configuration =
                    let idx = fsi.CommandLineArgs |> Array.tryFindIndex (fun arg -> arg = "-c")
                    match idx with
                    | None -> "Debug"
                    | Some idx ->
                        if idx + 1 < fsi.CommandLineArgs.Length then
                            fsi.CommandLineArgs.[idx + 1]
                        else
                            "Debug"

                do!
                    dotnet
                        $"build -c {configuration} src/Compiler/FSharp.Compiler.Service.fsproj /p:BUILDING_USING_DOTNET=true"
            }
        )
    }
    runIfOnlySpecified true
}

pipeline "Build" {
    workingDir __SOURCE_DIRECTORY__
    stage "clean" {
        run (fun _ ->
            async {
                let deleteIfExists folder =
                    if Directory.Exists folder then
                        Directory.Delete (folder, true)

                deleteIfExists (__SOURCE_DIRECTORY__ </> "bin")
                deleteIfExists (__SOURCE_DIRECTORY__ </> "output")
                deleteIfExists (__SOURCE_DIRECTORY__ </> "docs" </> ".tool" </> "dist")
                return 0
            }
        )
    }
    stage "lint" {
        run "dotnet tool restore"
        run "dotnet fantomas . --check"
    }
    stage "restore" { run "dotnet restore" }
    stage "build" {
        run (fun _ ->
            task {
                do! dotnet "build -c Release src/Compiler/FSharp.Compiler.Service.fsproj /p:BUILDING_USING_DOTNET=true"
            }
        )
        run "dotnet restore ./telplin.sln"
        run "dotnet build --no-restore -c Release ./telplin.sln"
    }
    stage "test" { run "dotnet test --no-restore --no-build -c Release" }
    stage "pack" { run "dotnet pack ./src/Telplin/Telplin.fsproj -c Release -o bin" }
    stage "docs" {
        run "dotnet fsi ./docs/.style/style.fsx"
        run "dotnet fsi ./tool/client/dev-server.fsx build"
        run (fun _ -> Shell.copyRecursive "./tool/client/dist" "./docs" true |> ignore)
        run "dotnet fsdocs build --nodefaultcontent --noapidocs"
    }
    stage "lambda" {
        workingDir "tool/server"
        run "dotnet lambda package"
    }
    stage "push" {
        whenCmdArg "--push"
        workingDir (__SOURCE_DIRECTORY__ </> "bin")
        run
            $"dotnet nuget push telplin.*.nupkg --source https://api.nuget.org/v3/index.json --api-key {apiKey} --skip-duplicate"
    }
    runIfOnlySpecified false
}

pipeline "Watch" {
    workingDir __SOURCE_DIRECTORY__
    stage "main" {
        paralle
        run "dotnet fsi ./tool/client/dev-server.fsx"
        run "dotnet fsdocs watch --port 7890 --noapidocs"
    }
    runIfOnlySpecified true
}

tryPrintPipelineCommandHelp ()
