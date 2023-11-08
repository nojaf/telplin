#r "nuget: Fun.Build, 0.3.8"
#r "nuget: Fake.IO.FileSystem, 6.0.0"

open System
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open Fun.Build

let apiKey = Environment.GetEnvironmentVariable "TELPLIN_NUGET_KEY"

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
        run "dotnet restore ./telplin.sln"
        run "dotnet build --no-restore -c Release ./telplin.sln"
    }
    stage "test" { run "dotnet test --no-restore --no-build -c Release" }
    stage "pack" { run "dotnet pack ./src/Telplin/Telplin.fsproj -c Release -o bin" }
    stage "docs" {
        run "dotnet fsi ./tool/client/dev-server.fsx build"
        run (fun _ -> Shell.copyRecursive "./tool/client/dist" "./docs" true |> ignore)
        run "dotnet fsdocs build --noapidocs"
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
