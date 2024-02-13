#r "nuget: Fun.Build, 1.0.4"
#r "nuget: Fake.IO.FileSystem, 6.0.0"

open System
open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open Fun.Build

let apiKey = Environment.GetEnvironmentVariable "TELPLIN_NUGET_KEY"
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
        run "dotnet fantomas . --check"
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
        run "dotnet fsdocs watch --port 7890 --noapidocs"
    }
    runIfOnlySpecified true
}

tryPrintPipelineCommandHelp ()
