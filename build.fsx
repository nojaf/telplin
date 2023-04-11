#r "nuget: Fun.Build, 0.3.8"
#r "nuget: Fake.IO.FileSystem, 6.0.0"

open System.IO
open Fake.IO
open Fake.IO.FileSystemOperators
open Fun.Build

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
    stage "build" {
        run "dotnet restore ./telplin.sln"
        run "dotnet build --no-restore -c Release ./telplin.sln"
    }
    stage "test" { run "dotnet test --no-restore --no-build -c Release" }
    stage "pack" {
        run "dotnet pack ./src/Telplin/Telplin.fsproj -c Release -o bin"
        run "dotnet pack ./src/Telplin.Common/Telplin.Common.fsproj -c Release -o bin"
        run "dotnet pack ./src/Telplin.UntypedTree/Telplin.UntypedTree.fsproj -c Release -o bin"
        run "dotnet pack ./src/Telplin.TypedTree/Telplin.TypedTree.fsproj -c Release -o bin"
        run "dotnet pack ./src/Telplin.Core/Telplin.Core.fsproj -c Release -o bin"
    }
    stage "docs" {
        run "dotnet fsi ./docs/.style/style.fsx"
        run "dotnet fsi ./tool/client/dev-server.fsx build"
        run (fun _ -> Shell.copyRecursive "./tool/client/dist" "./docs" true |> ignore)
        run "dotnet fsdocs build --nodefaultcontent --noapidocs"
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
