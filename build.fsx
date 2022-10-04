#r "nuget: Fun.Build, 0.1.8"
#r "nuget: CliWrap, 3.5.0"

open System.IO
open Fun.Build
open CliWrap
open CliWrap.Buffered

let (</>) a b = Path.Combine (a, b)

let runCmd file (arguments : string) =
    async {
        let! result = Cli.Wrap(file).WithArguments(arguments).ExecuteAsync().Task |> Async.AwaitTask
        return result.ExitCode
    }

let git (arguments : string) =
    async {
        let! result =
            Cli.Wrap("git").WithArguments(arguments).WithWorkingDirectory(
                __SOURCE_DIRECTORY__
            )
                .ExecuteBufferedAsync()
                .Task
            |> Async.AwaitTask
        return result.StandardOutput.Trim ()
    }

let fsharpExtensions = set [| ".fs" ; ".fsx" ; ".fsi" |]

let isFSharpFile (f : string) =
    Set.contains (Path.GetExtension (f)) fsharpExtensions

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
        run (fun _ ->
            async {
                let! branchName = git "branch --show-current"
                if branchName = "main" then
                    return! runCmd "dotnet" "fantomas . -r --check"
                else
                    let! branchingPoint = git "merge-base main HEAD"
                    let! output = git $"diff --name-only {branchingPoint}"
                    let fsharpFiles =
                        output.Split '\n' |> Array.filter isFSharpFile |> String.concat " "
                    return! runCmd "dotnet" $"fantomas --check {fsharpFiles}"
            }
        )
    }
    stage "build" {
        run "dotnet fsi ./docs/.style/style.fsx"
        run "dotnet restore ./telplin.sln"
        run "dotnet build --no-restore -c Release ./telplin.sln"
        stage "perla" {
            workingDir (__SOURCE_DIRECTORY__ </> "docs" </> ".tool")

            run "dotnet tool restore"
            run "dotnet perla b"

            run (fun _ ->
                let dist = __SOURCE_DIRECTORY__ </> "docs" </> ".tool" </> "dist"

                System.IO.Directory.EnumerateFiles (dist, "*.*")
                |> Seq.iter (fun srcFile ->
                    let destFile = __SOURCE_DIRECTORY__ </> "docs" </> Path.GetFileName srcFile

                    File.Copy (srcFile, destFile, true)
                )

                let envJs =
                    __SOURCE_DIRECTORY__
                    </> "docs"
                    </> ".tool"
                    </> "dist"
                    </> "telplin"
                    </> "env.js"

                if File.Exists envJs then
                    File.Copy (envJs, __SOURCE_DIRECTORY__ </> "docs" </> "env.js", true)
            )
        }
    }
    stage "test" { run "dotnet test --no-restore --no-build -c Release" }
    stage "pack" {
        run "dotnet pack .\src\Telplin\Telplin.fsproj -c Release -o bin"
        run "dotnet pack .\src\Telplin.Common\Telplin.Common.fsproj -c Release -o bin"
        run "dotnet pack .\src\Telplin.UntypedTree\Telplin.UntypedTree.fsproj -c Release -o bin"
        run "dotnet pack .\src\Telplin.TypedTree\Telplin.TypedTree.fsproj -c Release -o bin"
        run "dotnet pack .\src\Telplin.Core\Telplin.Core.fsproj -c Release -o bin"
    }
    stage "docs" { run "dotnet fsdocs build --nodefaultcontent --noapidocs" }
    runIfOnlySpecified false
}

pipeline "Watch" {
    workingDir __SOURCE_DIRECTORY__
    stage "main" {
        paralle
        run "dotnet fsi ./docs/.style/style.fsx --watch"
        run "dotnet run --project ./src/Telplin.Lambda/Telplin.Lambda.fsproj"
        stage "perla" {
            envVars [ "PERLA_API_ROOT", "http://127.0.0.1:8906" ]
            workingDir (__SOURCE_DIRECTORY__ </> "docs" </> ".tool")
            run "dotnet perla s"
        }
        run "dotnet fsdocs watch --port 7890 --noapidocs"
    }
    runIfOnlySpecified true
}
