#r "nuget: Fun.Build, 0.1.6"

open System.IO
open Fun.Build

let (</>) a b = Path.Combine (a, b)

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
                0
            }
        )
    }

    stage "lint" {
        run "dotnet tool restore"
        run "dotnet fantomas . -r --check"
    }

    stage "build" {
        stage "sass" {
            run (fun _ ->
                async {
                    let p = System.Diagnostics.Process.Start ("dotnet", "fsi ./docs/.style/style.fsx")

                    p.WaitForExit ()

                    return (if p.ExitCode = 0 || p.ExitCode = 139 then 0 else 1)
                }
            )
        }

        run "dotnet restore ./telplin.sln"
        run "dotnet build --no-restore -c Release ./telplin.sln"

        stage "perla" {
            workingDir (__SOURCE_DIRECTORY__ </> "docs" </> ".tool")

            envVars
                [
                    "PERLA_API_ROOT",
                    "https://g8pt5e44zi.execute-api.eu-west-3.amazonaws.com/telplin-main-stage-230a206"
                ]

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

        stage "lambda" {
            workingDir (__SOURCE_DIRECTORY__ </> "src" </> "Telplin.Lambda")
            run "dotnet lambda package"
        }
    }

    stage "docs" { run "dotnet fsdocs build --nodefaultcontent --noapidocs" }
    runImmediate
}
