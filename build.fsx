#r "nuget: Fun.Build, 0.1.6"

open System.IO
open Fun.Build

pipeline "Build" {
    workingDir __SOURCE_DIRECTORY__

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
            workingDir (Path.Combine (__SOURCE_DIRECTORY__, "docs", ".tool"))

            envVars
                [
                    "PERLA_API_ROOT",
                    "https://g8pt5e44zi.execute-api.eu-west-3.amazonaws.com/telplin-main-stage-230a206"
                ]

            run "dotnet tool restore"
            run "dotnet perla b"

            run (fun _ ->
                let dist = Path.Combine (__SOURCE_DIRECTORY__, "docs", ".tool", "dist")

                System.IO.Directory.EnumerateFiles (dist, "*.*")
                |> Seq.iter (fun srcFile ->
                    let destFile =
                        Path.Combine (__SOURCE_DIRECTORY__, "docs", Path.GetFileName (srcFile))

                    File.Copy (srcFile, destFile, true)
                )

                let envJs =
                    Path.Combine (__SOURCE_DIRECTORY__, "docs", ".tool", "dist", "telplin", "env.js")

                File.Copy (envJs, Path.Combine (__SOURCE_DIRECTORY__, "docs", "env.js"))
            )
        }
    }

    stage "test" { run "dotnet test --no-restore --no-build -c Release" }

    stage "docs" { run "dotnet fsdocs build --nodefaultcontent --noapidocs" }
    runImmediate
}
