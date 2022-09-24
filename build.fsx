#r "nuget: Fun.Build, 0.1.6"

open System.IO
open Fun.Build

pipeline "Build" {
    envVars
        [
            "PERLA_API_ROOT", "https://g8pt5e44zi.execute-api.eu-west-3.amazonaws.com/telplin-main-stage-230a206"
        ]

    workingDir __SOURCE_DIRECTORY__

    stage "lint" {
        run "dotnet tool restore"
        run "dotnet fantomas . -r --check"
    }

    stage "build" {
        run "dotnet fsi ./docs/.style/style.fsx"
        run "dotnet restore ./telplin.sln"
        run "dotnet build --no-restore -c Release ./telplin.sln"

        stage "perla" {
            workingDir (Path.Combine (__SOURCE_DIRECTORY__, "docs", ".tool"))
            run "dotnet perla b"

            run (fun ctx ->
                let dist = Path.Combine (__SOURCE_DIRECTORY__, "docs", ".tool", "dist")

                System.IO.Directory.EnumerateFiles (dist, "*.*")
                |> Seq.iter (fun srcFile ->
                    let destFile =
                        Path.Combine (__SOURCE_DIRECTORY__, "docs", Path.GetFileName (srcFile))

                    File.Copy (srcFile, destFile, true)
                )
            )
        }
    }

    stage "test" { run "dotnet test --no-restore --no-build -c Release" }
    runImmediate
}
