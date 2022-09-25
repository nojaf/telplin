#r "nuget: Fun.Build, 0.1.6"

open Fun.Build

let (</>) a b = System.IO.Path.Combine (a, b)

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

    runImmediate
}
