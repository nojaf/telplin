#r "nuget: Fun.Build, 0.1.6"

open Fun.Build

pipeline "Watch" {
    envVars [ "PERLA_API_ROOT", "http://127.0.0.1:8906" ]
    workingDir __SOURCE_DIRECTORY__
    stage "main" {
        paralle
        run "dotnet fsi ./docs/.style/style.fsx --watch"
        run "dotnet run --project ./src/Telplin.Lambda/Telplin.Lambda.fsproj"
        stage "perla" {
            workingDir (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "docs", ".tool"))
            run "dotnet perla s"
        }
    }
    runImmediate
}