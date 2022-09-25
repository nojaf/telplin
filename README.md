# Telplin

![Logo](./Telplin.png)

It's a bit of secret for now.

## Scratch

dotnet build -v n /m:1 --no-incremental  | dotnet fsi .\script.fsx
dotnet build -v n /m:1 --no-incremental --no-dependencies | dotnet fsi ..\script.fsx
C:/Users/nojaf/Projects/telplin/src/Telplin/bin/Debug/net7.0/telplin.exe
dotnet build -bl --no-incremental ; C:/Users/nojaf/Projects/telplin/src/Telplin/bin/Debug/net7.0/telplin.exe .\msbuild.binlog --write


## Ideas

- If the entire signature is typed, just use it from untyped tree.
- Console runner
- Setting to skip the `private` nodes?
- Documentation limitations (conditional directives)

## TODO

- docs website
- keep a changelog
- license
- funding yml
- `--files` in ci tool
- Push to nuget
- Deploy lambda on commit to main

## Lamdbda

dotnet lambda package
dotnet publish --output "C:\Users\nojaf\Projects\telplin\src\Telplin.Lambda\bin\Release\net7.0\publish" --configuration "Release" --framework "net7.0" --self-contained true /p:GenerateRuntimeConfigurationFiles=true --runtime linux-x64

```http request
POST https://g8pt5e44zi.execute-api.eu-west-3.amazonaws.com/telplin-main-stage-230a206/telplin/signature
Content-Type: text/plain
```


Laurelin & Telperion

Telplin?