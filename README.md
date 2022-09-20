# Autograph

It's a bit of secret for now.

## Scratch

dotnet build -v n /m:1 --no-incremental  | dotnet fsi .\script.fsx
dotnet build -v n /m:1 --no-incremental --no-dependencies | dotnet fsi ..\script.fsx
C:/Users/nojaf/Projects/autograph/src/Autograph/bin/Debug/net7.0/autograph.exe
dotnet build -bl --no-incremental ; C:/Users/nojaf/Projects/autograph/src/Autograph/bin/Debug/net7.0/autograph.exe .\msbuild.binlog --write

## Ideas

- Console runner
- Setting to skip the `private` nodes?
- Documentation limitations (conditional directives)