#!/usr/bin/env -S dotnet fsi
// FSharpProjectOptions for a single in-memory file, using the reference assemblies in `reference/`.
// Same setup as the tests in Telplin.Core.Tests.
module Options

#r "nuget: FSharp.Compiler.Service, 43.12.400"

open System
open System.IO
open FSharp.Compiler.CodeAnalysis

let projectOptions : FSharpProjectOptions =
    let resolvedAssemblies =
        Path.Combine (__SOURCE_DIRECTORY__, "..", "reference")
        |> Directory.EnumerateFiles
        |> Seq.map (sprintf "-r:%s")
        |> Seq.toArray

    {
        ProjectFileName = "A"
        ProjectId = None
        SourceFiles = [| "A.fs" |]
        OtherOptions =
            [|
                "-g"
                "--debug:portable"
                "--noframework"
                "--define:TRACE"
                "--define:DEBUG"
                "--define:NET"
                "--define:NETCOREAPP"
                "--define:NET5_0_OR_GREATER"
                "--define:NET6_0_OR_GREATER"
                "--define:NET7_0_OR_GREATER"
                "--optimize-"
                "--tailcalls-"
                yield! resolvedAssemblies
                "--target:library"
                "--nowarn:IL2121"
                "--warn:3"
                "--warnaserror:3239,FS0025"
                "--fullpaths"
                "--flaterrors"
                "--highentropyva+"
                "--targetprofile:netcore"
                "--nocopyfsharpcore"
                "--deterministic+"
                "--simpleresolution"
            |]
        ReferencedProjects = [||]
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = false
        LoadTime = DateTime.UtcNow
        UnresolvedReferences = None
        OriginalLoadReferences = []
        Stamp = None
    }

/// Add `--define:X` for every define.
let withDefines (defines : string list) (options : FSharpProjectOptions) =
    let extra = defines |> List.map (sprintf "--define:%s") |> List.toArray

    { options with
        OtherOptions = Array.append options.OtherOptions extra
    }
