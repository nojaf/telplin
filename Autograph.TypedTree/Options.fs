module Autograph.TypedTree.Options

open System
open System.IO
open FSharp.Compiler.CodeAnalysis

let fsharpFiles = set [| ".fs" ; ".fsi" ; ".fsx" |]

let isFSharpFile (file : string) =
    Seq.exists (fun (ext : string) -> file.EndsWith ext) fsharpFiles

let mkOptions compilerArgs =
    let sourceFiles =
        compilerArgs
        |> Array.filter (fun (line : string) -> isFSharpFile line && File.Exists line)

    let otherOptions =
        compilerArgs |> Array.filter (fun line -> not (isFSharpFile line))

    {
        ProjectFileName = "Project"
        ProjectId = None
        SourceFiles = sourceFiles
        OtherOptions = otherOptions
        ReferencedProjects = [||]
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = false
        LoadTime = DateTime.Now
        UnresolvedReferences = None
        OriginalLoadReferences = []
        Stamp = None
    }
