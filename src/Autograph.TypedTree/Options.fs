module Autograph.TypedTree.Options

open System
open System.IO
open Microsoft.Build.Logging.StructuredLogger
open FSharp.Compiler.CodeAnalysis
open Autograph.OptionCE

module Seq =
    let tryChooseHead f = Seq.choose f >> Seq.tryHead

let fsharpFiles = set [| ".fs" ; ".fsi" ; ".fsx" |]

let isFSharpFile (file : string) =
    Seq.exists (fun (ext : string) -> file.EndsWith ext) fsharpFiles

let readCompilerArgsFromBinLog file =
    let build = BinaryLog.ReadBuild file

    let rebuild (build : Build) =
        build.Children
        |> Seq.tryChooseHead (
            function
            | :? Project as p when (p.TargetsText = "Rebuild") -> Some p
            | _ -> None
        )

    let coreCompile (project : Project) =
        project.Children
        |> Seq.tryChooseHead (
            function
            | :? Target as t when (t.Name = "CoreCompile") -> Some t
            | _ -> None
        )

    let fscTask (target : Target) =
        target.Children
        |> Seq.tryChooseHead (
            function
            | :? FscTask as fsc -> Some fsc
            | _ -> None
        )

    let message (fscTask : FscTask) =
        fscTask.Children
        |> Seq.tryChooseHead (
            function
            | :? Message as m -> Some m.Text
            | _ -> None
        )

    option {
        let! project = rebuild build
        let! target = coreCompile project
        let! fscTask = fscTask target
        let! message = message fscTask
        let idx = message.IndexOf "-o:"
        return message.Substring(idx).Split [| '\n' |]
    }

let mkOptions binLogPath =
    let compilerArgs =
        match readCompilerArgsFromBinLog binLogPath with
        | None -> failwith $"Could not parse binlog at {binLogPath}"
        | Some args -> args

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
