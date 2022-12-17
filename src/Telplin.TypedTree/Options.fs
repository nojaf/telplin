module Telplin.TypedTree.Options

open System
open System.IO
open Microsoft.Build.Logging.StructuredLogger
open FSharp.Compiler.CodeAnalysis

let fsharpFiles = set [| ".fs" ; ".fsi" ; ".fsx" |]

let isFSharpFile (file : string) =
    Seq.exists (fun (ext : string) -> file.EndsWith ext) fsharpFiles

let readCompilerArgsFromBinLog file =
    let build = BinaryLog.ReadBuild file

    let projectName =
        build.Children
        |> Seq.choose (
            function
            | :? Project as p -> Some p.Name
            | _ -> None
        )
        |> Seq.distinct
        |> Seq.exactlyOne

    let message (fscTask : FscTask) =
        fscTask.Children
        |> Seq.tryPick (
            function
            | :? Message as m when m.Text.Contains "fsc" -> Some m.Text
            | _ -> None
        )

    let mutable args = None

    build.VisitAllChildren<Task> (fun task ->
        match task with
        | :? FscTask as fscTask ->
            match fscTask.Parent.Parent with
            | :? Project as p when p.Name = projectName -> args <- message fscTask
            | _ -> ()
        | _ -> ()
    )

    match args with
    | None -> failwith $"Could not parse binlog at {file}"
    | Some args ->
        let idx = args.IndexOf "-o:"
        args.Substring(idx).Split [| '\n' |]

let mkOptions binLogPath =
    let compilerArgs = readCompilerArgsFromBinLog binLogPath

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
