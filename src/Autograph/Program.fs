open System.IO
open Argu
open FSharp.Compiler.Text
open Autograph

type CliArguments =
    | [<MainCommand>] Binary_log of path : string
    | Single_File of path : string
    | Write

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Binary_log _ -> "Binary log file to process"
            | Single_File _ -> "Process a single file in the current project. File path should be absolute"
            | Write -> "Write signature files to disk"

module Seq =
    let tryChooseHead f = Seq.choose f >> Seq.tryHead

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<CliArguments> (programName = "autograph")
    let arguments = parser.Parse args

    let file = arguments.GetResult <@ Binary_log @>

    let projectOptions = TypedTree.Options.mkOptions file

    let signatures =
        match arguments.TryGetResult <@ Single_File @> with
        | Some singleFile ->
            let code = File.ReadAllText singleFile
            let sourceText = SourceText.ofString code
            let resolver = TypedTree.Resolver.mkResolverFor singleFile sourceText projectOptions
            let signature = UntypedTree.Writer.mkSignatureFile resolver code
            [| singleFile, signature |]
        | None ->

        projectOptions.SourceFiles
        |> Array.filter (fun file -> file.EndsWith ".fs")
        |> Array.map (fun sourceFile ->
            printfn "process: %s" sourceFile
            let code = File.ReadAllText sourceFile
            let sourceText = SourceText.ofString code
            let resolver = TypedTree.Resolver.mkResolverFor sourceFile sourceText projectOptions
            let signature = UntypedTree.Writer.mkSignatureFile resolver code
            sourceFile, signature
        )

    Array.iter
        (fun (fileName, signature) ->
            if arguments.Contains <@ Write @> then
                let signaturePath = Path.ChangeExtension (fileName, ".fsi")
                File.WriteAllText (signaturePath, signature)
            else
                printfn $"| %s{fileName} |"
                printfn "%s" (String.init 100 (fun _ -> "-"))
                printfn "%s" signature
        )
        signatures

    0
