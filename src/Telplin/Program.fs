open System.IO
open Argu
open CliWrap
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Telplin

type CliArguments =
    | [<MainCommand>] FsProj of path : string
    | Files of path : string list
    | Write

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | FsProj _ -> "FSharp project file (.fsproj) to process. This project will be build first by Telplin."
            | Files _ -> "Process a subset of files in the current project."
            | Write ->
                "Write signature files to disk. By default, Telplin will only print the signatures to the console."

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<CliArguments> (programName = "Telplin")
    let arguments = parser.Parse (args, raiseOnUsage = false)

    if arguments.IsUsageRequested then
        parser.PrintUsage (programName = "Telplin") |> printfn "%s"
        exit 0
    else

    let checker = FSharpChecker.Create ()
    let fsProj = arguments.GetResult <@ FsProj @>

    if not (File.Exists fsProj) then
        printfn $"FSharp project \"%s{fsProj}\" does not exist."
        exit 1

    let binaryLog =
        let folder = FileInfo(fsProj).DirectoryName
        printfn $"Building %s{fsProj}..."

        Cli
            .Wrap("dotnet")
            .WithArguments($"build \"{fsProj}\" -bl:telplin.binlog --no-incremental")
            .WithValidation(CommandResultValidation.None)
            .ExecuteAsync()
            .Task.Result
        |> ignore

        Path.Combine (folder, "telplin.binlog")

    let projectOptions = TypedTree.Options.mkOptions binaryLog

    if File.Exists binaryLog then
        File.Delete binaryLog

    let signatures =
        let sourceFiles =
            match arguments.TryGetResult <@ Files @> with
            | None -> projectOptions.SourceFiles
            | Some files -> List.map Path.GetFullPath files |> List.toArray

        sourceFiles
        |> Array.filter (fun file -> file.EndsWith ".fs")
        |> Array.map (fun sourceFile ->
            printfn "process: %s" sourceFile
            let code = File.ReadAllText sourceFile
            let sourceText = SourceText.ofString code

            let resolver =
                TypedTree.Resolver.mkResolverFor checker sourceFile sourceText projectOptions

            let signature = UntypedTree.Writer.mkSignatureFile resolver code
            sourceFile, signature
        )

    Array.iter
        (fun (fileName, signature) ->
            if arguments.Contains <@ Write @> then
                let signaturePath = Path.ChangeExtension (fileName, ".fsi")
                File.WriteAllText (signaturePath, signature)
            else
                let length = fileName.Length + 4
                printfn "%s" (String.init length (fun _ -> "-"))
                printfn $"| %s{fileName} |"
                printfn "%s" (String.init length (fun _ -> "-"))
                printfn "%s" signature
        )
        signatures

    0
