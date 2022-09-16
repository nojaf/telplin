open System
open System.IO
open Argu
open Autograph
open FSharp.Compiler.Text

let helpMessage =
    """
This project expects the output of msbuild to be piped into the stdin.
The output of msbuild will be used to collect the fsharp compiler options.

dotnet build -v n /m:1 --no-incremental --no-dependencies | dotnet autograph

Build your project with:
/m:1                    => Runs msbuild using a single core
--no-incremental        => Rebuild
--no-dependencies       => Don't build any other projects
"""

type CliArguments =
    | Store
    | Single_File of path : string
    | Load of path : string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Store -> "Stored the current stdin scrapping"
            | Load _ -> "Load compiler arguments from text file"
            | Single_File _ -> "Process a single file in the current project. File path should be absolute"

let isPipedInput = Console.IsInputRedirected

let isDryRun = true

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<CliArguments> (programName = "autograph")
    let arguments = parser.Parse args

    if not isPipedInput && not (arguments.Contains Load) then
        Console.WriteLine helpMessage
        Console.WriteLine parser.HelpDescription
        1
    else

    let compilerLines =
        match arguments.TryGetResult <@ CliArguments.Load @> with
        | Some path -> File.ReadAllLines path
        | None ->
            let lines = Scrape.scrape ()

            if arguments.Contains Store then
                let path = Path.Combine (Directory.GetCurrentDirectory (), "autograph-input.txt")
                File.WriteAllLines (path, lines)

            lines

    let projectOptions = TypedTree.Options.mkOptions compilerLines

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
        |> Array.skip 1
        |> Array.map (fun sourceFile ->
            printfn "process: %s" sourceFile
            let code = File.ReadAllText sourceFile
            let sourceText = SourceText.ofString code
            let resolver = TypedTree.Resolver.mkResolverFor sourceFile sourceText projectOptions
            let signature = UntypedTree.Writer.mkSignatureFile resolver code
            sourceFile, signature
        )

    if isDryRun then
        Array.iter
            (fun (fileName, signature) ->
                printfn $"| %s{fileName} |"
                printfn "%s" (String.init 100 (fun _ -> "-"))
                printfn "%s" signature
            )
            signatures

    0
