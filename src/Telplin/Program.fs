open System.IO
open Argu
open CliWrap
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Telplin

let private meh = 5

type CliArguments =
    | [<MainCommand>] Input of path : string
    | Files of path : string list
    | Dry_Run
    | Record
    | Only_Record
    | Include_Private_Bindings

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Input _ ->
                """FSharp project file (.fsproj), binary log (.binlog) or response file (.rsp) to process. An fsproj will be build first by Telplin.
Additional build arguments for the .fsproj can be passed after the --.
Example: telplin MyProject.fsproj -- -c Release
"""
            | Files _ -> "Process a subset of files in the current project."
            | Dry_Run -> "Don't write signature files to disk. Only print the signatures to the console."
            | Record ->
                "Create a response file containing compiler arguments that can be used as an alternative input to the *.fsproj file, thus avoiding the need for a full project rebuild. The response file will be saved as a *.rsp file."
            | Only_Record ->
                "Alternative option for --record. Only create an *.rsp file without processing any of the files."
            | Include_Private_Bindings _ -> "Include private bindings in the signature file."

[<EntryPoint>]
let main args =
    let arguArgs, additionalArgs =
        let dashDashIdx = Array.tryFindIndex (fun arg -> arg = "--") args

        match dashDashIdx with
        | None -> args, Array.empty
        | Some idx -> args.[0 .. (idx - 1)], args.[(idx + 1) ..]

    let parser =
        ArgumentParser.Create<CliArguments> (programName = "Telplin", errorHandler = ProcessExiter ())

    let arguments = parser.Parse (arguArgs, raiseOnUsage = false)

    if arguments.IsUsageRequested then
        parser.PrintUsage (programName = "Telplin") |> printfn "%s"
        exit 0
    else

    let checker = FSharpChecker.Create ()
    let record = arguments.Contains <@ Record @>
    let onlyRecord = arguments.Contains <@ Only_Record @>
    let input = arguments.GetResult <@ Input @>

    if not (File.Exists input) then
        printfn $"Input \"%s{input}\" does not exist."
        exit 1

    let projectOptions =
        if input.EndsWith ".fsproj" then
            let binaryLog =
                let folder = FileInfo(input).DirectoryName
                let additionalArgs = String.concat " " additionalArgs
                printfn $"Building %s{input}..."

                Cli
                    .Wrap("dotnet")
                    .WithArguments($"build \"{input}\" -bl:telplin.binlog --no-incremental {additionalArgs}")
                    .WithValidation(CommandResultValidation.None)
                    .ExecuteAsync()
                    .Task.Result
                |> ignore

                Path.Combine (folder, "telplin.binlog")

            let options = TypedTree.Options.mkOptionsFromBinaryLog binaryLog

            if File.Exists binaryLog then
                File.Delete binaryLog

            options
        elif input.EndsWith ".binlog" then
            TypedTree.Options.mkOptionsFromBinaryLog input
        else
            TypedTree.Options.mkOptionsFromResponseFile input

    if record || onlyRecord then
        let responseFile = Path.ChangeExtension (input, ".rsp")

        let args =
            seq {
                yield! projectOptions.OtherOptions
                yield! projectOptions.SourceFiles
            }

        File.WriteAllLines (responseFile, args)
        printfn $"Wrote compiler arguments to %s{responseFile}"

    if not onlyRecord then
        let signatures =
            let sourceFiles =
                match arguments.TryGetResult <@ Files @> with
                | None -> projectOptions.SourceFiles
                | Some files -> List.map Path.GetFullPath files |> List.toArray

            sourceFiles
            |> Array.filter (fun file -> file.EndsWith ".fs")
            |> Array.choose (fun sourceFile ->
                printfn "process: %s" sourceFile

                if not (File.Exists sourceFile) then
                    printfn $"File \"%s{sourceFile}\" was skipped because it doesn't exist on disk."
                    None
                else

                let code = File.ReadAllText sourceFile
                let sourceText = SourceText.ofString code
                let includePrivateBindings = arguments.Contains <@ Include_Private_Bindings @>

                let resolver =
                    TypedTree.Resolver.mkResolverFor
                        checker
                        sourceFile
                        sourceText
                        projectOptions
                        includePrivateBindings

                let signature = UntypedTree.Writer.mkSignatureFile resolver code
                Some (sourceFile, signature)
            )

        for fileName, signature in signatures do
            if arguments.Contains <@ Dry_Run @> then
                let length = fileName.Length + 4
                printfn "%s" (String.init length (fun _ -> "-"))
                printfn $"| %s{fileName} |"
                printfn "%s" (String.init length (fun _ -> "-"))
                printfn "%s" signature
            else
                let signaturePath = Path.ChangeExtension (fileName, ".fsi")
                File.WriteAllText (signaturePath, signature)

    0
