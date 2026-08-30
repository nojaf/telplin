open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Telplin.Core
open Telplin
open Telplin.Theme

/// A complaint goes to standard error, with a pointer to the page that lists what is accepted.
let fail (message : string) : int =
    let theme = forOutput ()
    eprintfn "%s" (negative theme message)
    eprintfn "Run '%s --help' to see the flags Telplin accepts." (HelpPage.invocation ())
    1

let run (arguments : Arguments.Arguments) : int =
    match arguments.Input with
    | None -> fail "No input was given. Pass a project file (.fsproj) or a response file (.rsp)."
    | Some input when not (File.Exists input) -> fail $"Input \"%s{input}\" does not exist."
    | Some input ->

    let checker = FSharpChecker.Create ()
    let theme = forOutput ()

    let projectOptions =
        if input.EndsWith (".fsproj", StringComparison.Ordinal) then
            let additionalArgs = String.concat " " arguments.MsBuildArguments

            if arguments.OnlyRecord then
                TypedTree.Options.mkOptionsFromDesignTimeBuildWithoutReferences input additionalArgs
            else
                TypedTree.Options.mkOptionsFromDesignTimeBuild input additionalArgs
            |> Async.RunSynchronously
        else
            TypedTree.Options.mkOptionsFromResponseFile input

    if arguments.Record || arguments.OnlyRecord then
        let responseFile = Path.ChangeExtension (input, ".rsp")

        let args =
            seq {
                yield! projectOptions.OtherOptions
                yield! projectOptions.SourceFiles
            }

        File.WriteAllLines (responseFile, args)
        printfn $"Wrote compiler arguments to %s{responseFile}"

    if not arguments.OnlyRecord then
        let signatureResults =
            let sourceFiles =
                match arguments.Files with
                | [] -> projectOptions.SourceFiles
                | files -> List.map Path.GetFullPath files |> List.toArray

            sourceFiles
            |> Array.filter (fun file -> file.EndsWith (".fs", StringComparison.Ordinal))
            |> Array.choose (fun sourceFile ->
                printfn "process: %s" sourceFile

                if not (File.Exists sourceFile) then
                    printfn $"File \"%s{sourceFile}\" was skipped because it doesn't exist on disk."
                    None
                else

                let code = File.ReadAllText sourceFile
                let sourceText = SourceText.ofString code

                let resolver =
                    TypedTree.Resolver.mkResolverFor
                        checker
                        sourceFile
                        sourceText
                        projectOptions
                        arguments.IncludePrivateBindings

                let signature = UntypedTree.Writer.mkSignatureFile resolver code
                Some (sourceFile, signature)
            )

        for fileName, (signature, errors) in signatureResults do
            if not errors.IsEmpty then
                eprintfn "%s" (negative theme $"Errors in %s{fileName}:")

                for TelplinError (m, error) in errors do
                    eprintfn "%s" (negative theme $"%A{m}: %s{error}")

            if arguments.DryRun then
                let length = fileName.Length + 4
                printfn "%s" (String.init length (fun _ -> "-"))
                printfn $"| %s{fileName} |"
                printfn "%s" (String.init length (fun _ -> "-"))
                printfn "%s" signature
            else
                let signaturePath = Path.ChangeExtension (fileName, ".fsi")
                File.WriteAllText (signaturePath, signature)

    0

[<EntryPoint>]
let main args =
    match Arguments.parse args with
    | Error message -> fail message
    | Ok arguments when arguments.Help ->
        HelpPage.print ()
        0
    | Ok arguments when arguments.Version ->
        printfn "%s" (HelpPage.version ())
        0
    | Ok arguments -> run arguments
