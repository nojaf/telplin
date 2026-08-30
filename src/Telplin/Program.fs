open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
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

let formatDiagnostic (diagnostic : FSharpDiagnostic) : string =
    let severity =
        match diagnostic.Severity with
        | FSharpDiagnosticSeverity.Error -> "error"
        | FSharpDiagnosticSeverity.Warning -> "warning"
        | FSharpDiagnosticSeverity.Info
        | FSharpDiagnosticSeverity.Hidden -> "info"

    $"%s{diagnostic.FileName}(%d{diagnostic.StartLine},%d{diagnostic.StartColumn + 1}): %s{severity} FS%04d{diagnostic.ErrorNumber}: %s{diagnostic.Message}"

/// Which source file of the project a `--files` argument means. A path that exists as given, from
/// the working directory, wins. Otherwise it is matched against the end of the project's source
/// paths, so `Arguments.fs` or `Telplin/Arguments.fs` finds the file wherever the command runs.
let resolveFile (projectOptions : FSharpProjectOptions) (file : string) : Result<string, string> =
    if File.Exists file then
        Ok (Path.GetFullPath file)
    else

    let normalised = file.Replace ('\\', '/')

    let candidates =
        projectOptions.SourceFiles
        |> Array.filter (fun sourceFile ->
            let sourceFile = sourceFile.Replace ('\\', '/')

            sourceFile.EndsWith ("/" + normalised, StringComparison.Ordinal)
            || sourceFile = normalised
        )

    match candidates with
    | [| sourceFile |] -> Ok sourceFile
    | [||] -> Error $"\"%s{file}\" is not a file of the project, as given or relative to the project."
    | many ->
        let listed = many |> Array.map (sprintf "  %s") |> String.concat "\n"
        Error $"\"%s{file}\" matches more than one file of the project, give more of the path:\n%s{listed}"

/// The signatures that were asked for, generated in memory. Nothing is written yet.
let generateSignatures
    (checker : FSharpChecker)
    (projectOptions : FSharpProjectOptions)
    (arguments : Arguments.Arguments)
    (sourceFiles : string array)
    : (string * string * TelplinError list) list
    =
    // MSBuild adds these to the compilation from the intermediate folder. Nobody wrote them and
    // nobody wants a signature for them.
    let isGenerated (file : string) =
        file.EndsWith (".AssemblyInfo.fs", StringComparison.Ordinal)
        || file.EndsWith (".AssemblyAttributes.fs", StringComparison.Ordinal)

    sourceFiles
    |> Array.choose (fun sourceFile ->
        if
            not (sourceFile.EndsWith (".fs", StringComparison.Ordinal))
            || isGenerated sourceFile
        then
            None
        else

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

        let signature, errors = UntypedTree.Writer.mkSignatureFile resolver code
        Some (sourceFile, signature, errors)
    )
    |> Array.toList

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

    if arguments.OnlyRecord then
        0
    else

    // Telplin reads types off a project that compiles. One that does not has nothing reliable to
    // say about its own signatures, so this stops here rather than producing something partial.
    let existingErrors =
        let result = checker.ParseAndCheckProject projectOptions |> Async.RunSynchronously

        result.Diagnostics
        |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

    if not (Array.isEmpty existingErrors) then
        eprintfn
            "%s"
            (negative
                theme
                "The project does not compile. Telplin needs a project that compiles before it can generate signatures:")

        for diagnostic in existingErrors do
            eprintfn "  %s" (formatDiagnostic diagnostic)

        1
    else

    let requestedFiles =
        match arguments.Files with
        | [] -> Ok projectOptions.SourceFiles
        | files ->
            let resolved = List.map (resolveFile projectOptions) files

            match
                List.choose Result.toOption resolved,
                List.choose
                    (fun r ->
                        match r with
                        | Error e -> Some e
                        | Ok _ -> None
                    )
                    resolved
            with
            | files, [] -> Ok (List.toArray files)
            | _, errors -> Error (String.concat "\n" errors)

    match requestedFiles with
    | Error message -> fail message
    | Ok sourceFiles ->

    let signatures = generateSignatures checker projectOptions arguments sourceFiles

    for fileName, _, errors in signatures do
        if not errors.IsEmpty then
            eprintfn "%s" (negative theme $"Errors in %s{fileName}:")

            for TelplinError (m, error) in errors do
                eprintfn "%s" (negative theme $"%A{m}: %s{error}")

    // The whole project is checked once, with every new signature in front of its implementation.
    // Checking a file on its own is not enough: a signature hides what later files could see.
    let verified =
        if not arguments.Verify || signatures.IsEmpty then
            true
        else
            let pairs = signatures |> List.map (fun (file, signature, _) -> file, signature)

            match TypedTree.Resolver.typeCheckProjectWithSignatures projectOptions pairs with
            | [||] ->
                let count =
                    match signatures.Length with
                    | 1 -> "1 signature file"
                    | n -> $"%d{n} signature files"

                printfn "%s" (positive theme $"Verified: the project compiles with %s{count} in place.")
                true
            | diagnostics ->
                eprintfn "%s" (negative theme "The project does not compile with the new signatures in place:")

                for diagnostic in diagnostics do
                    eprintfn "  %s" (formatDiagnostic diagnostic)

                false

    if arguments.DryRun then
        for fileName, signature, _ in signatures do
            let length = fileName.Length + 4
            printfn "%s" (String.init length (fun _ -> "-"))
            printfn $"| %s{fileName} |"
            printfn "%s" (String.init length (fun _ -> "-"))
            printfn "%s" signature

        if verified then 0 else 1
    elif verified || arguments.Force then
        let signaturePaths =
            signatures
            |> List.map (fun (fileName, signature, _) ->
                let signaturePath = Path.ChangeExtension (fileName, ".fsi")
                File.WriteAllText (signaturePath, signature)
                printfn "%s" (positive theme $"Wrote %s{signaturePath}")
                signaturePath
            )

        let isProject = input.EndsWith (".fsproj", StringComparison.Ordinal)

        if isProject && arguments.UpdateProject then
            let text, outcomes = ProjectFile.addSignatures input signaturePaths
            let projectName = Path.GetFileName input

            if outcomes |> List.exists (fun o -> o.IsAdded) then
                File.WriteAllText (input, text)

            for outcome in outcomes do
                match outcome with
                | ProjectFile.Outcome.Added path ->
                    printfn "%s" (positive theme $"Listed %s{Path.GetFileName path} in %s{projectName}")
                | ProjectFile.Outcome.AlreadyListed _ -> ()
                | ProjectFile.Outcome.NotFound path ->
                    printfn
                        "%s"
                        (attention
                            theme
                            $"%s{Path.GetFileName path} is not listed in %s{projectName}: its implementation file is not a literal <Compile> item. Add it before the implementation file yourself.")
        else
            printfn "%s" (attention theme "The signature files are not listed in the project.")

            printfn
                "%s"
                (attention
                    theme
                    "List each one in the project file directly before its implementation file, as <Compile Include=\"File.fsi\" />.")

        if verified then 0 else 1
    else
        eprintfn "%s" (negative theme "No signature file was written. Pass --force to write them anyway.")
        1

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
