open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open Telplin.Core
open Telplin.Core.TypedTree.FSharpProjectExtensions
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
    (projectResults : FSharpCheckProjectResults)
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

        eprintfn "process: %s" sourceFile

        if not (File.Exists sourceFile) then
            eprintfn $"File \"%s{sourceFile}\" was skipped because it doesn't exist on disk."
            None
        else

        let code = File.ReadAllText sourceFile
        let sourceText = SourceText.ofString code

        let keepBinding =
            if arguments.OnlyUsed then
                TypedTree.Resolver.bindingsUsedElsewhere projectResults sourceFile
            else
                TypedTree.Resolver.keepEveryBinding

        let resolver =
            TypedTree.Resolver.mkResolverFor
                checker
                sourceFile
                sourceText
                projectOptions
                arguments.IncludePrivateBindings
                keepBinding

        let signature, errors = UntypedTree.Writer.mkSignatureFile resolver code
        Some (sourceFile, signature, errors)
    )
    |> Array.toList

/// The implementation without its redundant `private` keywords, and how many were removed. Each
/// range is one keyword on one line; the space after it goes with it.
let removePrivateKeywords (code : string) (ranges : FSharp.Compiler.Text.range list) : string * int =
    let newline = if code.Contains "\r\n" then "\r\n" else "\n"
    let lines = code.Split newline

    for range in List.sortByDescending (fun (r : FSharp.Compiler.Text.range) -> r.StartLine, r.StartColumn) ranges do
        let line = lines.[range.StartLine - 1]
        let after = range.EndColumn

        let after =
            if after < line.Length && line.[after] = ' ' then
                after + 1
            else
                after

        lines.[range.StartLine - 1] <- line.Substring (0, range.StartColumn) + line.Substring after

    String.Join (newline, lines), ranges.Length

/// The project a source file belongs to. The folders above the file are walked upwards; in each
/// one that holds project files, MSBuild is asked for their Compile items, so a file brought in
/// by a glob or an import counts too. The first folder with a project that has the file wins.
/// Two such projects is a tie, and the run says so rather than guess.
let findProjectOf (file : string) (additionalArguments : string) : Result<string, string> =
    let file = Path.GetFullPath file

    let rec walk (directory : string) : Result<string, string> =
        if isNull directory then
            Error $"No project file (.fsproj) above \"%s{file}\" has it as a Compile item."
        else

        let owners =
            Directory.GetFiles (directory, "*.fsproj")
            |> Array.sort
            |> Array.filter (fun project ->
                TypedTree.Options.compileItems project additionalArguments
                |> Async.RunSynchronously
                |> Array.contains file
            )

        match owners with
        | [||] -> walk (Path.GetDirectoryName directory)
        | [| project |] -> Ok project
        | candidates ->

        let listed = candidates |> Array.map (sprintf "  %s") |> String.concat "\n"

        Error
            $"\"%s{file}\" is a Compile item of more than one project, pass the project and --files instead:\n%s{listed}"

    walk (Path.GetDirectoryName file)

/// The project or response file a run is about, and the files it was asked for by way of the
/// input. A folder stands for the one project file in it; with none or several there is nothing
/// to pick, and the run says so rather than guess. A source file (.fs) stands for its project,
/// and is the one file to process.
let resolveInput (input : string) (additionalArguments : string) : Result<string * string list, string> =
    if File.Exists input then
        if input.EndsWith (".fs", StringComparison.OrdinalIgnoreCase) then
            findProjectOf input additionalArguments
            |> Result.map (fun project -> project, [ Path.GetFullPath input ])
        else
            Ok (input, [])
    elif Directory.Exists input then
        match Directory.GetFiles (input, "*.fsproj") |> Array.sort with
        | [| project |] -> Ok (project, [])
        | [||] -> Error $"There is no project file (.fsproj) in \"%s{input}\"."
        | projects ->
            let listed = projects |> Array.map (sprintf "  %s") |> String.concat "\n"
            Error $"\"%s{input}\" holds more than one project file, name the one to use:\n%s{listed}"
    else
        Error $"Input \"%s{input}\" does not exist."

let run (arguments : Arguments.Arguments) : int =
    let additionalArgs = String.concat " " arguments.MsBuildArguments

    match Option.map (fun input -> resolveInput input additionalArgs) arguments.Input with
    | None ->
        fail
            "No input was given. Pass a project file (.fsproj), its folder, a source file (.fs) of a project, or a response file (.rsp)."
    | Some (Error message) -> fail message
    | Some (Ok (input, inputFiles)) ->

    let arguments =
        { arguments with
            Files = inputFiles @ arguments.Files
        }

    let checker = FSharpChecker.Create ()
    let theme = forOutput ()

    let projectOptions =
        if input.EndsWith (".fsproj", StringComparison.Ordinal) then
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
    let projectResults =
        checker.ParseAndCheckProject projectOptions |> Async.RunSynchronously

    let existingErrors =
        projectResults.Diagnostics
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

    let signatures =
        generateSignatures checker projectOptions projectResults arguments sourceFiles

    // Every declaration Telplin left out of a signature, with the source it could not convert
    // underlined. The signature is still produced without it.
    for fileName, _, errors in signatures do
        if not errors.IsEmpty then
            let lines = File.ReadAllLines fileName

            for TelplinError (m, error) in errors do
                for line in Diagnostics.report theme fileName lines m error do
                    eprintfn "%s" line

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

        // With the signature in place, `private` on a binding it leaves out says nothing the
        // signature does not. Only after verification: an implementation is only edited when the
        // pair is known to compile.
        if verified && not arguments.KeepPrivate && not arguments.IncludePrivateBindings then
            for fileName, _, _ in signatures do
                let code = File.ReadAllText fileName

                match UntypedTree.Writer.redundantPrivateKeywords projectOptions.Defines code with
                | [] -> ()
                | ranges ->
                    let edited, count = removePrivateKeywords code ranges
                    File.WriteAllText (fileName, edited)

                    let keywords =
                        if count = 1 then
                            "1 private keyword"
                        else
                            $"%d{count} private keywords"

                    printfn "%s" (positive theme $"Removed %s{keywords} from %s{fileName}")

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
                    eprintfn
                        "%s"
                        (attention
                            theme
                            $"%s{Path.GetFileName path} is not listed in %s{projectName}: its implementation file is not a literal <Compile> item. Add it before the implementation file yourself.")
        else
            eprintfn "%s" (attention theme "The signature files are not listed in the project.")

            eprintfn
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
