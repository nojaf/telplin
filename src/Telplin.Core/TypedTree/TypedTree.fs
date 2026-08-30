module Telplin.Core.TypedTree.Resolver

#nowarn "57"

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Collections.Concurrent
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open Telplin.Core.TypedTree.FSharpProjectExtensions

/// FCS prints a `DefaultParameterValue` whose value is the default of a struct as `(default :> obj)`,
/// which does not parse. The parameter attributes from the source are used in the end,
/// so the text only needs to be parseable.
let sanitizeSignatureText (sigText : string) : string =
    sigText.Replace ("(default :> obj)", "null")

/// FCS qualifies a type declared in a module with `ModuleSuffix` (explicit, or implicit when a type
/// has the same name as the module) with the display name of that module, `Telplin.V`, even from
/// inside that module where the name `Telplin` does not resolve. Strip that qualifier for every
/// enclosing module with a suffix. See https://github.com/nojaf/telplin/issues/71
let stripModuleSuffixQualifiers (mfv : FSharpMemberOrFunctionOrValue) (sigText : string) : string =
    let rec enclosingModulesWithSuffix (entity : FSharpEntity option) =
        match entity with
        | None -> []
        | Some entity ->

        let rest = enclosingModulesWithSuffix entity.DeclaringEntity

        if entity.IsFSharpModule && entity.HasFSharpModuleSuffix then
            entity.DisplayName :: rest
        else
            rest

    (sigText, enclosingModulesWithSuffix mfv.DeclaringEntity)
    ||> List.fold (fun text name ->
        // Only a leading segment: not `Other.Telplin.V`, and not `MyTelplin.V`.
        let pattern = $"(?<![\\w.`]){Regex.Escape name}\\."
        Regex.Replace (text, pattern, "")
    )

type ISourceText with

    member x.GetContentAt (range : range) : string =
        let startLine = range.StartLine - 1
        let line = x.GetLineString startLine

        if range.StartLine = range.EndLine then
            let length = range.EndColumn - range.StartColumn
            line.Substring (range.StartColumn, length)
        else

        let firstLineContent = line.Substring range.StartColumn
        let sb = StringBuilder().AppendLine firstLineContent

        for lineNumber in [ range.StartLine .. range.EndLine - 2 ] do
            sb.AppendLine (x.GetLineString lineNumber) |> ignore

        let lastLine = x.GetLineString (range.EndLine - 1)
        sb.Append(lastLine.Substring (0, range.EndColumn)).ToString()

let fileCache = ConcurrentDictionary<string, ISourceText>()

let documentSource fileName =
    async {
        match fileCache.TryGetValue fileName with
        | true, sourceText -> return Some sourceText
        | false, _ -> return None
    }

let inMemoryChecker =
    FSharpChecker.Create (documentSource = DocumentSource.Custom documentSource)

type TypedTreeInfoResolver
    (
        defines,
        includePrivateBindings,
        keepBinding : range -> bool,
        sourceText : ISourceText,
        checkFileResults : FSharpCheckFileResults
    )
    =
    member val Defines = defines
    member val IncludePrivateBindings = includePrivateBindings
    member _.KeepBinding (nameRange : range) : bool = keepBinding nameRange

    member _.GetValText (name, range : range, predicate) =
        try
            let line = sourceText.GetLineString (range.StartLine - 1)

            let symbolUseInfo =
                match predicate with
                | None ->
                    checkFileResults.GetSymbolUseAtLocation (range.StartLine, range.EndColumn, line, [ name ])
                    |> Option.bind (fun symbolUse ->
                        match symbolUse.Symbol with
                        | :? FSharpMemberOrFunctionOrValue as mfv ->
                            Some (symbolUse.DisplayContext, symbolUse.Range, mfv)
                        | _ -> None
                    )
                | Some predicate ->
                    let symbolUses =
                        checkFileResults.GetSymbolUsesAtLocation (range.StartLine, range.EndColumn, line, [ name ])

                    symbolUses
                    |> List.tryPick (fun symbolUse ->
                        match symbolUse.Symbol with
                        | :? FSharpMemberOrFunctionOrValue as mfv when predicate mfv ->
                            Some (symbolUse.DisplayContext, symbolUse.Range, mfv)
                        | _ -> None
                    )

            match symbolUseInfo with
            | None -> Error "No FSharpMemberOrFunctionOrValue found"
            | Some (displayContext, m, mfv) ->
                let sigTextOpt = mfv.GetValSignatureText (displayContext, m)

                match sigTextOpt with
                | None -> Error $"No sig text for %A{mfv}"
                | Some sigText -> Ok (sigText |> sanitizeSignatureText |> stripModuleSuffixQualifiers mfv)

        with ex ->
            Error ex.Message

    member _.GetValTextForConstructor (range : range) =
        try
            let line = sourceText.GetLineString (range.StartLine - 1)

            let valText =
                checkFileResults.GetSymbolUsesAtLocation (range.StartLine, range.EndColumn, line, [ ".ctor" ])
                |> List.tryPick (fun symbolUse ->
                    match symbolUse.Symbol with
                    | :? FSharpMemberOrFunctionOrValue as mfv when mfv.CompiledName = ".ctor" ->
                        mfv.GetValSignatureText (symbolUse.DisplayContext, symbolUse.Range)
                        |> Option.map (sanitizeSignatureText >> stripModuleSuffixQualifiers mfv)
                    | _ -> None
                )

            match valText with
            | None -> Error "No FSharpMemberOrFunctionOrValue was found for .ctor"
            | Some valText -> Ok valText

        with ex ->
            Error ex.Message

    member _.IsStructWithoutComparison (range : range) =
        try
            let line = sourceText.GetLineString (range.StartLine - 1)
            let name = sourceText.GetContentAt range

            let allSymbols =
                checkFileResults.GetSymbolUsesAtLocation (range.StartLine, range.EndColumn, line, [ name ])

            let entityOpt =
                allSymbols
                |> List.tryPick (fun symbolUse ->
                    match symbolUse.Symbol with
                    | :? FSharpEntity as entity -> Some entity
                    | _ -> None
                )

            match entityOpt with
            | None -> Error "No FSharpEntity was found"
            | Some entity ->

            let doesNotHaveIComparable () =
                let hasIComparable =
                    entity.DeclaredInterfaces
                    |> Seq.exists (fun i -> i.TypeDefinition.FullName = "System.IComparable")

                let hasIComparableOfT =
                    entity.DeclaredInterfaces
                    |> Seq.exists (fun i -> i.TypeDefinition.FullName = "System.IComparable`1")

                not (hasIComparable && hasIComparableOfT)

            Ok (entity.IsValueType && not entity.IsEnum && doesNotHaveIComparable ())

        with ex ->
            Error ex.Message

/// Every binding is kept.
let keepEveryBinding (_ : range) : bool = true

/// The module-level bindings of `sourceFile` that some other file of the project uses, as a test
/// on the range of a binding's name. What is asked is whether a use exists outside the file: a
/// binding only its own file calls has no reason to be in the signature, omitting it makes it
/// private. Members and types are not looked at, they are always kept.
let bindingsUsedElsewhere (projectResults : FSharpCheckProjectResults) (sourceFile : string) : range -> bool =
    let sameFile (a : string) (b : string) =
        String.Equals (Path.GetFullPath a, Path.GetFullPath b, StringComparison.OrdinalIgnoreCase)

    // Where a symbol is declared in this file, if it is. When the file already has a signature,
    // the declaration location is in that signature; the implementation is where the name ranges
    // being compared to come from.
    let implementationLocation (symbol : FSharpSymbol) : range option =
        symbol.ImplementationLocation
        |> Option.orElse symbol.DeclarationLocation
        |> Option.filter (fun location -> sameFile location.FileName sourceFile)

    let declarations =
        projectResults.GetAllUsesOfAllSymbols ()
        |> Array.choose (fun symbolUse ->
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue as mfv when
                mfv.IsModuleValueOrMember
                && not mfv.IsMember
                && not symbolUse.IsFromDefinition
                && not (sameFile symbolUse.FileName sourceFile)
                ->
                implementationLocation mfv
            // A match on `A.Even` is a use of the case, not of the binding that defines it. The
            // case is declared inside the binding's name, `(|Even|Odd|)`, so it matches the same way.
            | :? FSharpActivePatternCase as case when
                not symbolUse.IsFromDefinition && not (sameFile symbolUse.FileName sourceFile)
                ->
                implementationLocation case
            | _ -> None
        )

    fun (nameRange : range) ->
        // The declaration range of a value is its identifier. Matching on the line and on the
        // column falling inside the name is forgiving of the bars around an active pattern.
        declarations
        |> Array.exists (fun declaration ->
            declaration.StartLine = nameRange.StartLine
            && declaration.StartColumn >= nameRange.StartColumn
            && declaration.StartColumn <= nameRange.EndColumn
        )

let mkResolverFor
    (checker : FSharpChecker)
    sourceFileName
    sourceText
    projectOptions
    includePrivateBindings
    (keepBinding : range -> bool)
    =
    let _, checkFileAnswer =
        checker.ParseAndCheckFileInProject (sourceFileName, 1, sourceText, projectOptions)
        |> Async.RunSynchronously

    match checkFileAnswer with
    | FSharpCheckFileAnswer.Succeeded checkFileResults ->
        let firstErrorDiag =
            checkFileResults.Diagnostics
            |> Array.tryFind (fun diag -> diag.Severity = FSharpDiagnosticSeverity.Error)

        match firstErrorDiag with
        | Some diag ->
            failwithf $"Type-checking %s{projectOptions.ProjectFileName} lead to errors. The first one being %A{diag}"
        | None ->
            TypedTreeInfoResolver (
                projectOptions.Defines,
                includePrivateBindings,
                keepBinding,
                sourceText,
                checkFileResults
            )
    | FSharpCheckFileAnswer.Aborted -> failwith $"type checking aborted for %s{sourceFileName}"

let mkResolverForCode projectOptions (includePrivateBindings : bool) (code : string) : TypedTreeInfoResolver =
    let sourceFileName = "A.fs"

    let projectOptions : FSharpProjectOptions =
        { projectOptions with
            SourceFiles = [| sourceFileName |]
        }

    let sourceText = SourceText.ofString code

    mkResolverFor inMemoryChecker sourceFileName sourceText projectOptions includePrivateBindings keepEveryBinding

/// A resolver for the first file of a small in-memory project, keeping only the bindings the other
/// files use. This is the seam the tests use for `--only-used`; the console application computes
/// the same from the project on disk.
let mkResolverForCodeOnlyUsed
    projectOptions
    (includePrivateBindings : bool)
    (code : string)
    (otherFiles : (string * string) list)
    : TypedTreeInfoResolver
    =
    let sourceFileName = "A.fs"

    // An existing signature of the file, `A.fsi` among the other files, has to come before it.
    let signature, otherFiles =
        List.partition (fun (name : string, _) -> name = "A.fsi") otherFiles

    let files = signature @ (sourceFileName, code) :: otherFiles

    for name, content in files do
        fileCache.[name] <- SourceText.ofString content

    let projectOptions : FSharpProjectOptions =
        { projectOptions with
            SourceFiles = files |> List.map fst |> List.toArray
        }

    let projectResults =
        inMemoryChecker.ParseAndCheckProject projectOptions |> Async.RunSynchronously

    let keepBinding = bindingsUsedElsewhere projectResults sourceFileName

    let resolver =
        mkResolverFor
            inMemoryChecker
            sourceFileName
            (SourceText.ofString code)
            projectOptions
            includePrivateBindings
            keepBinding

    for name, _ in files do
        fileCache.TryRemove name |> ignore

    resolver

let filterDiagnostics diagnostics =
    diagnostics
    |> Array.filter (fun (d : FSharpDiagnostic) ->
        match d.Severity with
        | FSharpDiagnosticSeverity.Error
        | FSharpDiagnosticSeverity.Warning -> true
        | FSharpDiagnosticSeverity.Info
        | FSharpDiagnosticSeverity.Hidden -> false
    )

let typeCheckForImplementation projectOptions sourceCode =
    let projectOptions : FSharpProjectOptions =
        { projectOptions with
            SourceFiles = [| "A.fs" |]
        }

    let _, result =
        inMemoryChecker.ParseAndCheckFileInProject ("A.fs", 1, SourceText.ofString sourceCode, projectOptions)
        |> Async.RunSynchronously

    match result with
    | FSharpCheckFileAnswer.Aborted -> Choice1Of2 ()
    | FSharpCheckFileAnswer.Succeeded checkFileResults -> filterDiagnostics checkFileResults.Diagnostics |> Choice2Of2

let typeCheckForPair projectOptions implementation signature =
    let fileName = System.Guid.NewGuid().ToString "N"
    let signatureName = $"%s{fileName}.fsi"
    let implementationName = $"%s{fileName}.fs"

    fileCache.TryAdd (signatureName, SourceText.ofString signature) |> ignore

    let projectOptions : FSharpProjectOptions =
        { projectOptions with
            SourceFiles = [| signatureName ; implementationName |]
        }

    let _, signatureCheckResult =
        inMemoryChecker.ParseAndCheckFileInProject (signatureName, 0, SourceText.ofString signature, projectOptions)
        |> Async.RunSynchronously

    let _, implementationCheckResult =
        inMemoryChecker.ParseAndCheckFileInProject (
            implementationName,
            0,
            SourceText.ofString implementation,
            projectOptions
        )
        |> Async.RunSynchronously

    fileCache.TryRemove signatureName |> ignore
    fileCache.TryRemove implementationName |> ignore

    [|
        match signatureCheckResult with
        | FSharpCheckFileAnswer.Aborted -> ()
        | FSharpCheckFileAnswer.Succeeded checkFileResults -> yield! checkFileResults.Diagnostics

        match implementationCheckResult with
        | FSharpCheckFileAnswer.Aborted -> ()
        | FSharpCheckFileAnswer.Succeeded checkFileResults -> yield! checkFileResults.Diagnostics
    |]
    |> filterDiagnostics

/// Type check the whole project with each signature placed in front of its implementation file.
/// The signatures are served from memory, every other file is read from disk. Only errors are
/// returned: a warning the project already had is not something the signatures introduced.
let typeCheckProjectWithSignatures (projectOptions : FSharpProjectOptions) (signatures : (string * string) list) =
    let signaturePaths =
        signatures
        |> List.map (fun (implementation, signature) ->
            let signaturePath = Path.ChangeExtension (implementation, ".fsi")
            fileCache.[signaturePath] <- SourceText.ofString signature
            implementation, signaturePath
        )
        |> Map.ofList

    let provided = signaturePaths |> Map.values |> Set.ofSeq

    let sourceFiles =
        projectOptions.SourceFiles
        |> Array.collect (fun file ->
            if provided.Contains file then
                // Already listed in the project, it is placed again in front of its implementation.
                [||]
            else
                match Map.tryFind file signaturePaths with
                | Some signaturePath -> [| signaturePath ; file |]
                | None -> [| file |]
        )

    let projectOptions =
        { projectOptions with
            SourceFiles = sourceFiles
        }

    let result =
        inMemoryChecker.ParseAndCheckProject projectOptions |> Async.RunSynchronously

    for signaturePath in provided do
        fileCache.TryRemove signaturePath |> ignore

    result.Diagnostics
    |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

let FCSSignature options implementation =
    let projectOptions : FSharpProjectOptions =
        { options with
            SourceFiles = [| "A.fs" |]
        }

    let _, result =
        inMemoryChecker.ParseAndCheckFileInProject ("A.fs", 1, SourceText.ofString implementation, projectOptions)
        |> Async.RunSynchronously

    match result with
    | FSharpCheckFileAnswer.Aborted -> Choice1Of2 ()
    | FSharpCheckFileAnswer.Succeeded checkFileResults ->
        match checkFileResults.GenerateSignature (pageWidth = 120) with
        | None -> Choice1Of2 ()
        | Some signature -> Choice2Of2 (string<ISourceText> signature)
