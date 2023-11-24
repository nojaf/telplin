module Telplin.Core.TypedTree.Resolver

#nowarn "57"

open System.Text
open System.Collections.Concurrent
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open Telplin.Core.TypedTree.FSharpProjectExtensions

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
        sb.Append(lastLine.Substring (0, range.EndColumn)).ToString ()

let fileCache = ConcurrentDictionary<string, ISourceText> ()

let documentSource fileName =
    async {
        match fileCache.TryGetValue fileName with
        | true, sourceText -> return Some sourceText
        | false, _ -> return None
    }

let inMemoryChecker =
    FSharpChecker.Create (documentSource = DocumentSource.Custom documentSource)

type TypedTreeInfoResolver
    (defines, includePrivateBindings, sourceText : ISourceText, checkFileResults : FSharpCheckFileResults)
    =
    member val Defines = defines
    member val IncludePrivateBindings = includePrivateBindings

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
                | Some sigText -> Ok sigText

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

let mkResolverFor (checker : FSharpChecker) sourceFileName sourceText projectOptions includePrivateBindings =
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
        | None -> TypedTreeInfoResolver (projectOptions.Defines, includePrivateBindings, sourceText, checkFileResults)
    | FSharpCheckFileAnswer.Aborted -> failwith $"type checking aborted for {sourceFileName}"

let mkResolverForCode projectOptions (includePrivateBindings : bool) (code : string) : TypedTreeInfoResolver =
    let sourceFileName = "A.fs"

    let projectOptions : FSharpProjectOptions =
        { projectOptions with
            SourceFiles = [| sourceFileName |]
        }

    let sourceText = SourceText.ofString code

    mkResolverFor inMemoryChecker sourceFileName sourceText projectOptions includePrivateBindings

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
    let signatureName = $"{fileName}.fsi"
    let implementationName = $"{fileName}.fs"

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
