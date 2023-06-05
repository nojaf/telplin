module Telplin.Core.TypedTree.Resolver

#nowarn "57"

open System.Collections.Concurrent
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open Telplin.Core
open Telplin.Core.TypedTree.FSharpProjectExtensions

let fileCache = ConcurrentDictionary<string, ISourceText> ()

let documentSource fileName =
    async {
        match fileCache.TryGetValue fileName with
        | true, sourceText -> return Some sourceText
        | false, _ -> return None
    }

let inMemoryChecker =
    FSharpChecker.Create (documentSource = DocumentSource.Custom documentSource)

let mkResolverFor (checker : FSharpChecker) sourceFileName sourceText projectOptions includePrivateBindings =
    let _, checkFileAnswer =
        checker.ParseAndCheckFileInProject (sourceFileName, 1, sourceText, projectOptions)
        |> Async.RunSynchronously

    match checkFileAnswer with
    | FSharpCheckFileAnswer.Succeeded checkFileResults ->
        { new TypedTreeInfoResolver with
            member resolver.GetValText (name, bindingNameRange) : Result<string, string> =
                try
                    let line = sourceText.GetLineString (bindingNameRange.StartLine - 1)

                    let symbolUse =
                        checkFileResults.GetSymbolUseAtLocation (
                            bindingNameRange.StartLine,
                            bindingNameRange.EndColumn,
                            line,
                            [ name ]
                        )

                    match symbolUse with
                    | None -> failwith "no symbol use"
                    | Some symbolUse ->
                        match symbolUse.Symbol with
                        | :? FSharpMemberOrFunctionOrValue as mfv ->
                            let sigTextOpt = mfv.GetValSignatureText (symbolUse.DisplayContext, symbolUse.Range)

                            match sigTextOpt with
                            | None -> Error $"No sig text for %A{mfv}"
                            | Some sigText -> Ok sigText

                        | _ -> Error "Symbol is not FSharpMemberOrFunctionOrValue"

                with ex ->
                    Error ex.Message

            member resolver.Defines = projectOptions.Defines
            member resolver.IncludePrivateBindings = includePrivateBindings

        }
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
        | FSharpDiagnosticSeverity.Error _
        | FSharpDiagnosticSeverity.Warning _ -> true
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
        | Some signature -> Choice2Of2 (string signature)
