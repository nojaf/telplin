module Autograph.TypedTree.Resolver

open System
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open Autograph.Common

let equalProxyRange (proxyRange : RangeProxy) (m : range) : bool =
    proxyRange.StartLine = m.StartLine
    && proxyRange.StartColumn = m.StartColumn
    && proxyRange.EndLine = m.EndLine
    && proxyRange.EndColumn = m.EndColumn

let checker = FSharpChecker.Create ()

let mkResolverFor sourceFileName sourceText projectOptions =
    let _, checkFileAnswer =
        checker.ParseAndCheckFileInProject (sourceFileName, 1, sourceText, projectOptions)
        |> Async.RunSynchronously

    match checkFileAnswer with
    | FSharpCheckFileAnswer.Succeeded checkFileResults ->
        let allSymbols =
            checkFileResults.GetAllUsesOfAllSymbolsInFile ()
            |> Seq.sortBy (fun r -> r.Range.StartLine, r.Range.StartColumn)
            |> Seq.toArray

        let printException ex proxyRange =
            printfn $"Exception for {proxyRange} in {sourceFileName}\n{ex}"

        let tryFindSymbol proxyRange =
            allSymbols
            |> Array.tryFind (fun symbol -> equalProxyRange proxyRange symbol.Range)
            |> Option.map (fun symbol ->
                match symbol.Symbol with
                | :? FSharpMemberOrFunctionOrValue as valSymbol -> valSymbol
                | _ -> failwith $"Unexpected type of {symbol} for {proxyRange}"
            )

        let findSymbol proxyRange =
            match tryFindSymbol proxyRange with
            | None -> failwith $"Failed to resolve symbols for {proxyRange}"
            | Some symbol -> symbol

        let findTypeSymbol proxyRange =
            let symbol =
                allSymbols
                |> Array.tryFind (fun symbol -> equalProxyRange proxyRange symbol.Range)

            match symbol with
            | None -> failwith $"Failed to resolve symbols for {proxyRange}"
            | Some symbol ->
                match symbol.Symbol with
                | :? FSharpEntity as typeSymbol -> typeSymbol
                | _ -> failwith $"Failed to resolve FSharpType for for {proxyRange}"

        { new TypedTreeInfoResolver with
            member resolver.GetTypeInfo proxyRange =
                try
                    let symbol = findTypeSymbol proxyRange
                    { IsClass = symbol.IsClass }
                with ex ->
                    printException ex proxyRange
                    raise ex

            member resolver.GetFullForBinding bindingNameRange =
                try
                    let valSymbol = findSymbol bindingNameRange

                    let s =
                        valSymbol.FormatLayout FSharpDisplayContext.Empty
                        |> Array.choose (fun (t : TaggedText) ->
                            match t.Tag with
                            | TextTag.UnknownEntity -> None
                            | _ -> Some t.Text
                        )
                        |> String.concat ""

                    s
                with ex ->
                    printException ex bindingNameRange
                    raise ex
        }
    | FSharpCheckFileAnswer.Aborted -> failwith $"type checking aborted for {sourceFileName}"

let mkResolverForCode (code : string) : TypedTreeInfoResolver =
    let sourceFileName = "A.fs"

    let projectOptions : FSharpProjectOptions =
        {
            ProjectFileName = "A"
            ProjectId = None
            SourceFiles = [| sourceFileName |]
            OtherOptions = [||]
            ReferencedProjects = [||]
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = false
            LoadTime = DateTime.Now
            UnresolvedReferences = None
            OriginalLoadReferences = []
            Stamp = None
        }

    let sourceText = SourceText.ofString code

    mkResolverFor sourceFileName sourceText projectOptions

let assertTypeCheckFor implementationPath signaturePath =
    let projectOptions : FSharpProjectOptions =
        {
            ProjectFileName = "A"
            ProjectId = None
            SourceFiles = [| signaturePath ; implementationPath |]
            OtherOptions = [||]
            ReferencedProjects = [||]
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = false
            LoadTime = DateTime.Now
            UnresolvedReferences = None
            OriginalLoadReferences = []
            Stamp = None
        }

    let result = checker.ParseAndCheckProject projectOptions |> Async.RunSynchronously

    if not (Array.isEmpty result.Diagnostics) then
        Array.iter (fun d -> printfn "%A" d) result.Diagnostics
        failwith "Could not compile source with signature file"
