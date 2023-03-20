module Telplin.TypedTree.Resolver

#nowarn "57"

open System.Collections.Concurrent
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open Telplin.Common

let equalProxyRange (proxyRange : RangeProxy) (m : range) : bool =
    proxyRange.StartLine = m.StartLine
    && proxyRange.StartColumn = m.StartColumn
    && proxyRange.EndLine = m.EndLine
    && proxyRange.EndColumn = m.EndColumn

let fileCache = ConcurrentDictionary<string, ISourceText> ()

let documentSource fileName =
    match fileCache.TryGetValue fileName with
    | true, sourceText -> Some sourceText
    | false, _ -> None

let inMemoryChecker =
    FSharpChecker.Create (documentSource = DocumentSource.Custom documentSource)

let mkResolverFor (checker : FSharpChecker) sourceFileName sourceText projectOptions =
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
                | :? FSharpMemberOrFunctionOrValue as valSymbol -> valSymbol, symbol.DisplayContext
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
                | :? FSharpEntity as typeSymbol -> typeSymbol, symbol.DisplayContext
                | _ -> failwith $"Failed to resolve FSharpType for for {proxyRange}"

        let mkGenericParameters
            (displayContext : FSharpDisplayContext)
            (genericParameters : FSharpGenericParameter seq)
            : GenericParameter list
            =
            genericParameters
            |> Seq.map (fun gp ->
                let constraints =
                    gp.Constraints
                    |> Seq.map (fun c ->
                        let memberConstraintData =
                            if not c.IsMemberConstraint then
                                None
                            else
                                let fullType =
                                    let parameters =
                                        c.MemberConstraintData.MemberArgumentTypes
                                        |> Seq.map (fun at -> at.Format displayContext)
                                        |> String.concat " * "

                                    let rt = c.MemberConstraintData.MemberReturnType.Format displayContext

                                    let arrow =
                                        if Seq.isEmpty c.MemberConstraintData.MemberArgumentTypes then
                                            ""
                                        else
                                            " -> "

                                    $"{parameters} {arrow} {rt}".TrimStart ()

                                Some
                                    {
                                        IsStatic = c.MemberConstraintData.MemberIsStatic
                                        MemberName = c.MemberConstraintData.MemberName
                                        Type = fullType
                                    }

                        let coercesToTarget =
                            if c.IsCoercesToConstraint then
                                Some (c.CoercesToTarget.Format displayContext)
                            else
                                None

                        {
                            IsEqualityConstraint = c.IsEqualityConstraint
                            IsComparisonConstraint = c.IsComparisonConstraint
                            IsReferenceTypeConstraint = c.IsReferenceTypeConstraint
                            IsSupportsNullConstraint = c.IsSupportsNullConstraint
                            CoercesToTarget = coercesToTarget
                            MemberConstraint = memberConstraintData
                        }
                    )
                    |> Seq.toList

                {
                    ParameterName = gp.DisplayName
                    IsHeadType = gp.IsSolveAtCompileTime
                    IsCompilerGenerated = gp.IsCompilerGenerated
                    Constraints = constraints
                }
            )
            |> Seq.toList

        let mkBindingInfo displayContext (valSymbol : FSharpMemberOrFunctionOrValue) : BindingInfo =
            let typeGenericParameters =
                match valSymbol.DeclaringEntity with
                | None -> []
                | Some entity -> entity.GenericParameters |> mkGenericParameters displayContext

            let isTypeGenericParameter : string -> bool =
                let typeGenericParameterNames =
                    typeGenericParameters |> List.map (fun gp -> gp.ParameterName) |> set

                fun name -> Set.contains name typeGenericParameterNames

            let genericParameters =
                valSymbol.GenericParameters
                |> Seq.filter (fun gp -> not (isTypeGenericParameter gp.Name))
                |> mkGenericParameters displayContext

            let returnTypeText =
                if List.isEmpty genericParameters then
                    valSymbol.FullType.Format displayContext
                else
                    valSymbol.FullType.FormatWithConstraints displayContext

            let returnTypeText =
                if not valSymbol.IsInstanceMember then
                    returnTypeText
                else
                // The owning type of the member will be included in the returnTypeText.
                // For example:
                // type Meh =
                //     member x.Foo (a:int : int = 0
                // Will be: "Meh -> int -> int"
                // We need to strip the first arrow.
                let firstArrowIdx = returnTypeText.IndexOf ("->")

                if firstArrowIdx = -1 then
                    returnTypeText
                else

                returnTypeText.Substring(firstArrowIdx + 2).TrimStart ()

            {
                ReturnTypeString = returnTypeText
                BindingGenericParameters = genericParameters
                TypeGenericParameters = typeGenericParameters
            }

        { new TypedTreeInfoResolver with
            member resolver.GetTypeInfo proxyRange =
                try
                    let typeSymbol, displayContext = findTypeSymbol proxyRange

                    let ctor =
                        typeSymbol.TryGetMembersFunctionsAndValues ()
                        |> Seq.choose (fun (valSymbol : FSharpMemberOrFunctionOrValue) ->
                            if valSymbol.CompiledName = ".ctor" then
                                let bindingInfo = mkBindingInfo displayContext valSymbol
                                Some bindingInfo
                            else
                                None
                        )
                        |> Seq.tryHead // Assume one constructor for now

                    let doesNotHaveClassAttribute =
                        let hasAttribute =
                            typeSymbol.Attributes
                            |> Seq.exists (fun a ->
                                match a.AttributeType.TryFullName with
                                | None -> false
                                | Some typeName -> "Microsoft.FSharp.Core.ClassAttribute" = typeName
                            )

                        not hasAttribute

                    {
                        NeedsClassAttribute = typeSymbol.IsClass && doesNotHaveClassAttribute
                        ConstructorInfo = ctor
                    }
                with ex ->
                    printException ex proxyRange
                    raise ex

            member resolver.GetFullForBinding bindingNameRange =
                try
                    let valSymbol, displayContext = findSymbol bindingNameRange
                    mkBindingInfo displayContext valSymbol
                with ex ->
                    printException ex bindingNameRange
                    raise ex

            member resolver.GetTypeTyparNames range =
                try
                    let typeSymbol, _ = findTypeSymbol range
                    let getName (typar : FSharpGenericParameter) = typar.FullName
                    typeSymbol.GenericParameters |> Seq.map getName |> Seq.toList
                with ex ->
                    printException ex range
                    raise ex
        }
    | FSharpCheckFileAnswer.Aborted -> failwith $"type checking aborted for {sourceFileName}"

let mkResolverForCode projectOptions (code : string) : TypedTreeInfoResolver =
    let sourceFileName = "A.fs"

    let projectOptions : FSharpProjectOptions =
        { projectOptions with
            SourceFiles = [| sourceFileName |]
        }

    let sourceText = SourceText.ofString code

    mkResolverFor inMemoryChecker sourceFileName sourceText projectOptions

let mapDiagnostics diagnostics =
    diagnostics
    |> Array.choose (fun (d : FSharpDiagnostic) ->
        match d.Severity with
        | FSharpDiagnosticSeverity.Error ->
            {
                Severity = FSharpDiagnosticInfoSeverity.Error
                Message = d.Message
                ErrorNumber = d.ErrorNumberText
                Range = RangeProxy (d.StartLine, d.StartColumn, d.EndLine, d.EndColumn)
            }
            |> Some
        | FSharpDiagnosticSeverity.Warning ->
            {
                Severity = FSharpDiagnosticInfoSeverity.Warning
                Message = d.Message
                ErrorNumber = d.ErrorNumberText
                Range = RangeProxy (d.StartLine, d.StartColumn, d.EndLine, d.EndColumn)
            }
            |> Some
        | FSharpDiagnosticSeverity.Info
        | FSharpDiagnosticSeverity.Hidden -> None
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
    | FSharpCheckFileAnswer.Succeeded checkFileResults -> mapDiagnostics checkFileResults.Diagnostics |> Choice2Of2

let typeCheckForPair projectOptions implementation signature =
    let fileName = System.Guid.NewGuid().ToString "N"
    let signatureName = $"{fileName}.fsi"
    let implementationName = $"{fileName}.fs"

    fileCache.TryAdd (signatureName, SourceText.ofString signature) |> ignore

    fileCache.TryAdd (implementationName, SourceText.ofString implementation)
    |> ignore

    let projectOptions : FSharpProjectOptions =
        { projectOptions with
            SourceFiles = [| signatureName ; implementationName |]
        }

    let result =
        inMemoryChecker.ParseAndCheckProject projectOptions |> Async.RunSynchronously

    fileCache.TryRemove signatureName |> ignore
    fileCache.TryRemove implementationName |> ignore

    mapDiagnostics result.Diagnostics
