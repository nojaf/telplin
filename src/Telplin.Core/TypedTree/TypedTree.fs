module Telplin.Core.TypedTree.Resolver

#nowarn "57"

open System.Collections.Concurrent
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open Telplin.Core
open Telplin.Core.TypedTree.FSharpProjectExtensions

let equalProxyRange (proxyRange : range) (m : range) : bool =
    proxyRange.StartLine = m.StartLine
    && proxyRange.StartColumn = m.StartColumn
    && proxyRange.EndLine = m.EndLine
    && proxyRange.EndColumn = m.EndColumn

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
        let allSymbols =
            checkFileResults.GetAllUsesOfAllSymbolsInFile ()
            |> Seq.sortBy (fun r -> r.Range.StartLine, r.Range.StartColumn)
            |> Seq.toArray

        let tryFindSymbolAux predicate proxyRange =
            allSymbols
            |> Array.tryPick (fun symbol ->
                if not (equalProxyRange proxyRange symbol.Range) then
                    None
                else
                    match symbol.Symbol with
                    | :? FSharpMemberOrFunctionOrValue as valSymbol ->
                        if not (predicate valSymbol) then
                            None
                        else
                            Some (valSymbol, symbol.DisplayContext)
                    | _ -> None
            )

        let tryFindSymbol proxyRange =
            tryFindSymbolAux (fun _ -> true) proxyRange

        let findSymbolForName proxyRange name =
            match tryFindSymbolAux (fun valSymbol -> valSymbol.CompiledName = name) proxyRange with
            | None -> failwith $"Failed to resolve symbol for {proxyRange} and CompiledName {name}"
            | Some symbol -> symbol

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

        let mkBindingInfo displayContext (valSymbol : FSharpMemberOrFunctionOrValue) : Result<BindingInfo, string> =
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

            let stripParens (returnTypeText : string) =
                if returnTypeText.[0] = '(' && returnTypeText.[returnTypeText.Length - 1] = ')' then
                    returnTypeText.Substring (1, returnTypeText.Length - 2)
                else
                    returnTypeText

            let getReturnTypeText () =
                if List.isEmpty genericParameters then
                    valSymbol.FullType.Format displayContext
                else
                    // Remove parentheses around the type (before the constraints)
                    let withoutConstraints = valSymbol.FullType.Format displayContext
                    let withConstraints = valSymbol.FullType.FormatWithConstraints displayContext

                    let whenIndex = withConstraints.IndexOf "when"

                    if whenIndex > -1 then
                        let constraintText = withConstraints.Substring(whenIndex).Trim ()

                        $"{withoutConstraints} {constraintText}"
                    else
                        withConstraints
                |> stripParens

            let stripFirstType (returnTypeText : string) =
                // The owning type of the member will be included in the returnTypeText.
                // For example:
                // type Meh =
                //     member x.Foo (a:int) : int = 0
                // Will be: "Meh -> int -> int"
                // We need to strip the first arrow.
                let firstArrowIdx = returnTypeText.IndexOf "->"

                if firstArrowIdx = -1 then
                    returnTypeText
                else

                returnTypeText.Substring(firstArrowIdx + 2).TrimStart ()

            let takeLastType (returnTypeText : string) =
                let lastArrowIdx = returnTypeText.LastIndexOf "->"
                returnTypeText.Substring(lastArrowIdx + 2).TrimStart ()

            let returnTypeText =
                let returnTypeText = getReturnTypeText ()

                if valSymbol.IsPropertyGetterMethod then
                    let firstGroup = valSymbol.CurriedParameterGroups.[0]

                    if
                        firstGroup.Count = 1
                        && firstGroup.[0].Type.BasicQualifiedName = "Microsoft.FSharp.Core.unit"
                    then
                        takeLastType returnTypeText
                    else
                        stripFirstType returnTypeText
                elif valSymbol.IsPropertySetterMethod then
                    // TODO: clean up

                    // In case of an indexed setter we only need to remove the leading type name
                    if valSymbol.CurriedParameterGroups.[0].Count = 2 then
                        let indexType = valSymbol.CurriedParameterGroups.[0].[0].Type.Format displayContext

                        let propertyType =
                            valSymbol.CurriedParameterGroups.[0].[1].Type.Format displayContext

                        $"{indexType} -> {propertyType}"
                    else
                        // Take the type of the property
                        valSymbol.CurriedParameterGroups.[0].[0].Type.Format displayContext
                elif valSymbol.IsInstanceMember then
                    stripFirstType returnTypeText
                else
                    returnTypeText

            Ok
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

                    Ok
                        {
                            NeedsClassAttribute = typeSymbol.IsClass && doesNotHaveClassAttribute
                            ConstructorInfo =
                                ctor
                                |> Option.bind (
                                    function
                                    | Ok ctor -> Some ctor
                                    | Error _ -> None
                                )
                        }
                with ex ->
                    Result.Error ex.Message

            member resolver.GetFullForBinding bindingNameRange =
                try
                    let valSymbol, displayContext = findSymbol bindingNameRange
                    mkBindingInfo displayContext valSymbol
                with ex ->
                    Error ex.Message

            member resolver.GetPropertyWithIndex name range =
                try
                    let valSymbol, displayContext = findSymbolForName range name
                    mkBindingInfo displayContext valSymbol
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
