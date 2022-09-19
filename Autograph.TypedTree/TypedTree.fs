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

let transformToFSharpCoreAlias (fe : FSharpEntity) =
    match fe.AccessPath, fe.DisplayName with
    | "System", "Int32" -> "int"
    | _ -> fe.DisplayName

let rec mkParameterTypeName (t : FSharpType) : ParameterTypeName =
    if t.HasTypeDefinition then
        if Seq.isEmpty t.GenericArguments then
            ParameterTypeName.SingleIdentifier (transformToFSharpCoreAlias t.TypeDefinition)
        else

        let isPostFix =
            set [| "list" ; "option" ; "array" |]
            |> Set.contains t.TypeDefinition.DisplayName

        if isPostFix then
            let mt =
                let ga = t.GenericArguments[0]

                if ga.IsGenericParameter then
                    let gp = t.GenericArguments[0].GenericParameter
                    ParameterTypeName.GenericParameter (gp.DisplayName, gp.IsSolveAtCompileTime)
                else
                    mkParameterTypeName ga

            let pt = ParameterTypeName.SingleIdentifier t.TypeDefinition.DisplayName
            ParameterTypeName.PostFix (mt, pt)
        elif t.TypeDefinition.DisplayName = "[]" then
            ParameterTypeName.PostFix (
                mkParameterTypeName t.GenericArguments[0],
                ParameterTypeName.SingleIdentifier "array"
            )
        else
            let args = Seq.map mkParameterTypeName t.GenericArguments |> Seq.toList

            let typeName = ParameterTypeName.SingleIdentifier t.TypeDefinition.DisplayName
            ParameterTypeName.WithGenericArguments (typeName, args)

    elif t.IsGenericParameter then
        ParameterTypeName.GenericParameter (t.GenericParameter.DisplayName, t.GenericParameter.IsSolveAtCompileTime)
    elif t.IsFunctionType then
        let rec visit (t : FSharpType) : ParameterTypeName seq =
            if t.IsFunctionType then
                Seq.collect visit t.GenericArguments
            else
                Seq.singleton (mkParameterTypeName t)

        visit t |> Seq.toList |> ParameterTypeName.FunctionType
    elif t.IsTupleType then
        let ts = Seq.map mkParameterTypeName t.GenericArguments |> Seq.toList

        ParameterTypeName.Tuple ts
    else
        failwith "todo 4A2A9BDE-1AEB-49FF-B1FD-E57C2D9ADFBA"

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
                | :? FSharpMemberOrFunctionOrValue as valSymbol -> Choice1Of3 valSymbol
                | :? FSharpUnionCase as unionCaseSymbol -> Choice2Of3 unionCaseSymbol
                | :? FSharpActivePatternCase as activePatternCaseSymbol -> Choice3Of3 activePatternCaseSymbol
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
            member resolver.GetTypeNameFor parameterRange bindingRange =
                try
                    let parameterSymbol = tryFindSymbol parameterRange

                    match parameterSymbol with
                    | Some symbol ->
                        match symbol with
                        | Choice1Of3 valSymbol -> mkParameterTypeName valSymbol.FullType
                        | Choice2Of3 unionCaseSymbol -> mkParameterTypeName unionCaseSymbol.ReturnType
                        | Choice3Of3 activePatternCaseSymbol ->
                            mkParameterTypeName activePatternCaseSymbol.Group.OverallType.GenericArguments.[0]
                    | None ->
                        match findSymbol bindingRange with
                        | Choice1Of3 valSymbol ->
                            // The parameter didn't resolve any symbol on its own merits
                            // It might still be a resolve as an FSharpParameter
                            let parameterType =
                                valSymbol.CurriedParameterGroups
                                |> Seq.choose (fun pg ->
                                    pg
                                    |> Seq.choose (fun p ->
                                        if equalProxyRange parameterRange p.DeclarationLocation then
                                            Some p.Type
                                        else
                                            None
                                    )
                                    |> Seq.tryHead
                                )
                                |> Seq.tryHead

                            match parameterType with
                            | None ->
                                failwith
                                    $"Could not resolve parameter {parameterRange} in FSharpMemberOrFunctionOrValue {bindingRange}"
                            | Some p -> mkParameterTypeName p
                        | Choice2Of3 _
                        | Choice3Of3 _ -> failwith $"Expected {bindingRange} to resolve FSharpMemberOrFunctionOrValue"
                with ex ->
                    printException ex parameterRange
                    raise ex

            member resolver.GetReturnTypeFor proxyRange =
                try
                    let symbol = findSymbol proxyRange

                    // TODO: pass found number of parameters

                    match symbol with
                    | Choice1Of3 valSymbol ->
                        // TODO: just use the formatted layout?
                        let s = valSymbol.FormatLayout FSharpDisplayContext.Empty

                        let correctReturnParameterQuestionmark =
                            match mkParameterTypeName valSymbol.FullType with
                            | ParameterTypeName.FunctionType ts ->
                                ts
                                |> List.skip valSymbol.CurriedParameterGroups.Count
                                |> ParameterTypeName.FunctionType
                            | _ -> mkParameterTypeName valSymbol.ReturnParameter.Type
                        // if valSymbol.IsMember then
                        //     valSymbol.FullType.GenericArguments.[1]
                        // else
                        //     valSymbol.FullType

                        {
                            FullType = mkParameterTypeName valSymbol.FullType
                            ReturnParameter = correctReturnParameterQuestionmark
                        }

                    | Choice2Of3 unionCaseSymbol ->
                        {
                            FullType = mkParameterTypeName unionCaseSymbol.ReturnType
                            ReturnParameter = mkParameterTypeName unionCaseSymbol.ReturnType
                        }
                    | Choice3Of3 activePatternCaseSymbol -> failwith "todo EA3DEB1F-C6B9-4CFB-9A01-0C4B321C22FD"
                with ex ->
                    printException ex proxyRange
                    raise ex

            member resolver.GetTypeInfo proxyRange =
                try
                    let symbol = findTypeSymbol proxyRange
                    { IsClass = symbol.IsClass }
                with ex ->
                    printException ex proxyRange
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
