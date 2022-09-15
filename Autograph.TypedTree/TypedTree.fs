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

let rec mkParameterTypeName (t : FSharpType) : ParameterTypeName =
    if t.HasTypeDefinition then
        if Seq.isEmpty t.GenericArguments then
            ParameterTypeName.SingleIdentifier t.TypeDefinition.DisplayName
        else
            failwith "todo 1BD8CC45-36A6-43E3-8355-7B2A71190790"
    // let isPostFix =
    //     set [| "list"; "option"; "array" |]
    //     |> Set.contains t.TypeDefinition.DisplayName
    //
    // if isPostFix then
    //     let ga = t.GenericArguments[0].GenericParameter
    //     let tick = if ga.IsSolveAtCompileTime then "^" else "'"
    //     $"{tick}{ga.DisplayName} {t.TypeDefinition.DisplayName}"
    // else
    //     let args = Seq.map getTypeName t.GenericArguments |> String.concat ","
    //     $"{t.TypeDefinition.DisplayName}<{args}>"
    elif t.IsGenericParameter then
        failwith "todo C144D459-9279-4910-8944-56AE5BF340E9"
    // let tick = if t.GenericParameter.IsSolveAtCompileTime then "^" else "'"
    // $"{tick}{t.GenericParameter.DisplayName}"
    elif t.IsFunctionType then
        let rec visit (t : FSharpType) : ParameterTypeName seq =
            if t.IsFunctionType then
                Seq.collect visit t.GenericArguments
            else
                Seq.singleton (mkParameterTypeName t)

        visit t |> Seq.toList |> ParameterTypeName.FunctionType
    elif t.IsTupleType then
        failwith "todo CBDBA377-8BF2-4200-B2A2-98C80CF2C064"
    // t.GenericArguments
    // |> Seq.map getTypeName
    // |> String.concat " * "
    else
        failwith "todo 4A2A9BDE-1AEB-49FF-B1FD-E57C2D9ADFBA"

let mkResolver (code : string) : TypedTreeInfoResolver =
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

    let _, checkFileAnswer =
        checker.ParseAndCheckFileInProject (sourceFileName, 1, sourceText, projectOptions)
        |> Async.RunSynchronously

    match checkFileAnswer with
    | FSharpCheckFileAnswer.Succeeded checkFileResults ->
        let allSymbols = checkFileResults.GetAllUsesOfAllSymbolsInFile () |> Seq.toArray

        { new TypedTreeInfoResolver with
            member resolver.GetTypeNameFor proxyRange =
                let symbol =
                    allSymbols
                    |> Array.tryFind (fun symbol -> equalProxyRange proxyRange symbol.Range)

                match symbol with
                | None -> failwith $"Failed to resolve symbols for {proxyRange}"
                | Some symbol ->
                    match symbol.Symbol with
                    | :? FSharpMemberOrFunctionOrValue as valSymbol -> mkParameterTypeName valSymbol.FullType
                    | _ -> failwith $"Failed to resolve FSharpMemberOrFunctionOrValue for for {proxyRange}"

            member resolver.GetReturnTypeFor proxyRange =
                let symbol =
                    allSymbols
                    |> Array.tryFind (fun symbol -> equalProxyRange proxyRange symbol.Range)

                match symbol with
                | None -> failwith $"Failed to resolve symbols for {proxyRange}"
                | Some symbol ->
                    match symbol.Symbol with
                    | :? FSharpMemberOrFunctionOrValue as valSymbol ->
                        mkParameterTypeName valSymbol.ReturnParameter.Type
                    | _ -> failwith $"Failed to resolve FSharpMemberOrFunctionOrValue for for {proxyRange}"
        }
    | FSharpCheckFileAnswer.Aborted -> failwith "type checking aborted"
