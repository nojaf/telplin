module Telplin.UntypedTree.TypeForValNode

open FSharp.Compiler.Text
open Fantomas.Core.SyntaxOak
open Microsoft.FSharp.Core.CompilerServices
open Telplin.Common
open SourceParser
open ASTCreation

let mapTypeWithGlobalConstraintsNode (map : Type -> Type) (t : Type) =
    match t with
    | Type.WithGlobalConstraints globalConstraintsNode ->
        TypeWithGlobalConstraintsNode (
            map globalConstraintsNode.Type,
            globalConstraintsNode.TypeConstraints,
            globalConstraintsNode.Range
        )
        |> Type.WithGlobalConstraints
    | _ -> map t

/// Typed tree information for a `FSharpMemberOrFunctionOrValue`
type TypedTreeInfo =
    {
        ReturnType : Type
        BindingGenericParameters : GenericParameter list
        TypeGenericParameters : GenericParameter list
    }

/// <summary>
/// The `returnType` from the typed tree won't contain any parameter names (in case of a function).
/// And it might need some additional parentheses. For example, when the return type is a function type.
/// </summary>
/// <param name="typedTreeInfo">Resolved type information from the typed tree.</param>
/// <param name="typeParameterMap">A map of generic parameters found in the untyped tree with the ones found in the typed tree.</param>
/// <param name="parameters">Parameters found in the input Oak. These will be used to enhance the parameters in the `returnType` by adding the name (if present).</param>
let mkTypeForValNodeBasedOnTypedTree
    (typedTreeInfo : TypedTreeInfo)
    (typeParameterMap : Map<string, string>)
    (parameters : Pattern list)
    : Type
    =
    // The `returnType` constructed from the typed tree cannot be trusted 100%.
    // We might receive `int -> int -> int` while the Oak only contained a single parameter.
    // This needs to be transformed to `int -> (int -> int)` to reflect that the return type actually is a function type.
    let correctReturnTypeByActualParameters (returnType : Type) =
        match returnType with
        | Type.Funs funsNode ->
            if funsNode.Parameters.Length = parameters.Length then
                returnType
            else
                // We need to shift the extra parameters to the funsNode.ReturnType
                let actualParameters, additionalTypes =
                    List.take parameters.Length funsNode.Parameters, funsNode.Parameters |> List.skip parameters.Length

                let actualReturnType =
                    TypeParenNode (
                        stn "(",
                        TypeFunsNode (additionalTypes, funsNode.ReturnType, zeroRange) |> Type.Funs,
                        stn ")",
                        zeroRange
                    )
                    |> Type.Paren

                TypeFunsNode (actualParameters, actualReturnType, zeroRange) |> Type.Funs

        | _ -> returnType

    let returnTypeCorrectByActualParameters =
        mapTypeWithGlobalConstraintsNode correctReturnTypeByActualParameters typedTreeInfo.ReturnType

    let returnTypeWithParameterNames =
        let rec updateParameter
            // Only top level tuples can have parameter names
            (isTopLevel : bool)
            (pattern : Pattern)
            (typeTreeType : Type)
            : Type
            =
            match pattern with
            | Pattern.Paren parenNode -> updateParameter isTopLevel parenNode.Pattern typeTreeType

            | Pattern.Named namedNode ->
                Type.SignatureParameter (
                    TypeSignatureParameterNode (None, Some namedNode.Name, typeTreeType, zeroRange)
                )

            | Pattern.OptionalVal optionalValNode ->
                let t =
                    // Remove the `option` if the type is `int option`.
                    // Because the parameter name will already start with an "?" like `?x`.
                    match typeTreeType with
                    | Type.AppPostfix appPostFix ->
                        match appPostFix.Last with
                        | Type.LongIdent optionType ->
                            match optionType.Content with
                            | [ IdentifierOrDot.Ident optionIdent ] when optionIdent.Text = "option" -> appPostFix.First
                            | _ -> typeTreeType
                        | _ -> typeTreeType
                    | _ -> typeTreeType

                Type.SignatureParameter (TypeSignatureParameterNode (None, Some optionalValNode, t, zeroRange))

            | Pattern.Parameter parameter ->
                let parameterType =
                    // Sometimes the type in the untyped tree is more accurate than what the typed tree returned.
                    // For example `System.Text.RegularExpressions.Regex` by untyped tree versus `Regex` by typed.
                    match parameter.Type with
                    | Some (Type.Funs _ as t) -> wrapTypeInParentheses t
                    | Some parameterType ->
                        match parameterType, typeTreeType with
                        | Type.AppPrefix untypedAppPrefix, Type.AppPrefix typedAppPrefix ->
                            // go over type parameter per type parameter and pick the best on.
                            if untypedAppPrefix.Arguments.Length <> typedAppPrefix.Arguments.Length then
                                typeTreeType
                            else

                            let arguments =
                                (untypedAppPrefix.Arguments, typedAppPrefix.Arguments)
                                ||> List.zip
                                |> List.map (fun (untypedArg, typedArg) ->
                                    match untypedArg, typedArg with
                                    // Don't use wildcard
                                    | Type.Anon _, _ -> typedArg
                                    | _ -> untypedArg
                                )

                            TypeAppPrefixNode (
                                typedAppPrefix.Identifier,
                                typedAppPrefix.PostIdentifier,
                                typedAppPrefix.LessThen,
                                arguments,
                                typedAppPrefix.GreaterThan,
                                typedAppPrefix.Range
                            )
                            |> Type.AppPrefix
                        | _ -> parameterType
                    | None -> typeTreeType

                match parameter.Pattern with
                | Pattern.Named namedNode ->
                    Type.SignatureParameter (
                        TypeSignatureParameterNode (parameter.Attributes, Some namedNode.Name, parameterType, zeroRange)
                    )
                | Pattern.OptionalVal optValNode ->
                    Type.SignatureParameter (
                        TypeSignatureParameterNode (parameter.Attributes, Some optValNode, parameterType, zeroRange)
                    )
                | _ -> typeTreeType
            | Pattern.Tuple patTupleNode ->
                match typeTreeType with
                | Type.Tuple typeTupleNode when
                    (isTopLevel && patTupleNode.Patterns.Length = typeTupleNode.Types.Length)
                    ->
                    (patTupleNode.Patterns, typeTupleNode.Types)
                    ||> List.zip
                    |> List.map (fun (pat, t) -> updateParameter false pat t)
                    |> fun ts ->
                        let mutable collector = ListCollector<Choice<_, _>> ()

                        let rec visit ts =
                            match ts with
                            | [] -> ()
                            | [ last ] -> collector.Add (Choice1Of2 last)
                            | head :: rest ->
                                collector.Add (Choice1Of2 head)
                                collector.Add (Choice2Of2 (stn "*"))
                                visit rest

                        visit ts
                        TypeTupleNode (collector.Close (), zeroRange) |> Type.Tuple

                | _ -> typeTreeType
            | _ -> typeTreeType

        let updateParameters (t : Type) =
            match t with
            | Type.Funs funsNode ->
                let parameters =
                    funsNode.Parameters
                    |> List.mapi (fun idx (typeTreeType, arrow) ->
                        if idx > parameters.Length - 1 then
                            typeTreeType, arrow
                        else
                            // We might be able to replace the parameter name with the name we found in the Oak.
                            (updateParameter true parameters.[idx] typeTreeType), arrow
                    )

                TypeFunsNode (parameters, funsNode.ReturnType, zeroRange) |> Type.Funs
            | _ -> t

        mapTypeWithGlobalConstraintsNode updateParameters returnTypeCorrectByActualParameters

    let returnTypeWithCorrectGenericParameterNames =
        if Map.isEmpty typeParameterMap then
            returnTypeWithParameterNames
        else

        let mapTypar (typar : SingleTextNode) =
            match Map.tryFind typar.Text typeParameterMap with
            | None -> typar
            | Some untypedName -> stn untypedName

        let mapTypeConstraint (tc : TypeConstraint) =
            match tc with
            | TypeConstraint.Single singleConstraint ->
                TypeConstraintSingleNode (
                    mapTypar singleConstraint.Typar,
                    singleConstraint.Kind,
                    singleConstraint.Range
                )
                |> TypeConstraint.Single
            | _ -> tc

        { new TypeTransformerBase() with
            member x.TransformVar typar = mapTypar typar

            member x.TransformWithGlobalConstraints globalConstraintsNode =
                TypeWithGlobalConstraintsNode (
                    x.TransformType globalConstraintsNode.Type,
                    List.map mapTypeConstraint globalConstraintsNode.TypeConstraints,
                    globalConstraintsNode.Range
                )
        }
        |> TypeTransformer.transform returnTypeWithParameterNames

    returnTypeWithCorrectGenericParameterNames

let stripParens (t : Type) =
    let strip t =
        match t with
        | Type.Paren parenNode -> parenNode.Type
        | _ -> t

    mapTypeWithGlobalConstraintsNode strip t

let mkTypeForValNode
    (resolver : TypedTreeInfoResolver)
    (nameRange : range)
    (typeParameterMap : Map<string, string>)
    (parameters : Pattern list)
    : Type
    =
    let bindingInfo = resolver.GetFullForBinding nameRange.Proxy

    let t =
        mkTypeFromString bindingInfo.ReturnTypeString
        // Type may have unwanted parentheses.
        |> stripParens

    let typedTreeInfo =
        {
            ReturnType = t
            BindingGenericParameters = bindingInfo.BindingGenericParameters
            TypeGenericParameters = bindingInfo.TypeGenericParameters
        }

    let returnType =
        mkTypeForValNodeBasedOnTypedTree typedTreeInfo typeParameterMap parameters

    let returnType =
        // If the return parameter of a function type is a function type, we need to wrap it in parenthesis.
        // See test ``function return type``
        match returnType with
        | Type.Funs funsNode ->
            match funsNode.ReturnType with
            | Type.Funs _ ->
                let parenNode = TypeParenNode (stn "(", funsNode.ReturnType, stn ")", zeroRange)
                TypeFunsNode (funsNode.Parameters, Type.Paren parenNode, zeroRange) |> Type.Funs
            | _ -> returnType
        | _ -> returnType

    returnType
