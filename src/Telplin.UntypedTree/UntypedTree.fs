module Telplin.UntypedTree.Writer

open FSharp.Compiler.Text
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Telplin.Common

let zeroRange = Range.Zero

type Range with

    member r.Proxy : RangeProxy =
        RangeProxy (r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)

let stn v = SingleTextNode (v, zeroRange)

/// <summary>Replace `char[]` to `char array`</summary>
/// <remarks>This function is not tail recursive. Hopefully we can remove this function in a future FCS version.</remarks>
let rec sanitizeType (t : Type) =
    match t with
    | Type.Array typeArray ->
        let array =
            if typeArray.Rank = 1 then
                Type.LongIdent (IdentListNode ([ IdentifierOrDot.Ident (stn "array") ], zeroRange))
            else
                Type.LongIdent (
                    IdentListNode ([ IdentifierOrDot.Ident (stn $"array{typeArray.Rank - 1}d") ], zeroRange)
                )

        TypeAppPostFixNode (typeArray.Type, array, zeroRange) |> Type.AppPostfix

    | Type.AppPrefix appPrefix ->
        TypeAppPrefixNode (
            sanitizeType appPrefix.Identifier,
            appPrefix.PostIdentifier,
            appPrefix.LessThen,
            List.map sanitizeType appPrefix.Arguments,
            appPrefix.GreaterThan,
            zeroRange
        )
        |> Type.AppPrefix

    | Type.Funs funs ->
        let parameters =
            funs.Parameters |> List.map (fun (t, arrow) -> sanitizeType t, arrow)

        TypeFunsNode (parameters, sanitizeType funs.ReturnType, zeroRange) |> Type.Funs

    | Type.Paren paren ->
        TypeParenNode (paren.OpeningParen, sanitizeType paren.Type, paren.ClosingParen, zeroRange)
        |> Type.Paren

    | Type.Tuple tuple ->
        let path =
            tuple.Path
            |> List.map (
                function
                | Choice1Of2 t -> sanitizeType t |> Choice1Of2
                | Choice2Of2 operator -> Choice2Of2 operator
            )

        TypeTupleNode (path, zeroRange) |> Type.Tuple
    | _ -> t

let mkTypeFromString (typeText : string) : Type =
    let oak =
        CodeFormatter.ParseOakAsync (true, $"val v: {typeText}")
        |> Async.RunSynchronously
        |> Array.head
        |> fst

    match oak.ModulesOrNamespaces.[0].Declarations with
    | [ ModuleDecl.Val valNode ] -> sanitizeType valNode.Type
    | decls -> failwithf $"Unexpected module decls:%A{decls}"

let mkMember (resolver : TypedTreeInfoResolver) (md : MemberDefn) : MemberDefn option =
    match md with
    | MemberDefn.ValField _ -> Some md
    | _ -> None

let mkMembers (resolver : TypedTreeInfoResolver) (ms : MemberDefn list) : MemberDefn list =
    List.choose (mkMember resolver) ms

let mkTypeDefn (resolver : TypedTreeInfoResolver) (typeDefn : TypeDefn) : TypeDefn =
    let tdn = TypeDefn.TypeDefnNode typeDefn

    let typeName =
        TypeNameNode (
            tdn.TypeName.XmlDoc,
            tdn.TypeName.Attributes,
            tdn.TypeName.LeadingKeyword,
            tdn.TypeName.Accessibility,
            tdn.TypeName.Identifier,
            tdn.TypeName.TypeParameters,
            tdn.TypeName.Constraints,
            None,
            tdn.TypeName.EqualsToken,
            tdn.TypeName.WithKeyword,
            zeroRange
        )

    match typeDefn with
    | TypeDefn.Record recordNode ->
        TypeDefnRecordNode (
            typeName,
            recordNode.Accessibility,
            recordNode.OpeningBrace,
            recordNode.Fields,
            recordNode.ClosingBrace,
            mkMembers resolver tdn.Members,
            zeroRange
        )
        |> TypeDefn.Record
    | TypeDefn.Explicit explicitNode ->
        let body =
            TypeDefnExplicitBodyNode (
                explicitNode.Body.Kind,
                mkMembers resolver explicitNode.Body.Members,
                explicitNode.Body.End,
                zeroRange
            )

        TypeDefnExplicitNode (typeName, body, mkMembers resolver tdn.Members, zeroRange)
        |> TypeDefn.Explicit
    | TypeDefn.Regular _ ->
        TypeDefnRegularNode (typeName, mkMembers resolver tdn.Members, zeroRange)
        |> TypeDefn.Regular
    | _ -> failwith "todo, 17AA2504-F9C2-4418-8614-93E9CF6699BC"

/// <summary>
/// If a function is fully typed in the input Oak, we can re-use that exact information to construct the return type for the ValNode.
/// This only works when every parameter was typed and, the return type is present and no generics are involved.
/// </summary>
let mkTypeForValNodeBasedOnOak () : Type =
    failwith "todo, F0C88437-6D47-4D6E-B95C-8A1E74D6DF08"

/// <summary>
/// The `returnType` from the typed tree won't contain any parameter names (in case of a function).
/// And it might need some additional parentheses. For example, when the return type is a function type.
/// </summary>
/// <param name="returnType">Resolved type from the typed tree.</param>
/// <param name="parameters">Parameters found in the input Oak. These will be used to enhance the parameters in the `returnType` by adding the name (if present).</param>
/// <param name="returnTypeInSource">Optional return type from the input Oak.</param>
let mkTypeForValNodeBasedOnTypedTree
    (returnType : Type)
    (parameters : Pattern list)
    (returnTypeInSource : BindingReturnInfoNode option)
    : Type
    =
    let returnTypeWithParameterNames =
        match returnType with
        | Type.Funs funsNode ->
            let parameters =
                funsNode.Parameters
                |> List.mapi (fun idx parameterNameAndArrow ->
                    if idx > parameters.Length - 1 then
                        parameterNameAndArrow
                    else
                        // We might be able to replace the parameter name with the name we found in the Oak.
                        match parameters.[idx], parameterNameAndArrow with
                        | Pattern.Named namedNode, (typedTreeType, arrow) ->
                            Type.SignatureParameter (
                                TypeSignatureParameterNode (None, Some namedNode.Name, typedTreeType, zeroRange)
                            ),
                            arrow
                        | Pattern.Paren parenNode, (typedTreeType, arrow) ->
                            match parenNode.Pattern with
                            | Pattern.Parameter parameter ->
                                match parameter.Pattern with
                                | Pattern.Named namedNode ->
                                    Type.SignatureParameter (
                                        TypeSignatureParameterNode (
                                            parameter.Attributes,
                                            Some namedNode.Name,
                                            typedTreeType,
                                            zeroRange
                                        )
                                    ),
                                    arrow
                                | _ -> parameterNameAndArrow
                            | _ -> parameterNameAndArrow
                        | _ -> parameterNameAndArrow
                )

            TypeFunsNode (parameters, funsNode.ReturnType, zeroRange) |> Type.Funs
        | _ -> returnType

    returnTypeWithParameterNames

let mkTypeForValNode
    (resolver : TypedTreeInfoResolver)
    (nameRange : range)
    (parameters : Pattern list)
    (returnTypeInSource : BindingReturnInfoNode option)
    : Type
    =
    let returnTypeString, _ = resolver.GetFullForBinding nameRange.Proxy
    let t = mkTypeFromString returnTypeString

    let isTypedPatternWithoutGenerics (p : Pattern) =
        match p with
        | Pattern.Paren parenNode ->
            match parenNode.Pattern with
            | Pattern.Parameter _ -> true
            | _ -> false
        | _ -> false

    match returnTypeInSource with
    | Some _ ->
        if List.forall isTypedPatternWithoutGenerics parameters then
            mkTypeForValNodeBasedOnOak ()
        else
            mkTypeForValNodeBasedOnTypedTree t parameters returnTypeInSource
    | None -> mkTypeForValNodeBasedOnTypedTree t parameters None

let mkModuleDecl (resolver : TypedTreeInfoResolver) (mdl : ModuleDecl) : ModuleDecl =
    match mdl with
    | ModuleDecl.TopLevelBinding bindingNode ->
        match bindingNode.FunctionName with
        | Choice1Of2 name ->
            let valKw = MultipleTextsNode ([ stn "val" ], zeroRange)
            let nameRange = (name :> Node).Range

            let name =
                match name.Content with
                | [ IdentifierOrDot.Ident name ] -> name
                | _ -> failwith "todo, 38A9012C-2C4D-4387-9558-F75F6578402A"

            let t =
                mkTypeForValNode resolver nameRange bindingNode.Parameters bindingNode.ReturnType

            ValNode (
                bindingNode.XmlDoc,
                bindingNode.Attributes,
                Some valKw,
                None,
                false,
                None,
                name,
                None,
                t,
                Some (stn "="),
                None,
                zeroRange
            )
            |> ModuleDecl.Val
        | _ -> failwith "todo, C98DD050-A18C-4D8D-A025-083B352C57A5"

    | ModuleDecl.TypeDefn typeDefn -> mkTypeDefn resolver typeDefn |> ModuleDecl.TypeDefn
    | ModuleDecl.OpenList _ -> mdl
    | _ -> failwith "todo, 56EF9CEE-A28B-437D-8A0F-EBE7E0AA850F"

let mkModuleOrNamespace
    (resolver : TypedTreeInfoResolver)
    (moduleNode : ModuleOrNamespaceNode)
    : ModuleOrNamespaceNode
    =
    let decls = List.map (mkModuleDecl resolver) moduleNode.Declarations
    ModuleOrNamespaceNode (moduleNode.Header, decls, zeroRange)

let mkSignatureFile (resolver : TypedTreeInfoResolver) (code : string) =
    let implementationOak =
        CodeFormatter.ParseOakAsync (false, code)
        |> Async.RunSynchronously
        |> Array.find (fun (_, defines) -> List.isEmpty defines)
        |> fst

    let signatureOak =
        let mdns =
            List.map (mkModuleOrNamespace resolver) implementationOak.ModulesOrNamespaces

        Oak (implementationOak.ParsedHashDirectives, mdns, zeroRange)

    CodeFormatter.FormatOakAsync signatureOak |> Async.RunSynchronously
