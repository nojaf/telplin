module rec Telplin.UntypedTree.Writer

open FSharp.Compiler.Text
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Microsoft.FSharp.Core.CompilerServices
open Telplin.Common
open Telplin.UntypedTree.SourceParser

let zeroRange = Range.Zero

type Range with

    member r.Proxy : RangeProxy =
        RangeProxy (r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)

/// Create a `SingleTextNode` based on the value string.
let stn v = SingleTextNode (v, zeroRange)

/// Create an `IdentListNode` with a single IdentifierOrDot.Ident value.
let iln v =
    IdentListNode ([ IdentifierOrDot.Ident (stn v) ], zeroRange)

/// Check if `MultipleAttributeListNode` contains an attribute that contains a given `name`.
let hasAttribute (name : string) (multipleAttributeListNode : MultipleAttributeListNode option) =
    match multipleAttributeListNode with
    | None -> false
    | Some multipleAttributeListNode ->
        multipleAttributeListNode.AttributeLists
        |> List.collect (fun al -> al.Attributes)
        |> List.exists (fun a ->
            a.TypeName.Content
            |> List.exists (
                function
                | IdentifierOrDot.Ident attributeName -> attributeName.Text = name
                | _ -> false
            )
        )

/// Transform a string into Type by parsing a dummy `val` in a signature file.
let mkTypeFromString (typeText : string) : Type =
    let oak =
        CodeFormatter.ParseOakAsync (true, $"val v: {typeText}")
        |> Async.RunSynchronously
        |> Array.head
        |> fst

    match oak.ModulesOrNamespaces.[0].Declarations with
    | [ ModuleDecl.Val valNode ] -> valNode.Type
    | decls -> failwithf $"Unexpected module decls:%A{decls}"

let mkMember (resolver : TypedTreeInfoResolver) (md : MemberDefn) : MemberDefn option =
    match md with
    | MemberDefn.ValField _
    | MemberDefn.AbstractSlot _ -> Some md
    | MemberDefn.LetBinding _ -> None

    | MemberDefn.ImplicitInherit implicitInherit ->
        match implicitInherit with
        | InheritConstructor.Unit inheritCtor ->
            MemberDefnInheritNode (implicitInherit.InheritKeyword, inheritCtor.Type, zeroRange)
            |> MemberDefn.Inherit
            |> Some
        | _ -> None

    | MemberDefn.Member bindingNode ->
        match bindingNode.FunctionName with
        | Choice2Of2 _ -> None
        | Choice1Of2 name ->
            let valKw = bindingNode.LeadingKeyword

            let name =
                match name.Content with
                | [ IdentifierOrDot.Ident _this
                    (IdentifierOrDot.KnownDot _ | IdentifierOrDot.UnknownDot)
                    IdentifierOrDot.Ident name ] -> name
                | _ -> failwith "todo, 38A9012C-2C4D-4387-9558-F75F6578402A"

            let t =
                let rt = bindingNode.ReturnType |> Option.map (fun rt -> rt.Type)
                mkTypeForValNode resolver name.Range bindingNode.Parameters rt

            MemberDefnSigMemberNode (
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
                ),
                None,
                zeroRange
            )
            |> MemberDefn.SigMember
            |> Some

    | MemberDefn.AutoProperty autoProperty ->
        let valKw = MultipleTextsNode ([ stn "member" ], zeroRange)

        let name = autoProperty.Identifier

        let t = mkTypeForValNode resolver name.Range [] autoProperty.Type

        MemberDefnSigMemberNode (
            ValNode (
                autoProperty.XmlDoc,
                autoProperty.Attributes,
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
            ),
            None,
            zeroRange
        )
        |> MemberDefn.SigMember
        |> Some

    | _ -> failwith "todo, 32CF2FF3-D9AD-41B8-96B8-E559A2327E66"

let mkMembers (resolver : TypedTreeInfoResolver) (ms : MemberDefn list) : MemberDefn list =
    List.choose (mkMember resolver) ms

/// <summary>
/// Map a TypeDefn to its signature counterpart.
/// A lot of information can typically be re-used when the same syntax applies.
/// The most important things that need mapping are the implicit constructor and the type members.
/// </summary>
/// <param name="resolver">Resolves information from the Typed tree.</param>
/// <param name="typeDefn">Type definition DU case from the Untyped tree.</param>
let mkTypeDefn (resolver : TypedTreeInfoResolver) (typeDefn : TypeDefn) : TypeDefn =
    let tdn = TypeDefn.TypeDefnNode typeDefn

    let typeName =
        // To overcome
        // "typecheck error The representation of this type is hidden by the signature."
        // It must be given an attribute such as [<Sealed>], [<Class>] or [<Interface>] to indicate the characteristics of the type.
        // We insert an additional `[<Class>]` attribute when no constructor is present for a TypeDefn.Regular.
        let attributes =
            let hasClassAttribute = hasAttribute "Class" tdn.TypeName.Attributes

            let allMembersAreAbstract =
                tdn.Members
                |> List.forall (
                    function
                    | MemberDefn.AbstractSlot _ -> true
                    | _ -> false
                )

            match typeDefn with
            | TypeDefn.Regular _ ->
                if
                    tdn.TypeName.ImplicitConstructor.IsSome
                    || hasClassAttribute
                    || allMembersAreAbstract
                then
                    tdn.TypeName.Attributes
                else
                    let classAttribute =
                        AttributeListNode (
                            stn "[<",
                            [ AttributeNode (iln "Class", None, None, zeroRange) ],
                            stn ">]",
                            zeroRange
                        )

                    match tdn.TypeName.Attributes with
                    | None -> Some (MultipleAttributeListNode ([ classAttribute ], zeroRange))
                    | Some multipleAttributeListNode ->
                        Some (
                            MultipleAttributeListNode (
                                classAttribute :: multipleAttributeListNode.AttributeLists,
                                zeroRange
                            )
                        )
            | _ -> tdn.TypeName.Attributes

        TypeNameNode (
            tdn.TypeName.XmlDoc,
            attributes,
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

    let mkImplicitCtor
        (resolver : TypedTreeInfoResolver)
        (identifier : IdentListNode)
        (implicitCtor : ImplicitConstructorNode)
        =
        let { ConstructorInfo = ctor } = resolver.GetTypeInfo identifier.Range.Proxy

        let returnTypeFromIdentifier = Type.LongIdent identifier

        let returnType =
            match ctor with
            | None ->
                TypeFunsNode ([ Type.LongIdent (iln "unit"), stn "->" ], returnTypeFromIdentifier, zeroRange)
                |> Type.Funs
            | Some (typeString, _) -> mkTypeFromString typeString

        // Convert the SimplePats to Patterns
        let parameters =
            match implicitCtor.Parameters with
            | [] ->
                // Even when there are new explicit parameters we still need to pass `()` as a unit parameter.
                [ Pattern.Unit (UnitNode (stn "(", stn ")", zeroRange)) ]
            | parameters ->
                let wrapAsTupleIfMultiple (parameters : Pattern list) =
                    if parameters.Length < 2 then
                        parameters
                    else
                        // Wrap as tuple as that is the only way parameters can behave in an implicit constructor.
                        [ Pattern.Tuple (PatTupleNode (parameters, zeroRange)) ]

                parameters
                |> List.map (fun simplePat ->
                    PatParameterNode (
                        simplePat.Attributes,
                        Pattern.Named (PatNamedNode (None, simplePat.Identifier, zeroRange)),
                        simplePat.Type,
                        zeroRange
                    )
                    |> Pattern.Parameter
                )
                |> wrapAsTupleIfMultiple

        let returnType =
            mkTypeForValNodeBasedOnTypedTree returnType parameters (Some returnTypeFromIdentifier)

        MemberDefnSigMemberNode (
            ValNode (
                implicitCtor.XmlDoc,
                implicitCtor.Attributes,
                None,
                None,
                false,
                implicitCtor.Accessibility,
                stn "new",
                None,
                returnType,
                None,
                None,
                zeroRange
            ),
            None,
            zeroRange
        )
        |> MemberDefn.SigMember

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
        TypeDefnRegularNode (
            typeName,
            [
                match tdn.TypeName.ImplicitConstructor with
                | None -> ()
                | Some implicitCtor -> yield mkImplicitCtor resolver tdn.TypeName.Identifier implicitCtor
                yield! mkMembers resolver tdn.Members
            ],
            zeroRange
        )
        |> TypeDefn.Regular

    | TypeDefn.Union unionNode ->
        TypeDefnUnionNode (
            typeName,
            unionNode.Accessibility,
            unionNode.UnionCases,
            mkMembers resolver tdn.Members,
            zeroRange
        )
        |> TypeDefn.Union

    | TypeDefn.Abbrev abbrevNode ->
        TypeDefnAbbrevNode (typeName, abbrevNode.Type, mkMembers resolver tdn.Members, zeroRange)
        |> TypeDefn.Abbrev

    | TypeDefn.Augmentation _ ->
        TypeDefnAugmentationNode (typeName, mkMembers resolver tdn.Members, zeroRange)
        |> TypeDefn.Augmentation

    | _ -> failwith "todo, 17AA2504-F9C2-4418-8614-93E9CF6699BC"

let wrapTypeInParentheses (t : Type) =
    Type.Paren (TypeParenNode (stn "(", t, stn ")", zeroRange))

/// <summary>
/// If a function is fully typed in the input Oak, we can re-use that exact information to construct the return type for the ValNode.
/// This only works when every parameter was typed and, the return type is present and no generics are involved.
/// </summary>
let mkTypeForValNodeBasedOnOak (fullyTypedParameters : (Type * PatParameterNode) list) (returnType : Type) : Type =
    let parameters =
        fullyTypedParameters
        |> List.map (fun (t, parameter) ->
            let name =
                match parameter.Pattern with
                | Pattern.Named namedPat -> Some namedPat.Name
                | _ -> None

            let t =
                match t with
                | Type.Funs _ -> wrapTypeInParentheses t
                | _ -> t

            let parameterType =
                TypeSignatureParameterNode (parameter.Attributes, name, t, zeroRange)
                |> Type.SignatureParameter

            parameterType, stn "->"
        )

    TypeFunsNode (parameters, returnType, zeroRange) |> Type.Funs

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
    (returnTypeInSource : Type option)
    : Type
    =
    // The `returnType` constructed from the typed tree cannot be trusted 100%.
    // We might receive `int -> int -> int` while the Oak only contained a single parameter.
    // This needs to be transformed to `int -> (int -> int)` to reflect that the return type actually is a function type.
    let returnTypeCorrectByActualParameters =
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
                    | Some t -> t
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

        match returnTypeCorrectByActualParameters with
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
        | _ -> returnType

    returnTypeWithParameterNames

let mkTypeForValNode
    (resolver : TypedTreeInfoResolver)
    (nameRange : range)
    (parameters : Pattern list)
    (returnTypeInSource : Type option)
    : Type
    =
    let returnTypeString, _ = resolver.GetFullForBinding nameRange.Proxy
    let t = mkTypeFromString returnTypeString

    let isTypedPatternWithoutGenerics (p : Pattern) =
        match p with
        | Pattern.Paren parenNode ->
            match parenNode.Pattern with
            | Pattern.Parameter parameterNode -> parameterNode.Type |> Option.map (fun t -> t, parameterNode)
            | _ -> None
        | _ -> None

    let returnType =
        match returnTypeInSource with
        | Some returnType ->
            let fullyTypedParameters = List.choose isTypedPatternWithoutGenerics parameters

            if parameters.Length = fullyTypedParameters.Length then
                mkTypeForValNodeBasedOnOak fullyTypedParameters returnType
            else
                mkTypeForValNodeBasedOnTypedTree t parameters returnTypeInSource
        | None -> mkTypeForValNodeBasedOnTypedTree t parameters None

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

let mkModuleDecl (resolver : TypedTreeInfoResolver) (mdl : ModuleDecl) : ModuleDecl option =
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
                let rt = bindingNode.ReturnType |> Option.map (fun rt -> rt.Type)
                mkTypeForValNode resolver nameRange bindingNode.Parameters rt

            let expr =
                if hasAttribute "Literal" bindingNode.Attributes then
                    Some bindingNode.Expr
                else
                    None

            ValNode (
                bindingNode.XmlDoc,
                bindingNode.Attributes,
                Some valKw,
                bindingNode.Inline,
                false,
                bindingNode.Accessibility,
                name,
                None,
                t,
                Some (stn "="),
                expr,
                zeroRange
            )
            |> ModuleDecl.Val
            |> Some
        | _ -> failwith "todo, C98DD050-A18C-4D8D-A025-083B352C57A5"

    | ModuleDecl.TypeDefn typeDefn -> mkTypeDefn resolver typeDefn |> ModuleDecl.TypeDefn |> Some
    | ModuleDecl.OpenList _ -> Some mdl
    | ModuleDecl.DeclExpr _ -> None
    | ModuleDecl.NestedModule nestedModule ->
        NestedModuleNode (
            nestedModule.XmlDoc,
            nestedModule.Attributes,
            nestedModule.Module,
            nestedModule.Accessibility,
            false,
            nestedModule.Identifier,
            nestedModule.Equals,
            List.choose (mkModuleDecl resolver) nestedModule.Declarations,
            zeroRange
        )
        |> ModuleDecl.NestedModule
        |> Some
    | ModuleDecl.Exception exceptionNode ->
        ExceptionDefnNode (
            exceptionNode.XmlDoc,
            exceptionNode.Attributes,
            exceptionNode.Accessibility,
            exceptionNode.UnionCase,
            exceptionNode.WithKeyword,
            List.choose (mkMember resolver) exceptionNode.Members,
            zeroRange
        )
        |> ModuleDecl.Exception
        |> Some
    | ModuleDecl.ModuleAbbrev _ -> Some mdl
    | _ -> failwith "todo, 56EF9CEE-A28B-437D-8A0F-EBE7E0AA850F"

let mkModuleOrNamespace
    (resolver : TypedTreeInfoResolver)
    (moduleNode : ModuleOrNamespaceNode)
    : ModuleOrNamespaceNode
    =
    let decls = List.choose (mkModuleDecl resolver) moduleNode.Declarations
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
