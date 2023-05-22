module rec Telplin.UntypedTree.Writer

open FSharp.Compiler.Text
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Telplin.Common
open ASTCreation
open TypeForValNode
open SourceParser

let mkLeadingKeywordForProperty (propertyNode : MemberDefnPropertyGetSetNode) =
    let hasDefault =
        propertyNode.LeadingKeyword.Content
        |> List.exists (fun stn -> stn.Text = "default")

    if hasDefault then
        mtn "override"
    else
        propertyNode.LeadingKeyword

let mkMember
    (resolver : TypedTreeInfoResolver)
    (typeParameterMap : Map<string, string>)
    (md : MemberDefn)
    : MemberDefn list
    =
    match md with
    | MemberDefn.ValField _
    | MemberDefn.AbstractSlot _
    | MemberDefn.Inherit _ -> List.singleton md
    | MemberDefn.LetBinding _
    | MemberDefn.DoExpr _ -> List.empty

    | MemberDefn.ImplicitInherit implicitInherit ->
        let t =
            match implicitInherit with
            | InheritConstructor.Unit inheritCtor -> inheritCtor.Type
            | InheritConstructor.Paren inheritCtor -> inheritCtor.Type
            | InheritConstructor.Other inheritCtor -> inheritCtor.Type
            | InheritConstructor.TypeOnly inheritCtor -> inheritCtor.Type

        MemberDefnInheritNode (implicitInherit.InheritKeyword, t, zeroRange)
        |> MemberDefn.Inherit
        |> List.singleton

    | MemberDefn.Member bindingNode ->
        match bindingNode.FunctionName with
        | Choice2Of2 _ -> List.empty
        | Choice1Of2 name ->
            let valKw = bindingNode.LeadingKeyword

            let name =
                match name.Content with
                // member this.Foo
                | [ IdentifierOrDot.Ident _
                    (IdentifierOrDot.KnownDot _ | IdentifierOrDot.UnknownDot)
                    IdentifierOrDot.Ident name ]
                // static member Foo
                | [ IdentifierOrDot.Ident name ] -> name
                | _ -> failwith "todo, 38A9012C-2C4D-4387-9558-F75F6578402A"

            let returnType =
                mkTypeForValNode resolver name.Range typeParameterMap bindingNode.Parameters

            MemberDefnSigMemberNode (
                ValNode (
                    bindingNode.XmlDoc,
                    bindingNode.Attributes,
                    Some valKw,
                    bindingNode.Inline,
                    false,
                    bindingNode.Accessibility,
                    name,
                    None,
                    returnType,
                    Some (stn "="),
                    None,
                    zeroRange
                ),
                None,
                zeroRange
            )
            |> MemberDefn.SigMember
            |> List.singleton

    | MemberDefn.AutoProperty autoProperty ->
        let valKw =
            autoProperty.LeadingKeyword.Content
            |> List.filter (fun stn -> stn.Text <> "val")
            |> fun keywords -> MultipleTextsNode (keywords, autoProperty.LeadingKeyword.Range)

        let name = autoProperty.Identifier
        let returnType = mkTypeForValNode resolver name.Range Map.empty []

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
                returnType,
                Some (stn "="),
                None,
                zeroRange
            ),
            autoProperty.WithGetSet,
            zeroRange
        )
        |> MemberDefn.SigMember
        |> List.singleton

    | MemberDefn.ExplicitCtor explicitNode ->
        let name = explicitNode.New

        let returnType =
            mkTypeForValNode resolver name.Range Map.empty [ explicitNode.Pattern ]

        MemberDefnSigMemberNode (
            ValNode (
                explicitNode.XmlDoc,
                explicitNode.Attributes,
                None,
                None,
                false,
                None,
                name,
                None,
                returnType,
                Some (stn "="),
                None,
                zeroRange
            ),
            None,
            zeroRange
        )
        |> MemberDefn.SigMember
        |> List.singleton

    | MemberDefn.Interface interfaceNode ->
        MemberDefnInterfaceNode (interfaceNode.Interface, interfaceNode.Type, None, [], zeroRange)
        |> MemberDefn.Interface
        |> List.singleton

    // We need to create two val in this case, see #52
    | PropertyGetSetWithExtraParameter (propertyNode, getBinding, setBinding) ->
        let name =
            match List.tryLast propertyNode.MemberName.Content with
            | Some (IdentifierOrDot.Ident name) -> name
            | _ -> failwith "Property does not have a name?"

        let leadingKeyword = mkLeadingKeywordForProperty propertyNode

        let getSigMember =
            let returnType =
                mkTypeForGetSetMemberValNode resolver $"get_%s{name.Text}" name.Range Map.empty getBinding.Parameters

            MemberDefnSigMemberNode (
                ValNode (
                    propertyNode.XmlDoc,
                    propertyNode.Attributes,
                    Some leadingKeyword,
                    None,
                    false,
                    None,
                    name,
                    None,
                    returnType,
                    Some (stn "="),
                    None,
                    zeroRange
                ),
                Some (MultipleTextsNode ([ stn "with" ; stn "get" ], zeroRange)),
                zeroRange
            )
            |> MemberDefn.SigMember

        let setSigMember =
            let returnType =
                mkTypeForGetSetMemberValNode
                    resolver
                    $"set_%s{name.Text}"
                    name.Range
                    Map.empty
                    [ setBinding.Parameters.[0] ]

            MemberDefnSigMemberNode (
                ValNode (
                    propertyNode.XmlDoc,
                    propertyNode.Attributes,
                    Some leadingKeyword,
                    None,
                    false,
                    None,
                    name,
                    None,
                    returnType,
                    Some (stn "="),
                    None,
                    zeroRange
                ),
                Some (MultipleTextsNode ([ stn "with" ; stn "set" ], zeroRange)),
                zeroRange
            )
            |> MemberDefn.SigMember

        [ getSigMember ; setSigMember ]

    | MemberDefn.PropertyGetSet propertyNode ->
        let name =
            match List.tryLast propertyNode.MemberName.Content with
            | Some (IdentifierOrDot.Ident name) -> name
            | _ -> failwith "Property does not have a name?"

        let leadingKeyword = mkLeadingKeywordForProperty propertyNode

        let returnType =
            match propertyNode.LastBinding with
            | None ->
                let binding = propertyNode.FirstBinding

                if binding.LeadingKeyword.Text = "set" && binding.Parameters.Length = 2 then
                    // If we are dealing with an indexed setter, the signature is rather funky.
                    // member x.Set (idx:int) (v: string) = ()
                    // Will become member Set: idx: int -> string with set
                    mkTypeForGetSetMemberValNode
                        resolver
                        $"set_%s{name.Text}"
                        name.Range
                        Map.empty
                        [ binding.Parameters.[0] ]
                else
                    mkTypeForValNode resolver name.Range Map.empty propertyNode.FirstBinding.Parameters
            | Some lastBinding ->
                match propertyNode.FirstBinding.Parameters, lastBinding.Parameters with
                | [ Pattern.Unit _ ], [ _ ]
                | [ _ ], [ Pattern.Unit _ ] ->
                    // The getter takes a unit argument:
                    //     member __.DisableInMemoryProjectReferences
                    //          with get () = disableInMemoryProjectReferences
                    //          and set (value) = disableInMemoryProjectReferences <- value
                    mkTypeForValNode resolver name.Range Map.empty []
                | parameters -> failwith $"Unexpected get/set property %A{parameters}"

        let withGetSet =
            match propertyNode.LastBinding with
            | None -> [ propertyNode.FirstBinding.LeadingKeyword ]
            | Some lastBinding ->
                [
                    stn $"%s{propertyNode.FirstBinding.LeadingKeyword.Text},"
                    lastBinding.LeadingKeyword
                ]

        MemberDefnSigMemberNode (
            ValNode (
                propertyNode.XmlDoc,
                propertyNode.Attributes,
                Some leadingKeyword,
                None,
                false,
                None,
                name,
                None,
                returnType,
                Some (stn "="),
                None,
                zeroRange
            ),
            Some (MultipleTextsNode ([ stn "with" ; yield! withGetSet ], zeroRange)),
            zeroRange
        )
        |> MemberDefn.SigMember
        |> List.singleton

    | md -> failwith $"todo, 32CF2FF3-D9AD-41B8-96B8-E559A2327E66, {(MemberDefn.Node md).Range} {md}"

let mkMembers
    (resolver : TypedTreeInfoResolver)
    (typeParameterMap : Map<string, string>)
    (ms : MemberDefn list)
    : MemberDefn list
    =
    List.collect (mkMember resolver typeParameterMap) ms

/// <summary>
/// Map a TypeDefn to its signature counterpart.
/// A lot of information can typically be re-used when the same syntax applies.
/// The most important things that need mapping are the implicit constructor and the type members.
/// </summary>
/// <param name="resolver">Resolves information from the Typed tree.</param>
/// <param name="forceAndKeyword">In case of a recursive nested module, subsequent types need the `and` keyword.</param>>
/// <param name="typeDefn">Type definition DU case from the Untyped tree.</param>
let mkTypeDefn (resolver : TypedTreeInfoResolver) (forceAndKeyword : bool) (typeDefn : TypeDefn) : TypeDefn =
    let tdn = TypeDefn.TypeDefnNode typeDefn

    let typeName =
        // To overcome
        // "typecheck error The representation of this type is hidden by the signature."
        // It must be given an attribute such as [<Sealed>], [<Class>] or [<Interface>] to indicate the characteristics of the type.
        // We insert an additional `[<Class>]` attribute when no constructor is present for a TypeDefn.Regular.
        let attributes =
            let hasExistingAttribute =
                hasAnyAttribute
                    (set [| "Class" ; "ClassAttribute" ; "Struct" ; "StructAttribute" |])
                    tdn.TypeName.Attributes

            let allMembersAreAbstractOrInherit =
                tdn.Members
                |> List.forall (
                    function
                    | MemberDefn.AbstractSlot _
                    | MemberDefn.Inherit _ -> true
                    | _ -> false
                )

            match typeDefn with
            | TypeDefn.Regular _ ->
                if
                    tdn.TypeName.ImplicitConstructor.IsSome
                    || hasExistingAttribute
                    || allMembersAreAbstractOrInherit
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

        let leadingKeyword =
            if forceAndKeyword then
                stn "and"
            else
                tdn.TypeName.LeadingKeyword

        TypeNameNode (
            tdn.TypeName.XmlDoc,
            attributes,
            leadingKeyword,
            tdn.TypeName.Accessibility,
            tdn.TypeName.Identifier,
            tdn.TypeName.TypeParameters,
            tdn.TypeName.Constraints,
            None,
            tdn.TypeName.EqualsToken,
            tdn.TypeName.WithKeyword,
            zeroRange
        )

    let typeNameType = Type.LongIdent tdn.TypeName.Identifier

    let typarMap =
        match tdn.TypeName.TypeParameters with
        | Some (TyparDecls.PostfixList prefixListNode) ->
            let untypedTreeNames =
                prefixListNode.Decls |> List.map (fun typarDecl -> typarDecl.TypeParameter.Text)

            let typedTreeNames : string list =
                resolver.GetTypeTyparNames tdn.TypeName.Identifier.Range.Proxy

            if untypedTreeNames.Length <> typedTreeNames.Length then
                failwith "unexpected difference in typar count"
            else
                List.zip typedTreeNames untypedTreeNames |> Map
        | _ -> Map.empty

    ignore typarMap

    let mkImplicitCtor
        (resolver : TypedTreeInfoResolver)
        (identifier : IdentListNode)
        (implicitCtor : ImplicitConstructorNode)
        =
        let bindingInfo = resolver.GetTypeInfo identifier.Range.Proxy

        let returnType =
            match bindingInfo.ConstructorInfo with
            | None ->
                TypeFunsNode ([ Type.LongIdent (iln "unit"), stn "->" ], typeNameType, zeroRange)
                |> Type.Funs
            | Some { ReturnTypeString = typeString } -> mkTypeFromString typeString

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
            let typedTreeInfo =
                {
                    ReturnType = returnType
                    BindingGenericParameters = []
                    TypeGenericParameters = []
                }

            mkTypeForValNodeBasedOnTypedTree typedTreeInfo Map.empty parameters

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

    let mkMembersForType members = mkMembers resolver typarMap members

    match typeDefn with
    | TypeDefn.Record recordNode ->
        TypeDefnRecordNode (
            typeName,
            recordNode.Accessibility,
            recordNode.OpeningBrace,
            recordNode.Fields,
            recordNode.ClosingBrace,
            mkMembersForType tdn.Members,
            zeroRange
        )
        |> TypeDefn.Record

    | TypeDefn.Explicit explicitNode ->
        let body =
            TypeDefnExplicitBodyNode (
                explicitNode.Body.Kind,
                [
                    match tdn.TypeName.ImplicitConstructor with
                    | None -> ()
                    | Some implicitCtor -> yield mkImplicitCtor resolver tdn.TypeName.Identifier implicitCtor
                    yield! mkMembersForType explicitNode.Body.Members
                ],
                explicitNode.Body.End,
                zeroRange
            )

        TypeDefnExplicitNode (typeName, body, mkMembersForType tdn.Members, zeroRange)
        |> TypeDefn.Explicit

    | TypeDefn.Regular _ ->
        TypeDefnRegularNode (
            typeName,
            [
                match tdn.TypeName.ImplicitConstructor with
                | None -> ()
                | Some implicitCtor -> yield mkImplicitCtor resolver tdn.TypeName.Identifier implicitCtor
                yield! mkMembersForType tdn.Members
            ],
            zeroRange
        )
        |> TypeDefn.Regular

    | TypeDefn.Union unionNode ->
        TypeDefnUnionNode (
            typeName,
            unionNode.Accessibility,
            unionNode.UnionCases,
            mkMembersForType tdn.Members,
            zeroRange
        )
        |> TypeDefn.Union

    | TypeDefn.Abbrev abbrevNode ->
        TypeDefnAbbrevNode (typeName, abbrevNode.Type, mkMembersForType tdn.Members, zeroRange)
        |> TypeDefn.Abbrev

    | TypeDefn.Augmentation _ ->
        TypeDefnAugmentationNode (typeName, mkMembersForType tdn.Members, zeroRange)
        |> TypeDefn.Augmentation

    | TypeDefn.None _ -> tdn.TypeName |> TypeDefn.None
    | TypeDefn.Enum _
    | TypeDefn.Delegate _ -> typeDefn

let getLastIdentFromList (identList : IdentListNode) =
    match identList.Content with
    | [ IdentifierOrDot.Ident name ] -> name
    | _ -> failwith "todo, 38A9012C-2C4D-4387-9558-F75F6578402A"

let mkModuleDecl (resolver : TypedTreeInfoResolver) (mdl : ModuleDecl) : ModuleDecl option =
    match mdl with
    | ModuleDecl.DeclExpr _
    | ModuleDecl.Attributes _ -> None
    | ModuleDecl.OpenList _
    | ModuleDecl.Val _
    | ModuleDecl.HashDirectiveList _
    | ModuleDecl.ModuleAbbrev _ -> Some mdl
    | ModuleDecl.TopLevelBinding bindingNode ->
        match bindingNode.FunctionName with
        | Choice1Of2 name ->
            let valKw = mtn "val"
            let nameRange = name.Range
            let name = getLastIdentFromList name

            let returnType =
                mkTypeForValNode resolver nameRange Map.empty bindingNode.Parameters

            let expr =
                if hasAnyAttribute (Set.singleton "Literal") bindingNode.Attributes then
                    Some bindingNode.Expr
                else
                    None

            let typeParameters =
                let rec hasGenericType t =
                    match t with
                    | Type.Var _ -> true
                    | Type.Paren p -> hasGenericType p.Type
                    | Type.WithGlobalConstraints gc -> hasGenericType gc.Type
                    | Type.AppPostfix appPostFix -> hasGenericType appPostFix.First || hasGenericType appPostFix.Last
                    | Type.SignatureParameter sp -> hasGenericType sp.Type
                    | Type.Funs funsNode ->
                        funsNode.Parameters |> List.exists (fun (t, _arrow) -> hasGenericType t)
                        || hasGenericType funsNode.ReturnType
                    | _ -> false

                let returnTypeHasConstraint =
                    match returnType with
                    | Type.WithGlobalConstraints _ -> true
                    | _ -> false

                // Only re-use the type parameters if the return type is not generic.
                if hasGenericType returnType then
                    if returnTypeHasConstraint then
                        None
                    else
                    // This scenario the untyped tree may have constraints while the typed tree didn't have them.
                    match bindingNode.GenericTypeParameters with
                    | Some (TyparDecls.PostfixList postfixList) when not postfixList.Constraints.IsEmpty ->
                        // This of course is a bit questionable whether we should just included the untyped generic type parameters
                        // or try to transform the return type to include any constraints.
                        bindingNode.GenericTypeParameters
                    | _ -> None
                else
                    bindingNode.GenericTypeParameters

            ValNode (
                bindingNode.XmlDoc,
                bindingNode.Attributes,
                Some valKw,
                bindingNode.Inline,
                false,
                bindingNode.Accessibility,
                name,
                typeParameters,
                returnType,
                Some (stn "="),
                expr,
                zeroRange
            )
            |> ModuleDecl.Val
            |> Some
        | _ -> failwith "todo, C98DD050-A18C-4D8D-A025-083B352C57A5"

    | ModuleDecl.TypeDefn typeDefn -> mkTypeDefn resolver false typeDefn |> ModuleDecl.TypeDefn |> Some

    | ModuleDecl.NestedModule nestedModule ->
        let decls =
            if not nestedModule.IsRecursive then
                List.choose (mkModuleDecl resolver) nestedModule.Declarations
            else
                // A nested module cannot be recursive in a signature file.
                // Any subsequent types (SynModuleDecl.Types) should be transformed to use the `and` keyword.
                let rec visit
                    (lastItemIsType : bool)
                    (decls : ModuleDecl list)
                    (continuation : ModuleDecl list -> ModuleDecl list)
                    : ModuleDecl list
                    =
                    match decls with
                    | [] -> continuation []
                    | currentDecl :: nextDecls ->

                    let isType, sigDecl =
                        match currentDecl with
                        | ModuleDecl.TypeDefn typeDefnNode ->
                            let sigDecl =
                                mkTypeDefn resolver lastItemIsType typeDefnNode |> ModuleDecl.TypeDefn |> Some

                            true, sigDecl
                        | decl -> false, mkModuleDecl resolver decl

                    visit
                        isType
                        nextDecls
                        (fun sigDecls ->
                            match sigDecl with
                            | None -> sigDecls
                            | Some sigDecl -> sigDecl :: sigDecls
                            |> continuation
                        )

                visit false nestedModule.Declarations id

        NestedModuleNode (
            nestedModule.XmlDoc,
            nestedModule.Attributes,
            nestedModule.Module,
            nestedModule.Accessibility,
            false,
            nestedModule.Identifier,
            nestedModule.Equals,
            decls,
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
            mkMembers resolver Map.empty exceptionNode.Members,
            zeroRange
        )
        |> ModuleDecl.Exception
        |> Some
    | ModuleDecl.ExternBinding externBindingNode ->
        let nameRange = externBindingNode.Identifier.Range
        let name = getLastIdentFromList externBindingNode.Identifier

        let returnType =
            let bindingInfo = resolver.GetFullForBinding nameRange.Proxy

            mkTypeFromString bindingInfo.ReturnTypeString
            // Type may have unwanted parentheses.
            |> function
                | Type.Paren parenNode -> parenNode.Type
                | t -> t

        ValNode (
            externBindingNode.XmlDoc,
            externBindingNode.Attributes,
            Some (mtn "val"),
            None,
            false,
            externBindingNode.Accessibility,
            name,
            None,
            returnType,
            Some (stn "="),
            None,
            zeroRange
        )
        |> ModuleDecl.Val
        |> Some

let mkModuleOrNamespace
    (resolver : TypedTreeInfoResolver)
    (moduleNode : ModuleOrNamespaceNode)
    : ModuleOrNamespaceNode
    =
    let decls = List.choose (mkModuleDecl resolver) moduleNode.Declarations
    ModuleOrNamespaceNode (moduleNode.Header, decls, zeroRange)

let mkSignatureFile (resolver : TypedTreeInfoResolver) (defines : string list) (code : string) =
    let ast, _diagnostics =
        Fantomas.FCS.Parse.parseFile false (SourceText.ofString code) defines

    let implementationOak = CodeFormatter.TransformAST (ast, code)

    let signatureOak =
        let mdns =
            List.map (mkModuleOrNamespace resolver) implementationOak.ModulesOrNamespaces

        Oak (implementationOak.ParsedHashDirectives, mdns, zeroRange)

    CodeFormatter.FormatOakAsync signatureOak |> Async.RunSynchronously
