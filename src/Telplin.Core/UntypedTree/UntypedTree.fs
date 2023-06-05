module rec Telplin.Core.UntypedTree.Writer

open Fantomas.FCS.Text
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Microsoft.FSharp.Core
open Telplin.Core
open Telplin.Core.UntypedTree.ASTCreation
open Telplin.Core.UntypedTree.TypeForValNode
open Telplin.Core.UntypedTree.SourceParser

let mkLeadingKeywordForProperty (propertyNode : MemberDefnPropertyGetSetNode) =
    let hasDefault =
        propertyNode.LeadingKeyword.Content
        |> List.exists (fun stn -> stn.Text = "default")

    if hasDefault then
        mtn "override"
    else
        propertyNode.LeadingKeyword

[<RequireQualifiedAccess>]
type MemberDefnResult =
    | None
    | SingleMember of MemberDefn
    | GetAndSetMember of MemberDefn * MemberDefn
    | Error of TelplinError

let mkMember (resolver : TypedTreeInfoResolver) (md : MemberDefn) : MemberDefnResult =
    let mdRange = (MemberDefn.Node md).Range

    match md with
    | MemberDefn.ValField _
    | MemberDefn.AbstractSlot _
    | MemberDefn.Inherit _ -> MemberDefnResult.SingleMember md
    | MemberDefn.LetBinding _
    | MemberDefn.DoExpr _ -> MemberDefnResult.None

    | MemberDefn.ImplicitInherit implicitInherit ->
        let t =
            match implicitInherit with
            | InheritConstructor.Unit inheritCtor -> inheritCtor.Type
            | InheritConstructor.Paren inheritCtor -> inheritCtor.Type
            | InheritConstructor.Other inheritCtor -> inheritCtor.Type
            | InheritConstructor.TypeOnly inheritCtor -> inheritCtor.Type

        MemberDefnInheritNode (implicitInherit.InheritKeyword, t, zeroRange)
        |> MemberDefn.Inherit
        |> MemberDefnResult.SingleMember

    | MemberDefn.Member bindingNode ->
        match bindingNode.FunctionName with
        | Choice2Of2 _ -> MemberDefnResult.None
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

            let returnTypeResult = mkTypeForValNode resolver name.Range bindingNode.Parameters

            match returnTypeResult with
            | Error error -> MemberDefnResult.Error (TelplinError (mdRange, error))
            | Ok returnType ->

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
            |> MemberDefnResult.SingleMember

    | MemberDefn.AutoProperty autoProperty ->
        let valKw =
            autoProperty.LeadingKeyword.Content
            |> List.filter (fun stn -> stn.Text <> "val")
            |> fun keywords -> MultipleTextsNode (keywords, autoProperty.LeadingKeyword.Range)

        let name = autoProperty.Identifier
        let returnTypeResult = mkTypeForValNode resolver name.Range []

        match returnTypeResult with
        | Error error -> MemberDefnResult.Error (TelplinError (mdRange, error))
        | Ok returnType ->

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
        |> MemberDefnResult.SingleMember

    | MemberDefn.ExplicitCtor explicitNode ->
        let name = explicitNode.New

        let returnTypeResult = mkTypeForValNode resolver name.Range [ explicitNode.Pattern ]

        match returnTypeResult with
        | Error error -> MemberDefnResult.Error (TelplinError (mdRange, error))
        | Ok returnType ->

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
        |> MemberDefnResult.SingleMember

    | MemberDefn.Interface interfaceNode ->
        MemberDefnInterfaceNode (interfaceNode.Interface, interfaceNode.Type, None, [], zeroRange)
        |> MemberDefn.Interface
        |> MemberDefnResult.SingleMember

    // We need to create two val in this case, see #52
    | PropertyGetSetWithExtraParameter (propertyNode, getBinding, setBinding) ->
        let name =
            match List.tryLast propertyNode.MemberName.Content with
            | Some (IdentifierOrDot.Ident name) -> name
            | _ -> failwith "Property does not have a name?"

        let leadingKeyword = mkLeadingKeywordForProperty propertyNode

        let getReturnType =
            mkTypeForGetSetMemberValNode resolver $"get_%s{name.Text}" name.Range getBinding.Parameters

        let setReturnType =
            mkTypeForGetSetMemberValNode resolver $"set_%s{name.Text}" name.Range [ setBinding.Parameters.[0] ]

        match getReturnType, setReturnType with
        | Error error, Ok _
        | Ok _, Error error
        | Error error, Error _ -> MemberDefnResult.Error (TelplinError (mdRange, error))
        | Ok getReturnType, Ok setReturnType ->

        let getSigMember =
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
                    getReturnType,
                    Some (stn "="),
                    None,
                    zeroRange
                ),
                Some (MultipleTextsNode ([ stn "with" ; stn "get" ], zeroRange)),
                zeroRange
            )
            |> MemberDefn.SigMember

        let setSigMember =
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
                    setReturnType,
                    Some (stn "="),
                    None,
                    zeroRange
                ),
                Some (MultipleTextsNode ([ stn "with" ; stn "set" ], zeroRange)),
                zeroRange
            )
            |> MemberDefn.SigMember

        let sigs =
            if Position.posGt getBinding.Range.Start setBinding.Range.Start then
                getSigMember, setSigMember
            else
                setSigMember, getSigMember

        MemberDefnResult.GetAndSetMember sigs

    | MemberDefn.PropertyGetSet propertyNode ->
        let name =
            match List.tryLast propertyNode.MemberName.Content with
            | Some (IdentifierOrDot.Ident name) -> name
            | _ -> failwith "Property does not have a name?"

        let leadingKeyword = mkLeadingKeywordForProperty propertyNode

        let returnTypeResult =
            match propertyNode.LastBinding with
            | None ->
                let binding = propertyNode.FirstBinding

                if binding.LeadingKeyword.Text = "set" && binding.Parameters.Length = 2 then
                    // If we are dealing with an indexed setter, the signature is rather funky.
                    // member x.Set (idx:int) (v: string) = ()
                    // Will become member Set: idx: int -> string with set
                    mkTypeForGetSetMemberValNode resolver $"set_%s{name.Text}" name.Range [ binding.Parameters.[0] ]
                else
                    mkTypeForValNode resolver name.Range propertyNode.FirstBinding.Parameters
            | Some lastBinding ->
                match propertyNode.FirstBinding.Parameters, lastBinding.Parameters with
                | [ Pattern.Unit _ ], [ _ ]
                | [ _ ], [ Pattern.Unit _ ] ->
                    // The getter takes a unit argument:
                    //     member __.DisableInMemoryProjectReferences
                    //          with get () = disableInMemoryProjectReferences
                    //          and set (value) = disableInMemoryProjectReferences <- value
                    mkTypeForValNode resolver name.Range []
                | parameters -> failwith $"Unexpected get/set property %A{parameters}"

        let withGetSet =
            match propertyNode.LastBinding with
            | None -> [ propertyNode.FirstBinding.LeadingKeyword ]
            | Some lastBinding ->
                [
                    stn $"%s{propertyNode.FirstBinding.LeadingKeyword.Text},"
                    lastBinding.LeadingKeyword
                ]

        match returnTypeResult with
        | Error error -> MemberDefnResult.Error (TelplinError (mdRange, error))
        | Ok returnType ->

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
        |> MemberDefnResult.SingleMember

    | md -> MemberDefnResult.Error (TelplinError (mdRange, $"Not implemented MemberDefn: %A{md}"))

let mkMembers (resolver : TypedTreeInfoResolver) (ms : MemberDefn list) : MemberDefn list * TelplinError list =
    (ms, ([], []))
    ||> List.foldBack (fun md (sigMembers, errors) ->
        match mkMember resolver md with
        | MemberDefnResult.None -> sigMembers, errors
        | MemberDefnResult.Error error -> sigMembers, error :: errors
        | MemberDefnResult.SingleMember md -> md :: sigMembers, errors
        | MemberDefnResult.GetAndSetMember (g, s) -> s :: g :: sigMembers, errors
    )

/// <summary>
/// Map a TypeDefn to its signature counterpart.
/// A lot of information can typically be re-used when the same syntax applies.
/// The most important things that need mapping are the implicit constructor and the type members.
/// </summary>
/// <param name="resolver">Resolves information from the Typed tree.</param>
/// <param name="forceAndKeyword">In case of a recursive nested module, subsequent types need the `and` keyword.</param>>
/// <param name="typeDefn">Type definition DU case from the Untyped tree.</param>
let mkTypeDefn
    (resolver : TypedTreeInfoResolver)
    (forceAndKeyword : bool)
    (typeDefn : TypeDefn)
    : TypeDefn * TelplinError list
    =
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

    let mkImplicitCtor
        (resolver : TypedTreeInfoResolver)
        (identifier : IdentListNode)
        (implicitCtor : ImplicitConstructorNode)
        : Result<MemberDefn, TelplinError>
        =
        let bindingInfoResult = resolver.GetTypeInfo identifier.Range.FCSRange

        match bindingInfoResult with
        | Error error -> TelplinError (implicitCtor.Range, error) |> Result.Error
        | Ok bindingInfo ->

        let returnTypeResult =
            match bindingInfo.ConstructorInfo with
            | None ->
                TypeFunsNode ([ Type.LongIdent (iln "unit"), stn "->" ], typeNameType, zeroRange)
                |> Type.Funs
                |> Ok
            | Some { ReturnTypeString = typeString } -> mkTypeFromString typeString

        // Convert the SimplePats to Patterns
        let parameters =
            let ctorPatterns =
                implicitCtor.Items
                |> List.choose (
                    function
                    | Choice1Of2 sp -> Some sp
                    | Choice2Of2 _ -> None
                )

            match ctorPatterns with
            | [] ->
                // Even when there are new explicit parameters we still need to pass `()` as a unit parameter.
                [ Pattern.Unit (UnitNode (stn "(", stn ")", zeroRange)) ]
            | parameters ->
                let wrapAsTupleIfMultiple (parameters : Pattern list) =
                    match parameters with
                    | []
                    | [ _ ] -> parameters
                    | h :: tail ->

                        // Wrap as tuple as that is the only way parameters can behave in an implicit constructor.
                        let tupleParameters =
                            [
                                yield Choice1Of2 h
                                for p in tail do
                                    yield Choice2Of2 (stn ",")
                                    yield Choice1Of2 p
                            ]

                        [ Pattern.Tuple (PatTupleNode (tupleParameters, zeroRange)) ]

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

        match returnTypeResult with
        | Error error -> TelplinError (implicitCtor.Range, error) |> Result.Error
        | Ok returnType ->

        let returnType =
            let typedTreeInfo =
                {
                    ReturnType = returnType
                    BindingGenericParameters = []
                    TypeGenericParameters = []
                }

            mkTypeForValNodeBasedOnTypedTree typedTreeInfo parameters

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
        |> Result.Ok

    // let mkMembersForType members = mkMembers resolver members

    match typeDefn with
    | TypeDefn.Record recordNode ->
        let members, memberErrors = mkMembers resolver tdn.Members

        let sigRecord =
            TypeDefnRecordNode (
                typeName,
                recordNode.Accessibility,
                recordNode.OpeningBrace,
                recordNode.Fields,
                recordNode.ClosingBrace,
                members,
                zeroRange
            )
            |> TypeDefn.Record

        sigRecord, memberErrors

    | TypeDefn.Explicit explicitNode ->

        let members, memberErrors =
            let members, memberErrors = mkMembers resolver explicitNode.Body.Members

            match tdn.TypeName.ImplicitConstructor with
            | None -> members, memberErrors
            | Some implicitCtor ->
                match mkImplicitCtor resolver tdn.TypeName.Identifier implicitCtor with
                | Error error -> members, error :: memberErrors
                | Ok sigCtor -> sigCtor :: members, memberErrors

        let body =
            TypeDefnExplicitBodyNode (explicitNode.Body.Kind, members, explicitNode.Body.End, zeroRange)

        let extraMembers, extraMemberErrors = mkMembers resolver tdn.Members

        let sigExplicit =
            TypeDefnExplicitNode (typeName, body, extraMembers, zeroRange)
            |> TypeDefn.Explicit

        sigExplicit, (memberErrors @ extraMemberErrors)

    | TypeDefn.Regular _ ->
        let members, memberErrors =
            let members, memberErrors = mkMembers resolver tdn.Members

            match tdn.TypeName.ImplicitConstructor with
            | None -> members, memberErrors
            | Some implicitCtor ->
                match mkImplicitCtor resolver tdn.TypeName.Identifier implicitCtor with
                | Error error -> members, error :: memberErrors
                | Ok sigCtor -> sigCtor :: members, memberErrors

        let sigRegular =
            TypeDefnRegularNode (typeName, members, zeroRange) |> TypeDefn.Regular

        sigRegular, memberErrors

    | TypeDefn.Union unionNode ->
        let members, memberErrors = mkMembers resolver tdn.Members

        let sigUnion =
            TypeDefnUnionNode (typeName, unionNode.Accessibility, unionNode.UnionCases, members, zeroRange)
            |> TypeDefn.Union

        sigUnion, memberErrors

    | TypeDefn.Abbrev abbrevNode ->
        let members, memberErrors = mkMembers resolver tdn.Members

        let sigAbbrev =
            TypeDefnAbbrevNode (typeName, abbrevNode.Type, members, zeroRange)
            |> TypeDefn.Abbrev

        sigAbbrev, memberErrors

    | TypeDefn.Augmentation _ ->
        let members, memberErrors = mkMembers resolver tdn.Members

        let sigAugmentation =
            TypeDefnAugmentationNode (typeName, members, zeroRange) |> TypeDefn.Augmentation

        sigAugmentation, memberErrors

    | TypeDefn.None _ -> (TypeDefn.None tdn.TypeName), []
    | TypeDefn.Enum _
    | TypeDefn.Delegate _ -> typeDefn, []

let getLastIdentFromList (identList : IdentListNode) =
    match identList.Content with
    | [ IdentifierOrDot.Ident name ] -> name
    | _ -> failwith "todo, 38A9012C-2C4D-4387-9558-F75F6578402A"

[<RequireQualifiedAccess>]
type ModuleDeclResult =
    | None
    | SingleModuleDecl of ModuleDecl
    | Error of TelplinError
    | Nested of parent : ModuleDecl * childErrors : TelplinError list

let mkModuleDecl (resolver : TypedTreeInfoResolver) (mdl : ModuleDecl) : ModuleDeclResult =
    let mdlRange = (ModuleDecl.Node mdl).Range

    match mdl with
    | ModuleDecl.DeclExpr _
    | ModuleDecl.Attributes _ -> ModuleDeclResult.None
    | ModuleDecl.OpenList _
    | ModuleDecl.Val _
    | ModuleDecl.HashDirectiveList _
    | ModuleDecl.ModuleAbbrev _ -> ModuleDeclResult.SingleModuleDecl mdl
    | PrivateTopLevelBinding when not resolver.IncludePrivateBindings -> ModuleDeclResult.None
    | ModuleDecl.TopLevelBinding bindingNode ->
        match bindingNode.FunctionName with
        | Choice1Of2 name ->
            let valKw = mtn "val"
            let nameRange = name.Range
            let name = getLastIdentFromList name

            let returnTypeResult = mkTypeForValNode resolver nameRange bindingNode.Parameters

            match returnTypeResult with
            | Error error -> ModuleDeclResult.Error (TelplinError (mdlRange, error))
            | Ok returnType ->

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
            |> ModuleDeclResult.SingleModuleDecl
        | Choice2Of2 _ -> ModuleDeclResult.Error (TelplinError (mdlRange, "Pattern identifiers are not supported"))

    | ModuleDecl.TypeDefn typeDefn ->
        let sigTypeDefn, memberErrors = mkTypeDefn resolver false typeDefn
        ModuleDeclResult.Nested (ModuleDecl.TypeDefn sigTypeDefn, memberErrors)

    | ModuleDecl.NestedModule nestedModule ->
        let sigs, errors =
            if not nestedModule.IsRecursive then
                (nestedModule.Declarations, ([], []))
                ||> List.foldBack (fun decl (sigs, errors) ->
                    match mkModuleDecl resolver decl with
                    | ModuleDeclResult.None -> sigs, errors
                    | ModuleDeclResult.SingleModuleDecl sigDecl -> sigDecl :: sigs, errors
                    | ModuleDeclResult.Error error -> sigs, error :: errors
                    | ModuleDeclResult.Nested (sigDecl, nestedErrors) -> sigDecl :: sigs, nestedErrors @ errors
                )
            else
                // A nested module cannot be recursive in a signature file.
                // Any subsequent types (SynModuleDecl.Types) should be transformed to use the `and` keyword.
                let rec visit
                    (lastItemIsType : bool)
                    (decls : ModuleDecl list)
                    (continuation : ModuleDecl list * TelplinError list -> ModuleDecl list * TelplinError list)
                    : ModuleDecl list * TelplinError list
                    =
                    match decls with
                    | [] -> continuation ([], [])
                    | currentDecl :: nextDecls ->

                    let isType, declResult =
                        match currentDecl with
                        | ModuleDecl.TypeDefn typeDefnNode ->
                            let sigTypeDefn, errors = mkTypeDefn resolver lastItemIsType typeDefnNode

                            true, ModuleDeclResult.Nested (ModuleDecl.TypeDefn sigTypeDefn, errors)
                        | decl -> false, mkModuleDecl resolver decl

                    visit
                        isType
                        nextDecls
                        (fun (sigs, errors) ->
                            match declResult with
                            | ModuleDeclResult.None -> sigs, errors
                            | ModuleDeclResult.SingleModuleDecl sigDecl -> sigDecl :: sigs, errors
                            | ModuleDeclResult.Error error -> sigs, error :: errors
                            | ModuleDeclResult.Nested (sigDecl, nestedErrors) -> sigDecl :: sigs, nestedErrors @ errors
                            |> continuation
                        )

                visit false nestedModule.Declarations id

        let sigNestedModule =
            NestedModuleNode (
                nestedModule.XmlDoc,
                nestedModule.Attributes,
                nestedModule.Module,
                nestedModule.Accessibility,
                false,
                nestedModule.Identifier,
                nestedModule.Equals,
                sigs,
                zeroRange
            )
            |> ModuleDecl.NestedModule

        ModuleDeclResult.Nested (sigNestedModule, errors)
    | ModuleDecl.Exception exceptionNode ->
        let sigMembers, errors = mkMembers resolver exceptionNode.Members

        let sigException =
            ExceptionDefnNode (
                exceptionNode.XmlDoc,
                exceptionNode.Attributes,
                exceptionNode.Accessibility,
                exceptionNode.UnionCase,
                exceptionNode.WithKeyword,
                sigMembers,
                zeroRange
            )
            |> ModuleDecl.Exception

        ModuleDeclResult.Nested (sigException, errors)
    | ModuleDecl.ExternBinding externBindingNode ->
        let nameRange = externBindingNode.Identifier.Range
        let name = getLastIdentFromList externBindingNode.Identifier
        let bindingInfoResult = resolver.GetFullForBinding nameRange.FCSRange

        match bindingInfoResult with
        | Error error -> ModuleDeclResult.Error (TelplinError (mdlRange, error))
        | Ok bindingInfo ->

        match mkTypeFromString bindingInfo.ReturnTypeString with
        | Error error -> ModuleDeclResult.Error (TelplinError (mdlRange, error))
        | Ok returnType ->

        let returnType =
            match returnType with
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
        |> ModuleDeclResult.SingleModuleDecl

let mkModuleOrNamespace
    (resolver : TypedTreeInfoResolver)
    (moduleNode : ModuleOrNamespaceNode)
    : ModuleOrNamespaceNode * TelplinError list
    =
    let decls, errors =
        (moduleNode.Declarations, ([], []))
        ||> List.foldBack (fun mdl (sigs, errors) ->
            match mkModuleDecl resolver mdl with
            | ModuleDeclResult.None -> sigs, errors
            | ModuleDeclResult.SingleModuleDecl sigDecl -> sigDecl :: sigs, errors
            | ModuleDeclResult.Error error -> sigs, error :: errors
            | ModuleDeclResult.Nested (sigDecl, childErrors) -> sigDecl :: sigs, childErrors @ errors
        )

    ModuleOrNamespaceNode (moduleNode.Header, decls, zeroRange), errors

let mkSignatureFile (resolver : TypedTreeInfoResolver) (code : string) : string * TelplinError list =
    let ast, _diagnostics =
        Fantomas.FCS.Parse.parseFile false (SourceText.ofString code) resolver.Defines

    let implementationOak = CodeFormatter.TransformAST (ast, code)

    let signatureOak, (errors : TelplinError list) =
        let mdns, errors =
            List.map (mkModuleOrNamespace resolver) implementationOak.ModulesOrNamespaces
            |> List.unzip

        Oak (implementationOak.ParsedHashDirectives, mdns, zeroRange), List.concat errors

    let code = CodeFormatter.FormatOakAsync signatureOak |> Async.RunSynchronously
    code, errors
