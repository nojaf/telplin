module Autograph.UntypedTree.Writer

open System.IO
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open Autograph.Common
open Autograph.UntypedTree.SourceParser

let zeroRange : range = Range.Zero
let unitExpr : SynExpr = SynExpr.Const (SynConst.Unit, zeroRange)

[<RequireQualifiedAccess>]
type TypesFromPattern =
    | FailedToResolve of fullType : SynType
    | HasNoParameters
    | HasParameters of types : SynType list

let rec mkSynTypeFun (types : SynType list) : SynType =
    match types with
    | [] -> failwith "unexpected empty list"
    | [ t ] -> t
    | t :: rest -> SynType.Fun (t, mkSynTypeFun rest, zeroRange, { ArrowRange = zeroRange })

let mkSynTypeParen (t : SynType) : SynType = SynType.Paren (t, zeroRange)

let mkSynTypeTuple mkType ts =
    match ts with
    | []
    | [ _ ] -> failwith "ts cannot have a single or zero types"
    | h :: rest ->
        let types =
            SynTupleTypeSegment.Type (mkType h)
            :: List.collect (fun t -> [ SynTupleTypeSegment.Star zeroRange ; SynTupleTypeSegment.Type (mkType t) ]) rest

        SynType.Tuple (false, types, zeroRange)

let rec mkSynTypeOfParameterTypeName (p : ParameterTypeName) =
    match p with
    | ParameterTypeName.SingleIdentifier displayName ->
        SynType.LongIdent (SynLongIdent ([ Ident (displayName, zeroRange) ], [], [ None ]))
    | ParameterTypeName.FunctionType types ->
        List.map mkSynTypeOfParameterTypeName types |> mkSynTypeFun |> mkSynTypeParen
    | ParameterTypeName.GenericParameter (name, isSolveAtCompileTime) ->
        let typarStaticReq =
            if isSolveAtCompileTime then
                TyparStaticReq.HeadType
            else
                TyparStaticReq.None

        SynType.Var (SynTypar.SynTypar (Ident (name, zeroRange), typarStaticReq, false), zeroRange)
    | ParameterTypeName.PostFix (mainType, postType) ->
        SynType.App (
            mkSynTypeOfParameterTypeName postType,
            None,
            [ wrapInParenIfTuple mainType ],
            [],
            None,
            true,
            zeroRange
        )
    | ParameterTypeName.WithGenericArguments (name, parameterTypeNames) ->
        let args = List.map mkSynTypeOfParameterTypeName parameterTypeNames
        SynType.App (mkSynTypeOfParameterTypeName name, Some zeroRange, args, [], Some zeroRange, false, zeroRange)
    | ParameterTypeName.Tuple ts -> mkSynTypeTuple wrapInParenIfTuple ts

and wrapInParenIfTuple (p : ParameterTypeName) : SynType =
    match p with
    | ParameterTypeName.Tuple _ -> mkSynTypeOfParameterTypeName p |> mkSynTypeParen
    | _ -> mkSynTypeOfParameterTypeName p

let mkSynIdent text =
    SynIdent (Ident (text, zeroRange), None)

let mkSynLongIdent text =
    SynLongIdent ([ Ident (text, zeroRange) ], [], [ None ])

type Range with

    member r.Proxy : RangeProxy =
        RangeProxy (r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)

let wrapInParenWhenFunType (t : SynType) : SynType =
    match t with
    | SynType.Fun _ as ft -> mkSynTypeParen ft
    | t -> t

let removeParensInPat (pat : SynPat) =
    let rec visit (pat : SynPat) : SynPat =
        match pat with
        | SynPat.Paren (pat = pat) -> visit pat
        | pat -> pat

    visit pat

let removeParensInType (t : SynType) : SynType =
    let rec visit t =
        match t with
        | SynType.Paren (t, _) -> visit t
        | _ -> t

    visit t

let rec mkTypeFromPat
    (resolver : TypedTreeInfoResolver)
    (bindingRange : range)
    (useName : bool)
    (pat : SynPat)
    : SynType
    =
    match removeParensInPat pat with
    | SynPat.Typed (SynPat.Named(ident = SynIdent (ident, _)), synType, _) when useName ->
        SynType.SignatureParameter ([], false, Some ident, wrapInParenWhenFunType synType, zeroRange)
    | SynPat.Named(ident = SynIdent (ident, _)) ->
        let t =
            resolver.GetTypeNameFor ident.idRange.Proxy bindingRange.Proxy
            |> mkSynTypeOfParameterTypeName
            |> wrapInParenWhenFunType

        SynType.SignatureParameter ([], false, Some ident, t, zeroRange)
    | SynPat.Tuple (_, pats, _) ->
        let ts =
            List.map
                (fun pat ->
                    let useName =
                        useName
                        && match pat with
                           | SynPat.Paren(pat = SynPat.Tuple _) -> false
                           | _ -> true

                    mkTypeFromPat resolver bindingRange useName pat
                )
                pats

        mkSynTypeTuple id ts
    | SynPat.LongIdent (range = m) ->
        resolver.GetTypeNameFor m.Proxy bindingRange.Proxy
        |> mkSynTypeOfParameterTypeName
        |> wrapInParenWhenFunType
    | SynPat.Const (SynConst.Unit, _) -> ParameterTypeName.SingleIdentifier "unit" |> mkSynTypeOfParameterTypeName
    | SynPat.Attrib (pat, attrs, _range) ->
        match removeParensInType (mkTypeFromPat resolver bindingRange useName pat) with
        | SynType.SignatureParameter (_, optional, ident, t, range) ->
            SynType.SignatureParameter (attrs, optional, ident, t, range)
        | t -> SynType.SignatureParameter (attrs, false, None, t, zeroRange)
        |> wrapInParenWhenFunType
    | SynPat.OptionalVal (ident, _range) ->
        let t =
            resolver.GetTypeNameFor ident.idRange.Proxy bindingRange.Proxy
            |> mkSynTypeOfParameterTypeName
            |> wrapInParenWhenFunType

        // This returns an optional type, we need to unwrap it
        let t =
            match t with
            | SynType.App (SynType.LongIdent(longDotId = SynLongIdent(id = [ optionIdent ])),
                           lessRange,
                           typeArgs,
                           commaRanges,
                           greaterRange,
                           isPostfix,
                           range) when optionIdent.idText = "option" ->
                match typeArgs with
                | [] -> failwith "invalid AST in SynType.App"
                | [ single ] -> wrapInParenWhenFunType single
                | head :: tail -> SynType.App (head, lessRange, tail, commaRanges, greaterRange, isPostfix, range)
            | t -> t

        SynType.SignatureParameter ([], true, Some ident, t, zeroRange)
    | SynPat.Typed (SynPat.OptionalVal (ident, _range), synType, _) ->
        SynType.SignatureParameter ([], true, Some ident, wrapInParenWhenFunType synType, zeroRange)
    | SynPat.Typed (_, synType, _) -> wrapInParenWhenFunType synType
    | SynPat.As (SynPat.LongIdent (longDotId = synIdent), _rhs, _range) ->
        resolver.GetTypeNameFor synIdent.Range.Proxy bindingRange.Proxy
        |> mkSynTypeOfParameterTypeName
    | pat -> failwith $"todo 233BA311-87C9-49DA-BFE0-9BDFAB0B09BB, {pat}"

let collectParametersFromSynArgPats
    (resolver : TypedTreeInfoResolver)
    (bindingRange : range)
    (argPats : SynArgPats)
    : TypesFromPattern
    =
    match argPats with
    | SynArgPats.Pats [] -> TypesFromPattern.HasNoParameters
    | SynArgPats.Pats pats ->
        try
            List.map (mkTypeFromPat resolver bindingRange true) pats
            |> TypesFromPattern.HasParameters
        with ex ->
            // TODO: if return type is a function type

            // If a parameter didn't resolve we should just try and resolve the fullType instead as a last resort
            // We want to inspect the `FullType` anyways
            let response = resolver.GetReturnTypeFor bindingRange.Proxy

            let parameterType =
                match response.FullType, response.ReturnParameter with
                | ParameterTypeName.FunctionType fullType,
                  (ParameterTypeName.FunctionType returnType as returnTypeParameterType) ->
                    // If both types are function types we need to consider the return type as one argument
                    let actualParameters = List.take (fullType.Length - returnType.Length) fullType

                    [ yield! actualParameters ; yield returnTypeParameterType ]
                    |> ParameterTypeName.FunctionType
                | _ -> response.FullType

            parameterType
            |> mkSynTypeOfParameterTypeName
            |> removeParensInType
            |> TypesFromPattern.FailedToResolve
    | SynArgPats.NamePatPairs _ -> failwith "todo C4E1220D-B309-4E34-A972-C0CD8431DE70"

let rec mkSignatureFile (resolver : TypedTreeInfoResolver) (code : string) : string =
    let input, _ = Fantomas.FCS.Parse.parseFile false (SourceText.ofString code) []

    let output =
        match input with
        | ParsedInput.SigFile _ -> input
        | ParsedInput.ImplFile (ParsedImplFileInput (fileName,
                                                     _isScript,
                                                     qualifiedNameOfFile,
                                                     scopedPragmas,
                                                     parsedHashDirectives,
                                                     synModuleOrNamespaces,
                                                     _isLastCompiland,
                                                     _parsedImplFileInputTrivia)) ->
            let fileName = Path.ChangeExtension (fileName, ".fsi")

            let synModuleOrNamespaces =
                List.map (mkSynModuleOrNamespaceSig resolver) synModuleOrNamespaces

            ParsedSigFileInput (
                fileName,
                qualifiedNameOfFile,
                scopedPragmas,
                parsedHashDirectives,
                synModuleOrNamespaces,
                {
                    ConditionalDirectives = []
                    CodeComments = []
                }
            )
            |> ParsedInput.SigFile

    Fantomas.Core.CodeFormatter.FormatASTAsync output |> Async.RunSynchronously

and mkSynModuleOrNamespaceSig
    (resolver : TypedTreeInfoResolver)
    (SynModuleOrNamespace (longId,
                           isRecursive,
                           synModuleOrNamespaceKind,
                           synModuleDecls,
                           preXmlDoc,
                           synAttributeLists,
                           synAccessOption,
                           range,
                           synModuleOrNamespaceTrivia))
    : SynModuleOrNamespaceSig
    =
    let decls = List.collect (mkSynModuleSigDecl resolver) synModuleDecls

    let trivia : SynModuleOrNamespaceSigTrivia =
        {
            ModuleKeyword = synModuleOrNamespaceTrivia.ModuleKeyword
            NamespaceKeyword = synModuleOrNamespaceTrivia.NamespaceKeyword
        }

    SynModuleOrNamespaceSig (
        longId,
        isRecursive,
        synModuleOrNamespaceKind,
        decls,
        preXmlDoc,
        synAttributeLists,
        synAccessOption,
        range,
        trivia
    )

and mkSynModuleSigDecl (resolver : TypedTreeInfoResolver) (decl : SynModuleDecl) : SynModuleSigDecl list =
    match decl with
    | SynModuleDecl.Let (_, bindings, _) ->
        bindings
        |> List.map (fun binding ->
            let valSig = mkSynValSig resolver binding
            SynModuleSigDecl.Val (valSig, zeroRange)
        )
    | SynModuleDecl.Open (synOpenDeclTarget, _range) -> [ SynModuleSigDecl.Open (synOpenDeclTarget, zeroRange) ]
    | SynModuleDecl.Types (typeDefns, _) ->
        let types = List.map (mkSynTypeDefnSig resolver) typeDefns
        [ SynModuleSigDecl.Types (types, zeroRange) ]
    | SynModuleDecl.Exception (synExceptionDefn, _range) ->
        let exnSig = mkSynExceptionDefn resolver synExceptionDefn
        [ SynModuleSigDecl.Exception (exnSig, zeroRange) ]
    | SynModuleDecl.ModuleAbbrev (ident, longId, _range) -> [ SynModuleSigDecl.ModuleAbbrev (ident, longId, zeroRange) ]
    | SynModuleDecl.NestedModule (synComponentInfo, isRecursive, synModuleDecls, _isContinuing, _range, trivia) ->
        [
            mkSynModuleSigDeclNestedModule resolver synComponentInfo isRecursive synModuleDecls trivia
        ]
    | _ -> []

and mkSynValSig
    (resolver : TypedTreeInfoResolver)
    (SynBinding (_synAccessOption,
                 _synBindingKind,
                 isInline,
                 isMutable,
                 synAttributeLists,
                 preXmlDoc,
                 (SynValData (memberFlags = memberFlags) as synValData),
                 headPat,
                 synBindingReturnInfoOption,
                 synExpr,
                 _range,
                 _debugPointAtBinding,
                 _synBindingTrivia))
    : SynValSig
    =
    let ident, mBindingName, parameterTypes, vis =
        match memberFlags, headPat with
        // new
        | NewConstructorPattern (longDotId, argPats, vis) ->
            let identTrivia = List.tryHead longDotId.Trivia
            let mBindingName = longDotId.LongIdent.[0].idRange

            SynIdent (longDotId.LongIdent.[0], identTrivia),
            longDotId.LongIdent.[0].idRange,
            collectParametersFromSynArgPats resolver mBindingName argPats,
            vis
        | Some _,
          SynPat.LongIdent (longDotId = SynLongIdent (id = [ _ ; ident ] ; trivia = [ _ ; identTrivia ])
                            argPats = argPats
                            accessibility = vis) ->
            let mBindingName = ident.idRange

            SynIdent (ident, identTrivia),
            mBindingName,
            collectParametersFromSynArgPats resolver mBindingName argPats,
            vis

        // static member without property
        | Some _,
          SynPat.LongIdent (longDotId = (SynLongIdent (id = [ ident ] ; trivia = [ identTrivia ]))
                            argPats = argPats
                            accessibility = vis) ->
            let mBindingName = ident.idRange

            SynIdent (ident, identTrivia),
            mBindingName,
            collectParametersFromSynArgPats resolver mBindingName argPats,
            vis

        | None, SynPat.LongIdent (longDotId = longDotId ; argPats = argPats ; accessibility = vis) ->
            let identTrivia = List.tryHead longDotId.Trivia
            let mBindingName = longDotId.LongIdent.[0].idRange

            SynIdent (longDotId.LongIdent.[0], identTrivia),
            mBindingName,
            collectParametersFromSynArgPats resolver mBindingName argPats,
            vis

        | _, SynPat.Named (ident = SynIdent (ident, _) as synIdent ; accessibility = vis) ->
            synIdent, ident.idRange, TypesFromPattern.HasNoParameters, vis

        | _ -> failwith $"todo 245B29E5-9303-4911-ABBE-0C3EA80DB536 {headPat.Range.Proxy}"

    let returnType =
        match synBindingReturnInfoOption with
        | Some (SynBindingReturnInfo (typeName = t)) -> t
        | None ->
            let knownParameterCount =
                match parameterTypes with
                | TypesFromPattern.HasNoParameters _ -> 0
                | TypesFromPattern.FailedToResolve _ -> -1
                | TypesFromPattern.HasParameters ps -> ps.Length

            let response = resolver.GetReturnTypeFor mBindingName.Proxy

            if knownParameterCount > 0 then
                match response.FullType with
                | ParameterTypeName.FunctionType fts ->
                    // Only rely on ReturnParameter if it is not a function itself
                    if fts.Length = knownParameterCount + 1 then
                        response.ReturnParameter
                    else
                        let actualReturnType = List.skip knownParameterCount fts
                        ParameterTypeName.FunctionType actualReturnType
                | _ -> response.ReturnParameter
            elif knownParameterCount = -1 then
                response.FullType
            else
                response.FullType
            |> mkSynTypeOfParameterTypeName
        |> wrapInParenWhenFunType

    let t =
        match parameterTypes with
        | TypesFromPattern.HasNoParameters ->
            match memberFlags, returnType with
            | Some { IsInstance = true },
              SynType.Paren(innerType = SynType.Fun (argType = SynType.LongIdent _
                                                     returnType = SynType.Fun (returnType = t))) -> t
            | Some { IsInstance = false },
              SynType.Paren(innerType = SynType.Fun (argType = UnitType _ ; returnType = t)) -> t
            | _ -> returnType
        | TypesFromPattern.FailedToResolve fullType -> fullType
        | TypesFromPattern.HasParameters ts -> mkSynTypeFun [ yield! ts ; yield returnType ]

    let expr =
        match synAttributeLists with
        | [ {
                Attributes = [ {
                                   TypeName = SynLongIdent(id = [ literalIdent ])
                               } ]
            } ] when literalIdent.idText = "Literal" -> Some synExpr
        | _ -> None

    SynValSig (
        synAttributeLists,
        ident,
        SynValTyparDecls (None, false),
        t,
        // This isn't correct but doesn't matter for Fantomas
        synValData.SynValInfo,
        isInline,
        isMutable,
        preXmlDoc,
        vis,
        expr,
        zeroRange,
        {
            ValKeyword = Some zeroRange
            WithKeyword = None
            EqualsRange = None
        }
    )

and mkSynTypeDefnSig
    resolver
    (SynTypeDefn (SynComponentInfo (longId = typeInfo) as componentInfo,
                  typeRepr,
                  members,
                  implicitConstructor,
                  _range,
                  trivia))
    : SynTypeDefnSig
    =
    let typeSigRepr =
        match typeRepr with
        | SynTypeDefnRepr.Simple (synTypeDefnSimpleRepr, _range) ->
            SynTypeDefnSigRepr.Simple (synTypeDefnSimpleRepr, zeroRange)
        // Type extensions
        | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.Augmentation _, _synMemberDefns, _range) ->
            SynTypeDefnSigRepr.Simple (SynTypeDefnSimpleRepr.None zeroRange, zeroRange)
        | SynTypeDefnRepr.ObjectModel (synTypeDefnKind, synMemberDefns, _range) ->
            let members = List.choose (mkSynMemberSig resolver typeInfo) synMemberDefns
            SynTypeDefnSigRepr.ObjectModel (synTypeDefnKind, members, zeroRange)
        | _ -> failwith "todo 88635304-1D34-4AE0-96A4-348C7E47588E"

    let withKeyword =
        match typeRepr with
        | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.Augmentation _, _, _) -> Some zeroRange
        | _ -> trivia.WithKeyword

    let members = members |> List.choose (mkSynMemberSig resolver typeInfo)

    let componentInfo =
        match typeRepr with
        | SynTypeDefnRepr.ObjectModel(kind = SynTypeDefnKind.Unspecified) when implicitConstructor.IsNone ->
            // To overcome
            // "typecheck error The representation of this type is hidden by the signature."
            // It must be given an attribute such as [<Sealed>], [<Class>] or [<Interface>] to indicate the characteristics of the type.
            match componentInfo with
            | SynComponentInfo (synAttributeLists,
                                synTyparDeclsOption,
                                synTypeConstraints,
                                longId,
                                preXmlDoc,
                                preferPostfix,
                                synAccessOption,
                                range) ->
                let attributes =
                    match longId with
                    | [ singleIdent ] ->
                        let typeInfo = resolver.GetTypeInfo singleIdent.idRange.Proxy

                        if typeInfo.IsClass then
                            ({
                                Attributes =
                                    [
                                        {
                                            TypeName = mkSynLongIdent "Class"
                                            ArgExpr = unitExpr
                                            Target = None
                                            AppliesToGetterAndSetter = false
                                            Range = zeroRange
                                        }
                                    ]
                                Range = zeroRange
                            } : SynAttributeList)
                            :: synAttributeLists
                        else
                            synAttributeLists
                    | _ -> synAttributeLists

                SynComponentInfo (
                    attributes,
                    synTyparDeclsOption,
                    synTypeConstraints,
                    longId,
                    preXmlDoc,
                    preferPostfix,
                    synAccessOption,
                    range
                )

        | _ -> componentInfo

    SynTypeDefnSig (
        componentInfo,
        typeSigRepr,
        members,
        zeroRange,
        {
            EqualsRange = trivia.EqualsRange
            TypeKeyword = trivia.TypeKeyword
            WithKeyword = withKeyword
        }
    )

and mkSynExceptionDefn resolver (SynExceptionDefn (synExceptionDefnRepr, withKeyword, synMemberDefns, _range)) =
    let (SynExceptionDefnRepr (longId = longId)) = synExceptionDefnRepr
    let longId = Option.defaultValue [] longId

    let members = List.choose (mkSynMemberSig resolver longId) synMemberDefns
    SynExceptionSig (synExceptionDefnRepr, withKeyword, members, zeroRange)

and mkSynModuleSigDeclNestedModule resolver synComponentInfo isRecursive synModuleDecls trivia : SynModuleSigDecl =
    let decls = List.collect (mkSynModuleSigDecl resolver) synModuleDecls

    SynModuleSigDecl.NestedModule (
        synComponentInfo,
        isRecursive,
        decls,
        zeroRange,
        {
            ModuleKeyword = trivia.ModuleKeyword
            EqualsRange = trivia.EqualsRange
        }
    )

and mkSynMemberSig resolver (typeIdent : LongIdent) (md : SynMemberDefn) : SynMemberSig option =
    match md with
    | SynMemberDefn.ValField (fieldInfo, _range) -> Some (SynMemberSig.ValField (fieldInfo, zeroRange))

    | SynMemberDefn.Member (SynBinding(valData = SynValData (memberFlags = memberFlags)) as binding, _range) ->
        if Option.isNone memberFlags then
            failwith "SynMemberDefn.Member does not have memberFlags"

        let valSig = mkSynValSig resolver binding
        Some (SynMemberSig.Member (valSig, memberFlags.Value, zeroRange))

    | SynMemberDefn.AbstractSlot (synValSig, synMemberFlags, _range) ->
        Some (SynMemberSig.Member (synValSig, synMemberFlags, zeroRange))

    | SynMemberDefn.Interface (interfaceType, _withKeyword, _synMemberDefnsOption, _range) ->
        Some (SynMemberSig.Interface (interfaceType, zeroRange))

    | SynMemberDefn.Inherit (baseType, _identOption, _range) -> Some (SynMemberSig.Inherit (baseType, zeroRange))

    | SynMemberDefn.ImplicitCtor (vis, synAttributeLists, _synSimplePats, _selfIdentifier, preXmlDoc, _range) ->
        let t =
            mkSynTypeFun
                [
                    mkSynTypeOfParameterTypeName (ParameterTypeName.SingleIdentifier "unit")
                    SynType.LongIdent (SynLongIdent (typeIdent, [], List.replicate typeIdent.Length None))
                ]

        let valSig =
            // This doesn't need to be correct for Fantomas
            let valInfo = SynValInfo ([], SynArgInfo ([], false, None))

            SynValSig (
                synAttributeLists,
                mkSynIdent "new",
                SynValTyparDecls (None, false),
                t,
                valInfo,
                false,
                false,
                preXmlDoc,
                vis,
                None,
                zeroRange,
                SynValSigTrivia.Zero
            )

        Some (
            SynMemberSig.Member (
                valSig,
                {
                    IsInstance = false
                    IsDispatchSlot = false
                    IsOverrideOrExplicitImpl = false
                    IsFinal = false
                    GetterOrSetterIsCompilerGenerated = false
                    MemberKind = SynMemberKind.Constructor
                    Trivia = SynMemberFlagsTrivia.Zero
                },
                zeroRange
            )
        )
    | SynMemberDefn.ImplicitInherit (inheritType, _inheritArgs, _inheritAlias, _range) ->
        Some (SynMemberSig.Inherit (inheritType, zeroRange))
    | SynMemberDefn.LetBindings _
    | SynMemberDefn.Open _ -> None
    | SynMemberDefn.AutoProperty _
    | SynMemberDefn.GetSetMember _
    | SynMemberDefn.ImplicitInherit _
    | SynMemberDefn.NestedType _ -> failwith $"todo EDB4CD44-E0D5-47F8-BF76-BBC74CC3B0C9, {md} {md.Range.Proxy}"
