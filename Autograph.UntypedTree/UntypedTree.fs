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

let stripOptionType t =
    match t with
    | SynType.App (typeName = SynType.LongIdent(longDotId = SynLongIdent(id = [ optionIdent ])) ; typeArgs = [ t ]) when
        optionIdent.idText = "option"
        ->
        t
    | _ -> t

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

let mkSynTypeLongIdent text = SynType.LongIdent (mkSynLongIdent text)

let rec sanitizeType (synType : SynType) : SynType =
    match synType with
    | IdentType "Int32" _ -> mkSynTypeLongIdent "int"
    | SynType.Array (1, t, _) -> SynType.App (mkSynTypeLongIdent "array", None, [ t ], [], None, true, zeroRange)
    | SynType.App (typeName, rangeOption, typeArgs, commaRanges, greaterRange, isPostfix, range) ->
        SynType.App (
            sanitizeType typeName,
            rangeOption,
            List.map sanitizeType typeArgs,
            commaRanges,
            greaterRange,
            isPostfix,
            range
        )
    | t -> t

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
                 synValData,
                 headPat,
                 synBindingReturnInfoOption,
                 synExpr,
                 _range,
                 _debugPointAtBinding,
                 _synBindingTrivia))
    : SynValSig
    =
    let ident, mBindingName, argPats, vis =
        match headPat with
        | SynPat.LongIdent (longDotId = longDotId ; argPats = SynArgPats.Pats argPats ; accessibility = vis) ->
            let identTrivia = List.tryLast longDotId.Trivia
            let lastIdent = (List.last longDotId.LongIdent)
            let mBindingName = lastIdent.idRange

            SynIdent (lastIdent, identTrivia), mBindingName, argPats, vis

        | SynPat.Named (ident = SynIdent (ident, _) as synIdent ; accessibility = vis) ->
            synIdent, ident.idRange, [], vis

        | _ -> failwith $"todo 245B29E5-9303-4911-ABBE-0C3EA80DB536 {headPat.Range.Proxy}"

    let returnTypeText = resolver.GetFullForBinding mBindingName.Proxy

    let returnTypeInImpl : (SynType list * SynType) option =
        match synBindingReturnInfoOption with
        | Some (SynBindingReturnInfo(typeName = TFuns ts as t)) -> Some (ts, t)
        | _ -> None

    let t =
        let aliasAST, _ =
            Fantomas.FCS.Parse.parseFile false (SourceText.ofString $"type Alias = {returnTypeText}") []

        let ts =
            match aliasAST with
            | ParsedInput.ImplFile (ParsedImplFileInput(modules = [ SynModuleOrNamespace(decls = [ SynModuleDecl.Types ([ SynTypeDefn(typeRepr = SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.TypeAbbrev(rhsType = TFuns ts))) ],
                                                                                                                        _) ]) ])) ->
                ts |> List.map sanitizeType
            | _ -> failwith "should not fail"

        let ts =
            match returnTypeInImpl with
            | Some (rts, rt) when rts.Length > 1 ->
                let skipEnd = List.take (ts.Length - rts.Length) ts
                [ yield! skipEnd ; wrapInParenWhenFunType rt ]
            | _ ->
                if argPats.Length = 0 || ts.Length = argPats.Length + 1 then
                    ts
                else
                    let parameterTypes = List.take argPats.Length ts

                    let returnType =
                        List.skip argPats.Length ts |> mkSynTypeFun |> wrapInParenWhenFunType

                    [ yield! parameterTypes ; yield returnType ]

        let ps =
            [
                yield! List.map Some argPats
                yield! List.replicate (ts.Length - argPats.Length) None
            ]

        List.zip ts ps
        |> List.map (fun (t, p) ->
            match p with
            | None -> t
            | Some p ->
                match p with
                | NamedPat ident -> SynType.SignatureParameter ([], false, Some ident, t, zeroRange)
                | ParenPat (SynPat.Tuple (elementPats = ps)) ->
                    match t with
                    | SynType.Tuple(path = TupleTypes ts) ->
                        let ts =
                            List.zip ts ps
                            |> List.map (fun (t, p) ->
                                match p with
                                | NamedPat ident -> SynType.SignatureParameter ([], false, Some ident, t, zeroRange)
                                | TypedPat (NamedPat ident, _) ->
                                    SynType.SignatureParameter ([], false, Some ident, t, zeroRange)
                                | SynPat.OptionalVal (ident, _)
                                | TypedPat (SynPat.OptionalVal (ident, _), _) ->
                                    SynType.SignatureParameter ([], true, Some ident, stripOptionType t, zeroRange)
                                | _ -> t
                            )

                        mkSynTypeTuple id ts
                    | _ -> t
                | ParenPat (SynPat.Typed(pat = SynPat.Named(ident = SynIdent (ident, _)))) ->
                    SynType.SignatureParameter ([], false, Some ident, t, zeroRange)

                | ParenPat (SynPat.Typed(pat = SynPat.OptionalVal (ident, _))) ->
                    SynType.SignatureParameter ([], true, Some ident, stripOptionType t, zeroRange)
                | ParenPat (SynPat.Attrib (attributes = attributes
                                           pat = (SynPat.Typed(pat = NamedPat ident) | NamedPat ident))) ->
                    SynType.SignatureParameter (attributes, false, Some ident, t, zeroRange)
                | ParenPat (SynPat.OptionalVal (ident, _)) ->
                    SynType.SignatureParameter ([], true, Some ident, stripOptionType t, zeroRange)
                | _ -> t
        )
        |> mkSynTypeFun

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
