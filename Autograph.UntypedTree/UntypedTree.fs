module Autograph.UntypedTree.Writer

open System.IO
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open Autograph.Common
open Autograph.UntypedTree.SourceParser

let zeroRange : range = Range.Zero

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
            [ mkSynTypeOfParameterTypeName mainType ],
            [],
            None,
            true,
            zeroRange
        )
    | ParameterTypeName.WithGenericArguments (name, parameterTypeNames) ->
        let args = List.map mkSynTypeOfParameterTypeName parameterTypeNames
        SynType.App (mkSynTypeOfParameterTypeName name, Some zeroRange, args, [], Some zeroRange, false, zeroRange)
    | ParameterTypeName.Tuple ts -> mkSynTypeTuple mkSynTypeOfParameterTypeName ts

let mkSynIdent text =
    SynIdent (Ident (text, zeroRange), None)

type Range with

    member r.ToProxy () : RangeProxy =
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

let rec mkTypeFromPat (resolver : TypedTreeInfoResolver) (pat : SynPat) : SynType =
    match removeParensInPat pat with
    | SynPat.Typed (SynPat.Named(ident = SynIdent (ident, _)), synType, _) ->
        // TODO: attributes and optional parameters
        SynType.SignatureParameter ([], false, Some ident, wrapInParenWhenFunType synType, zeroRange)
    | SynPat.Named(ident = SynIdent (ident, _)) ->
        let t =
            resolver.GetTypeNameFor (ident.idRange.ToProxy ())
            |> mkSynTypeOfParameterTypeName
            |> wrapInParenWhenFunType

        SynType.SignatureParameter ([], false, Some ident, t, zeroRange)
    | SynPat.Tuple (_, pats, _) ->
        let ts = List.map (mkTypeFromPat resolver) pats
        mkSynTypeTuple id ts
    | SynPat.LongIdent (range = m) ->
        resolver.GetTypeNameFor (m.ToProxy ())
        |> mkSynTypeOfParameterTypeName
        |> wrapInParenWhenFunType
    | SynPat.Const (SynConst.Unit, _) -> ParameterTypeName.SingleIdentifier "unit" |> mkSynTypeOfParameterTypeName
    | _ -> failwith "todo 233BA311-87C9-49DA-BFE0-9BDFAB0B09BB"

let collectParametersFromSynArgPats (resolver : TypedTreeInfoResolver) (argPats : SynArgPats) : SynType list =
    match argPats with
    | SynArgPats.Pats pats -> List.map (mkTypeFromPat resolver) pats
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
                 _synExpr,
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

            SynIdent (longDotId.LongIdent.[0], identTrivia),
            longDotId.LongIdent.[0].idRange,
            collectParametersFromSynArgPats resolver argPats,
            vis
        | Some _,
          SynPat.LongIdent (longDotId = SynLongIdent (id = [ _ ; ident ] ; trivia = [ _ ; identTrivia ])
                            argPats = argPats
                            accessibility = vis) ->
            SynIdent (ident, identTrivia), ident.idRange, collectParametersFromSynArgPats resolver argPats, vis

        // static member without property
        | Some _,
          SynPat.LongIdent (longDotId = (SynLongIdent (id = [ ident ] ; trivia = [ identTrivia ]))
                            argPats = argPats
                            accessibility = vis) ->
            SynIdent (ident, identTrivia), ident.idRange, collectParametersFromSynArgPats resolver argPats, vis

        | None, SynPat.LongIdent (longDotId = longDotId ; argPats = argPats ; accessibility = vis) ->
            let identTrivia = List.tryHead longDotId.Trivia

            SynIdent (longDotId.LongIdent.[0], identTrivia),
            longDotId.LongIdent.[0].idRange,
            collectParametersFromSynArgPats resolver argPats,
            vis

        | _, SynPat.Named (ident = SynIdent (ident, _) as synIdent ; accessibility = vis) ->
            synIdent, ident.idRange, [], vis

        | _ -> failwith $"todo 245B29E5-9303-4911-ABBE-0C3EA80DB536 {headPat.Range.ToProxy ()}"

    let returnType =
        match synBindingReturnInfoOption with
        | Some (SynBindingReturnInfo (typeName = t)) -> t
        | None ->
            resolver.GetReturnTypeFor (mBindingName.ToProxy ()) (not parameterTypes.IsEmpty)
            |> mkSynTypeOfParameterTypeName
        |> wrapInParenWhenFunType

    let t =
        match parameterTypes with
        | [] -> returnType
        | ts -> mkSynTypeFun [ yield! ts ; yield returnType ]

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
        None,
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
                  _implicitConstructor,
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
    | SynMemberDefn.LetBindings _
    | SynMemberDefn.Open _ -> None
    | SynMemberDefn.AutoProperty _
    | SynMemberDefn.GetSetMember _
    | SynMemberDefn.ImplicitInherit _
    | SynMemberDefn.NestedType _ -> failwith $"todo EDB4CD44-E0D5-47F8-BF76-BBC74CC3B0C9, {md} {md.Range.ToProxy ()}"
