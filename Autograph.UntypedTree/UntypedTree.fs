module Autograph.UntypedTree.Writer

open System.IO
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open Autograph.Common
open FSharp.Compiler.Text

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

type Range with

    member r.ToProxy () : RangeProxy =
        RangeProxy (r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)

let (|NewConstructorPattern|_|)
    (
        memberFlags : SynMemberFlags option,
        headPat : SynPat
    )
    : (SynLongIdent * SynArgPats * SynAccess option) option
    =
    match memberFlags, headPat with
    | Some _,
      SynPat.LongIdent (longDotId = SynLongIdent(id = newIdent :: _) as longDotId
                        argPats = argPats
                        accessibility = vis) when newIdent.idText = "new" -> Some (longDotId, argPats, vis)
    | _ -> None

let (|RemoveParensInPat|) (pat : SynPat) =
    let rec visit (pat : SynPat) : SynPat =
        match pat with
        | SynPat.Paren (pat = pat) -> visit pat
        | pat -> pat

    visit pat

let collectInfoFromSynArgPats (argPats : SynArgPats) : Map<string, SynType> =
    match argPats with
    | SynArgPats.Pats pats ->
        List.choose
            (function
            | RemoveParensInPat (SynPat.Typed (SynPat.Named(ident = SynIdent (ident, _)), synType, _)) ->
                Some (ident.idText, synType)
            | _ -> None)
            pats
        |> Map.ofList
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

    Fantomas.Core.CodeFormatter.FormatASTAsync (output) |> Async.RunSynchronously

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
    let ident, mBindingName, existingTypedParameters, vis =
        match memberFlags, headPat with
        // new
        | NewConstructorPattern (longDotId, argPats, vis) ->
            let identTrivia = List.tryHead longDotId.Trivia

            SynIdent (longDotId.LongIdent.[0], identTrivia),
            longDotId.LongIdent.[0].idRange,
            collectInfoFromSynArgPats argPats,
            vis
        | Some _,
          SynPat.LongIdent (longDotId = (SynLongIdent (id = [ _ ; ident ] ; trivia = [ _ ; identTrivia ]) as longDotId)
                            argPats = argPats
                            accessibility = vis) ->
            SynIdent (ident, identTrivia), ident.idRange, collectInfoFromSynArgPats argPats, vis

        | None, SynPat.LongIdent (longDotId = longDotId ; argPats = argPats ; accessibility = vis) ->
            let identTrivia = List.tryHead longDotId.Trivia

            SynIdent (longDotId.LongIdent.[0], identTrivia),
            longDotId.LongIdent.[0].idRange,
            collectInfoFromSynArgPats argPats,
            vis

        | None, SynPat.Named (ident = SynIdent (ident, _) as synIdent ; accessibility = vis) ->
            synIdent, ident.idRange, Map.empty, vis

        | _ -> failwith "todo 245B29E5-9303-4911-ABBE-0C3EA80DB536"

    let existingReturnType =
        synBindingReturnInfoOption
        |> Option.map (fun (SynBindingReturnInfo (typeName = t)) -> t)

    let arity = synValData.SynValInfo.CurriedArgInfos

    let t =
        mkSynTypeForArity resolver mBindingName arity existingTypedParameters existingReturnType

    let synValInfo =
        match memberFlags, synValData.SynValInfo with
        | Some _, SynValInfo ([ [ SynArgInfo ([], false, None) ] ; [] ], returnTypeArgInfo) ->
            SynValInfo ([ [ SynArgInfo ([], false, None) ] ], returnTypeArgInfo)
        | _ -> synValData.SynValInfo

    SynValSig (
        synAttributeLists,
        ident,
        SynValTyparDecls (None, false),
        t,
        synValInfo,
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

and mkSynTypeForArity resolver mBindingName arity existingTypedParameters existingReturnType : SynType =
    let bindingRange = mBindingName.ToProxy ()

    let returnType =
        match existingReturnType with
        | Some t -> t
        | None ->
            resolver.GetReturnTypeFor bindingRange (not arity.IsEmpty)
            |> mkSynTypeOfParameterTypeName

    if arity.IsEmpty then
        returnType
    else
        let mkTypeForArgInfo (ident : Ident) =
            match Map.tryFind ident.idText existingTypedParameters with
            | Some t -> t
            | None ->
                resolver.GetTypeNameFor (ident.idRange.ToProxy ())
                |> mkSynTypeOfParameterTypeName

        let allTypes =
            let parameters =
                match arity with
                | [ [ SynArgInfo(ident = None) ] ; [] ] ->
                    [ ParameterTypeName.SingleIdentifier "unit" |> mkSynTypeOfParameterTypeName ]
                | arity ->

                arity
                |> List.mapi (fun idx group ->
                    match group with
                    | [] -> ParameterTypeName.SingleIdentifier "unit" |> mkSynTypeOfParameterTypeName
                    | [ SynArgInfo(ident = None) ] ->
                        resolver.GetTypeForCurriedParameterGroup bindingRange idx
                        |> mkSynTypeOfParameterTypeName
                    | [ SynArgInfo(ident = Some ident) ] -> mkTypeForArgInfo ident
                    | ts ->
                        ts
                        |> List.choose (fun (SynArgInfo (ident = indent)) -> indent)
                        |> mkSynTypeTuple mkTypeForArgInfo
                )

            [ yield! parameters ; yield returnType ]

        let rec visit types =
            match types with
            | [] -> failwith "unexpected empty list"
            | [ t ] -> t
            | t :: rest -> SynType.Fun (t, visit rest, zeroRange, { ArrowRange = zeroRange })

        visit allTypes

and mkSynTypeDefnSig
    resolver
    (SynTypeDefn (typeInfo, typeRepr, members, implicitConstructor, _range, trivia))
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
            let members = List.choose (mkSynMemberSig resolver) synMemberDefns
            SynTypeDefnSigRepr.ObjectModel (synTypeDefnKind, members, zeroRange)
        | _ -> failwith "todo 88635304-1D34-4AE0-96A4-348C7E47588E"

    let withKeyword =
        match typeRepr with
        | SynTypeDefnRepr.ObjectModel (SynTypeDefnKind.Augmentation _, _, _) -> Some zeroRange
        | _ -> trivia.WithKeyword

    let members = members |> List.choose (mkSynMemberSig resolver)

    SynTypeDefnSig (
        typeInfo,
        typeSigRepr,
        members,
        zeroRange,
        {
            EqualsRange = trivia.EqualsRange
            TypeKeyword = trivia.TypeKeyword
            WithKeyword = withKeyword
        }
    )

and mkSynExceptionDefn resolver (SynExceptionDefn (synExceptionDefnRepr, withKeyword, synMemberDefns, range)) =
    let members = []
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

and mkSynMemberSig resolver (md : SynMemberDefn) : SynMemberSig option =
    match md with
    | SynMemberDefn.ValField (fieldInfo, _range) -> Some (SynMemberSig.ValField (fieldInfo, zeroRange))
    | SynMemberDefn.Member (SynBinding(valData = SynValData (memberFlags = memberFlags)) as binding, _range) ->
        if Option.isNone memberFlags then
            failwith "SynMemberDefn.Member does not have memberFlags"

        let valSig = mkSynValSig resolver binding
        Some (SynMemberSig.Member (valSig, memberFlags.Value, zeroRange))
    | SynMemberDefn.AbstractSlot (synValSig, synMemberFlags, _range) ->
        Some (SynMemberSig.Member (synValSig, synMemberFlags, zeroRange))
    | SynMemberDefn.AutoProperty _
    | SynMemberDefn.GetSetMember _
    | SynMemberDefn.ImplicitCtor _
    | SynMemberDefn.ImplicitInherit _
    | SynMemberDefn.Inherit _
    | SynMemberDefn.Interface _
    | SynMemberDefn.LetBindings _
    | SynMemberDefn.Open _
    | SynMemberDefn.NestedType _ -> failwith "todo EDB4CD44-E0D5-47F8-BF76-BBC74CC3B0C9"
