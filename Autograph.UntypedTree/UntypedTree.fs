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
    | ParameterTypeName.WithGenericArguments _ -> failwith "todo 3538EC9A-02FC-492E-9473-D8463971F750"
    | ParameterTypeName.Tuple ts -> mkSynTypeTuple mkSynTypeOfParameterTypeName ts

type Range with

    member r.ToProxy () : RangeProxy =
        RangeProxy (r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)

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
    let decls = List.choose (mkSynModuleSigDecl resolver) synModuleDecls

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

and mkSynModuleSigDecl (resolver : TypedTreeInfoResolver) (decl : SynModuleDecl) : SynModuleSigDecl option =
    match decl with
    | SynModuleDecl.Let (_, [ binding ], _) ->
        let valSig = mkSynValSig resolver binding
        Some (SynModuleSigDecl.Val (valSig, zeroRange))
    | SynModuleDecl.Open (synOpenDeclTarget, range) -> Some (SynModuleSigDecl.Open (synOpenDeclTarget, zeroRange))
    | SynModuleDecl.Types (typeDefns, _) ->
        let types = List.map (mkSynTypeDefnSig resolver) typeDefns
        Some (SynModuleSigDecl.Types (types, zeroRange))
    | SynModuleDecl.Exception (synExceptionDefn, _range) ->
        let exnSig = mkSynExceptionDefn resolver synExceptionDefn
        Some (SynModuleSigDecl.Exception (exnSig, zeroRange))
    | SynModuleDecl.ModuleAbbrev (ident, longId, _range) ->
        Some (SynModuleSigDecl.ModuleAbbrev (ident, longId, zeroRange))
    | SynModuleDecl.NestedModule (synComponentInfo, isRecursive, synModuleDecls, _isContinuing, _range, trivia) ->
        Some (mkSynModuleSigDeclNestedModule resolver synComponentInfo isRecursive synModuleDecls trivia)
    | _ -> None

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
                 _synExpr,
                 _range,
                 _debugPointAtBinding,
                 _synBindingTrivia))
    =
    let ident, mBindingName, existingTypedParameters, vis =
        match headPat with
        | SynPat.LongIdent (longDotId = longDotId ; argPats = argPats ; accessibility = vis) ->
            SynIdent (longDotId.LongIdent.[0], None),
            longDotId.LongIdent.[0].idRange,
            collectInfoFromSynArgPats argPats,
            vis
        | SynPat.Named (ident = SynIdent (ident, _) as synIdent ; accessibility = vis) ->
            synIdent, ident.idRange, Map.empty, vis
        | _ -> failwith "todo 245B29E5-9303-4911-ABBE-0C3EA80DB536"

    let existingReturnType =
        synBindingReturnInfoOption
        |> Option.map (fun (SynBindingReturnInfo (typeName = t)) -> t)

    let arity = synValData.SynValInfo.CurriedArgInfos

    let t =
        mkSynTypeForArity resolver mBindingName arity existingTypedParameters existingReturnType

    SynValSig (
        synAttributeLists,
        ident,
        SynValTyparDecls (None, false),
        t,
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

and mkSynTypeForArity resolver mBindingName arity existingTypedParameters existingReturnType : SynType =
    let returnType =
        match existingReturnType with
        | Some t -> t
        | None ->
            resolver.GetReturnTypeFor (mBindingName.ToProxy ())
            |> mkSynTypeOfParameterTypeName

    if arity.IsEmpty then
        returnType
    else
        let mkTypeForArgInfo (SynArgInfo (ident = ident)) =
            match ident with
            | None -> failwith "expected ident"
            | Some ident ->
                match Map.tryFind ident.idText existingTypedParameters with
                | Some t -> t
                | None ->
                    resolver.GetTypeNameFor (ident.idRange.ToProxy ())
                    |> mkSynTypeOfParameterTypeName

        let allTypes =
            let parameters =
                arity
                |> List.map (fun group ->
                    match group with
                    | [] -> failwith "unexpected empty arity group"
                    | [ singleArg ] -> mkTypeForArgInfo singleArg
                    | ts -> mkSynTypeTuple mkTypeForArgInfo ts
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
    let typeRepr =
        match typeRepr with
        | SynTypeDefnRepr.Simple (synTypeDefnSimpleRepr, _range) ->
            SynTypeDefnSigRepr.Simple (synTypeDefnSimpleRepr, zeroRange)
        | SynTypeDefnRepr.ObjectModel (synTypeDefnKind, synMemberDefns, range) ->
            let members = []
            SynTypeDefnSigRepr.ObjectModel (synTypeDefnKind, members, zeroRange)
        | _ -> failwith "todo 88635304-1D34-4AE0-96A4-348C7E47588E"

    let members = []

    SynTypeDefnSig (
        typeInfo,
        typeRepr,
        members,
        zeroRange,
        {
            EqualsRange = trivia.EqualsRange
            TypeKeyword = trivia.TypeKeyword
            WithKeyword = trivia.WithKeyword
        }
    )

and mkSynExceptionDefn resolver (SynExceptionDefn (synExceptionDefnRepr, withKeyword, synMemberDefns, range)) =
    let members = []
    SynExceptionSig (synExceptionDefnRepr, withKeyword, members, zeroRange)

and mkSynModuleSigDeclNestedModule resolver synComponentInfo isRecursive synModuleDecls trivia : SynModuleSigDecl =
    let decls = List.choose (mkSynModuleSigDecl resolver) synModuleDecls

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
