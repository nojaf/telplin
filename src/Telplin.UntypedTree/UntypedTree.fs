module Telplin.UntypedTree.Writer

open System
open System.IO
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open Telplin.Common
open Telplin.UntypedTree.SourceParser

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

let mkSynTypeTuple ts =
    match ts with
    | []
    | [ _ ] -> failwith "ts cannot have a single or zero types"
    | h :: rest ->
        let types =
            SynTupleTypeSegment.Type h
            :: List.collect (fun t -> [ SynTupleTypeSegment.Star zeroRange ; SynTupleTypeSegment.Type t ]) rest

        SynType.Tuple (false, types, zeroRange)

let mkIdent text = Ident (text, zeroRange)

let mkSynIdent text = SynIdent (mkIdent text, None)

let mkSynLongIdent text =
    SynLongIdent ([ mkIdent text ], [], [ None ])

let mkSynTypeLongIdent text = SynType.LongIdent (mkSynLongIdent text)

/// Clean up some less nice results from the typed tree.
/// Change `Int32` to `int`, or `int[]` to `int array`.
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

/// The type in the source code could have had more information
/// For example: it could have been prefixed with a namespace
/// `Regex` was resolved by the typed tree, but the untyped tree had `System.Text.RegularExpressions.Regex`
/// or have a different generic type argument name
let enhanceResolvedType (typeInSource : SynType) (resolvedType : SynType) : SynType =
    let resolveNameDifference
        (originalName : Ident list)
        (resolvedName : Ident list)
        (originalType : SynType)
        (resolvedType : SynType)
        : SynType
        =
        let idText o =
            Option.map (fun (i : Ident) -> i.idText) o

        let originalNameEnd = List.tryLast originalName |> idText
        let resolvedNameEnd = List.tryLast resolvedName |> idText

        if originalName.Length > resolvedName.Length && originalNameEnd = resolvedNameEnd then
            originalType
        else
            resolvedType

    let rec visit tis rt =
        match tis, rt with
        | SynType.App (typeName = otn ; typeArgs = oArgs),
          SynType.App (rtn, rangeOption, rArgs, commaRanges, greaterRange, isPostfix, range) ->
            let typeName = visit otn rtn

            let typeArgs =
                if oArgs.Length <> rArgs.Length then
                    rArgs
                else
                    List.zip oArgs rArgs |> List.map (fun (o, r) -> visit o r)

            SynType.App (typeName, rangeOption, typeArgs, commaRanges, greaterRange, isPostfix, range)

        | LongIdentType originalName, LongIdentType resolvedName ->
            resolveNameDifference originalName resolvedName tis rt
        | _ -> rt

    match typeInSource, resolvedType with
    | SynType.HashConstraint _, _ -> typeInSource
    | _ -> visit typeInSource resolvedType

let rec convertToSynPat (simplePat : SynSimplePat) : SynPat =
    match simplePat with
    | SynSimplePat.Attrib (pat, attributes, range) -> SynPat.Attrib (convertToSynPat pat, attributes, range)
    | SynSimplePat.Id (ident = ident ; isThisVal = isThisVal ; isOptional = isOptional ; range = range) ->
        if isOptional then
            SynPat.OptionalVal (ident, range)
        else
            SynPat.Named (SynIdent (ident, None), isThisVal, None, range)
    | SynSimplePat.Typed (pat, t, range) -> SynPat.Typed (convertToSynPat pat, t, range)

let mkSynTypFromText (typeText : string) : SynType =
    let aliasAST, _ =
        Fantomas.FCS.Parse.parseFile true (SourceText.ofString $"val v: {typeText}") []

    match aliasAST with
    | ParsedInput.SigFile (ParsedSigFileInput(contents = [ SynModuleOrNamespaceSig(decls = [ SynModuleSigDecl.Val(valSig = SynValSig (synType = t)) ]) ])) ->
        t
    | _ -> failwith $"should not fail, but did for {typeText}"

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
    let ident, mBindingName, argPats, constraintsFromSource, vis =
        match headPat with
        | SynPat.LongIdent (longDotId = longDotId
                            argPats = SynArgPats.Pats argPats
                            typarDecls = constraints
                            accessibility = vis) ->
            let identTrivia = List.tryLast longDotId.Trivia
            let lastIdent = (List.last longDotId.LongIdent)
            let mBindingName = lastIdent.idRange

            SynIdent (lastIdent, identTrivia), mBindingName, argPats, constraints, vis

        | SynPat.Named (ident = SynIdent (ident, _) as synIdent ; accessibility = vis) ->
            synIdent, ident.idRange, [], None, vis

        | _ -> failwith $"todo 245B29E5-9303-4911-ABBE-0C3EA80DB536 {headPat.Range.Proxy}"

    let expr =
        match synAttributeLists with
        | [ {
                Attributes = [ {
                                   TypeName = SynLongIdent(id = [ literalIdent ])
                               } ]
            } ] when literalIdent.idText = "Literal" -> Some synExpr
        | _ -> None

    let returnTypeText, resolvedConstraints =
        resolver.GetFullForBinding mBindingName.Proxy

    let returnTypeInImpl : (SynType list * SynType) option =
        match synBindingReturnInfoOption with
        | Some (SynBindingReturnInfo(typeName = TFuns ts as t)) -> Some (ts, t)
        | _ -> None

    let t =
        mkSynTypForSignature returnTypeText resolvedConstraints constraintsFromSource argPats returnTypeInImpl

    let synValTyparDecls =
        match constraintsFromSource with
        | Some c -> c
        | None -> SynValTyparDecls (None, false)

    SynValSig (
        synAttributeLists,
        ident,
        synValTyparDecls,
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

and mkSynTypForSignature
    returnTypeText
    resolvedConstraints
    constraintsFromSource
    (argPats : SynPat list)
    (returnTypeInImpl : (SynType list * SynType) option)
    : SynType
    =
    let isTypedPat p =
        match p with
        | TypedPat _
        | ParenPat (TypedPat _) -> true
        | _ -> false

    match returnTypeInImpl with
    | Some (_, returnType) when List.forall isTypedPat argPats ->
        mkSynTypForSignatureBasedOnUntypedTree argPats returnType
    | _ ->
        mkSynTypForSignatureBasedOnTypedTree
            returnTypeText
            resolvedConstraints
            constraintsFromSource
            argPats
            returnTypeInImpl

and mkSynTypForSignatureBasedOnUntypedTree (argPats : SynPat list) (returnType : SynType) : SynType =
    let parameters =
        argPats
        |> List.choose (
            function
            | ParenPat (TypedPat (NamedPat parameterName, t)) ->
                Some (SynType.SignatureParameter ([], false, Some parameterName, wrapInParenWhenFunType t, zeroRange))
            | _ -> None
        )

    [ yield! parameters ; yield (wrapInParenWhenFunType returnType) ]
    |> mkSynTypeFun

and mkSynTypForSignatureBasedOnTypedTree
    returnTypeText
    resolvedConstraints
    constraintsFromSource
    (argPats : SynPat list)
    (returnTypeInImpl : (SynType list * SynType) option)
    : SynType
    =
    let fullType =
        let ts =
            match mkSynTypFromText returnTypeText with
            | SynType.WithGlobalConstraints(typeName = TFuns ts)
            | TFuns ts -> List.map sanitizeType ts

        let ts =
            match returnTypeInImpl with
            | Some (rts, rt) when rts.Length > 1 ->
                let skipEnd = List.take (ts.Length - rts.Length) ts
                [ yield! skipEnd ; wrapInParenWhenFunType rt ]
            | _ ->
                if argPats.Length = 0 then
                    [ ts |> mkSynTypeFun |> wrapInParenWhenFunType ]
                elif ts.Length = argPats.Length + 1 then
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
                | ParenPat (NamedPat ident)
                | NamedPat ident -> SynType.SignatureParameter ([], false, Some ident, t, zeroRange)
                | ParenPat (SynPat.Tuple (elementPats = ps)) ->
                    match t with
                    | SynType.Tuple(path = TupleTypes ts) ->
                        let ts =
                            List.zip ts ps
                            |> List.map (fun (t, p) ->
                                match p with
                                | NamedPat ident -> SynType.SignatureParameter ([], false, Some ident, t, zeroRange)
                                | TypedPat (NamedPat ident, st) ->
                                    SynType.SignatureParameter (
                                        [],
                                        false,
                                        Some ident,
                                        enhanceResolvedType st t,
                                        zeroRange
                                    )
                                | SynPat.OptionalVal (ident, _)
                                | TypedPat (SynPat.OptionalVal (ident, _), _) ->
                                    SynType.SignatureParameter ([], true, Some ident, stripOptionType t, zeroRange)
                                | AttribPat (attrs, TypedPat (NamedPat ident, t)) ->
                                    SynType.SignatureParameter (attrs, false, Some ident, stripOptionType t, zeroRange)
                                | _ -> t
                            )

                        mkSynTypeTuple ts
                    | _ -> t
                | ParenPat (TypedPat (NamedPat ident, st)) ->
                    SynType.SignatureParameter ([], false, Some ident, enhanceResolvedType st t, zeroRange)

                | ParenPat (TypedPat (SynPat.OptionalVal (ident, _), tis)) ->
                    let t = enhanceResolvedType tis (stripOptionType t)
                    SynType.SignatureParameter ([], true, Some ident, t, zeroRange)
                | ParenPat (AttribPat (attributes, (SynPat.Typed(pat = NamedPat ident) | NamedPat ident))) ->
                    SynType.SignatureParameter (attributes, false, Some ident, t, zeroRange)
                | ParenPat (SynPat.OptionalVal (ident, _)) ->
                    SynType.SignatureParameter ([], true, Some ident, stripOptionType t, zeroRange)
                | _ -> t
        )
        |> mkSynTypeFun

    if resolvedConstraints.IsEmpty then
        fullType
    else

    let existingTypars =
        match constraintsFromSource with
        | Some (SynValTyparDecls(typars = Some typar)) ->
            typar.Constraints
            |> List.choose (
                function
                | TyparInConstraint typar -> Some typar
                | _ -> None
            )
        | _ -> []

    // This isn't bullet proof but good enough for now.
    let typarComparer =
        { new System.Collections.Generic.IEqualityComparer<SynTypar> with
            member this.Equals (x, y) =
                match x, y with
                | SynTypar (identX, typarStaticReqX, _), SynTypar (identY, typarStaticReqY, _) ->
                    identX.idText = identY.idText && typarStaticReqX = typarStaticReqY

            member this.GetHashCode (SynTypar (identX, typarStaticReqX, isCompGenX)) =
                HashCode.Combine (identX, typarStaticReqX, isCompGenX)
        }

    let constraints =
        resolvedConstraints
        |> List.collect (fun rc ->
            let typarStaticReq =
                if rc.IsHeadType then
                    TyparStaticReq.HeadType
                else
                    TyparStaticReq.None

            let typar =
                SynTypar (mkIdent rc.ParameterName, typarStaticReq, rc.IsCompilerGenerated)

            if System.Linq.Enumerable.Contains (existingTypars, typar, typarComparer) then
                []
            else

            rc.Constraints
            |> List.choose (fun c ->
                if c.IsEqualityConstraint then
                    Some (SynTypeConstraint.WhereTyparIsEquatable (typar, zeroRange))
                elif c.IsReferenceTypeConstraint then
                    Some (SynTypeConstraint.WhereTyparIsReferenceType (typar, zeroRange))
                elif c.IsSupportsNullConstraint then
                    Some (SynTypeConstraint.WhereTyparSupportsNull (typar, zeroRange))
                elif Option.isSome c.CoercesToTarget then
                    let isHashConstraints =
                        argPats
                        |> List.choose (
                            function
                            | TypedPat (_, t)
                            | ParenPat (TypedPat (_, t)) -> Some t
                            | _ -> None
                        )
                        |> List.exists (
                            function
                            | SynType.HashConstraint (SynType.App(typeArgs = [ SynType.Var (typar = hashTyPar) ]), _) ->
                                typarComparer.Equals (hashTyPar, typar)
                            | _ -> false
                        )

                    if isHashConstraints then
                        None
                    else

                    c.CoercesToTarget
                    |> Option.map (fun coercesToTarget ->
                        SynTypeConstraint.WhereTyparSubtypeOfType (typar, mkSynTypFromText coercesToTarget, zeroRange)
                    )
                else
                    None
            )
        )

    SynType.WithGlobalConstraints (fullType, constraints, zeroRange)

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

                        if typeInfo.NeedsClassAttribute then
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

    | SynMemberDefn.Member (SynBinding(valData = SynValData(memberFlags = ForceMemberFlags "SynMemberDefn.Member"
                                                                                           memberFlags)) as binding,
                            _range) ->
        let valSig = mkSynValSig resolver binding
        Some (SynMemberSig.Member (valSig, memberFlags, zeroRange))

    | SynMemberDefn.AbstractSlot (synValSig, synMemberFlags, _range) ->
        Some (SynMemberSig.Member (synValSig, synMemberFlags, zeroRange))

    | SynMemberDefn.Interface (interfaceType, _withKeyword, _synMemberDefnsOption, _range) ->
        Some (SynMemberSig.Interface (interfaceType, zeroRange))

    | SynMemberDefn.Inherit (baseType, _identOption, _range) -> Some (SynMemberSig.Inherit (baseType, zeroRange))

    | SynMemberDefn.ImplicitCtor (vis, synAttributeLists, synSimplePats, _selfIdentifier, preXmlDoc, _range) ->
        let t =
            let { ConstructorInfo = ctor } = resolver.GetTypeInfo typeIdent.Head.idRange.Proxy

            let returnType =
                SynType.LongIdent (SynLongIdent (typeIdent, [], List.replicate typeIdent.Length None))

            match ctor with
            | None -> mkSynTypeFun [ mkSynTypeLongIdent "unit" ; returnType ]
            | Some (signatureText, resolvedGenericParameters) ->
                let argPats : SynPat list =
                    match synSimplePats with
                    | SynSimplePats.SimplePats (pats = pats) ->
                        let pats = pats |> List.map convertToSynPat

                        match pats with
                        | [] -> []
                        | [ pat ] -> [ SynPat.Paren (pat, zeroRange) ]
                        | pats -> [ SynPat.Paren (SynPat.Tuple (false, pats, zeroRange), zeroRange) ]
                    | SynSimplePats.Typed _ -> []

                mkSynTypForSignatureBasedOnTypedTree
                    signatureText
                    resolvedGenericParameters
                    None
                    argPats
                    (Some ([ returnType ], returnType))
                |> removeParensInType

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
    | SynMemberDefn.Open _
    | SynMemberDefn.GetSetMember (None, None, _, _) -> None
    | SynMemberDefn.AutoProperty (attributes = attributes
                                  ident = ident
                                  memberFlags = memberFlags
                                  xmlDoc = xmlDoc
                                  accessibility = vis) ->
        let t =
            let typeString, _ = resolver.GetFullForBinding ident.idRange.Proxy
            mkSynTypFromText typeString

        let valSig =
            SynValSig (
                attributes,
                SynIdent (ident, None),
                SynValTyparDecls (None, true),
                t,
                SynValInfo ([], SynArgInfo ([], false, None)),
                false,
                false,
                xmlDoc,
                vis,
                None,
                zeroRange,
                SynValSigTrivia.Zero
            )

        Some (SynMemberSig.Member (valSig, memberFlags, zeroRange))
    | SynMemberDefn.GetSetMember (Some (SynBinding (valData = SynValData(memberFlags = ForceMemberFlags "SynMemberDefn.GetSetMember"
                                                                                                        memberFlags)
                                                    headPat = headPat) as binding),
                                  None,
                                  _range,
                                  _trivia) ->
        let binding =
            // Transform getter with unit to Named
            // `member __.DisableInMemoryProjectReferences with get () = disableInMemoryProjectReferences`
            // becomes `member DisableInMemoryProjectReferences : bool`
            match memberFlags.MemberKind, headPat with
            | SynMemberKind.PropertyGet, GetMemberLongIdentPat (propertyNameIdent, vis) ->
                let headPat =
                    SynPat.Named (SynIdent (propertyNameIdent, None), false, vis, zeroRange)

                let (SynBinding (a, k, il, is, attrs, xml, vd, _, rto, e, r, d, t)) = binding
                SynBinding (a, k, il, is, attrs, xml, vd, headPat, rto, e, r, d, t)
            | _ -> binding

        let valSig = mkSynValSig resolver binding
        Some (SynMemberSig.Member (valSig, memberFlags, zeroRange))

    | SynMemberDefn.GetSetMember (None,
                                  Some (SynBinding (headPat = headPat
                                                    accessibility = vis
                                                    attributes = attributes
                                                    xmlDoc = xmlDoc
                                                    valData = SynValData(memberFlags = ForceMemberFlags "SynMemberDefn.GetSetMember"
                                                                                                        memberFlags) as synValData) as binding),
                                  _range,
                                  _trivia) ->
        match headPat with
        | SetMemberLongIdentPat nameIdent ->
            let text, _ = resolver.GetFullForBinding nameIdent.idRange.Proxy
            let t = text.Replace (" with set", "") |> mkSynTypFromText

            let valSig =
                SynValSig (
                    attributes,
                    SynIdent (nameIdent, None),
                    SynValTyparDecls (None, false),
                    t,
                    // This isn't correct but doesn't matter for Fantomas
                    synValData.SynValInfo,
                    false,
                    false,
                    xmlDoc,
                    vis,
                    None,
                    zeroRange,
                    {
                        ValKeyword = Some zeroRange
                        WithKeyword = None
                        EqualsRange = None
                    }
                )

            Some (SynMemberSig.Member (valSig, memberFlags, zeroRange))
        | _ ->
            let valSig = mkSynValSig resolver binding
            Some (SynMemberSig.Member (valSig, memberFlags, zeroRange))

    // member Name: type with get, set
    | SimpleGetSetBinding (nameIdent, attributes, valInfo, xmlDoc, vis, memberFlags) ->
        let text, _ = resolver.GetFullForBinding nameIdent.idRange.Proxy
        let t = mkSynTypFromText text

        let valSig =
            SynValSig (
                attributes,
                SynIdent (nameIdent, None),
                SynValTyparDecls (None, false),
                t,
                valInfo,
                false,
                false,
                xmlDoc,
                vis,
                None,
                zeroRange,
                {
                    ValKeyword = Some zeroRange
                    WithKeyword = None
                    EqualsRange = None
                }
            )

        Some (SynMemberSig.Member (valSig, memberFlags, zeroRange))

    | IndexedGetSetBinding (nameIdent, attributes, valInfo, xmlDoc, vis, memberFlags, indexArgName) ->
        let text, _ = resolver.GetFullForBinding nameIdent.idRange.Proxy
        let text = text.Replace (" with get", "")

        let t =
            match mkSynTypFromText text, indexArgName with
            | SynType.Fun (argType, returnType, range, trivia), Some ident ->
                SynType.Fun (
                    SynType.SignatureParameter ([], false, Some ident, argType, zeroRange),
                    returnType,
                    range,
                    trivia
                )
            | t, _ -> t

        let valSig =
            SynValSig (
                attributes,
                SynIdent (nameIdent, None),
                SynValTyparDecls (None, false),
                t,
                valInfo,
                false,
                false,
                xmlDoc,
                vis,
                None,
                zeroRange,
                {
                    ValKeyword = Some zeroRange
                    WithKeyword = None
                    EqualsRange = None
                }
            )

        Some (SynMemberSig.Member (valSig, memberFlags, zeroRange))
    | SynMemberDefn.GetSetMember _ -> failwith "todo EF1C9796-DA9D-4810-8BD6-B28983C6C259"
    | SynMemberDefn.ImplicitInherit _
    | SynMemberDefn.NestedType _ -> failwith $"todo EDB4CD44-E0D5-47F8-BF76-BBC74CC3B0C9, {md} {md.Range.Proxy}"
