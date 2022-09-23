module Autograph.UntypedTree.SourceParser

open FSharp.Compiler.Syntax

let (|TFuns|) (t : SynType) : SynType list =
    let rec visit t finalContinuation =
        match t with
        | SynType.Fun (at, rt, _, _) -> visit rt (fun ts -> at :: ts |> finalContinuation)
        | t -> finalContinuation [ t ]

    visit t id

let (|TupleTypes|) ts =
    ts
    |> List.choose (
        function
        | SynTupleTypeSegment.Type t -> Some t
        | _ -> None
    )

let (|NamedPat|_|) p =
    match p with
    | SynPat.Named(ident = SynIdent (ident, _)) -> Some ident
    | _ -> None

let (|IdentType|_|) text t =
    match t with
    | SynType.LongIdent (SynLongIdent(id = [ ident ])) when ident.idText = text -> Some t
    | _ -> None

let (|LongIdentType|_|) t =
    match t with
    | SynType.LongIdent (SynLongIdent (id = id)) -> Some id
    | _ -> None

let (|ParenPat|_|) p =
    match p with
    | SynPat.Paren (pat = pat) -> Some pat
    | _ -> None

let (|TypedPat|_|) p =
    match p with
    | SynPat.Typed (synPat, targetType, _range) -> Some (synPat, targetType)
    | _ -> None

let (|AttribPat|_|) p =
    match p with
    | SynPat.Attrib (p, a, _) -> Some (a, p)
    | _ -> None

let (|TyparInConstraint|_|) tc =
    match tc with
    | SynTypeConstraint.WhereTyparIsEquatable (typar = typar)
    | SynTypeConstraint.WhereTyparIsValueType (typar = typar)
    | SynTypeConstraint.WhereTyparIsReferenceType (typar = typar)
    | SynTypeConstraint.WhereTyparIsUnmanaged (typar = typar)
    | SynTypeConstraint.WhereTyparSupportsNull (typar = typar)
    | SynTypeConstraint.WhereTyparIsComparable (typar = typar)
    | SynTypeConstraint.WhereTyparIsEquatable (typar = typar)
    | SynTypeConstraint.WhereTyparDefaultsToType (typar = typar)
    | SynTypeConstraint.WhereTyparSubtypeOfType (typar = typar)
    | SynTypeConstraint.WhereTyparIsEnum (typar = typar)
    | SynTypeConstraint.WhereTyparIsDelegate (typar = typar) -> Some typar
    | SynTypeConstraint.WhereTyparSupportsMember _
    | SynTypeConstraint.WhereSelfConstrained _ -> None

let (|ForceMemberFlags|) (nodeName : string) (mf : SynMemberFlags option) =
    match mf with
    | None -> failwith $"{nodeName} does not have memberFlags, weird"
    | Some mf -> mf

let (|GetMemberLongIdentPat|_|) p =
    match p with
    | SynPat.LongIdent (longDotId = SynLongIdent(id = [ _thisIdent ; propertyNameIdent ])
                        extraId = Some _
                        accessibility = vis
                        argPats = SynArgPats.Pats [ ParenPat (SynPat.Const (SynConst.Unit, _)) ]) ->
        Some (propertyNameIdent, vis)
    | _ -> None

let (|SetMemberLongIdentPat|_|) p =
    match p with
    | SynPat.LongIdent (longDotId = SynLongIdent(id = [ _thisIdent ; nameIdent ]) ; argPats = SynArgPats.Pats [ _ ]) ->
        Some nameIdent
    | _ -> None

let (|SimpleGetSetBinding|_|) (md : SynMemberDefn) =
    match md with
    | SynMemberDefn.GetSetMember (Some (SynBinding (headPat = GetMemberLongIdentPat (nameIdent, _vis)
                                                    valData = (SynValData(memberFlags = ForceMemberFlags "SimpleGetSetBinding"
                                                                                                         mf)) as synValData
                                                    attributes = attributes
                                                    xmlDoc = xmlDoc
                                                    accessibility = vis)),
                                  Some (SynBinding(headPat = SetMemberLongIdentPat _alsoNameIdent)),
                                  _,
                                  _) ->
        Some (
            nameIdent,
            attributes,
            synValData.SynValInfo,
            xmlDoc,
            vis,
            { mf with
                MemberKind = SynMemberKind.PropertyGetSet
            }
        )
    | _ -> None

let (|IndexedGetSetBinding|_|) (md : SynMemberDefn) =
    match md with
    | SynMemberDefn.GetSetMember (Some (SynBinding (headPat = SynPat.LongIdent(argPats = SynArgPats.Pats [ getIndexPat ]) & SynPat.LongIdent(longDotId = SynLongIdent(id = [ _thisIdent
                                                                                                                                                                             propertyNameIdent ]))
                                                    valData = (SynValData(memberFlags = ForceMemberFlags "IndexedGetSetBinding"
                                                                                                         mf)) as synValData
                                                    attributes = attributes
                                                    xmlDoc = xmlDoc
                                                    accessibility = vis)),
                                  (Some (SynBinding(headPat = SynPat.LongIdent (longDotId = SynLongIdent(id = [ _ ; _ ])
                                                                                argPats = SynArgPats.Pats [ SynPat.Tuple (_,
                                                                                                                          [ setIndexPat
                                                                                                                            _ ],
                                                                                                                          _) ])))),
                                  _,
                                  _) ->

        let (|EventualNamedPat|_|) p =
            match p with
            | NamedPat ident
            | ParenPat (NamedPat ident)
            | ParenPat (TypedPat (NamedPat ident, _)) -> Some ident
            | _ -> None

        let indexArgName =
            match getIndexPat, setIndexPat with
            | EventualNamedPat gpi, EventualNamedPat spi -> if gpi.idText = spi.idText then Some gpi else None
            | _ -> None

        Some (
            propertyNameIdent,
            attributes,
            synValData.SynValInfo,
            xmlDoc,
            vis,
            { mf with
                MemberKind = SynMemberKind.PropertyGetSet
            },
            indexArgName
        )
    | _ -> None
