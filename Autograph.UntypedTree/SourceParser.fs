module Autograph.UntypedTree.SourceParser

open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text

let (|StaticMemberFlags|_|) (memberFlags : SynMemberFlags) =
    match memberFlags.Trivia with
    | {
          StaticRange = Some _
          MemberRange = Some _
      } -> Some memberFlags
    | _ -> None

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

let (|EmptySynArgInfo|_|) argInfo =
    match argInfo with
    | SynArgInfo ([], false, None) -> Some argInfo
    | _ -> None

let (|UnitType|_|) t =
    match t with
    | SynType.LongIdent (SynLongIdent(id = [ ident ])) when ident.idText = "unit" -> Some t
    | _ -> None

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

let (|ParenPat|_|) p =
    match p with
    | SynPat.Paren (pat = pat) -> Some pat
    | _ -> None

let (|TypedPat|_|) p =
    match p with
    | SynPat.Typed (synPat, targetType, range) -> Some (synPat, targetType)
    | _ -> None
