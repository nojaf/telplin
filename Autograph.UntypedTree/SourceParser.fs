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
