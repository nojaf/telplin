module Autograph.UntypedTree.SourceParser

open FSharp.Compiler.Syntax

val (|StaticMemberFlags|_|) : memberFlags : SynMemberFlags -> SynMemberFlags option

val (|NewConstructorPattern|_|) :
    memberFlags : SynMemberFlags option * headPat : SynPat -> (SynLongIdent * SynArgPats * SynAccess option) option

val (|EmptySynArgInfo|_|) : argInfo : SynArgInfo -> SynArgInfo option
val (|UnitType|_|) : t : SynType -> SynType option
val (|TFuns|) : t : SynType -> SynType list
val (|TupleTypes|) : ts : SynTupleTypeSegment list -> SynType list
val (|NamedPat|_|) : p : SynPat -> Ident option
val (|IdentType|_|) : text : string -> t : SynType -> SynType option
val (|ParenPat|_|) : p : SynPat -> SynPat option
val (|TypedPat|_|) : p : SynPat -> (SynPat * SynType) option
