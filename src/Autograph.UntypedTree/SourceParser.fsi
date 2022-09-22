module Autograph.UntypedTree.SourceParser

open FSharp.Compiler.Syntax

val (|TFuns|) : t : SynType -> SynType list
val (|TupleTypes|) : ts : SynTupleTypeSegment list -> SynType list
val (|NamedPat|_|) : p : SynPat -> Ident option
val (|IdentType|_|) : text : string -> t : SynType -> SynType option
val (|LongIdentType|_|) : t : SynType -> LongIdent option
val (|ParenPat|_|) : p : SynPat -> SynPat option
val (|TypedPat|_|) : p : SynPat -> (SynPat * SynType) option
val (|TyparInConstraint|_|) : tc : SynTypeConstraint -> SynTypar option
val (|ForceMemberFlags|) : nodeName : string -> mf : SynMemberFlags option -> SynMemberFlags
