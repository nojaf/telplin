module Autograph.UntypedTree.SourceParser

open FSharp.Compiler.Syntax

val (|StaticMemberFlags|_|) : memberFlags : SynMemberFlags -> SynMemberFlags option

val (|NewConstructorPattern|_|) :
    memberFlags : SynMemberFlags option * headPat : SynPat -> (SynLongIdent * SynArgPats * SynAccess option) option

val (|EmptySynArgInfo|_|) : argInfo : SynArgInfo -> SynArgInfo option

val (|UnitType|_|) : t : SynType -> SynType option
