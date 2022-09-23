module Telplin.UntypedTree.SourceParser

open FSharp.Compiler.Syntax
open FSharp.Compiler.Xml

val (|TFuns|) : t : SynType -> SynType list
val (|TupleTypes|) : ts : SynTupleTypeSegment list -> SynType list
val (|NamedPat|_|) : p : SynPat -> Ident option
val (|IdentType|_|) : text : string -> t : SynType -> SynType option
val (|LongIdentType|_|) : t : SynType -> LongIdent option
val (|ParenPat|_|) : p : SynPat -> SynPat option
val (|TypedPat|_|) : p : SynPat -> (SynPat * SynType) option
val (|AttribPat|_|) : p : SynPat -> (SynAttributes * SynPat) option
val (|TyparInConstraint|_|) : tc : SynTypeConstraint -> SynTypar option
val (|ForceMemberFlags|) : nodeName : string -> mf : SynMemberFlags option -> SynMemberFlags
val (|GetMemberLongIdentPat|_|) : p : SynPat -> (Ident * SynAccess option) option
val (|SetMemberLongIdentPat|_|) : p : SynPat -> Ident option

val (|SimpleGetSetBinding|_|) :
    md : SynMemberDefn -> (Ident * SynAttributes * SynValInfo * PreXmlDoc * SynAccess option * SynMemberFlags) option

val (|IndexedGetSetBinding|_|) :
    md : SynMemberDefn ->
        (Ident * SynAttributes * SynValInfo * PreXmlDoc * SynAccess option * SynMemberFlags * Ident option) option
