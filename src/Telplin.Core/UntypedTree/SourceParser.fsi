module Telplin.Core.UntypedTree.SourceParser

open Fantomas.Core.SyntaxOak

val (|PropertyGetSetWithExtraParameter|_|) :
    md : MemberDefn -> (MemberDefnPropertyGetSetNode * PropertyGetSetBindingNode * PropertyGetSetBindingNode) option

val (|PrivateTopLevelBinding|_|) : ModuleDecl -> unit option
val (|PrivateConstructor|_|) : ImplicitConstructorNode -> unit option
val (|PrivateMemberDefn|_|) : MemberDefn -> unit option
val (|PatParen|_|) : Pattern -> Pattern option
val (|SingleIdentType|_|) : Type -> string option
val (|NameOfPat|_|) : Pattern -> SingleTextNode option
