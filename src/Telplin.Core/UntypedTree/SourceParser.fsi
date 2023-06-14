module Telplin.Core.UntypedTree.SourceParser

open Fantomas.Core.SyntaxOak

val (|PropertyGetSetWithExtraParameter|_|) :
    md : MemberDefn -> (MemberDefnPropertyGetSetNode * PropertyGetSetBindingNode * PropertyGetSetBindingNode) option

val (|PrivateTopLevelBinding|_|) : ModuleDecl -> unit option
val (|PrivateConstructor|_|) : ImplicitConstructorNode -> unit option
val (|PrivateMemberDefn|_|) : MemberDefn -> unit option
/// Augmentation with a private member.
/// Might be excluded via configuration
val (|PrivateTypeDefnAugmentation|_|) : TypeDefn -> unit option
val (|PatParen|_|) : Pattern -> Pattern option
val (|SingleIdentType|_|) : Type -> string option
val (|NameOfPat|_|) : Pattern -> SingleTextNode option
