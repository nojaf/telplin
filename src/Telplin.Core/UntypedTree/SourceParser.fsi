module Telplin.Core.UntypedTree.SourceParser

open Fantomas.Core.SyntaxOak

[<return : Struct>]
val (|PropertyGetSetThatNeedSplit|_|) : md : MemberDefn -> MemberDefnPropertyGetSetNode voption

[<return : Struct>]
val (|PrivateTopLevelBinding|_|) : ModuleDecl -> unit voption

[<return : Struct>]
val (|PrivateConstructor|_|) : ImplicitConstructorNode -> unit voption

[<return : Struct>]
val (|PrivateMemberDefn|_|) : MemberDefn -> unit voption

/// Augmentation with a private member.
/// Might be excluded via configuration
[<return : Struct>]
val (|PrivateTypeDefnAugmentation|_|) : TypeDefn -> unit voption

[<return : Struct>]
val (|PatParen|_|) : Pattern -> Pattern voption

[<return : Struct>]
val (|SingleIdentType|_|) : Type -> string voption

[<return : Struct>]
val (|NameOfPat|_|) : Pattern -> SingleTextNode voption
