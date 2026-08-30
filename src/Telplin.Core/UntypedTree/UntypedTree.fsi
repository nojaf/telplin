module Telplin.Core.UntypedTree.Writer

open Telplin.Core

/// A generated signature, with the ranges in the implementation of the XML doc comments the
/// signature took over. Those belong to declarations the signature has; a doc on a binding it
/// leaves out is not among them.
type Signature =
    {
        Code : string
        Errors : TelplinError list
        XmlDocRanges : FSharp.Compiler.Text.range list
    }

val mkSignature : resolver : TypedTree.Resolver.TypedTreeInfoResolver -> code : string -> Signature

/// Create signature file for a given implementation file code.
/// This happens by parsing the code to an Oak and transforming it to it's signature counterpart.
/// The resolver can provide typed-tree information when necessary.
val mkSignatureFile : resolver : TypedTree.Resolver.TypedTreeInfoResolver -> code : string -> string * TelplinError list

/// The `private` keywords on module-level let bindings of the implementation, which a signature
/// file that leaves those bindings out makes redundant. Members are not included.
val redundantPrivateKeywords : defines : string list -> code : string -> FSharp.Compiler.Text.range list
