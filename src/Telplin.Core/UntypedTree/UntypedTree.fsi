module Telplin.Core.UntypedTree.Writer

open Telplin.Core

/// Create signature file for a given implementation file code.
/// This happens by parsing the code to an Oak and transforming it to it's signature counterpart.
/// The resolver can provide typed-tree information when necessary.
val mkSignatureFile : resolver : TypedTree.Resolver.TypedTreeInfoResolver -> code : string -> string * TelplinError list

/// The `private` keywords on module-level let bindings of the implementation, which a signature
/// file that leaves those bindings out makes redundant. Members are not included.
val redundantPrivateKeywords : defines : string list -> code : string -> FSharp.Compiler.Text.range list
