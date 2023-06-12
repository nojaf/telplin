module Telplin.Core.UntypedTree.Writer

open Telplin.Core

/// Create signature file for a given implementation file code.
/// This happens by parsing the code to an Oak and transforming it to it's signature counterpart.
/// The resolver can provide typed-tree information when necessary.
val mkSignatureFile : resolver : TypedTree.Resolver.TypedTreeInfoResolver -> code : string -> string * TelplinError list
