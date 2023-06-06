module Telplin.Core.UntypedTree.Writer

open Telplin.Core

val mkSignatureFile : resolver : TypedTree.Resolver.TypedTreeInfoResolver -> code : string -> string * TelplinError list
