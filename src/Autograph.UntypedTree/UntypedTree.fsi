module Autograph.UntypedTree.Writer

open Autograph.Common

val mkSignatureFile : resolver : TypedTreeInfoResolver -> code : string -> string
