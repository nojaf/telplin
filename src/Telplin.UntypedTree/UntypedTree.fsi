module Telplin.UntypedTree.Writer

open Telplin.Common

val mkSignatureFile : resolver : TypedTreeInfoResolver -> defines : string list -> code : string -> string
