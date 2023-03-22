module Telplin.UntypedTree.SourceParser

open Fantomas.Core.SyntaxOak

type TypeTupleNode with

    member Types : Type list

val (|TParen|_|) : Type -> Type option
