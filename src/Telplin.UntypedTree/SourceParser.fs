module Telplin.UntypedTree.SourceParser

open Fantomas.Core.SyntaxOak

type TypeTupleNode with

    member x.Types =
        x.Path
        |> List.choose (
            function
            | Choice1Of2 t -> Some t
            | Choice2Of2 _ -> None
        )
