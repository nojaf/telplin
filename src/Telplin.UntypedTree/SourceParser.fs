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

let (|TParen|_|) =
    function
    | Type.Paren parenNode -> Some parenNode.Type
    | _ -> None

let (|PropertyGetSetWithExtraParameter|_|) (md : MemberDefn) =
    match md with
    | MemberDefn.PropertyGetSet node ->
        node.LastBinding
        |> Option.bind (fun lastBinding ->
            if node.FirstBinding.Parameters.Length = lastBinding.Parameters.Length then
                None
            else

            let getBinding, setBinding =
                if node.FirstBinding.LeadingKeyword.Text = "get" then
                    node.FirstBinding, lastBinding
                else
                    lastBinding, node.FirstBinding

            Some (node, getBinding, setBinding)
        )
    | _ -> None
