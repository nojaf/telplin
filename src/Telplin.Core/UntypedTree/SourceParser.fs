module Telplin.Core.UntypedTree.SourceParser

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

let (|Private|_|) (stn : SingleTextNode) =
    if stn.Text = "private" then Some () else None

let (|PrivateTopLevelBinding|_|) (mdl : ModuleDecl) =
    match mdl with
    | ModuleDecl.TopLevelBinding binding ->
        match binding.Accessibility with
        | Some Private -> Some ()
        | _ -> None
    | _ -> None

let (|PrivateConstructor|_|) (implicitCtor : ImplicitConstructorNode) =
    match implicitCtor.Accessibility with
    | Some Private -> Some ()
    | _ -> None

let (|PrivateMemberDefnExplicitCtor|_|) =
    function
    | MemberDefn.ExplicitCtor node ->
        match node.Accessibility with
        | Some Private -> Some ()
        | _ -> None
    | _ -> None

let (|PatParen|_|) p =
    match p with
    | Pattern.Paren parenNode -> Some parenNode.Pattern
    | _ -> None

let (|SingleIdentType|_|) t =
    match t with
    | Type.LongIdent node ->
        match node.Content with
        | [ IdentifierOrDot.Ident ident ] -> Some ident.Text
        | _ -> None
    | _ -> None

let (|NameOfPat|_|) p =
    match p with
    | Pattern.Named name -> Some name.Name
    | _ -> None
