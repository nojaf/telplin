module Telplin.Core.UntypedTree.SourceParser

open Fantomas.Core.SyntaxOak

let (|PropertyGetSetThatNeedSplit|_|) (md : MemberDefn) =
    match md with
    | MemberDefn.PropertyGetSet node ->
        node.LastBinding
        |> Option.bind (fun lastBinding ->
            if
                node.FirstBinding.Parameters.Length = lastBinding.Parameters.Length
                && node.FirstBinding.Accessibility = lastBinding.Accessibility
            then
                None
            else
                Some node
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

let (|PrivateMemberDefn|_|) =
    function
    | MemberDefn.ExplicitCtor node ->
        match node.Accessibility with
        | Some Private -> Some ()
        | _ -> None
    | MemberDefn.Member node ->
        match node.Accessibility with
        | Some Private -> Some ()
        | _ -> None
    | _ -> None

let (|PrivateTypeDefnAugmentation|_|) typeDefn =
    match typeDefn with
    | TypeDefn.Augmentation _ ->
        let tdn = TypeDefn.TypeDefnNode typeDefn

        let allMembersArePrivate =
            tdn.Members
            |> List.forall (
                function
                | PrivateMemberDefn _ -> true
                | _ -> false
            )

        if allMembersArePrivate then Some () else None
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
