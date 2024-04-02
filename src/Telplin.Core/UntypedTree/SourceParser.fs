module Telplin.Core.UntypedTree.SourceParser

open Fantomas.Core.SyntaxOak

let toVOption =
    function
    | None -> ValueNone
    | Some x -> ValueSome x

[<return : Struct>]
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
        |> toVOption
    | _ -> ValueNone

[<return : Struct>]
let (|Private|_|) (stn : SingleTextNode) =
    if stn.Text = "private" then ValueSome () else ValueNone

[<return : Struct>]
let (|PrivateTopLevelBinding|_|) (mdl : ModuleDecl) =
    match mdl with
    | ModuleDecl.TopLevelBinding binding ->
        match binding.Accessibility with
        | Some Private -> ValueSome ()
        | _ -> ValueNone
    | _ -> ValueNone

[<return : Struct>]
let (|PrivateConstructor|_|) (implicitCtor : ImplicitConstructorNode) =
    match implicitCtor.Accessibility with
    | Some Private -> ValueSome ()
    | _ -> ValueNone

[<return : Struct>]
let (|PrivateMemberDefn|_|) md =
    match md with
    | MemberDefn.ExplicitCtor node ->
        match node.Accessibility with
        | Some Private -> ValueSome ()
        | _ -> ValueNone
    | MemberDefn.Member node ->
        match node.Accessibility with
        | Some Private -> ValueSome ()
        | _ -> ValueNone
    | _ -> ValueNone

[<return : Struct>]
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

        if allMembersArePrivate then ValueSome () else ValueNone
    | _ -> ValueNone

[<return : Struct>]
let (|PatParen|_|) p =
    match p with
    | Pattern.Paren parenNode -> ValueSome parenNode.Pattern
    | _ -> ValueNone

[<return : Struct>]
let (|SingleIdentType|_|) t =
    match t with
    | Type.LongIdent node ->
        match node.Content with
        | [ IdentifierOrDot.Ident ident ] -> ValueSome ident.Text
        | _ -> ValueNone
    | _ -> ValueNone

[<return : Struct>]
let (|NameOfPat|_|) p =
    match p with
    | Pattern.Named name -> ValueSome name.Name
    | _ -> ValueNone
