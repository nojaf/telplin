module Telplin.Core.UntypedTree.ASTCreation

open Fantomas.FCS.Text
open Fantomas.Core
open Fantomas.Core.SyntaxOak

type Range with

    member this.FCSRange : FSharp.Compiler.Text.Range =
        FSharp.Compiler.Text.Range.mkRange
            this.FileName
            (FSharp.Compiler.Text.Position.mkPos this.StartLine this.StartColumn)
            (FSharp.Compiler.Text.Position.mkPos this.EndLine this.EndColumn)

let zeroRange = Range.Zero

/// Create a `SingleTextNode` based on the value string.
let stn v = SingleTextNode (v, zeroRange)

/// Create an `IdentListNode` with a single IdentifierOrDot.Ident value.
let iln v =
    IdentListNode ([ IdentifierOrDot.Ident (stn v) ], zeroRange)

/// Create a `MultipleTextsNode` with a single value.
let mtn v =
    MultipleTextsNode ([ stn v ], zeroRange)

/// Check if `MultipleAttributeListNode` contains an attribute that contains any of the given `name`.
let hasAnyAttribute (names : Set<string>) (multipleAttributeListNode : MultipleAttributeListNode option) =
    match multipleAttributeListNode with
    | None -> false
    | Some multipleAttributeListNode ->
        multipleAttributeListNode.AttributeLists
        |> List.collect (fun al -> al.Attributes)
        |> List.exists (fun a ->
            a.TypeName.Content
            |> List.exists (
                function
                | IdentifierOrDot.Ident attributeName -> names.Contains attributeName.Text
                | _ -> false
            )
        )

let mkValFromString (valText : string) : Result<ValNode, string> =
    try
        let oak =
            CodeFormatter.ParseOakAsync (true, valText)
            |> Async.RunSynchronously
            |> Array.head
            |> fst

        match oak.ModulesOrNamespaces.[0].Declarations with
        | [ ModuleDecl.Val valNode ] -> Ok valNode
        | decls -> Error $"Unexpected module decls:%A{decls}"
    with ex ->
        Error $"Could not parse:\n%s{valText}"

let mkMemberSigFromString (memberText : string) : Result<MemberDefnSigMemberNode, string> =
    let pseudoSignature =
        $"""
type A =
    new: unit -> A
    {memberText}
"""

    try
        let oak =
            CodeFormatter.ParseOakAsync (true, pseudoSignature)
            |> Async.RunSynchronously
            |> Array.head
            |> fst

        match oak.ModulesOrNamespaces.[0].Declarations.[0] with
        | ModuleDecl.TypeDefn typeDefn ->
            let tdn = TypeDefn.TypeDefnNode typeDefn

            match tdn.Members with
            | [ _ctor ; MemberDefn.SigMember sigMember ] -> Ok sigMember
            | ms -> Error $"Unexpected members:%A{ms}"

        | decls -> Error $"Unexpected module decls:%A{decls}"
    with ex ->
        Error $"Could not parse:\n%s{pseudoSignature}"

let mkPrimaryConstructorFromString (primaryCtorText : string) : Result<MemberDefnSigMemberNode, string> =
    let pseudoSignature =
        $"""
            type A =
                {primaryCtorText}
            """

    try
        let oak =
            CodeFormatter.ParseOakAsync (true, pseudoSignature)
            |> Async.RunSynchronously
            |> Array.head
            |> fst

        match oak.ModulesOrNamespaces.[0].Declarations.[0] with
        | ModuleDecl.TypeDefn typeDefn ->
            let tdn = TypeDefn.TypeDefnNode typeDefn

            match tdn.Members.[0] with
            | MemberDefn.SigMember ctor -> Ok ctor
            | ms -> Error $"Unexpected members:%A{ms}"

        | decls -> Error $"Unexpected module decls:%A{decls}"
    with ex ->
        Error $"Could not parse:\n%s{pseudoSignature}"
