module Telplin.UntypedTree.Writer

open FSharp.Compiler.Text
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Telplin.Common

let zeroRange = Range.Zero

type Range with

    member r.Proxy : RangeProxy =
        RangeProxy (r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)

let stn v = SingleTextNode (v, zeroRange)

let mkTypeFromString (typeText : string) : Type =
    let oak =
        CodeFormatter.ParseOakAsync (true, $"val v: {typeText}")
        |> Async.RunSynchronously
        |> Array.head
        |> fst

    match oak.ModulesOrNamespaces.[0].Declarations with
    | [ ModuleDecl.Val valNode ] -> valNode.Type
    | _ -> failwith "todo"

let mkMember (resolver : TypedTreeInfoResolver) (md : MemberDefn) : MemberDefn option =
    match md with
    | MemberDefn.ValField _ -> Some md
    | _ -> None

let mkMembers (resolver : TypedTreeInfoResolver) (ms : MemberDefn list) : MemberDefn list =
    List.choose (mkMember resolver) ms

let mkTypeDefn (resolver : TypedTreeInfoResolver) (typeDefn : TypeDefn) : TypeDefn =
    let tdn = TypeDefn.TypeDefnNode typeDefn

    let typeName =
        TypeNameNode (
            tdn.TypeName.XmlDoc,
            tdn.TypeName.Attributes,
            tdn.TypeName.LeadingKeyword,
            tdn.TypeName.Accessibility,
            tdn.TypeName.Identifier,
            tdn.TypeName.TypeParameters,
            tdn.TypeName.Constraints,
            None,
            tdn.TypeName.EqualsToken,
            tdn.TypeName.WithKeyword,
            zeroRange
        )

    match typeDefn with
    | TypeDefn.Record recordNode ->
        TypeDefnRecordNode (
            typeName,
            recordNode.Accessibility,
            recordNode.OpeningBrace,
            recordNode.Fields,
            recordNode.ClosingBrace,
            mkMembers resolver tdn.Members,
            zeroRange
        )
        |> TypeDefn.Record
    | TypeDefn.Explicit explicitNode ->
        let body =
            TypeDefnExplicitBodyNode (
                explicitNode.Body.Kind,
                mkMembers resolver explicitNode.Body.Members,
                explicitNode.Body.End,
                zeroRange
            )

        TypeDefnExplicitNode (typeName, body, mkMembers resolver tdn.Members, zeroRange)
        |> TypeDefn.Explicit
    | TypeDefn.Regular _ ->
        TypeDefnRegularNode (typeName, mkMembers resolver tdn.Members, zeroRange)
        |> TypeDefn.Regular
    | _ -> failwith "todo"

let mkModuleDecl (resolver : TypedTreeInfoResolver) (mdl : ModuleDecl) : ModuleDecl =
    match mdl with
    | ModuleDecl.TopLevelBinding bindingNode ->
        match bindingNode.FunctionName with
        | Choice1Of2 name ->
            let nameRange = (name :> Node).Range.Proxy
            let returnTypeString, _ = resolver.GetFullForBinding nameRange
            let valKw = MultipleTextsNode ([ stn "val" ], zeroRange)

            let name =
                match name.Content with
                | [ IdentifierOrDot.Ident name ] -> name
                | _ -> failwith "todo"

            let t = mkTypeFromString returnTypeString

            ValNode (
                bindingNode.XmlDoc,
                bindingNode.Attributes,
                Some valKw,
                None,
                false,
                None,
                name,
                None,
                t,
                Some (stn "="),
                None,
                zeroRange
            )
            |> ModuleDecl.Val
        | _ -> failwith "todo"

    | ModuleDecl.TypeDefn typeDefn -> mkTypeDefn resolver typeDefn |> ModuleDecl.TypeDefn
    | _ -> failwith "todo"

let mkModuleOrNamespace
    (resolver : TypedTreeInfoResolver)
    (moduleNode : ModuleOrNamespaceNode)
    : ModuleOrNamespaceNode
    =
    let decls = List.map (mkModuleDecl resolver) moduleNode.Declarations
    ModuleOrNamespaceNode (moduleNode.Header, decls, zeroRange)

let mkSignatureFile (resolver : TypedTreeInfoResolver) (code : string) =
    let implementationOak =
        CodeFormatter.ParseOakAsync (false, code)
        |> Async.RunSynchronously
        |> Array.find (fun (_, defines) -> List.isEmpty defines)
        |> fst

    let signatureOak =
        let mdns =
            List.map (mkModuleOrNamespace resolver) implementationOak.ModulesOrNamespaces

        Oak (implementationOak.ParsedHashDirectives, mdns, zeroRange)

    CodeFormatter.FormatOakAsync (signatureOak) |> Async.RunSynchronously
