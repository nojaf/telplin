module Telplin.UntypedTree.TypeForValNode

open FSharp.Compiler.Text
open Fantomas.Core.SyntaxOak
open Telplin.Common

/// Typed tree information for a `FSharpMemberOrFunctionOrValue`
type TypedTreeInfo =
    { ReturnType: Type
      BindingGenericParameters: GenericParameter list
      TypeGenericParameters: GenericParameter list }

/// <summary>
/// The `returnType` from the typed tree won't contain any parameter names (in case of a function).
/// And it might need some additional parentheses. For example, when the return type is a function type.
/// </summary>
/// <param name="typedTreeInfo">Resolved type information from the typed tree.</param>
/// <param name="typeParameterMap">A map of generic parameters found in the untyped tree with the ones found in the typed tree.</param>
/// <param name="parameters">Parameters found in the input Oak. These will be used to enhance the parameters in the `returnType` by adding the name (if present).</param>
val mkTypeForValNodeBasedOnTypedTree:
    typedTreeInfo: TypedTreeInfo -> typeParameterMap: Map<string, string> -> parameters: Pattern list -> Type

val mkTypeForValNode:
    resolver: TypedTreeInfoResolver ->
    nameRange: range ->
    typeParameterMap: Map<string, string> ->
    parameters: Pattern list ->
        Type
