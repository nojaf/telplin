module Telplin.Core.TypedTree.Resolver

open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics

[<Class>]
type TypedTreeInfoResolver =
    /// Tries to find a FSharpSymbol for the given name and range.
    /// Returns the signature text for the found symbol.
    member GetValText : name : string * range : range -> Result<string, string>
    member Defines : string list
    member IncludePrivateBindings : bool

val mkResolverFor :
    checker : FSharpChecker ->
    sourceFileName : string ->
    sourceText : ISourceText ->
    projectOptions : FSharpProjectOptions ->
    includePrivateBindings : bool ->
        TypedTreeInfoResolver

val mkResolverForCode :
    projectOptions : FSharpProjectOptions -> includePrivateBindings : bool -> code : string -> TypedTreeInfoResolver

val typeCheckForPair :
    projectOptions : FSharpProjectOptions -> implementation : string -> signature : string -> FSharpDiagnostic array

val typeCheckForImplementation :
    projectOptions : FSharpProjectOptions -> sourceCode : string -> Choice<unit, FSharpDiagnostic array>

val FCSSignature : options : FSharpProjectOptions -> implementation : string -> Choice<unit, string>
