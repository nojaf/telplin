module Telplin.Core.TypedTree.Resolver

open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open Telplin.Core

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
    projectOptions : FSharpProjectOptions -> implementation : string -> signature : string -> FSharpDiagnosticInfo array

val typeCheckForImplementation :
    projectOptions : FSharpProjectOptions -> sourceCode : string -> Choice<unit, FSharpDiagnosticInfo array>

val FCSSignature : options : FSharpProjectOptions -> implementation : string -> Choice<unit, string>
