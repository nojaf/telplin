module Telplin.Core.TypedTree.Resolver

open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
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
    projectOptions : FSharpProjectOptions -> implementation : string -> signature : string -> FSharpDiagnostic array

val typeCheckForImplementation :
    projectOptions : FSharpProjectOptions -> sourceCode : string -> Choice<unit, FSharpDiagnostic array>

val FCSSignature : options : FSharpProjectOptions -> implementation : string -> Choice<unit, string>
