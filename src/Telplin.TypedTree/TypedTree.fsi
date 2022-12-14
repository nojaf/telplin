module Telplin.TypedTree.Resolver

open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open Telplin.Common

val mkResolverFor :
    sourceFileName : string ->
    sourceText : ISourceText ->
    projectOptions : FSharpProjectOptions ->
        TypedTreeInfoResolver

val mkResolverForCode : projectOptions : FSharpProjectOptions -> code : string -> TypedTreeInfoResolver

val typeCheckForPair :
    projectOptions : FSharpProjectOptions ->
    implementationPath : string ->
    signaturePath : string ->
        FSharpDiagnosticInfo array

val typeCheckForImplementation :
    projectOptions : FSharpProjectOptions -> sourceCode : string -> Choice<unit, FSharpDiagnosticInfo array>
