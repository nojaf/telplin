module Autograph.TypedTree.Resolver

open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open Autograph.Common

val mkResolverFor :
    sourceFileName : string ->
    sourceText : ISourceText ->
    projectOptions : FSharpProjectOptions ->
        TypedTreeInfoResolver

val mkResolverForCode : code : string -> TypedTreeInfoResolver
val typeCheckForPair : implementationPath : string -> signaturePath : string -> FSharpDiagnosticInfo array
val typeCheckForImplementation : implementationPath : string -> FSharpDiagnosticInfo array
