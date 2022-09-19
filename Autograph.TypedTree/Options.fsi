module Autograph.TypedTree.Options

open FSharp.Compiler.CodeAnalysis

val mkOptions: compilerArgs: string array -> FSharpProjectOptions
