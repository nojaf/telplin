module Telplin.TypedTree.Options

open FSharp.Compiler.CodeAnalysis

val mkOptionsFromBinaryLog : binLogPath : string -> FSharpProjectOptions
val mkOptionsFromResponseFile : responseFilePath : string -> FSharpProjectOptions
