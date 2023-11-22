module Telplin.Core.TypedTree.Options

open FSharp.Compiler.CodeAnalysis

val mkOptionsFromDesignTimeBuild : fsproj : string -> additionalArguments : string -> Async<FSharpProjectOptions>
val mkOptionsFromResponseFile : responseFilePath : string -> FSharpProjectOptions
