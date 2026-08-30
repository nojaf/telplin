module Telplin.Core.TypedTree.Options

open FSharp.Compiler.CodeAnalysis

/// <summary>
/// Construct FSharpProjectOption via the `dotnet msbuild` command.
/// This happens out-of-process and is the coolest thing ever.
/// </summary>
/// <param name="fsproj">Input FSharp project</param>
/// <param name="additionalArguments">Additional MSBuild parameters append to each cli invocation.</param>
val mkOptionsFromDesignTimeBuild : fsproj : string -> additionalArguments : string -> Async<FSharpProjectOptions>

/// <summary>
/// Does the same as `mkOptionsFromDesignTimeBuild` but won't collect any F# project references.
/// </summary>
/// <see cref="mkOptionsFromDesignTimeBuild"/>
/// <param name="fsproj"></param>
/// <param name="additionalArguments"></param>
val mkOptionsFromDesignTimeBuildWithoutReferences :
    fsproj : string -> additionalArguments : string -> Async<FSharpProjectOptions>

/// The full paths of the `Compile` items of a project, as MSBuild evaluates them. Globs and
/// imports are expanded; no target runs, so this is cheaper than a design time build.
val compileItems : fsproj : string -> additionalArguments : string -> Async<string array>

val mkOptionsFromResponseFile : responseFilePath : string -> FSharpProjectOptions
