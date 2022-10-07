---
index: 3
---
# Technical overview

## General premise

Telpin uses both the `untyped` and the `typed` syntax tree to construct a signature file.  
It will traverse over all the nodes of the `untyped` tree and, where applicable, construct the signature counter parts.

For example a [SynBinding](https://fsprojects.github.io/fantomas/reference/fsharp-compiler-syntax-synbinding.html) becomes a [SynValSig](https://fsprojects.github.io/fantomas/reference/fsharp-compiler-syntax-synvalsig.html).  
The signature AST is really constructed as close as possible to the implementation AST.  
You'd be surprised how many information can just be re-used going from implementation to signature AST.

When the `untyped` tree does not contain sufficient information, a `FSharpSymbol` will be queried from the `typed` tree to enrich the missing information.

## Solution structure

<div class="mermaid text-center">
graph TD
    A[Telplin.Common] --> B
    A --> C
    B[Telplin.UntypedTree] --> D[Teplin.Core]
    C[Telplin.TypedTree] --> D
    D --> E[Telplin]
    D --> F[Telplin.Lambda]
    D --> G[Telplin.Core.Tests]
    H[Telplin.Deploy]
    I[OnlineTool]
 </div>

### Telplin.Common

Contains some shared types and interfaces.

### Teplin.UntypedTree

Has a reference to [Fantomas.Core](https://www.nuget.org/packages/Fantomas.Core) and processes the `untyped` tree.
Note that Fantomas does not use [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service).  
The `untyped` tree types that `Fantomas.FCS` exposes are nearly identical to `FCS` but are not binary compatible.

In short, Fantomas needs *Fantomas flavoured* AST to produce source code and that is why there is a strict boundary drawn between the `untyped` and `typed` trees.

### Telplin.TypedTree

References the [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service) and is being used to type check the implementation file.  
The `untyped` tree constructed by the `FSharpChecker` is very deliberately not used, due to its incompatibility with Fantomas.

### Telplin.Core

Orchestrates the processing of both syntax trees.

### Telplin

A command line tool application that reads a [MSBuild binlog file](https://msbuildlog.com/#usingbl) and can generate signatures.  
The main reason to use a `binlog` file, is that Telplin needs the compiler options to get the `typed` tree.  
Another benefit would be the insurance that all referenced projects are built and available.

### Telplin.Lambda

An `AWS` lambda that powers the back-end of the [online tool](../index.html).

### Telplin.Core.Tests

An `NUnit` unit test project that verifies the core logic.

### OnlineTool

A `Fable` application that powers the front-end of the [online tool](../index.html).

<tp-nav previous="./usage.html" next="./contributing.html"></tp-nav>