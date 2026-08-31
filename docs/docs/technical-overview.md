---
index: 3
---

# Technical overview

## General premise

Telplin uses both the `untyped` and the `typed` syntax tree to construct a signature file.

The implementation file is parsed with Fantomas's own parser (`Fantomas.FCS`) and transformed to its [Oak](https://fsprojects.github.io/fantomas/reference/fantomas-core-syntaxoak.html) model. Telplin walks the Oak and constructs the signature counterpart of every node: a [BindingNode](https://fsprojects.github.io/fantomas/reference/fantomas-core-syntaxoak-bindingnode.html) becomes a [ValNode](https://fsprojects.github.io/fantomas/reference/fantomas-core-syntaxoak-valnode.html), for example.  
The signature tree is constructed as close as possible to the implementation tree, and you'd be surprised how much information can just be re-used going from one to the other.

When the Oak does not contain sufficient information (an inferred return type, say), a `FSharpSymbol` is queried from the `typed` tree to fill in the gap.  
The finished signature Oak is printed by Fantomas, so the output is formatted source code from the start.

## Solution structure

<div class="mermaid text-center">
graph TD
    D[Telplin.Core]
    E[Telplin]
    F[Telplin.Lambda]
    G[Telplin.Core.Tests]
    D --> E
    D --> F
    D --> G
    H[Telplin.Deploy]
    I[OnlineTool]
 </div>

### Telplin.Core

Has a reference to [Fantomas.Core](https://www.nuget.org/packages/Fantomas.Core) and processes the `untyped` tree using the `Oak` model.
Note that Fantomas does not use [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service): it ships its own parser, `Fantomas.FCS`.

Also references the [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service), which is used to type check the implementation files.  
The `untyped` tree constructed by the `FSharpChecker` is very deliberately not used, due to its incompatibility with Fantomas.

### Telplin

The command line tool. It resolves the input (a source file, a project, a folder or a response file), runs a _design time build_ through MSBuild to obtain the compiler arguments, and orchestrates the run described on the [start page](./index.html): generate, verify, write, clean up the implementation files and list the signatures in the project file.

### Telplin.Lambda

An `AWS` lambda that powers the back-end of the [online tool](../index.html).

### Telplin.Core.Tests

An `NUnit` unit test project that verifies the core logic.

### OnlineTool

A `Fable` application that powers the front-end of the [online tool](../index.html).

### Telplin.Deploy

A `Pulumi` program that deploys the online tool's infrastructure to `AWS`.

<tp-nav previous="./usage.html" next="./contributing.html"></tp-nav>
