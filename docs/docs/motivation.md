---
index: 1
---
# Motivation

## The merits of signature files

[Signature files](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/signature-files) have three significant benefits to an F# code base.  

### References assemblies

In `dotnet` 7, F# supports [references assemblies](https://learn.microsoft.com/en-us/dotnet/standard/assembly/reference-assemblies).  
These can produced by adding `<ProduceReferenceAssembly>true</ProduceReferenceAssembly>` to your `fsproj`.

An important part of a reference assembly is the generated [mvid](https://learn.microsoft.com/en-us/dotnet/api/system.reflection.module.moduleversionid?view=net-7.0).  
This `mvid` should only change when the public API changes. Alas, this doesn't always work in F# code. Adding a new `let private a` binding could potentially still influence the `mvid`.
When signature files are used, the user has perfect controle over the public API and thus the `mvid`.

### Background checker speed up

When `enablePartialTypeChecking` is enabled in the `F# Checker`, the your IDE will skip the typechecking of implementation files that are backed by a signature when type information is requested for a file.

So imagine the following file structure:

```
A.fsi
A.fs
B.fsi
B.fs
C.fs
D.fs
```

If you open file `D.fs` and your editor requests type information, it will need to know what happened in all the files that came before `D`.  
As signature files expose all the same information, the background compiler can skip over `A.fs` and `B.fs`. Because it processed `A.fsi` and `B.fsi`, will contain the same information.
This improvement can make the IDE feel a lot snappier when working in a large codebase.

### Compilation improvement

In [dotnet/fsharp#13737](https://github.com/dotnet/fsharp/pull/13737), a similar feature was introduced to optimized the compiler. If an implementation file is backed by a signature file, the verification of whether the implementation and its signature match will be done in parallel.
This can be enabled by adding `<OtherFlags>--test:ParallelCheckingWithSignatureFilesOn</OtherFlags>` to your `fsproj`.

## Why this tool?

The `F#` compiler currently exposes a feature to generate signature files during a build.  
This can be enabled by adding `<OtherFlags>--allsigs</OtherFlags>` to your `fsproj`.

So why introduce an alternative for this?

### Typed tree only

`--allsigs` will generate a signature file based on the typed tree. This leads to some rather mixed results when you compare it to your implementation file.

Example:

```fsharp
module MyNamespace.MyModule

open System
open System.Collections.Generic

[<Literal>]
let Warning = "Some warning"

type Foo() =
    [<Obsolete(Warning)>]
    member this.Bar(x: int) = 0

    member this.Barry(x: int, y: int) = x + y
    member this.CollectKeys(d: IDictionary<string, string>) = d.Keys
```

Leads to

```fsharp
namespace MyNamespace
    
    module MyModule =
        
        [<Literal>]
        val Warning: string = "Some warning"
        
        type Foo =
            
            new: unit -> Foo
            
            [<System.Obsolete ("Some warning")>]
            member Bar: x: int -> int
            
            member Barry: x: int * y: int -> int
            
            member
              CollectKeys: d: System.Collections.Generic.IDictionary<string,
                                                                     string>
                             -> System.Collections.Generic.ICollection<string>
```

Syntactically this is a correct signature file, however, it is quite the departure from the source material.  
The typed tree misses a lot of context the implementation file has.

`Telplin` works a bit different and tries to remain as faithful as possible to the original implementation file using both the untyped and the typed tree.

### Faster release cycle.

As the `--allsigs` flag is part of the F# compiler, this means fixes to this feature are tied to `dotnet` SDK releases.  
The release cadence of the `dotnet` SDK can be somewhat unpredictable and it could take a while before a fix finally reaches end-users.

`Telplin` is a standalone tool that should be able to ship fixes shortly after they got merged.

<tp-nav previous="./index.html" next="./usage.html"></tp-nav>