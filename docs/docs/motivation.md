---
index: 1
---

# Motivation

Introducing signature files to an existing code base is tedious by hand. Writing the signatures is only half the work: you also have to decide what belongs in them, and the implementation files deserve a cleanup once the signatures are in charge.

Telplin does all of that in one verified run:

- A signature is generated for every implementation file, faithful to how the original source is written.
- Each signature exposes only what the rest of the project actually uses. A module-level binding no other file touches is left out, which makes it private.
- The implementation files are cleaned up afterwards: the now-redundant `private` keywords are dropped, and the XML doc comments move to the signature file, the copy your tooling reads.
- Nothing is written until the whole project type checks again with the new signatures in place.

The result is a reviewable diff that pins down the actual API of every file, derived from the compiler's own type information rather than guesswork. Every default has an opt-out flag, see [Usage](./usage.html).

## The merits of signature files

Beyond documenting an API, [signature files](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/signature-files) buy an F# code base a few concrete things.

### Reference assemblies

[Reference assemblies](https://learn.microsoft.com/en-us/dotnet/standard/assembly/reference-assemblies) (`<ProduceReferenceAssembly>true</ProduceReferenceAssembly>` in the `fsproj`) let a build skip recompiling downstream projects when the public API did not change. That check hinges on a stable [mvid](https://learn.microsoft.com/en-us/dotnet/api/system.reflection.module.moduleversionid), and without signature files it is fragile in F#: adding a `let private` binding can change the `mvid` even though the public API did not move. With signature files in place, the `mvid` only changes when a signature changes.

### A snappier IDE

With partial type checking (`enablePartialTypeChecking` in the F# checker), the background checker of your IDE skips implementation files that are backed by a signature. Imagine the following file structure:

```
A.fsi
A.fs
B.fsi
B.fs
C.fs
D.fs
```

Opening `D.fs` requires type information for every file before it, but `A.fs` and `B.fs` can be skipped: their signatures carry the same information. In a large code base this makes the editor feel a lot snappier.

### Faster compilation

[Graph-based type checking](https://github.com/dotnet/fsharp/pull/14494) (`<ParallelCompilation>true</ParallelCompilation>` in the `fsproj`) lets the compiler check files in parallel. Signature files help twice: an implementation is verified against its own signature in parallel, and files that depend on it only need the signature to check themselves.

## What about `--allsigs`?

The F# compiler can generate signature files itself during a build, by adding `<OtherFlags>--allsigs</OtherFlags>` to the `fsproj`. Its output has improved considerably over the years, and it is a perfectly reasonable way to get valid signatures out of a build.

Generating the signature is also where `--allsigs` stops, and where Telplin starts. `--allsigs` exposes every declaration as is; Telplin checks what the rest of the project actually uses and trims each signature down to that, then cleans up the implementation files, lists the signatures in the project file, and verifies the whole project before writing anything. That is nowadays the main reason to reach for it.

### Closer to the source

Telplin also stays closer to how the implementation is written. Given

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

`--allsigs` produces

```fsharp
module MyNamespace.MyModule

[<Literal>]
val Warning: string = "Some warning"

type Foo =

    new: unit -> Foo

    [<System.Obsolete (Warning)>]
    member Bar: x: int -> int

    member Barry: x: int * y: int -> int

    member
      CollectKeys: d: System.Collections.Generic.IDictionary<string,string> ->
                     System.Collections.Generic.ICollection<string>
```

while Telplin writes

```fsharp
module MyNamespace.MyModule

open System
open System.Collections.Generic

[<Literal>]
val Warning: string = "Some warning"

type Foo =
    new: unit -> Foo

    [<Obsolete(Warning)>]
    member Bar: x: int -> int

    member Barry: x: int * y: int -> int
    member CollectKeys: d: IDictionary<string, string> -> ICollection<string>
```

Both are valid, but Telplin keeps the `open` statements, leaves types unqualified, and formats the result with Fantomas, so the signature reads like the implementation it belongs to.

And since Telplin ships independently of the `dotnet` SDK, a fix lands shortly after it is merged instead of waiting for an SDK release.

<tp-nav previous="./index.html" next="./usage.html"></tp-nav>
