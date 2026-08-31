# Telplin

<blockquote class="fancy">
<p>And each day of the Valar in Aman contained twelve hours, and ended with the second mingling of the lights, in which Laure<strong>lin</strong> was waning but <strong>Telp</strong>erion was waxing.<br>
&mdash; The Silmarillion, Ch 1, Of the Beginning of Days</p>
</blockquote>

Telplin **generates signature files** for F# implementation files, using the [F# compiler](https://github.com/dotnet/fsharp) and [Fantomas](https://github.com/fsprojects/fantomas).  
It was designed to create a starting point when introducing signature files to a code base.

## Installation

The recommendation is to install Telplin globally:

```shell
dotnet tool install -g telplin
```

Run `telplin --help` for the full list of flags.

## What a run does

The typical run asks for a signature file for one file of your project:

```shell
telplin src/App/Api.fs
```

The nearest project above the file that has it as a `Compile` item is used. Then:

1. The project is built (a _design time build_, which asks MSBuild for the compiler arguments) and type checked. A project that does not compile is refused: Telplin reads its types off the checker, and a broken project has nothing reliable to say.
2. A signature is generated for the input file. When the input is a project, every implementation file gets one, or only those named with `--files`.
3. The whole project is type checked again with the new signatures in front of their implementations. When that fails, the diagnostics are printed and nothing is written.
4. The signature files are written next to their implementation files, and each one is listed in the project file directly before its implementation.
5. The `private` keyword is removed from the let bindings the signature leaves out. With a signature file in place, a binding it does not mention is private whether or not it says so.
6. The XML doc comments of the declarations the signature has are removed from the implementation file. Tooling reads the docs from the signature file, and a second copy in the implementation only drifts apart from it. Docs on declarations the signature leaves out stay.

### For example

`Api.fs` below has two bindings, and `Program.fs`, the only other file of the project, calls `greet` and nothing else:

```fsharp
module App.Api

let normalize (name: string) = name.Trim().ToLowerInvariant()

/// Greet a user by name.
let greet (name: string) = $"Hello %s{normalize name}!"
```

The run above leaves this git diff. A new `Api.fsi` holds the public face of the file: only `greet`, since nothing else uses `normalize`, and the doc comment moved along:

```diff
+++ b/src/App/Api.fsi
+module App.Api
+
+/// Greet a user by name.
+val greet: name: string -> string
```

The implementation gave up its copy of the doc comment; the signature is where tooling reads it from now:

```diff
--- a/src/App/Api.fs
+++ b/src/App/Api.fs
 let normalize (name: string) = name.Trim().ToLowerInvariant()

-/// Greet a user by name.
 let greet (name: string) = $"Hello %s{normalize name}!"
```

And the project file lists the signature directly before its implementation:

```diff
--- a/src/App/App.fsproj
+++ b/src/App/App.fsproj
   <ItemGroup>
+    <Compile Include="Api.fsi" />
     <Compile Include="Api.fs" />
     <Compile Include="Program.fs" />
   </ItemGroup>
```

`normalize` is now private to `Api.fs` without a `private` keyword in sight, and the project was type checked with the signature in place before any of this was written.

To take on the whole project at once, pass the `fsproj` itself, a folder that holds exactly one project file, or a [response file](./usage.html#Response-files):

```shell
telplin src/App/App.fsproj
```

Anything after `--` is passed to the _design time build_:

```shell
telplin src/App/Api.fs -- -p:Configuration=Release
```

<tp-nav next="./motivation.html"></tp-nav>
