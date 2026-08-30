---
index: 2
---
# Usage

Telplin was designed to create a starting point when introducing signature files to a code base.  
It is meant to be used **one time**, and was not designed to keep your signature files up to date.

## Installation

The recommendation is to install Telplin globally:

> dotnet tool install -g telplin

Run `telplin --help` for the full list of flags.

## What a run does

> telplin src/App/App.fsproj

1. The project is built (a design time build, which asks MSBuild for the compiler arguments) and type checked. A project that does not compile is refused: Telplin reads its types off the checker, and a broken project has nothing reliable to say.
2. A signature is generated for every implementation file, or for the ones named with `--files`.
3. The whole project is type checked again with the new signatures in front of their implementations. When that fails, the diagnostics are printed and nothing is written.
4. The signature files are written next to their implementation files, and each one is listed in the project file directly before its implementation.
5. The `private` keyword is removed from the let bindings the signature leaves out. With a signature file in place, a binding it does not mention is private whether or not it says so.

The input can also be a folder that holds exactly one project file, or a response file (see below).

Anything after `--` is passed to the design time build:

> telplin src/App/App.fsproj -- -p:Configuration=Release

## Flags

| Flag | |
| --- | --- |
| `--files <paths>` | Process only these files of the project. A path can be given as is, or by the end of its path inside the project, so `Api.fs` or `App/Api.fs` both work from anywhere. |
| `--dry-run` | Print the signatures to the console and write nothing. The project is still verified. |
| `--record` | Also save the compiler arguments of the design time build next to the project, as a `.rsp` file. Passing that file as the input later skips the build. |
| `--only-record` | Save the compiler arguments as with `--record`, and stop there. |
| `--include-private-bindings` | Include private bindings in the signature file. |
| `--keep-unused` | Keep let bindings that no other file of this project uses. See below. |
| `--no-verify` | Skip the check that the project still compiles with the new signatures in place. |
| `--no-project` | Do not list the signature files in the project file. |
| `--keep-private` | Leave the `private` keyword on let bindings in the implementation file. |
| `--force` | Write the signatures even when the check fails. For debugging purposes only. |
| `--version` | Print the version and exit. |
| `-h`, `--help` | Display the help page and exit. |

## Keeping only what is used

A signature file is more effective when it exposes only what the rest of the code base needs.  
By default, a module-level `let` binding is left out of the signature when no other file of the project uses it, which makes it private. Types, members and bindings that carry an attribute (an entry point, a test, a literal) are always kept.

> telplin src/App/App.fsproj --files App/Api.fs

Be aware of what "used" means here: **only this project is looked at**. A binding used by another project, by a test project, through reflection, or by consumers of a published library is not seen, and would be made private. The default is for files that are internal to a project. For a public API, pass `--keep-unused` to keep every binding; `--include-private-bindings` implies it.

## Response files

The design time build is the slow part of a run. `--record` saves its result as a `.rsp` file next to the project, and that file can be the input of later runs:

> telplin src/App/App.rsp --files App/Api.fs

There is no project to update in that case, so the signature files have to be listed in the project by hand.

<tp-nav previous="./motivation.html" next="./technical-overview.html"></tp-nav>
