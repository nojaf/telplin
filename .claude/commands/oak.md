---
description: Print the Fantomas Oak of F# source (the tree Telplin's UntypedTree code walks)
allowed-tools: Bash(dotnet fsi:*), Bash(scripts/*), Bash(echo:*), Bash(cat:*)
---

Pass a file path, or pipe the source via stdin:

```
scripts/oak.fsx [--signature] [--member] [--define FOO,BAR] <file>
echo '<source>' | scripts/oak.fsx [--signature] [--member]
```

- `--signature`: parse as a signature file (`.fsi` is detected from the file name).
- `--member`: the input is one member signature line, as returned by `GetValSignatureText`.
  It is wrapped in `type A = ...` exactly like `mkMemberSigFromString` in `ASTCreation.fs` does,
  so this tells you whether Telplin can parse what FCS gave it.

No build needed, the script uses the Fantomas.Core package from NuGet (the version pinned in `Directory.Packages.props`).

$ARGUMENTS
