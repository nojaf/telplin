---
description: Run the locally built Telplin.Core on F# source and show the signature, errors and diagnostics
allowed-tools: Bash(dotnet fsi:*), Bash(scripts/*), Bash(dotnet build:*), Bash(echo:*), Bash(cat:*)
---

First build: `dotnet build src/Telplin.Core`

Then pass a file path, or pipe the source via stdin:

```
scripts/telplin.fsx [--define FOO,BAR] [--no-private] [--fcs] <file>
echo '<source>' | scripts/telplin.fsx
```

It prints the generated signature, the Telplin errors (with ranges), and the diagnostics of compiling the
implementation together with the signature.

- `--no-private`: leave out private bindings.
- `--fcs`: use `GenerateSignature` from FCS instead of Telplin, to compare both.

$ARGUMENTS
