---
description: Type check F# source and print the raw FCS signature text of every definition
allowed-tools: Bash(dotnet fsi:*), Bash(scripts/*), Bash(echo:*), Bash(cat:*)
---

Pass a file path, or pipe the source via stdin:

```
scripts/symbols.fsx [--define FOO,BAR] [--fcs] <file>
echo '<source>' | scripts/symbols.fsx
```

For every definition it prints the position, the symbol and the text of
`FSharpMemberOrFunctionOrValue.GetValSignatureText`. That text is what `TypedTreeInfoResolver.GetValText`
hands to the parser in `ASTCreation.fs`, so when Telplin reports `Could not parse`, this shows the input.

- `--fcs`: also print `FSharpCheckFileResults.GenerateSignature()`, the signature the compiler itself makes.

Type checking uses the reference assemblies in `reference/`, the same as the tests. No build needed.

$ARGUMENTS
