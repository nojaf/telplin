---
description: Investigate and fix a Telplin signature generation issue from GitHub
---

The input is a Telplin GitHub issue URL or number (e.g. https://github.com/nojaf/telplin/issues/348).

Follow these steps in order.

## 1. Fetch the issue

`gh issue view <number> --repo nojaf/telplin --json title,body`. Issues from the online tool have an
`#### Implementation` block (the input), a `#### Signature` block (what Telplin produced) and a
`#### Problem description`. Extract the implementation code.

## 2. Reproduce

Write the implementation to a temp file and run `/telplin` on it. Confirm the problem: an error in
`== Telplin errors`, a wrong signature, or diagnostics when implementation and signature are compiled together.
Trim the example down to the smallest input that still shows it.

## 3. Add a failing test

Tests live in `tests/Telplin.Core.Tests/`, grouped by concept (`TypeTests.fs`, `BindingTests.fs`,
`ModuleOrNamespaceTests.fs`, ...). Grep for a related existing test and add the new one next to it.
Use `assertSignature implementation expectedSignature`; the expected signature is verified by compiling it
together with the implementation, so it has to be correct, not just what is printed today.

Naming: lowercase start, issue number at the end after a comma: `` let ``my description, 348`` () = ``.

Run only that test and assert it fails: `dotnet test tests/Telplin.Core.Tests --filter "FullyQualifiedName~348"`.

## 4. Find the root cause

Telplin works in two halves, and the failing one decides where to look:

- **Typed tree** (`src/Telplin.Core/TypedTree/TypedTree.fs`): FCS gives a signature string per binding or
  member via `GetValSignatureText`. Run `/symbols` on the input to see these strings exactly as Telplin
  gets them. `Could not parse` errors nearly always mean this string is not valid F# (keywords, generated
  names, odd attribute arguments).
- **Untyped tree** (`src/Telplin.Core/UntypedTree/UntypedTree.fs` and `ASTCreation.fs`): the FCS string is
  parsed with Fantomas into an Oak and merged with the Oak of the source (attributes, parameter names,
  xml docs, accessibility come from the source). Run `/oak` on the source, and `/oak --member` on the FCS
  string, to see both trees. `sanitizeReturnType` is where parameters of both trees are zipped.
- Wrong output that compiles is usually the merge; output that does not compile is often a missing
  attribute or constraint in the typed tree half.

## 5. Fix

Keep the change targeted. If the FCS text only needs to be parseable because the source Oak wins in the
end, sanitize the text (`sanitizeSignatureText`) rather than teaching the parser new syntax.
Run the new test, then the whole suite: `dotnet test tests/Telplin.Core.Tests`.

## 6. Changelog

Add an entry under `## [Unreleased]` in `CHANGELOG.md`, creating that section at the top when it is
missing. Never add to a released version. Fixes go under `### Fixed` with a link:
`* <what no longer goes wrong>. [#348](https://github.com/nojaf/telplin/issues/348)`

## 7. Format

Run `dotnet fantomas --check src tests scripts` and fix what it reports.
