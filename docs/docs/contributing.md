---
index: 4
---
# Contribution Guidelines

## Getting started

### Recommended workflow

We recommend the following overall workflow when developing for this repository:

* Fork this repository
* Always work in your fork
* Always keep your fork up to date

Before updating your fork, run this command:

```shell
git remote add upstream https://github.com/nojaf/telplin.git
```

This will make management of multiple forks and your own work easier over time.

### Updating your fork

We recommend the following commands to update your fork:

```shell
git checkout main
git clean -xdf
git fetch upstream
git rebase upstream/main
git push
```

Or more succinctly:

```shell
git checkout main && git clean -xdf && git fetch upstream && git rebase upstream/main && git push
```

This will update your fork with the latest from `nojaf/telplin` on your machine and push those updates to your remote fork.

## Initial build

After cloning the repository, you can restore the local .NET tools:

```shell
dotnet tool restore
```

Next, you should run the `build.fsx` script.
This will build the solution, run all unit tests and do everything that the CI build does.

```shell
dotnet fsi build.fsx
```

## Troubleshooting scripts

The `scripts` folder holds F# scripts that show the intermediate steps of a signature run. They use the NuGet packages pinned in `Directory.Packages.props`, no build of Fantomas or FCS is needed. Each takes a file path or the source on stdin.

```shell
# what FCS gives Telplin per binding or member (GetValSignatureText), add --fcs for GenerateSignature
scripts/symbols.fsx input.fs
# the Fantomas Oak of the source, or of one FCS member line with --member
scripts/oak.fsx input.fs
# run the locally built Telplin.Core: signature, errors and the diagnostics of both files together
dotnet build src/Telplin.Core && scripts/telplin.fsx input.fs
```

## Documentation &amp; Online tool

You can run `dotnet fsi build.fsx -p Watch` to launch the online tool (<small>front-end &amp; back-end</small>) and the documentation.

* docs (fsdocs): [http://localhost:7890/docs/index.html](http://localhost:7890/docs/index.html)
* frontend (perla): [https://localhost:8900](https://localhost:8900)
* backend (suave): [http://localhost:8906](http://localhost:8906)

## Pull request ground rules

* Code should be formatted before commiting: `dotnet fantomas . -r`
* Your local build needs to pass: `dotnet fsi build.fsx`
* Use proper pull request titles. Not `Fixes #someNumber`!
* Always aim to introduce the least amount of changes to achieve your goal.
* Write unit tests in the same style as the existing ones. The name should start with a lowercase letter!
* Ask questions when you are unsure 😸!

<tp-nav previous="./technical-overview.html"></tp-nav>