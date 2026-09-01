#!/usr/bin/env -S dotnet fsi
// Run the locally built Telplin.Core on F# source: print the signature, the Telplin errors
// and the diagnostics of compiling implementation and signature together.
// Build first: dotnet build src/Telplin.Core
//
//   scripts/telplin.fsx [--define FOO,BAR] [--no-private] [--fcs] <file>
//   echo '<source>' | scripts/telplin.fsx
//
// --no-private  leave out private bindings (the default of the CLI is to include them here)
// --fcs         use FCS GenerateSignature instead of Telplin, to compare
#r "nuget: FSharp.Compiler.Service, 43.12.400"
#r "nuget: Fantomas.Core, 8.0.0-beta-001"
#r "../artifacts/bin/Telplin.Core/debug/Telplin.Core.dll"
#load "shared.fsx"
#load "options.fsx"

open Telplin.Core
open Shared
open Options

let run (args : Args) =
    let options = withDefines args.Defines projectOptions
    let includePrivate = not (args.Flags.Contains "--no-private")

    let mkSignature : MkSignature =
        if args.Flags.Contains "--fcs" then
            SignatureCreation.fcs
        else
            SignatureCreation.telplin includePrivate

    let signature, errors = mkSignature options args.Source

    printfn "== Signature\n%s" signature

    if not (List.isEmpty errors) then
        printfn "== Telplin errors"

        for TelplinError (m, error) in errors do
            printfn "(%d,%d--%d,%d): %s" m.StartLine m.StartColumn m.EndLine m.EndColumn error

    let diagnostics = TypedTree.Resolver.typeCheckForPair options args.Source signature

    if not (Array.isEmpty diagnostics) then
        printfn "== Diagnostics of implementation + signature"

        for d in diagnostics do
            printfn "%s" (d.ToString ())
    else
        printfn "== Implementation + signature compile"

if isEntryScript __SOURCE_DIRECTORY__ __SOURCE_FILE__ then
    run (parseArgs fsi.CommandLineArgs.[1..])
