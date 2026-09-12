#!/usr/bin/env -S dotnet fsi
// Type check F# source and print what FCS knows about every definition: the raw
// `GetValSignatureText` Telplin parses per member or binding, plus the signature FCS
// itself would generate. Use this to see what Telplin receives from the typed tree.
//
//   scripts/symbols.fsx [--define FOO,BAR] [--fcs] <file>
//   echo '<source>' | scripts/symbols.fsx
//
// --fcs  also print FSharpCheckFileResults.GenerateSignature()
#load "shared.fsx"
#load "options.fsx"

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open Shared
open Options

let run (args : Args) =
    let checker = FSharpChecker.Create ()
    let options = withDefines args.Defines projectOptions
    let sourceText = SourceText.ofString args.Source

    let _, checkAnswer =
        checker.ParseAndCheckFileInProject ("A.fs", 0, sourceText, options)
        |> Async.RunSynchronously

    match checkAnswer with
    | FSharpCheckFileAnswer.Aborted -> stdout.WriteLine "Type check aborted"
    | FSharpCheckFileAnswer.Succeeded checkResults ->

    let errors =
        checkResults.Diagnostics
        |> Array.filter (fun d -> d.Severity = FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Error)

    if not (Array.isEmpty errors) then
        stdout.WriteLine "== Diagnostics"

        for d in errors do
            printfn "%s" (d.ToString ())

        stdout.WriteLine ""

    stdout.WriteLine "== Definitions (GetValSignatureText)"

    checkResults.GetAllUsesOfAllSymbolsInFile ()
    |> Seq.filter (fun su -> su.IsFromDefinition)
    |> Seq.sortBy (fun su -> su.Range.StartLine, su.Range.StartColumn)
    |> Seq.iter (fun su ->
        let m = su.Range

        match su.Symbol with
        | :? FSharpMemberOrFunctionOrValue as mfv ->
            let sigText =
                mfv.GetValSignatureText (su.DisplayContext, m)
                |> Option.defaultValue "<no signature text>"

            printfn
                "(%d,%d) %s [CompiledName=%s]\n    %s"
                m.StartLine
                m.StartColumn
                mfv.DisplayName
                mfv.CompiledName
                sigText
        | :? FSharpEntity as entity -> printfn "(%d,%d) entity %s" m.StartLine m.StartColumn entity.DisplayName
        | other -> printfn "(%d,%d) %s %s" m.StartLine m.StartColumn (other.GetType().Name) other.DisplayName
    )

    if args.Flags.Contains "--fcs" then
        stdout.WriteLine "\n== FCS GenerateSignature"

        match checkResults.GenerateSignature () with
        | None -> stdout.WriteLine "<none>"
        | Some s -> printfn "%s" (string<ISourceText> s)

if isEntryScript __SOURCE_DIRECTORY__ __SOURCE_FILE__ then
    run (parseArgs fsi.CommandLineArgs.[1..])
