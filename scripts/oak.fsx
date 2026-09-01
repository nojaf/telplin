#!/usr/bin/env -S dotnet fsi
// Print the Fantomas Oak of F# source, the tree Telplin's UntypedTree code works on.
//
//   scripts/oak.fsx [--signature] [--member] [--define FOO,BAR] <file>
//   echo '<source>' | scripts/oak.fsx [--signature] [--member]
//
// --signature  parse as a signature file (.fsi is detected from the file name)
// --member     the input is a single member signature (`static member B: int -> int`);
//              it is wrapped in `type A = ...` the way Telplin's mkMemberSigFromString does.
#r "nuget: Fantomas.Core, 8.0.0-beta-001"
#load "shared.fsx"

open Fantomas.Core
open Shared

let wrapMember (memberText : string) =
    $"""
type A =
    new: unit -> A
    %s{memberText}
"""

let parseOak (input : string) (isSignature : bool) (defines : string list) =
    async {
        try
            let! oaks = CodeFormatter.ParseOakAsync (isSignature, input)

            let result =
                if List.isEmpty defines then
                    Array.tryHead oaks
                else
                    let sorted = List.sort defines
                    oaks |> Array.tryFind (fun (_, d) -> List.sort d = sorted)

            match result with
            | None -> return "No Oak found for the given defines"
            | Some (oak, _) -> return string oak
        with ex ->
            return $"Error while parsing to Oak:\n%s{ex.Message}"
    }

if isEntryScript __SOURCE_DIRECTORY__ __SOURCE_FILE__ then
    let args = parseArgs fsi.CommandLineArgs.[1..]
    let isMember = args.Flags.Contains "--member"
    let source = if isMember then wrapMember args.Source else args.Source
    let isSignature = args.IsSignature || isMember

    parseOak source isSignature args.Defines
    |> Async.RunSynchronously
    |> printfn "%s"
