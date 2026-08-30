#!/usr/bin/env -S dotnet fsi
// Argument parsing shared by the troubleshooting scripts. No package references here,
// so every script picks its own.
module Shared

open System.IO

type Args =
    {
        Source : string
        IsSignature : bool
        Defines : string list
        Flags : Set<string>
    }

/// The source is the last positional argument when that is a file, otherwise stdin.
/// Flags: --signature, --define FOO,BAR and any other --flag (collected in `Flags`).
let parseArgs (args : string array) : Args =
    let defineIdx = args |> Array.tryFindIndex (fun a -> a = "--define")

    let defines =
        match defineIdx with
        | Some idx when idx + 1 < args.Length -> args.[idx + 1].Split ',' |> Array.toList
        | _ -> []

    let skip =
        match defineIdx with
        | Some idx -> set [ idx ; idx + 1 ]
        | None -> Set.empty

    let flags =
        args
        |> Array.indexed
        |> Array.choose (fun (i, a) ->
            if a.StartsWith "--" && not (skip.Contains i) then
                Some a
            else
                None
        )
        |> Set.ofArray

    let positional =
        args
        |> Array.indexed
        |> Array.choose (fun (i, a) ->
            if a.StartsWith "--" || skip.Contains i then
                None
            else
                Some a
        )

    let hasSignatureFlag = flags.Contains "--signature"

    match Array.tryLast positional with
    | Some path when File.Exists path ->
        {
            Source = File.ReadAllText path
            IsSignature = hasSignatureFlag || path.EndsWith ".fsi"
            Defines = defines
            Flags = flags
        }
    | _ ->
        {
            Source = stdin.ReadToEnd ()
            IsSignature = hasSignatureFlag
            Defines = defines
            Flags = flags
        }

/// True when the script is the one `dotnet fsi` was started with (not a #load).
/// Call as `isEntryScript __SOURCE_DIRECTORY__ __SOURCE_FILE__`.
let isEntryScript (sourceDirectory : string) (sourceFile : string) =
    match Array.tryHead fsi.CommandLineArgs with
    | Some entry -> FileInfo(entry).FullName = FileInfo(Path.Combine (sourceDirectory, sourceFile)).FullName
    | None -> false
