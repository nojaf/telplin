module Telplin.Arguments

open System

/// What a run was asked to do. Everything after `--` is handed to the design time build untouched.
type Arguments =
    {
        Input : string option
        Files : string list
        DryRun : bool
        Record : bool
        OnlyRecord : bool
        IncludePrivateBindings : bool
        Verify : bool
        Force : bool
        UpdateProject : bool
        Help : bool
        Version : bool
        MsBuildArguments : string list
    }

let empty : Arguments =
    {
        Input = None
        Files = []
        DryRun = false
        Record = false
        OnlyRecord = false
        IncludePrivateBindings = false
        Verify = true
        Force = false
        UpdateProject = true
        Help = false
        Version = false
        MsBuildArguments = []
    }

/// The flags Telplin has: short form, long form, the value that follows, and the description the
/// help page prints. The parser and the page both read this one list.
let flags : (string * string * string * string list) list =
    [
        ("",
         "--files",
         "<paths>",
         [
             "Process only these files of the project. Every source file"
             "is processed when this is not given."
         ])
        ("", "--dry-run", "", [ "Print the signatures to the console and write nothing." ])
        ("",
         "--record",
         "",
         [
             "Also save the compiler arguments of the design time build"
             "next to the project, as a .rsp file. Passing that file as the"
             "input later skips the build."
         ])
        ("",
         "--only-record",
         "",
         [
             "Save the compiler arguments as with --record, and stop there."
             "No signature is generated."
         ])
        ("", "--include-private-bindings", "", [ "Include private bindings in the signature file." ])
        ("",
         "--no-verify",
         "",
         [
             "Skip the check that the project still compiles with the new"
             "signatures in place. Files are written without it."
         ])
        ("",
         "--no-project",
         "",
         [
             "Do not list the signature files in the project file. By"
             "default each one is added directly before its implementation"
             "file, when the input is a project."
         ])
        ("",
         "--force",
         "",
         [
             "Write the signatures even when that check fails."
             "For debugging purposes only."
         ])
        ("", "--version", "", [ "Print the version and exit." ])
        ("-h", "--help", "", [ "Display this page and exit." ])
    ]

let private isFlag (token : string) : bool =
    token.StartsWith ("-", StringComparison.Ordinal) && token <> "-"

/// The edit distance between two flags, to offer the one that was probably meant.
let private distance (a : string) (b : string) : int =
    let d = Array2D.zeroCreate (a.Length + 1) (b.Length + 1)

    for i in 0 .. a.Length do
        d.[i, 0] <- i

    for j in 0 .. b.Length do
        d.[0, j] <- j

    for i in 1 .. a.Length do
        for j in 1 .. b.Length do
            let cost = if a.[i - 1] = b.[j - 1] then 0 else 1
            d.[i, j] <- min (min (d.[i - 1, j] + 1) (d.[i, j - 1] + 1)) (d.[i - 1, j - 1] + cost)

    d.[a.Length, b.Length]

let suggestion (token : string) : string option =
    flags
    |> List.choose (fun (_, long, _, _) ->
        let cost = distance (token.ToLowerInvariant ()) long
        if cost <= 3 then Some (long, cost) else None
    )
    |> List.sortBy snd
    |> List.tryHead
    |> Option.map fst

/// Parse the command line. `--` ends the Telplin flags, whatever follows is for MSBuild.
let parse (args : string array) : Result<Arguments, string> =
    let own, msbuild =
        match Array.tryFindIndex (fun arg -> arg = "--") args with
        | None -> List.ofArray args, []
        | Some idx -> List.ofArray args.[.. idx - 1], List.ofArray args.[idx + 1 ..]

    let rec go (arguments : Arguments) (tokens : string list) : Result<Arguments, string> =
        match tokens with
        | [] -> Ok arguments
        | ("-h" | "--help") :: rest -> go { arguments with Help = true } rest
        | "--version" :: rest -> go { arguments with Version = true } rest
        | "--dry-run" :: rest -> go { arguments with DryRun = true } rest
        | "--record" :: rest -> go { arguments with Record = true } rest
        | "--only-record" :: rest -> go { arguments with OnlyRecord = true } rest
        | "--no-verify" :: rest -> go { arguments with Verify = false } rest
        | "--force" :: rest -> go { arguments with Force = true } rest
        | "--no-project" :: rest -> go { arguments with UpdateProject = false } rest
        | "--include-private-bindings" :: rest ->
            go
                { arguments with
                    IncludePrivateBindings = true
                }
                rest
        | "--files" :: rest ->
            // The run of paths directly after the flag, up to the next flag.
            let files = List.takeWhile (fun (token : string) -> not (isFlag token)) rest

            match files with
            | [] -> Error "--files needs at least one path."
            | _ ->
                go
                    { arguments with
                        Files = arguments.Files @ files
                    }
                    (List.skip files.Length rest)
        | token :: _ when isFlag token ->
            match suggestion token with
            | Some flag -> Error $"'%s{token}' is not a Telplin flag. Did you mean '%s{flag}'?"
            | None -> Error $"'%s{token}' is not a Telplin flag."
        | path :: rest ->
            match arguments.Input with
            | None -> go { arguments with Input = Some path } rest
            | Some existing ->
                Error
                    $"Telplin takes one input, and got both '%s{existing}' and '%s{path}'. Use --files to name files inside the project."

    go
        { empty with
            MsBuildArguments = msbuild
        }
        own
