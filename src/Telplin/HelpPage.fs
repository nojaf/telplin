module Telplin.HelpPage

open System
open System.IO
open System.Reflection
open Telplin.Arguments
open Telplin.Theme

/// The version as packed, without the build metadata after the `+`.
let version () : string =
    let informational =
        Assembly.GetExecutingAssembly().GetCustomAttribute<AssemblyInformationalVersionAttribute>()

    match informational with
    | null -> "unknown"
    | attribute ->
        match attribute.InformationalVersion.Split '+' with
        | [| number ; _ |] -> number
        | _ -> attribute.InformationalVersion

/// What follows the prompt when this Telplin was started, so the examples are runnable as printed.
let invocation () : string =
    match Option.ofObj Environment.ProcessPath with
    | None -> "telplin"
    | Some path ->
        match Path.GetFileNameWithoutExtension path with
        | "" -> "telplin"
        | executable when String.Equals (executable, "dotnet", StringComparison.OrdinalIgnoreCase) -> "dotnet telplin"
        | executable -> executable

let examples : (string * string) list =
    [
        ("src/App", "Write a signature file next to every source file of the project in that folder")
        ("App.fsproj --dry-run", "Print the signatures and write nothing")
        ("App.fsproj --files Api.fs", "Process one file of the project")
        ("App.fsproj --record", "Also save the compiler arguments to App.rsp")
        ("App.rsp", "Reuse recorded arguments and skip the build")
        ("App.fsproj -- -p:Configuration=Release", "Pass arguments to the design time build")
    ]

let links : (string * string) list =
    [
        ("Learn more about Telplin:", "https://nojaf.com/telplin/docs/")
        ("Try it online:", "https://nojaf.com/telplin/")
        ("Report an issue:", "https://github.com/nojaf/telplin/issues")
    ]

// The column the right hand half of a two column row starts in.
let descriptionColumn : int = 34
let linkColumn : int = 28

let exampleColumn (invocation : string) : int =
    examples
    |> List.map (fun (arguments : string, _) -> invocation.Length + 1 + arguments.Length)
    |> List.fold max 0
    |> fun longest -> longest + 3

let writeFlag
    (write : string -> unit)
    (theme : Theme)
    (short : string, long : string, argument : string, description : string list)
    : unit
    =
    let shortPart =
        if String.IsNullOrEmpty short then
            "    "
        else
            String.Concat (flagName theme short, ", ")

    let argumentPart =
        if String.IsNullOrEmpty argument then
            ""
        else
            String.Concat (" ", placeholder theme argument)

    let left = String.Concat ("  ", shortPart, flagName theme long, argumentPart)

    match description with
    | [] -> write left
    | first :: rest ->

    writeRow write descriptionColumn left first
    List.iter (writeContinuation write descriptionColumn) rest

let writeExample
    (write : string -> unit)
    (theme : Theme)
    (invocation : string)
    (arguments : string, description : string)
    : unit
    =
    writeRow
        write
        (exampleColumn invocation)
        (String.Concat ("  ", muted theme invocation, flagName theme (String.Concat (" ", arguments))))
        description

let render (theme : Theme) (invocation : string) : string list =
    let lines = ResizeArray<string>()
    let write (line : string) = lines.Add line
    let blank () = lines.Add ""

    write (
        String.Concat (
            title theme "Telplin",
            " generates signature files for F#. ",
            muted theme (String.Concat ("(", version (), ")"))
        )
    )

    blank ()

    write (
        String.Concat (
            heading theme "Usage:",
            " ",
            heading theme invocation,
            " ",
            flagName theme "[...flags] <input> [-- ...msbuild arguments]"
        )
    )

    blank ()
    write (heading theme "Examples:")
    List.iter (writeExample write theme invocation) examples
    blank ()
    write (heading theme "Flags:")
    List.iter (writeFlag write theme) flags
    blank ()
    write (heading theme "Input:")
    write "  A project file (.fsproj), a folder holding exactly one, or a response file (.rsp)."
    write "  A project is built first, a design time build that asks MSBuild for the compiler"
    write "  arguments; anything after -- is passed to that build. A response file holds those"
    write "  arguments as saved by --record and skips the build. Signature files are written"
    write "  next to the source files, as .fsi."
    blank ()

    for label, url in links do
        writeRow write linkColumn label (link theme url)

    blank ()
    List.ofSeq lines

let print () : unit =
    render (forOutput ()) (invocation ()) |> List.iter Console.Out.WriteLine
