module Telplin.Theme

open System
open System.Text.RegularExpressions

[<RequireQualifiedAccess ; Struct>]
type Palette =
    | NoColour
    | Ansi

[<Struct>]
type Theme = { Palette : Palette }

// Select graphic rendition sequences, so a decorated string can still be measured.
let private escapeSequence : Regex =
    Regex ("\u001b\\[[0-9;]*m", RegexOptions.Compiled)

let plain : Theme = { Palette = Palette.NoColour }

/// Colour only when the stream is a terminal that can show it. NO_COLOR is honoured
/// (https://no-color.org), and a redirected stream is being read by a file, a pager or a script,
/// none of which want escape codes.
let forOutput () : Theme =
    let noColor = Environment.GetEnvironmentVariable "NO_COLOR"
    let term = Environment.GetEnvironmentVariable "TERM"

    if
        Console.IsOutputRedirected
        || not (String.IsNullOrEmpty noColor)
        || String.Equals (term, "dumb", StringComparison.OrdinalIgnoreCase)
    then
        plain
    else
        { Palette = Palette.Ansi }

let private decorate (theme : Theme) (code : string) (text : string) : string =
    match theme.Palette with
    | Palette.NoColour -> text
    | Palette.Ansi -> String.Concat ("\u001b[", code, "m", text, "\u001b[0m")

// 38;5;179 is the closest 256 colour to the gold the website uses (#d4ad42), and 221 is the
// lighter gold of its hover state.
let title (theme : Theme) (text : string) : string = decorate theme "1;38;5;179" text
let link (theme : Theme) (text : string) : string = decorate theme "38;5;179" text
let heading (theme : Theme) (text : string) : string = decorate theme "1" text
let flagName (theme : Theme) (text : string) : string = decorate theme "1;38;5;221" text
let placeholder (theme : Theme) (text : string) : string = decorate theme "38;5;245" text
let muted (theme : Theme) (text : string) : string = decorate theme "2" text
let negative (theme : Theme) (text : string) : string = decorate theme "31" text

let visibleLength (text : string) : int = escapeSequence.Replace(text, "").Length

let writeRow (write : string -> unit) (column : int) (left : string) (right : string) : unit =
    let padding = String (' ', max 1 (column - visibleLength left))
    write (String.Concat (left, padding, right))

let writeContinuation (write : string -> unit) (column : int) (right : string) : unit =
    write (String.Concat (String (' ', column), right))
