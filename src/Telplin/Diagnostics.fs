module Telplin.Diagnostics

open System
open Fantomas.FCS.Text
open Telplin.Theme

/// How many lines of the file are shown either side of the one the carets point at.
let contextLines : int = 2

/// A tab is one column to the parser but any number of columns on screen. The line and the caret
/// indent are expanded the same way, so the carets stay under the token.
let tabStop : string = "    "

let expandTabs (line : string) : string = line.Replace ("\t", tabStop)

/// The compiler's format, so an editor or CI can jump to the line. The message is prose and stays
/// plain; the location and the severity are what a reader picks the line out by.
let headline (theme : Theme) (file : string) (range : range) (message : string) : string =
    let message = message.Replace("\r\n", " ").Replace("\n", " ")
    let location = $"%s{file}(%i{range.StartLine},%i{range.StartColumn + 1})"
    let severity = negative theme "error"
    $"%s{link theme location}: %s{severity}: %s{message}"

let caretRun (line : string) (range : range) : string * string =
    let startColumn = min range.StartColumn line.Length

    let endColumn =
        // A range that runs past its first line is underlined to the end of that line; the
        // following lines are in the snippet anyway.
        if range.EndLine = range.StartLine then
            min range.EndColumn line.Length
        else
            line.Length

    let indent = (expandTabs (line.Substring (0, startColumn))).Length

    let width =
        max 1 (expandTabs (line.Substring (startColumn, endColumn - startColumn))).Length

    String (' ', indent), String ('^', width)

/// The lines around the range, with a dimmed gutter of line numbers, and carets under the range.
/// The source between the gutters is the file's own text and is left as it is.
let snippet (theme : Theme) (lines : string array) (range : range) : string list =
    if range.StartLine < 1 || range.StartLine > lines.Length then
        []
    else

    let firstLine = max 1 (range.StartLine - contextLines)
    let lastLine = min lines.Length (range.StartLine + contextLines)
    let gutter = String.length (string<int> lastLine)
    let blankGutter = muted theme (String.Concat (String (' ', gutter), " |"))

    [
        for number in firstLine..lastLine do
            let lineNumber = (string<int> number).PadLeft gutter
            let numberedGutter = muted theme (String.Concat (lineNumber, " |"))
            yield String.Concat (numberedGutter, " ", expandTabs lines.[number - 1])

            if number = range.StartLine then
                let indent, carets = caretRun lines.[number - 1] range
                yield String.Concat (blankGutter, " ", indent, negative theme carets)
    ]

/// One error of Telplin: the headline and the snippet, with a blank line after it.
let report (theme : Theme) (file : string) (lines : string array) (range : range) (message : string) : string list =
    [
        yield headline theme file range message
        yield! snippet theme lines range
        yield ""
    ]
