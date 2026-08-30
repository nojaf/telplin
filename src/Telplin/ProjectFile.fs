module Telplin.ProjectFile

open System
open System.IO
open System.Text.RegularExpressions

/// What happened to one signature file when the project was updated.
[<RequireQualifiedAccess ; Struct>]
type Outcome =
    /// A `<Compile>` item was inserted directly before the implementation file.
    | Added of signaturePath : string
    /// The project already lists the signature file.
    | AlreadyListed of signaturePath : string
    /// The implementation file is not listed literally, a glob or an import brings it in, so
    /// there is no line to insert before. The user has to place it.
    | NotFound of signaturePath : string

// The opening tag of a Compile item, on one line, however it is closed. What is captured is the
// indentation, so the new line matches it, and the path, so it can be resolved.
let private compileItem : Regex =
    Regex ("""^(?<indent>[ \t]*)<Compile\s+Include\s*=\s*"(?<path>[^"]+)"[^>]*>""", RegexOptions.Compiled)

/// The full path a Compile item names, resolved against the folder of the project.
let private resolve (projectDirectory : string) (includePath : string) : string =
    Path.GetFullPath (Path.Combine (projectDirectory, includePath.Replace ('\\', Path.DirectorySeparatorChar)))

/// Whether an Include is one literal file rather than a pattern MSBuild expands.
let private isLiteral (includePath : string) : bool =
    not (
        includePath.Contains '*'
        || includePath.Contains '?'
        || includePath.Contains "$("
    )

/// The text of the project with each signature file listed directly before its implementation
/// file, and what happened to each one. Nothing about the file changes apart from the inserted
/// lines: the same indentation and path separator the implementation uses, the same line ending.
let addSignatures (projectPath : string) (signaturePaths : string list) : string * Outcome list =
    let projectDirectory = Path.GetDirectoryName (Path.GetFullPath projectPath)
    let text = File.ReadAllText projectPath
    let newline = if text.Contains "\r\n" then "\r\n" else "\n"
    let lines = ResizeArray (text.Split newline)

    let listed (path : string) : bool =
        lines
        |> Seq.exists (fun line ->
            let m = compileItem.Match line

            m.Success
            && isLiteral m.Groups.["path"].Value
            && resolve projectDirectory m.Groups.["path"].Value = path
        )

    let outcomes =
        signaturePaths
        |> List.map (fun signaturePath ->
            let signaturePath = Path.GetFullPath signaturePath
            let implementationPath = Path.ChangeExtension (signaturePath, ".fs")

            if listed signaturePath then
                Outcome.AlreadyListed signaturePath
            else

            let implementationLine =
                lines
                |> Seq.tryFindIndex (fun line ->
                    let m = compileItem.Match line

                    m.Success
                    && isLiteral m.Groups.["path"].Value
                    && resolve projectDirectory m.Groups.["path"].Value = implementationPath
                )

            match implementationLine with
            | None -> Outcome.NotFound signaturePath
            | Some index ->
                let m = compileItem.Match lines.[index]
                let indent = m.Groups.["indent"].Value
                let implementationInclude = m.Groups.["path"].Value
                // `Sub\File.fs` becomes `Sub\File.fsi`, whichever separator the project uses.
                let signatureInclude = Path.ChangeExtension (implementationInclude, ".fsi")
                lines.Insert (index, $"%s{indent}<Compile Include=\"%s{signatureInclude}\" />")
                Outcome.Added signaturePath
        )

    String.Join (newline, lines), outcomes
