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

/// The text of the project with each signature file listed directly before its implementation
/// file, and what happened to each one. Nothing about the file changes apart from the inserted
/// lines: the same indentation and path separator the implementation uses, the same line ending.
val addSignatures : projectPath : string -> signaturePaths : string list -> string * Outcome list
