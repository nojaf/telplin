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
        OnlyUsed : bool
        KeepPrivate : bool
        KeepXmlDocs : bool
        Help : bool
        Version : bool
        MsBuildArguments : string list
    }

/// The flags Telplin has: short form, long form, the value that follows, and the description the
/// help page prints. The parser and the page both read this one list.
val flags : (string * string * string * string list) list
/// Parse the command line. `--` ends the Telplin flags, whatever follows is for MSBuild.
val parse : args : string array -> Result<Arguments, string>
