module Telplin.HelpPage

open System
open System.IO
open System.Reflection
open Telplin.Arguments
open Telplin.Theme

/// The version as packed, without the build metadata after the `+`.
val version : unit -> string
/// What follows the prompt when this Telplin was started, so the examples are runnable as printed.
val invocation : unit -> string
val print : unit -> unit
