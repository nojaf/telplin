module Telplin.Diagnostics

open System
open Fantomas.FCS.Text
open Telplin.Theme

/// One error of Telplin: the headline and the snippet, with a blank line after it.
val report : theme : Theme -> file : string -> lines : string array -> range : range -> message : string -> string list
