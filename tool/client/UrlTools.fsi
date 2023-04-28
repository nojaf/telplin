module OnlineTool.UrlTools

open Thoth.Json

val updateUrlWithData : json : string -> unit
val restoreModelFromUrl : decoder : Decoder<'a> -> defaultValue : 'a -> 'a
