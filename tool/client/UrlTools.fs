module OnlineTool.UrlTools

open Fable.Core.JsInterop
open Fable.Core
open Thoth.Json
open Browser
open System

let setGetParam (encodedJson : string) : unit =
    if not (isNullOrUndefined history.pushState) then
        let ``params`` = URLSearchParams.Create ()
        ``params``.set ("data", encodedJson)

        let newUrl =
            $"%s{window.location.protocol}//%s{window.location.host}%s{window.location.pathname}#%s{``params``.ToString ()}"

        history.pushState ({| path = newUrl |}, "", newUrl)

let encodeUrl (_x : string) : string =
    import "compressToEncodedURIComponent" "lz-string"

let decodeUrl (_x : string) : string =
    import "decompressFromEncodedURIComponent" "lz-string"

let updateUrlWithData json = setGetParam (encodeUrl json)

[<return : Struct>]
let (|DataValueFromHash|_|) hash =
    if String.IsNullOrWhiteSpace hash then
        ValueNone
    elif hash.StartsWith ("#data=", StringComparison.Ordinal) then
        ValueSome (hash.Substring 6)
    else
        ValueNone

let restoreModelFromUrl decoder defaultValue =
    match Dom.window.location.hash with
    | DataValueFromHash v ->
        let json = JS.decodeURIComponent v |> decodeUrl
        let modelResult = Decode.fromString decoder json

        match modelResult with
        | Result.Ok m -> m
        | Error err ->
            printfn "%A" err
            defaultValue
    | _ -> defaultValue
