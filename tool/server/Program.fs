open System.Net
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.Writers
open Suave.RequestErrors
open Suave.ServerErrors
open Telplin.Lambda.Implementation

let setCORSHeaders =
    addHeader "Access-Control-Allow-Origin" "*"
    >=> addHeader "Access-Control-Allow-Headers" "*"
    >=> addHeader "Access-Control-Allow-Methods" "*"

let textPlain = setMimeType HeaderValues.TextPlain

let mkBytes (v : string) = System.Text.Encoding.UTF8.GetBytes v

[<EntryPoint>]
let main argv =
    let port =
        match List.ofArray argv with
        | [ "--port" ; port ] -> System.UInt16.Parse port
        | _ -> 8906us

    let routes =
        [
            GET >=> path "/telplin/version" >=> textPlain >=> OK "localhost"
            POST
            >=> path "/telplin/signature"
            >=> (fun (ctx : HttpContext) ->
                async {
                    let implementation = ctx.request.rawForm |> System.Text.Encoding.UTF8.GetString
                    let isFcs = ctx.request.query |> List.exists (fun (k, _) -> k = "fcs")

                    return!
                        mkProcessRequest
                            (fun signatureContent -> (textPlain >=> OK signatureContent) ctx)
                            (fun json -> bad_request (mkBytes json) ctx)
                            (fun json -> bad_request (mkBytes json) ctx)
                            (fun error -> internal_error (mkBytes error) ctx)
                            isFcs
                            implementation
                }
            )
        ]

    setCORSHeaders
    >=> choose [ OPTIONS >=> no_content ; yield! routes ; NOT_FOUND "Not found" ]
    |> startWebServer
        { defaultConfig with
            bindings = [ HttpBinding.create HTTP IPAddress.Loopback port ]
        }

    0
