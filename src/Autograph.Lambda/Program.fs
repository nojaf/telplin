open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open System.Net
open Suave.Writers
open Suave.RequestErrors
open Suave.ServerErrors
open Autograph.Core
open Autograph.Lambda

[<RequireQualifiedAccess>]
module HeaderValues =
    [<Literal>]
    let ApplicationText = "application/text"

    [<Literal>]
    let TextPlain = "text/plain"

    [<Literal>]
    let ApplicationJson = "application/json"

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
            GET >=> path "/autograph/version" >=> textPlain >=> OK "localhost"
            POST
            >=> path "/autograph/signature"
            >=> (fun (ctx : HttpContext) ->
                async {
                    let implementation = ctx.request.rawForm |> System.Text.Encoding.UTF8.GetString

                    try
                        let signatureContent = AutographApi.MkSignature implementation

                        let verification =
                            AutographApi.VerifySignatureWithImplementation (implementation, signatureContent)

                        match verification with
                        | SignatureVerificationResult.ValidSignature -> return! (textPlain >=> OK signatureContent) ctx
                        | SignatureVerificationResult.InvalidImplementationFile diags ->
                            let json = encodeInvalidImplementationFile diags |> mkBytes
                            return! bad_request json ctx
                        | SignatureVerificationResult.InvalidSignatureFile diags ->
                            let json = encodeInvalidSignatureFile diags signatureContent |> mkBytes
                            return! bad_request json ctx

                    with ex ->
                        let body = System.Text.Encoding.UTF8.GetBytes ex.Message
                        return! internal_error body ctx
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
