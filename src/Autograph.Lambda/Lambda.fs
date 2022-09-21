module Autograph.Lambda

open System.Collections.Generic
open System.Net
open Microsoft.Net.Http.Headers
open Thoth.Json.Net
open Amazon.Lambda.APIGatewayEvents
open Amazon.Lambda.Core
open Autograph.Common
open Autograph.Core

[<RequireQualifiedAccess>]
module HeaderValues =
    [<Literal>]
    let ApplicationText = "application/text"

    [<Literal>]
    let TextPlain = "text/plain"

    [<Literal>]
    let ApplicationJson = "application/json"

module Encoding =
    let encodeRange (r : RangeProxy) =
        Encode.object
            [
                "startLine", Encode.int r.StartLine
                "startColumn", Encode.int r.StartColumn
                "endLine", Encode.int r.EndLine
                "endColumn", Encode.int r.EndColumn
            ]

    let encodeDiagnostic (d : FSharpDiagnosticInfo) =
        Encode.object
            [
                "severity",
                Encode.string (
                    match d.Severity with
                    | FSharpDiagnosticInfoSeverity.Warning -> "warning"
                    | FSharpDiagnosticInfoSeverity.Error -> "error"
                    | _ -> "unknown"
                )
                "message", Encode.string d.Message
                "errorNumber", Encode.string d.ErrorNumber
                "range", encodeRange d.Range
            ]

    let encodeInvalidImplementationFile (diagnostics : FSharpDiagnosticInfo array) =
        Encode.object
            [
                "type", Encode.string "invalidImplementationFile"
                "diagnostics", Encode.array (Array.map encodeDiagnostic diagnostics)
            ]
        |> Encode.toString 4

    let encodeInvalidSignatureFile (diagnostics : FSharpDiagnosticInfo array) signatureContent =
        Encode.object
            [
                "type", Encode.string "invalidSignatureFile"
                "diagnostics", Encode.array (Array.map encodeDiagnostic diagnostics)
                "signature", Encode.string signatureContent
            ]
        |> Encode.toString 4

// Assembly attribute to enable the Lambda function's JSON input to be converted into a .NET class.
[<assembly : LambdaSerializer(typeof<Amazon.Lambda.Serialization.SystemTextJson.DefaultLambdaJsonSerializer>)>]
()

let mkProcessRequest<'t>
    (onValidSignature : string -> 't)
    (onInvalidImplementationFile : string -> 't)
    (onInvalidSignatureFile : string -> 't)
    (onInternalError : string -> 't)
    implementation
    =
    try
        let binlog : string = failwith "binlog file"
        let signatureContent = AutographApi.MkSignature (implementation, binlog)

        let verification =
            AutographApi.VerifySignatureWithImplementation (implementation, signatureContent, binlog)

        match verification with
        | SignatureVerificationResult.ValidSignature -> onValidSignature signatureContent
        | SignatureVerificationResult.InvalidImplementationFile diags ->
            onInvalidImplementationFile (Encoding.encodeInvalidImplementationFile diags)
        | SignatureVerificationResult.InvalidSignatureFile diags ->
            onInvalidSignatureFile (Encoding.encodeInvalidSignatureFile diags signatureContent)
    with ex ->
        onInternalError ex.Message

let createHeaders headers =
    Seq.fold
        (fun (acc : Dictionary<string, string>) (key, value) ->
            acc.[key] <- value
            acc
        )
        (Dictionary<string, string> ())
        headers

let mkAPIGatewayProxyResponse (statusCode : HttpStatusCode, contentTypeHeaderValue : string, body : string) =
    APIGatewayProxyResponse (
        StatusCode = int statusCode,
        Body = body,
        Headers = createHeaders [ HeaderNames.ContentType, contentTypeHeaderValue ]
    )

let PostSignature (request : APIGatewayProxyRequest) (_context : ILambdaContext) =
    mkProcessRequest
        (fun signature -> mkAPIGatewayProxyResponse (HttpStatusCode.OK, HeaderValues.ApplicationText, signature))
        (fun json -> mkAPIGatewayProxyResponse (HttpStatusCode.BadRequest, HeaderValues.ApplicationJson, json))
        (fun json -> mkAPIGatewayProxyResponse (HttpStatusCode.BadRequest, HeaderValues.ApplicationJson, json))
        (fun error -> mkAPIGatewayProxyResponse (HttpStatusCode.InternalServerError, HeaderValues.TextPlain, error))
        request.Body
