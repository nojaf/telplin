module Autograph.Lambda.Implementation

open FSharp.Compiler.CodeAnalysis
open Amazon.Lambda.APIGatewayEvents
open Amazon.Lambda.Core

[<RequireQualifiedAccess>]
module HeaderValues =
    [<Literal>]
    val ApplicationText : string = "application/text"

    [<Literal>]
    val TextPlain : string = "text/plain"

    [<Literal>]
    val ApplicationJson : string = "application/json"

val mkProcessRequest<'t> :
    onValidSignature : (string -> 't) ->
    onInvalidImplementationFile : (string -> 't) ->
    onInvalidSignatureFile : (string -> 't) ->
    onInternalError : (string -> 't) ->
    projectOptions : FSharpProjectOptions ->
    implementation : string ->
        't

val PostSignature : request : APIGatewayProxyRequest -> _context : ILambdaContext -> APIGatewayProxyResponse
