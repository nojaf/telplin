module Autograph.Lambda

open Thoth.Json.Net
open Autograph.Common

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
