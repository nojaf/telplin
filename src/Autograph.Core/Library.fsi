namespace Autograph.Core

open Autograph.Common

[<RequireQualifiedAccess>]
type SignatureVerificationResult =
    | ValidSignature
    | InvalidImplementationFile of diagnostics : FSharpDiagnosticInfo array
    | InvalidSignatureFile of diagnostics : FSharpDiagnosticInfo array

[<Class>]
type AutographApi =
    static member MkSignature : implementation : string -> string
    static member VerifySignatureWithImplementation :
        implementation : string * signature : string -> SignatureVerificationResult
