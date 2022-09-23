namespace Telplin.Core

open FSharp.Compiler.CodeAnalysis
open Telplin.Common

[<RequireQualifiedAccess>]
type SignatureVerificationResult =
    | ValidSignature
    | InvalidImplementationFile of diagnostics : FSharpDiagnosticInfo array
    | InvalidSignatureFile of diagnostics : FSharpDiagnosticInfo array

[<Class>]
type TelplinApi =
    static member MkSignature : implementation : string * binlog : string -> string
    static member VerifySignatureWithImplementation :
        implementation : string * signature : string * binlog : string -> SignatureVerificationResult

[<Class>]
type internal TelplinInternalApi =
    static member MkSignature : implementation : string * options : FSharpProjectOptions -> string
    static member VerifySignatureWithImplementation :
        implementation : string * signature : string * options : FSharpProjectOptions -> SignatureVerificationResult
