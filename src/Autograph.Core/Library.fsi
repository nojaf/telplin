namespace Autograph.Core

open FSharp.Compiler.CodeAnalysis
open Autograph.Common

[<RequireQualifiedAccess>]
type SignatureVerificationResult =
    | ValidSignature
    | InvalidImplementationFile of diagnostics : FSharpDiagnosticInfo array
    | InvalidSignatureFile of diagnostics : FSharpDiagnosticInfo array

[<Class>]
type AutographApi =
    static member MkSignature : implementation : string * binlog : string -> string
    static member VerifySignatureWithImplementation :
        implementation : string * signature : string * binlog : string -> SignatureVerificationResult

[<Class>]
type internal AutographInternalApi =
    static member MkSignature : implementation : string * options : FSharpProjectOptions -> string
    static member VerifySignatureWithImplementation :
        implementation : string * signature : string * options : FSharpProjectOptions -> SignatureVerificationResult
