namespace Telplin.Core

open FSharp.Compiler.CodeAnalysis
open Telplin.Common

[<RequireQualifiedAccess>]
type SignatureVerificationResult =
    | ValidSignature of signature : string
    | ImplementationFileAborted
    | FailedToCreateSignatureFile of error : string
    | InvalidImplementationFile of diagnostics : FSharpDiagnosticInfo array
    | InvalidSignatureFile of signature : string * diagnostics : FSharpDiagnosticInfo array

[<Class>]
type internal TelplinInternalApi =
    static member VerifySignatureWithImplementation :
        implementation : string * options : FSharpProjectOptions * ?assertSignature : (string -> unit) ->
            SignatureVerificationResult

[<Class>]
type TelplinApi =
    static member MkSignature : implementation : string * binlog : string -> string
