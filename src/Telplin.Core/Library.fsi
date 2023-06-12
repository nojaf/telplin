namespace Telplin.Core

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics

[<RequireQualifiedAccess>]
type SignatureVerificationResult =
    | ValidSignature of signature : string
    | ImplementationFileAborted
    | FailedToCreateSignatureFile of error : string
    | InvalidImplementationFile of diagnostics : FSharpDiagnostic array
    | InvalidSignatureFile of signature : string * diagnostics : FSharpDiagnostic array
    | PartialSignatureFile of signature : string * TelplinError list

type MkSignature = FSharpProjectOptions -> string -> string * TelplinError list

[<RequireQualifiedAccess>]
module SignatureCreation =
    /// Create a signature file for the given implementation file using Telplin.
    val telplin : includePrivateBindings : bool -> MkSignature
    /// Create a signature file for the given implementation file using `FSharpCheckFileResults.GenerateSignature`.
    val fcs : MkSignature

[<Class>]
type internal TelplinInternalApi =
    static member VerifySignatureWithImplementation :
        implementation : string *
        options : FSharpProjectOptions *
        mkSignature : MkSignature *
        ?assertSignature : (string -> unit) ->
            SignatureVerificationResult
