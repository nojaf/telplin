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
    val telplin : includePrivateBindings : bool -> MkSignature
    val fcs : MkSignature

[<Class>]
type internal TelplinInternalApi =
    static member VerifySignatureWithImplementation :
        implementation : string *
        options : FSharpProjectOptions *
        mkSignature : MkSignature *
        ?assertSignature : (string -> unit) ->
            SignatureVerificationResult
