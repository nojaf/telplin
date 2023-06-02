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

[<RequireQualifiedAccess>]
module SignatureCreation =
    val telplin : includePrivateBindings : bool -> options : FSharpProjectOptions -> implementation : string -> string
    val fcs : options : FSharpProjectOptions -> implementation : string -> string

[<Class>]
type internal TelplinInternalApi =
    static member VerifySignatureWithImplementation :
        implementation : string *
        options : FSharpProjectOptions *
        mkSignature : (FSharpProjectOptions -> string -> string) *
        ?assertSignature : (string -> unit) ->
            SignatureVerificationResult
