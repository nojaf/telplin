namespace Telplin.Core

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open Telplin.Core

[<RequireQualifiedAccess>]
type SignatureVerificationResult =
    | ValidSignature of signature : string
    | ImplementationFileAborted
    | FailedToCreateSignatureFile of error : string
    | InvalidImplementationFile of diagnostics : FSharpDiagnostic array
    | InvalidSignatureFile of signature : string * diagnostics : FSharpDiagnostic array

[<RequireQualifiedAccess>]
module SignatureCreation =
    let telplin includePrivateBindings (options : FSharpProjectOptions) (implementation : string) : string =
        let resolver =
            TypedTree.Resolver.mkResolverForCode options includePrivateBindings implementation

        UntypedTree.Writer.mkSignatureFile resolver implementation

    let fcs (options : FSharpProjectOptions) (implementation : string) : string =
        match TypedTree.Resolver.FCSSignature options implementation with
        | Choice1Of2 _ -> failwith "Could not generate a signature via FCS"
        | Choice2Of2 signature -> signature

type internal TelplinInternalApi =
    static member VerifySignatureWithImplementation
        (
            implementation : string,
            options : FSharpProjectOptions,
            mkSignature : FSharpProjectOptions -> string -> string,
            ?assertSignature : string -> unit
        )
        =
        let implCheckResult =
            TypedTree.Resolver.typeCheckForImplementation options implementation

        match implCheckResult with
        | Choice1Of2 _ -> SignatureVerificationResult.ImplementationFileAborted
        | Choice2Of2 implCheckDiagnostics ->

        if not (Array.isEmpty implCheckDiagnostics) then
            SignatureVerificationResult.InvalidImplementationFile implCheckDiagnostics
        else

        let signature =
            try
                mkSignature options implementation |> Result.Ok
            with ex ->
                Result.Error ex.Message

        match signature with
        | Error error -> SignatureVerificationResult.FailedToCreateSignatureFile error
        | Ok signature ->

        Option.iter (fun assertSignature -> assertSignature signature) assertSignature

        let pairCheckDiagnostics =
            TypedTree.Resolver.typeCheckForPair options implementation signature

        if not (Array.isEmpty pairCheckDiagnostics) then
            SignatureVerificationResult.InvalidSignatureFile (signature, pairCheckDiagnostics)
        else
            SignatureVerificationResult.ValidSignature signature
