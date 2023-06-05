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
    | PartialSignatureFile of signature : string * TelplinError list

type MkSignature = FSharpProjectOptions -> string -> string * TelplinError list

[<RequireQualifiedAccess>]
module SignatureCreation =
    let telplin includePrivateBindings (options : FSharpProjectOptions) (implementation : string) =
        let resolver =
            TypedTree.Resolver.mkResolverForCode options includePrivateBindings implementation

        UntypedTree.Writer.mkSignatureFile resolver implementation

    let fcs (options : FSharpProjectOptions) (implementation : string) =
        match TypedTree.Resolver.FCSSignature options implementation with
        | Choice1Of2 _ -> failwith "Could not generate a signature via FCS"
        | Choice2Of2 signature -> signature, List.empty<TelplinError>

type internal TelplinInternalApi =
    static member VerifySignatureWithImplementation
        (
            implementation : string,
            options : FSharpProjectOptions,
            mkSignature : MkSignature,
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
        | Ok (signature, errors) ->

        if not errors.IsEmpty then
            SignatureVerificationResult.PartialSignatureFile (signature, errors)
        else

        Option.iter (fun assertSignature -> assertSignature signature) assertSignature

        let pairCheckDiagnostics =
            TypedTree.Resolver.typeCheckForPair options implementation signature

        if not (Array.isEmpty pairCheckDiagnostics) then
            SignatureVerificationResult.InvalidSignatureFile (signature, pairCheckDiagnostics)
        else
            SignatureVerificationResult.ValidSignature signature
