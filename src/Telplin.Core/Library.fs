namespace Telplin.Core

open FSharp.Compiler.CodeAnalysis
open Telplin.Common
open Telplin.TypedTree.FSharpProjectExtensions

[<RequireQualifiedAccess>]
type SignatureVerificationResult =
    | ValidSignature of signature : string
    | ImplementationFileAborted
    | FailedToCreateSignatureFile of error : string
    | InvalidImplementationFile of diagnostics : FSharpDiagnosticInfo array
    | InvalidSignatureFile of signature : string * diagnostics : FSharpDiagnosticInfo array

[<RequireQualifiedAccess>]
module SignatureCreation =
    let telplin (options : FSharpProjectOptions) (implementation : string) : string =
        let resolver = Telplin.TypedTree.Resolver.mkResolverForCode options implementation
        Telplin.UntypedTree.Writer.mkSignatureFile resolver options.Defines implementation

    let fcs (options : FSharpProjectOptions) (implementation : string) : string =
        match Telplin.TypedTree.Resolver.FCSSignature options implementation with
        | Choice1Of2 _ -> failwith "Could not generate a signature via FCS"
        | Choice2Of2 signature -> signature

type internal TelplinInternalApi =
    static member MkSignature (implementation : string, options : FSharpProjectOptions) =
        let resolver = Telplin.TypedTree.Resolver.mkResolverForCode options implementation
        Telplin.UntypedTree.Writer.mkSignatureFile resolver options.Defines implementation

    static member VerifySignatureWithImplementation
        (
            implementation : string,
            options : FSharpProjectOptions,
            mkSignature : FSharpProjectOptions -> string -> string,
            ?assertSignature : string -> unit
        )
        =
        let implCheckResult =
            Telplin.TypedTree.Resolver.typeCheckForImplementation options implementation

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
            Telplin.TypedTree.Resolver.typeCheckForPair options implementation signature

        if not (Array.isEmpty pairCheckDiagnostics) then
            SignatureVerificationResult.InvalidSignatureFile (signature, pairCheckDiagnostics)
        else
            SignatureVerificationResult.ValidSignature signature

type TelplinApi =
    static member MkSignature (implementation : string, binlog : string) : string =
        let options = Telplin.TypedTree.Options.mkOptionsFromBinaryLog binlog
        TelplinInternalApi.MkSignature (implementation, options)
