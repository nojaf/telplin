namespace Autograph.Core

open System
open System.IO
open Autograph.Common
open FSharp.Compiler.CodeAnalysis

[<RequireQualifiedAccess>]
type SignatureVerificationResult =
    | ValidSignature
    | InvalidImplementationFile of diagnostics : FSharpDiagnosticInfo array
    | InvalidSignatureFile of diagnostics : FSharpDiagnosticInfo array

type internal AutographInternalApi =
    static member MkSignature (implementation : string, options : FSharpProjectOptions) =
        let resolver = Autograph.TypedTree.Resolver.mkResolverForCode options implementation
        Autograph.UntypedTree.Writer.mkSignatureFile resolver implementation

    static member VerifySignatureWithImplementation
        (
            implementation : string,
            signature : string,
            options : FSharpProjectOptions
        )
        =
        let randomName = Guid.NewGuid().ToString "N"
        let tempFolder = Path.Combine (Path.GetTempPath (), randomName)

        try
            let dirInfo = DirectoryInfo tempFolder
            dirInfo.Create ()
            let implPath = Path.Combine (tempFolder, "A.fs")
            File.WriteAllText (implPath, implementation)

            let implCheckDiagnostics =
                Autograph.TypedTree.Resolver.typeCheckForImplementation options implPath

            if not (Array.isEmpty implCheckDiagnostics) then
                SignatureVerificationResult.InvalidImplementationFile implCheckDiagnostics
            else

            let sigFPath = Path.Combine (tempFolder, "A.fsi")
            File.WriteAllText (sigFPath, signature)

            let pairCheckDiagnostics =
                Autograph.TypedTree.Resolver.typeCheckForPair options implPath sigFPath

            if not (Array.isEmpty pairCheckDiagnostics) then
                SignatureVerificationResult.InvalidSignatureFile pairCheckDiagnostics
            else
                SignatureVerificationResult.ValidSignature
        finally
            if Directory.Exists tempFolder then
                Directory.Delete (tempFolder, true)

type AutographApi =
    static member MkSignature (implementation : string, binlog : string) : string =
        let options = Autograph.TypedTree.Options.mkOptions binlog
        AutographInternalApi.MkSignature (implementation, options)

    static member VerifySignatureWithImplementation
        (
            implementation : string,
            signature : string,
            binlog : string
        )
        : SignatureVerificationResult
        =
        let options = Autograph.TypedTree.Options.mkOptions binlog
        AutographInternalApi.VerifySignatureWithImplementation (implementation, signature, options)
