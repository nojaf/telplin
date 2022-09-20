namespace Autograph.Core

open System
open System.IO
open Autograph.Common

[<RequireQualifiedAccess>]
type SignatureVerificationResult =
    | ValidSignature
    | InvalidImplementationFile of diagnostics : FSharpDiagnosticInfo array
    | InvalidSignatureFile of diagnostics : FSharpDiagnosticInfo array

type AutographApi =
    static member MkSignature (implementation : string) : string =
        let resolver = Autograph.TypedTree.Resolver.mkResolverForCode implementation
        Autograph.UntypedTree.Writer.mkSignatureFile resolver implementation

    static member VerifySignatureWithImplementation
        (
            implementation : string,
            signature : string
        )
        : SignatureVerificationResult
        =
        let randomName = Guid.NewGuid().ToString "N"
        let tempFolder = Path.Combine (Path.GetTempPath (), randomName)

        try
            let dirInfo = DirectoryInfo tempFolder
            dirInfo.Create ()
            let implPath = Path.Combine (tempFolder, "A.fs")
            File.WriteAllText (implPath, implementation)

            let implCheckDiagnostics =
                Autograph.TypedTree.Resolver.typeCheckForImplementation implPath

            if not (Array.isEmpty implCheckDiagnostics) then
                SignatureVerificationResult.InvalidImplementationFile implCheckDiagnostics
            else

            let sigFPath = Path.Combine (tempFolder, "A.fsi")
            File.WriteAllText (sigFPath, signature)

            let pairCheckDiagnostics =
                Autograph.TypedTree.Resolver.typeCheckForPair implPath sigFPath

            if not (Array.isEmpty pairCheckDiagnostics) then
                SignatureVerificationResult.InvalidSignatureFile pairCheckDiagnostics
            else
                SignatureVerificationResult.ValidSignature
        finally
            if Directory.Exists tempFolder then
                Directory.Delete (tempFolder, true)
