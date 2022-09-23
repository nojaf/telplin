namespace Telplin.Core

open System
open System.IO
open Telplin.Common
open FSharp.Compiler.CodeAnalysis

[<RequireQualifiedAccess>]
type SignatureVerificationResult =
    | ValidSignature
    | InvalidImplementationFile of diagnostics : FSharpDiagnosticInfo array
    | InvalidSignatureFile of diagnostics : FSharpDiagnosticInfo array

type internal TelplinInternalApi =
    static member MkSignature (implementation : string, options : FSharpProjectOptions) =
        let resolver = Telplin.TypedTree.Resolver.mkResolverForCode options implementation
        Telplin.UntypedTree.Writer.mkSignatureFile resolver implementation

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
                Telplin.TypedTree.Resolver.typeCheckForImplementation options implPath

            if not (Array.isEmpty implCheckDiagnostics) then
                SignatureVerificationResult.InvalidImplementationFile implCheckDiagnostics
            else

            let sigFPath = Path.Combine (tempFolder, "A.fsi")
            File.WriteAllText (sigFPath, signature)

            let pairCheckDiagnostics =
                Telplin.TypedTree.Resolver.typeCheckForPair options implPath sigFPath

            if not (Array.isEmpty pairCheckDiagnostics) then
                SignatureVerificationResult.InvalidSignatureFile pairCheckDiagnostics
            else
                SignatureVerificationResult.ValidSignature
        finally
            if Directory.Exists tempFolder then
                Directory.Delete (tempFolder, true)

type TelplinApi =
    static member MkSignature (implementation : string, binlog : string) : string =
        let options = Telplin.TypedTree.Options.mkOptions binlog
        TelplinInternalApi.MkSignature (implementation, options)

    static member VerifySignatureWithImplementation
        (
            implementation : string,
            signature : string,
            binlog : string
        )
        : SignatureVerificationResult
        =
        let options = Telplin.TypedTree.Options.mkOptions binlog
        TelplinInternalApi.VerifySignatureWithImplementation (implementation, signature, options)
