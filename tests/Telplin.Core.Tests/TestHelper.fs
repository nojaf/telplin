module Telplin.Core.Tests.TestHelper

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open NUnit.Framework
open Telplin.Core

let private shouldEqualWithPrepend (other : string) (this : string) =
    let this = String.Concat ("\n", this.Replace ("\r", ""))
    let other = other.Replace ("\r", "")
    Assert.AreEqual (other, this)

let options : FSharpProjectOptions =
    let resolvedAssemblies =
        Path.Combine (__SOURCE_DIRECTORY__, "..", "..", "reference")
        |> Directory.EnumerateFiles
        |> Seq.map (sprintf "-r:%s")
        |> Seq.toArray

    {
        ProjectFileName = "A"
        ProjectId = None
        SourceFiles = [| "A.fs" |]
        OtherOptions =
            [|
                "-g"
                "--debug:portable"
                "--noframework"
                "--define:TRACE"
                "--define:DEBUG"
                "--define:NET"
                "--define:NET7_0"
                "--define:NETCOREAPP"
                "--define:NET5_0_OR_GREATER"
                "--define:NET6_0_OR_GREATER"
                "--define:NET7_0_OR_GREATER"
                "--define:NETCOREAPP1_0_OR_GREATER"
                "--define:NETCOREAPP1_1_OR_GREATER"
                "--define:NETCOREAPP2_0_OR_GREATER"
                "--define:NETCOREAPP2_1_OR_GREATER"
                "--define:NETCOREAPP2_2_OR_GREATER"
                "--define:NETCOREAPP3_0_OR_GREATER"
                "--define:NETCOREAPP3_1_OR_GREATER"
                "--optimize-"
                "--tailcalls-"
                yield! resolvedAssemblies
                "--target:library"
                "--nowarn:IL2121"
                "--warn:3"
                "--warnaserror:3239,FS0025"
                "--fullpaths"
                "--flaterrors"
                "--highentropyva+"
                "--targetprofile:netcore"
                "--nocopyfsharpcore"
                "--deterministic+"
                "--simpleresolution"
            |]
        ReferencedProjects = [||]
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = false
        LoadTime = DateTime.UtcNow
        UnresolvedReferences = None
        OriginalLoadReferences = []
        Stamp = None
    }

let assertSignature implementation expectedSignature =
    let verificationResult =
        TelplinInternalApi.VerifySignatureWithImplementation (
            implementation,
            options,
            assertSignature = shouldEqualWithPrepend expectedSignature
        )

    match verificationResult with
    | SignatureVerificationResult.ValidSignature _ -> ()
    | SignatureVerificationResult.ImplementationFileAborted ->
        failwith "The implementation file could not be type checked."
    | SignatureVerificationResult.FailedToCreateSignatureFile error ->
        failwith $"Internal error when creating signature:\n{error}"
    | SignatureVerificationResult.InvalidImplementationFile diagnosticInfos ->
        Array.iter (fun d -> printfn "%A" d) diagnosticInfos
        failwith "Could not compile source implementation file"
    | SignatureVerificationResult.InvalidSignatureFile (_, diagnosticInfos) ->
        Array.iter (fun d -> printfn "%A" d) diagnosticInfos
        failwith "Could not compile source implementation file with signature file"
