module Autograph.Core.Tests.TestHelper

open System
open System.IO
open NUnit.Framework
open Autograph.Core

let private shouldEqualWithPrepend (other : string) (this : string) =
    let this = String.Concat ("\n", this.Replace ("\r", ""))
    let other = other.Replace ("\r", "")
    Assert.AreEqual (other, this)

let options =
    let sampleBinLog = Path.Combine (__SOURCE_DIRECTORY__, "sample.binlog")
    Autograph.TypedTree.Options.mkOptions sampleBinLog

let assertSignature implementation expectedSignature =
    let actualSignature = AutographInternalApi.MkSignature (implementation, options)
    shouldEqualWithPrepend expectedSignature actualSignature

    let verificationResult =
        AutographInternalApi.VerifySignatureWithImplementation (implementation, actualSignature, options)

    match verificationResult with
    | SignatureVerificationResult.ValidSignature -> ()
    | SignatureVerificationResult.InvalidImplementationFile diagnosticInfos ->
        Array.iter (fun d -> printfn "%A" d) diagnosticInfos
        failwith "Could not compile source implementation file"
    | SignatureVerificationResult.InvalidSignatureFile diagnosticInfos ->
        Array.iter (fun d -> printfn "%A" d) diagnosticInfos
        failwith "Could not compile source implementation file with signature file"
