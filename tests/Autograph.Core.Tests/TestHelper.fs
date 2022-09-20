module Autograph.Core.Tests.TestHelper

open System
open NUnit.Framework
open Autograph.Core

let private shouldEqualWithPrepend (other : string) (this : string) =
    let this = String.Concat ("\n", this.Replace ("\r", ""))
    let other = other.Replace ("\r", "")
    Assert.AreEqual (other, this)

let assertSignature implementation expectedSignature =
    let actualSignature = AutographApi.MkSignature implementation
    shouldEqualWithPrepend expectedSignature actualSignature

    let verificationResult =
        AutographApi.VerifySignatureWithImplementation (implementation, actualSignature)

    match verificationResult with
    | SignatureVerificationResult.ValidSignature -> ()
    | SignatureVerificationResult.InvalidImplementationFile diagnosticInfos ->
        Array.iter (fun d -> printfn "%A" d) diagnosticInfos
        failwith "Could not compile source implementation file"
    | SignatureVerificationResult.InvalidSignatureFile diagnosticInfos ->
        Array.iter (fun d -> printfn "%A" d) diagnosticInfos
        failwith "Could not compile source implementation file with signature file"
