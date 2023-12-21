module Telplin.Core.Tests.TestHelper

open System
open System.IO
open CliWrap
open FSharp.Compiler.CodeAnalysis
open CliWrap.Buffered
open NUnit.Framework
open Telplin.Core

let private shouldEqualWithPrepend (expected : string) (actual : string) =
    let actual = String.Concat ("\n", actual.Replace ("\r", ""))
    let expected = expected.Replace ("\r", "")
    Assert.That (actual, Is.EqualTo expected)

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

let assertSignatureWith
    (optionsBuilder : FSharpProjectOptions -> FSharpProjectOptions)
    (includePrivateBindings : bool)
    implementation
    expectedSignature
    =
    let verificationResult =
        TelplinInternalApi.VerifySignatureWithImplementation (
            implementation,
            optionsBuilder options,
            SignatureCreation.telplin includePrivateBindings,
            assertSignature = shouldEqualWithPrepend expectedSignature
        )

    match verificationResult with
    | SignatureVerificationResult.ValidSignature _ -> ()
    | SignatureVerificationResult.ImplementationFileAborted ->
        failwith "The implementation file could not be type checked."
    | SignatureVerificationResult.FailedToCreateSignatureFile error ->
        failwith $"Internal error when creating signature:\n%s{error}"
    | SignatureVerificationResult.InvalidImplementationFile diagnosticInfos ->
        Array.iter (fun d -> printfn "%A" d) diagnosticInfos
        failwith "Could not compile source implementation file"
    | SignatureVerificationResult.InvalidSignatureFile (_, diagnosticInfos) ->
        Array.iter (fun d -> printfn "%A" d) diagnosticInfos
        failwith "Could not compile source implementation file with signature file"
    | SignatureVerificationResult.PartialSignatureFile (signature, telplinErrors) ->
        ignore signature

        for TelplinError (m, error) in telplinErrors do
            printfn $"%A{m}: %s{error}"

        failwith "Only a partial signature file was created."

let assertSignature implementation expectedSignature =
    assertSignatureWith id true implementation expectedSignature

/// Downloads a nuget package and returns the dll for the selected target framework.
let downloadNugetPackage packageName version targetFramework =
    task {
        let tempDir =
            Path.Combine (Path.GetTempPath (), Guid.NewGuid().ToString "N") |> DirectoryInfo

        try
            tempDir.Create ()

            let! _ =
                Cli
                    .Wrap("dotnet")
                    .WithArguments("new classlib -f netstandard2.0")
                    .WithWorkingDirectory(tempDir.FullName)
                    .ExecuteAsync ()

            let! _ =
                Cli
                    .Wrap("dotnet")
                    .WithArguments($"add package %s{packageName} --version %s{version}")
                    .WithWorkingDirectory(tempDir.FullName)
                    .ExecuteAsync ()

            let! caches = Cli.Wrap("dotnet").WithArguments("nuget locals all -l").ExecuteBufferedAsync ()

            let caches =
                caches.StandardOutput.Split '\n'
                |> Array.choose (fun line ->
                    if not (line.Contains ": ") then
                        None
                    else
                        line.Split(": ").[1].Trim () |> Some
                )

            let nugetFolder =
                caches
                |> Array.tryPick (fun cacheFolder ->
                    let nugetPackageFolder =
                        Path.Combine (cacheFolder, packageName.ToLower (), version) |> DirectoryInfo

                    if nugetPackageFolder.Exists then
                        Some nugetPackageFolder
                    else
                        None
                )

            match nugetFolder with
            | None -> return failwith $"Failed to download %s{packageName}, %s{version}"
            | Some nugetFolder ->
                let assemblyFolder = Path.Combine (nugetFolder.FullName, "lib", targetFramework)

                if not (Path.Exists assemblyFolder) then
                    return failwith $"%s{packageName} does not contains %s{targetFramework}"
                else

                match Directory.EnumerateFiles (assemblyFolder, "*.dll") |> Seq.tryHead with
                | None -> return failwith $"No assembly found in %s{assemblyFolder}"
                | Some dll -> return dll

        finally
            if tempDir.Exists then
                tempDir.Delete true
    }
