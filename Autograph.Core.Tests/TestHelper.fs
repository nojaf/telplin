module Autograph.Core.Tests.TestHelper

open System
open System.IO
open NUnit.Framework

let private shouldEqualWithPrepend (other : string) (this : string) =
    let this = String.Concat ("\n", this.Replace ("\r", ""))
    let other = other.Replace ("\r", "")
    Assert.AreEqual (other, this)

let private mkSignature code =
    let resolver = Autograph.TypedTree.Resolver.mkResolverForCode code
    Autograph.UntypedTree.Writer.mkSignatureFile resolver code

let assertSignature implementation expectedSignature =
    let actualSignature = mkSignature implementation
    shouldEqualWithPrepend expectedSignature actualSignature

    let randomName = Guid.NewGuid().ToString "N"
    let tempFolder = Path.Combine (Path.GetTempPath (), randomName)

    try
        let dirInfo = DirectoryInfo tempFolder
        dirInfo.Create ()
        let implPath = Path.Combine (tempFolder, "A.fs")
        File.WriteAllText (implPath, implementation)
        let sigFPath = Path.Combine (tempFolder, "A.fsi")
        File.WriteAllText (sigFPath, actualSignature)
        Autograph.TypedTree.Resolver.assertTypeCheckFor implPath sigFPath
    finally
        if Directory.Exists tempFolder then
            Directory.Delete (tempFolder, true)
