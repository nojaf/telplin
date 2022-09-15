module Autograph.Core.Tests.TestHelper

open System
open NUnit.Framework

let shouldEqualWithPrepend (other : string) (this : string) =
    let this = String.Concat ("\n", this.Replace ("\r", ""))
    let other = other.Replace ("\r", "")
    Assert.AreEqual (other, this)

let mkSignature code =
    let resolver = Autograph.TypedTree.Resolver.mkResolverForCode code
    Autograph.UntypedTree.Writer.mkSignatureFile resolver code
