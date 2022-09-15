module Autograph.Core.Tests.TypeTests

open NUnit.Framework
open TestHelper

[<Test>]
let ``simple record`` () =
    mkSignature
        """
module A

type B = {
    Foo: string
    Bar: int
    SomethingElseButSomewhatLonger: float
}
"""
    |> shouldEqualWithPrepend
        """
module A

type B =
    { Foo: string
      Bar: int
      SomethingElseButSomewhatLonger: float }
"""
