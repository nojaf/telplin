module Telplin.Core.Tests.IncludePrivateBindingsTests

open NUnit.Framework
open TestHelper

[<Test>]
let ``private let binding is excluded`` () =
    assertSignatureWith
        id
        false
        """
module Telplin

let private a (b:string) : string = b
"""
        """
module Telplin
"""
