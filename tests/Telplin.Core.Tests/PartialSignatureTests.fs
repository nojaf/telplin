module Telplin.Core.Tests.PartialSignatureTests

open NUnit.Framework
open TestHelper

[<Test>]
let ``a declaration that cannot be converted is left out, the rest is kept, 72`` () =
    assertPartialSignature
        2
        """
module Telplin

let a, b = 1, 2

type T() =
    member _.Y = 2

let c, d = 3, 4
"""
        """
module Telplin

type T =
    new: unit -> T
    member Y: int
"""

[<Test>]
let ``a nested module whose only declaration cannot be converted is kept, 72`` () =
    assertPartialSignature
        1
        """
module Telplin

module M =
    let a, b = 1, 2

let x = 1
"""
        """
module Telplin

module M =
    begin end

val x: int
"""
