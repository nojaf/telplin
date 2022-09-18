module Autograph.Core.Tests.ModuleOrNamespaceTests

open NUnit.Framework
open TestHelper

[<Test>]
let ``single namespace`` () =
    mkSignature
        """
namespace A

type Foo = int
"""
    |> shouldEqualWithPrepend
        """
namespace A

type Foo = int
"""

[<Test>]
let ``module abbreviation`` () =
    mkSignature
        """
module A

let a = 0

module B = A
"""
    |> shouldEqualWithPrepend
        """
module A

val a: int
module B = A
"""

[<Test>]
let ``nested module`` () =
    mkSignature
        """
namespace Company

module A =

    let a = 0
"""
    |> shouldEqualWithPrepend
        """
namespace Company

module A =
    val a: int
"""
