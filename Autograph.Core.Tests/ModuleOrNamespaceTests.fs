module Autograph.Core.Tests.ModuleOrNamespaceTests

open NUnit.Framework
open TestHelper

[<Test>]
let ``single namespace`` () =
    assertSignature
        """
namespace A

type Foo = int
"""
        """
namespace A

type Foo = int
"""

[<Test>]
let ``module abbreviation`` () =
    assertSignature
        """
namespace Root

module A =

    let a = 0

module B = A
"""
        """
namespace Root

module A =
    val a: int

module B = A
"""

[<Test>]
let ``nested module`` () =
    assertSignature
        """
namespace Company

module A =

    let a = 0
"""
        """
namespace Company

module A =
    val a: int
"""
