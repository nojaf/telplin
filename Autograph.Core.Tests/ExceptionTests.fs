module Autograph.Core.Tests.ExceptionTests

open NUnit.Framework
open TestHelper

[<Test>]
let ``single namespace`` () =
    assertSignature
        """
namespace A

exception MyFSharpError1 of string
exception MyFSharpError2 of string * int
"""
        """
namespace A

exception MyFSharpError1 of string
exception MyFSharpError2 of string * int
"""

[<Test>]
let ``exception with member`` () =
    assertSignature
        """
namespace A

exception MyFSharpError1 of string with member this.Test a b = a + b
"""
        """
namespace A

exception MyFSharpError1 of string with
    member Test: a: int -> b: int -> int
"""
