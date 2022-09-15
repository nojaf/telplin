module Autograph.Core.Tests.ExceptionTests

open NUnit.Framework
open TestHelper

[<Test>]
let ``single namespace`` () =
    mkSignature
        """
namespace A

exception MyFSharpError1 of string
exception MyFSharpError2 of string * int
"""
    |> shouldEqualWithPrepend
        """
namespace A
exception MyFSharpError1 of string
exception MyFSharpError2 of string * int
"""

[<Ignore "Revisit once SynType.SignatureParameter is available">]
[<Test>]
let ``exception with member`` () =
    mkSignature
        """
namespace A

exception MyFSharpError1 of string with member this.Test a b = a + b
"""
    |> shouldEqualWithPrepend
        """
namespace A
exception MyFSharpError1 of string with
    member Test: a:int -> b:int -> int
"""
