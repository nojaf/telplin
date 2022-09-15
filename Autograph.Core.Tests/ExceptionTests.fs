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
