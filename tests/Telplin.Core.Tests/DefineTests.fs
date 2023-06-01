module Telplin.Core.Tests.DefineTests

open NUnit.Framework
open TestHelper

// The conditional directive not be present in the outcome is an acceptable limitation for now.
[<Test>]
let ``DEBUG define should be used for parsing`` () =
    assertSignatureWith
        (fun opt ->
            { opt with
                OtherOptions = [| yield! opt.OtherOptions ; "--define:DEBUG" |]
            }
        )
        true
        """
module Telplin

#if DEBUG
    let x = 1
#endif
"""
        """
module Telplin

val x: int
"""
