module Telplin.Core.Tests.OnlyUsedTests

open NUnit.Framework
open Telplin.Core
open Telplin.Core.Tests.TestHelper

/// A.fs is the implementation, B.fs is another file of the same project that uses some of it.
let private assertOnlyUsed (implementation : string) (otherFile : string) (expectedSignature : string) =
    let verificationResult =
        TelplinInternalApi.VerifySignatureWithImplementation (
            implementation,
            options,
            SignatureCreation.telplinOnlyUsed false [ "B.fs", otherFile ],
            assertSignature = shouldEqualWithPrepend expectedSignature
        )

    match verificationResult with
    | SignatureVerificationResult.ValidSignature _ -> ()
    | result -> failwithf $"Expected a valid signature, got %A{result}"

[<Test>]
let ``binding used by another file is kept, unused binding is left out`` () =
    assertOnlyUsed
        """
module A

let used (x: int) = x + 1
let unused (x: int) = x - 1
"""
        """
module B

let b () = A.used 1
"""
        """
module A

val used: x: int -> int
"""

[<Test>]
let ``binding used only inside its own file is left out`` () =
    assertOnlyUsed
        """
module A

let helper (x: int) = x * 2
let api (x: int) = helper x
"""
        """
module B

let b () = A.api 1
"""
        """
module A

val api: x: int -> int
"""

[<Test>]
let ``binding with an attribute is kept without a use`` () =
    assertOnlyUsed
        """
module A

[<Literal>]
let Name = "telplin"

let api () = Name
"""
        """
module B

let b () = A.api ()
"""
        """
module A

[<Literal>]
val Name: string = "telplin"

val api: unit -> string
"""

[<Test>]
let ``types and members are kept in full`` () =
    assertOnlyUsed
        """
module A

type Config =
    { Name: string }

    member this.Upper = this.Name.ToUpperInvariant()

let unused (c: Config) = c.Name
"""
        """
module B

let b (c: A.Config) = c.Name
"""
        """
module A

type Config =
    {
        Name: string
    }

    member Upper: string
"""

[<Test>]
let ``binding in a nested module follows the same rule`` () =
    assertOnlyUsed
        """
module A

module Nested =
    let used () = 1
    let unused () = 2
"""
        """
module B

let b () = A.Nested.used ()
"""
        """
module A

module Nested =
    val used: unit -> int
"""

[<Test>]
let ``active pattern used by another file is kept`` () =
    assertOnlyUsed
        """
module A

let (|Even|Odd|) (x: int) = if x % 2 = 0 then Even else Odd
let (|Unused|_|) (x: int) = if x = 0 then Some () else None
"""
        """
module B

let b (x: int) =
    match x with
    | A.Even -> true
    | A.Odd -> false
"""
        """
module A

val (|Even|Odd|): x: int -> Choice<unit, unit>
"""

[<Test>]
let ``binding used through an opened module is kept`` () =
    assertOnlyUsed
        """
module A

let viaOpen () = 1
let unused () = 2
"""
        """
module B

open A

let b () = viaOpen ()
"""
        """
module A

val viaOpen: unit -> int
"""

[<Test>]
let ``binding used by another file is kept when the file already has a signature`` () =
    let verificationResult =
        TelplinInternalApi.VerifySignatureWithImplementation (
            """
module A

let used (x: int) = x + 1
let unused (x: int) = x - 1
""",
            options,
            SignatureCreation.telplinOnlyUsed
                false
                [
                    "A.fsi",
                    """
module A

val used: x: int -> int
val unused: x: int -> int
"""
                    "B.fs",
                    """
module B

let b () = A.used 1
"""
                ],
            assertSignature =
                shouldEqualWithPrepend
                    """
module A

val used: x: int -> int
"""
        )

    match verificationResult with
    | SignatureVerificationResult.ValidSignature _ -> ()
    | result -> failwithf $"Expected a valid signature, got %A{result}"
