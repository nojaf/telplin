module Autograph.Core.Tests.BindingTests

open NUnit.Framework
open TestHelper

[<Test>]
let ``initial test`` () =
    mkSignature
        """
module A

let a b = b + 1
"""
    |> shouldEqualWithPrepend
        """
module A

val a: b: int -> int
"""

[<Test>]
let ``simple value`` () =
    mkSignature
        """
module A

let a = 1
"""
    |> shouldEqualWithPrepend
        """
module A

val a: int
"""

[<Test>]
let ``tupled argument`` () =
    mkSignature
        """
module A

let a (b: int, c) = printfn "%s" c ; b
"""
    |> shouldEqualWithPrepend
        """
module A

val a: b: int * c: string -> int
"""

[<Test>]
let ``function return type`` () =
    mkSignature
        """
module A

let a (b:string) : int -> int = failwith "not implemented"
"""
    |> shouldEqualWithPrepend
        """
module A

val a: b: string -> (int -> int)
"""

[<Test>]
let ``re-use existing type information is present`` () =
    mkSignature
        """
module A

let a (b:string list) : seq<int> = failwith "not implemented"
"""
    |> shouldEqualWithPrepend
        """
module A

val a: b: string list -> seq<int>
"""

// See: https://github.com/fsprojects/fantomas/issues/2502
[<Test>]
let ``open statements are re-used from source`` () =
    mkSignature
        """
module A

open System
open System.Collections

let a = DateTime.Now
"""
    |> shouldEqualWithPrepend
        """
module A
open System
open System.Collections

val a: DateTime
"""
