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

[<Test>]
let ``private access modifier`` () =
    mkSignature
        """
module Foo

let private x = 0
"""
    |> shouldEqualWithPrepend
        """
module Foo

val private x: int
"""

[<Test>]
let ``internal access modifier`` () =
    mkSignature
        """
module Foo

let internal x = 0
"""
    |> shouldEqualWithPrepend
        """
module Foo

val internal x: int
"""

[<Test>]
let ``inline private`` () =
    mkSignature
        """
module Foo

let inline private a (b:int) = b - 1
"""
    |> shouldEqualWithPrepend
        """
module Foo

val inline private a: b: int -> int
"""

[<Test>]
let ``function with tuple parameter`` () =
    mkSignature
        """
module Foo

let x (y:int , z:int) = y + z
"""
    |> shouldEqualWithPrepend
        """
module Foo

val x: y: int * z: int -> int
"""

[<Test>]
let ``generic function with annotations`` () =
    mkSignature
        """
module Foo

let g<'t> (h: 't list) = List.length h
"""
    |> shouldEqualWithPrepend
        """
module Foo

val g: h: 't list -> int
"""

[<Test>]
let ``generic function without annotations`` () =
    mkSignature
        """
module Foo

let k l m n o = l m n o
"""
    |> shouldEqualWithPrepend
        """
module Foo

val k: l: ('a -> 'b -> 'c -> 'd) -> m: 'a -> n: 'b -> o: 'c -> 'd
"""

[<Test>]
let ``generic function with generic type in parameter`` () =
    mkSignature
        """
module Foo

type Teq<'a, 'b> =
    class
    end

let map (f: 'b -> 'c) (t: Teq<'a, 'b>) : Teq<'a, 'c> = failwith "todo"
"""
    |> shouldEqualWithPrepend
        """
module Foo

type Teq<'a, 'b> =
    class
    end

val map: f: ('b -> 'c) -> t: Teq<'a, 'b> -> Teq<'a, 'c>
"""

[<Test>]
let ``statically resolved type parameters`` () =
    mkSignature
        """
module Foo

let inline fmap (f: ^a -> ^b) (a: ^a list) = List.map f a
"""
    |> shouldEqualWithPrepend
        """
module Foo

val inline fmap: f: (^a -> ^b) -> a: ^a list -> ^b list
"""

[<Test>]
let ``tuple value`` () =
    mkSignature
        """
module Foo

let v = 1, ""
"""
    |> shouldEqualWithPrepend
        """
module Foo

val v: int * string
"""

[<Test>]
let ``recursive functions`` () =
    mkSignature
        """
module Hej

let rec a (b: int) = b + 1
and c (d:int) = d - 1
"""
    |> shouldEqualWithPrepend
        """
module Hej

val a: b: int -> int
val c: d: int -> int
"""

[<Test>]
let ``active pattern with parameter`` () =
    mkSignature
        """
module Hej

let (|Foo|) (a:string) = a
"""
    |> shouldEqualWithPrepend
        """
module Hej

val (|Foo|): a: string -> string
"""

[<Test>]
let ``active pattern without parameter`` () =
    mkSignature
        """
module Hej

let (|Foo|) = function | 1 -> 2 | x -> x
"""
    |> shouldEqualWithPrepend
        """
module Hej

val (|Foo|): (int -> int)
"""

[<Test>]
let ``unwrapped discriminated union value`` () =
    mkSignature
        """
module A

type B = | B of string * int

let c (B(d,e)) = 4
"""
    |> shouldEqualWithPrepend
        """
module A

type B = B of string * int
val c: B -> int
"""

[<Test>]
let ``type with generic arguments`` () =
    mkSignature
        """
module Hej

let collectInfoFromSynArgPats (argPats : obj) =  Map.empty<string, obj>
"""
    |> shouldEqualWithPrepend
        """
module Hej

val collectInfoFromSynArgPats: argPats: obj -> Map<string, obj>
"""
