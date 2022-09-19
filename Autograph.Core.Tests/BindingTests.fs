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

[<Test>]
let ``postfix type in tuple return type`` () =
    mkSignature
        """
module SourceParser

type SynValTyparDecls =
    | SynValTyparDecls of int * int

let inline (|ValTyparDecls|) (SynValTyparDecls (tds, b)) = (Some tds, b)
"""
    |> shouldEqualWithPrepend
        """
module SourceParser

type SynValTyparDecls = SynValTyparDecls of int * int
val inline (|ValTyparDecls|): SynValTyparDecls -> int option * int
"""

[<Test>]
let ``active pattern choice return type`` () =
    mkSignature
        """
module Colour

open System

let (|Red|Blue|Yellow|) b =
    match b with
    | 0 -> Red("hey", DateTime.Now)
    | 1 -> Blue(9., [| 'a' |])
    | _ -> Yellow [ 1uy ]
"""
    |> shouldEqualWithPrepend
        """
module Colour

open System
val (|Red|Blue|Yellow|): b: int -> Choice<string * DateTime, float * char array, byte list>
"""

[<Test>]
let ``array type`` () =
    mkSignature
        """
module A

let a = [| 0 |]
"""
    |> shouldEqualWithPrepend
        """
module A

val a: int array
"""

[<Test>]
let ``multiple postfixes`` () =
    mkSignature
        """
module A

let v = [ [| None; Some 1 |] ]
"""
    |> shouldEqualWithPrepend
        """
module A

val v: int option array list
"""

[<Test>]
let ``attributes in parameter`` () =
    mkSignature
        """
module Foo

type BAttribute() =
    inherit System.Attribute()

let a ([<B>] c: int) : int = 0
"""
    |> shouldEqualWithPrepend
        """
module Foo

type BAttribute =
    new: unit -> BAttribute
    inherit System.Attribute

val a: [<B>] c: int -> int
"""

[<Test>]
let ``attributes in parameter with type`` () =
    mkSignature
        """
module Foo

type BAttribute() =
    inherit System.Attribute()

let a ([<B>] c) : int = c + 1
"""
    |> shouldEqualWithPrepend
        """
module Foo

type BAttribute =
    new: unit -> BAttribute
    inherit System.Attribute

val a: [<B>] c: int -> int
"""

[<Test>]
let ``optional parameter in member`` () =
    mkSignature
        """
namespace X

type Meh =
    member this.Foo (?x) = defaultArg x 0
"""
    |> shouldEqualWithPrepend
        """
namespace X

type Meh =
    member Foo: ?x: int -> int
"""

[<Test>]
let ``curried optional parameters in member`` () =
    mkSignature
        """
namespace X

type Meh =
    member this.Foo (?x, ?y:int) = defaultArg x 0 + defaultArg y 0
"""
    |> shouldEqualWithPrepend
        """
namespace X

type Meh =
    member Foo: ?x: int * ?y: int -> int
"""

[<Test>]
let ``optional function type in member`` () =
    mkSignature
        """
namespace X

type Meh =
    member this.Foo (?x:int -> int) = 0
"""
    |> shouldEqualWithPrepend
        """
namespace X

type Meh =
    member Foo: ?x: (int -> int) -> int
"""

[<Test>]
let ``typed single discrimination union`` () =
    mkSignature
        """
module A

type Folder = Folder of path: string

let private runToolListCmd (Folder workingDir: Folder) (globalFlag: bool) = ()
"""
    |> shouldEqualWithPrepend
        """
module A

type Folder = Folder of path: string
val private runToolListCmd: Folder -> globalFlag: bool -> unit
"""

[<Test>]
let ``aliased pattern parameter`` () =
    mkSignature
        """
module Hej

type DU = DU of one: string * two:int

let (|Two|) (DU.DU(one, two) as du) = two
"""
    |> shouldEqualWithPrepend
        """
module Hej

type DU = DU of one: string * two: int
val (|Two|): DU -> int
"""

[<Test>]
let ``wildcard pattern`` () =
    mkSignature
        """
module W

let (|Fst|) (a, _) = a

do
    match "", true with
    | Fst s -> printfn "%s" s
"""
    |> shouldEqualWithPrepend
        """
module W

val (|Fst|): 'a * 'b -> 'a
"""

[<Test>]
let ``tuple return type in partial active pattern`` () =
    mkSignature
        """
module A

let (|Twice|_|) (a:int) = Some (a, a) 
"""
    |> shouldEqualWithPrepend
        """
module A

val (|Twice|_|): a: int -> (int * int) option
"""

[<Test>]
let ``nested tuples`` () =
    mkSignature
        """
module T

open System

let a = ("", ('c', true)), DateTime.Now
"""
    |> shouldEqualWithPrepend
        """
module T

open System
val a: (string * (char * bool)) * DateTime
"""

[<Test>]
let ``extension type property`` () =
    mkSignature
        """
namespace P

type System.Object with
    member this.Range = 0
"""
    |> shouldEqualWithPrepend
        """
namespace P

type System.Object with

    member Range: int
"""

[<Test>]
let ``literal value should contain default value`` () =
    mkSignature
        """
module L

[<Literal>]
let MaxLength = 512
"""
    |> shouldEqualWithPrepend
        """
module L

[<Literal>]
val MaxLength: int = 512
"""

type System.Object with

    member this.Range = 0
