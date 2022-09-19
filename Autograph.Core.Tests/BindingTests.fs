module Autograph.Core.Tests.BindingTests

open NUnit.Framework
open TestHelper

[<Test>]
let ``initial test`` () =
    assertSignature
        """
module A

let a b = b + 1
"""
        """
module A

val a: b: int -> int
"""

[<Test>]
let ``simple value`` () =
    assertSignature
        """
module A

let a = 1
"""
        """
module A

val a: int
"""

[<Test>]
let ``tupled argument`` () =
    assertSignature
        """
module A

let a (b: int, c) = printfn "%s" c ; b
"""
        """
module A

val a: b: int * c: string -> int
"""

[<Test>]
let ``function return type`` () =
    assertSignature
        """
module A

let a (b:string) : int -> int = failwith "not implemented"
"""
        """
module A

val a: b: string -> (int -> int)
"""

[<Test>]
let ``re-use existing type information is present`` () =
    assertSignature
        """
module A

let a (b:string list) : seq<int> = failwith "not implemented"
"""
        """
module A

val a: b: string list -> seq<int>
"""

[<Test>]
let ``open statements are re-used from source`` () =
    assertSignature
        """
module A

open System
open System.Collections

let a = DateTime.Now
"""
        """
module A

open System
open System.Collections

val a: DateTime
"""

[<Test>]
let ``private access modifier`` () =
    assertSignature
        """
module Foo

let private x = 0
"""
        """
module Foo

val private x: int
"""

[<Test>]
let ``internal access modifier`` () =
    assertSignature
        """
module Foo

let internal x = 0
"""
        """
module Foo

val internal x: int
"""

[<Test>]
let ``inline private`` () =
    assertSignature
        """
module Foo

let inline private a (b:int) = b - 1
"""
        """
module Foo

val inline private a: b: int -> int
"""

[<Test>]
let ``function with tuple parameter`` () =
    assertSignature
        """
module Foo

let x (y:int , z:int) = y + z
"""
        """
module Foo

val x: y: int * z: int -> int
"""

[<Test>]
let ``generic function with annotations`` () =
    assertSignature
        """
module Foo

let g<'t> (h: 't list) = List.length h
"""
        """
module Foo

val g: h: 't list -> int
"""

[<Test>]
let ``generic function without annotations`` () =
    assertSignature
        """
module Foo

let k l m n o = l m n o
"""
        """
module Foo

val k: l: ('a -> 'b -> 'c -> 'd) -> m: 'a -> n: 'b -> o: 'c -> 'd
"""

[<Test>]
let ``generic function with generic type in parameter`` () =
    assertSignature
        """
module Foo

type Teq<'a, 'b> =
    class
    end

let map (f: 'b -> 'c) (t: Teq<'a, 'b>) : Teq<'a, 'c> = failwith "todo"
"""
        """
module Foo

type Teq<'a, 'b> =
    class
    end

val map: f: ('b -> 'c) -> t: Teq<'a, 'b> -> Teq<'a, 'c>
"""

[<Test>]
let ``statically resolved type parameters`` () =
    assertSignature
        """
module Foo

let inline fmap (f: ^a -> ^b) (a: ^a list) = List.map f a
"""
        """
module Foo

val inline fmap: f: (^a -> ^b) -> a: ^a list -> ^b list
"""

[<Test>]
let ``tuple value`` () =
    assertSignature
        """
module Foo

let v = 1, ""
"""
        """
module Foo

val v: int * string
"""

[<Test>]
let ``recursive functions`` () =
    assertSignature
        """
module Hej

let rec a (b: int) = b + 1
and c (d:int) = d - 1
"""
        """
module Hej

val a: b: int -> int
val c: d: int -> int
"""

[<Test>]
let ``active pattern with parameter`` () =
    assertSignature
        """
module Hej

let (|Foo|) (a:string) = a
"""
        """
module Hej

val (|Foo|): a: string -> string
"""

[<Test>]
let ``active pattern without parameter`` () =
    assertSignature
        """
module Hej

let (|Foo|) = function | 1 -> 2 | x -> x
"""
        """
module Hej

val (|Foo|): (int -> int)
"""

[<Test>]
let ``unwrapped discriminated union value`` () =
    assertSignature
        """
module A

type B = | B of string * int

let c (B(d,e)) = 4
"""
        """
module A

type B = B of string * int
val c: B -> int
"""

[<Test>]
let ``type with generic arguments`` () =
    assertSignature
        """
module Hej

let collectInfoFromSynArgPats (argPats : obj) =  Map.empty<string, obj>
"""
        """
module Hej

val collectInfoFromSynArgPats: argPats: obj -> Map<string, obj>
"""

[<Test>]
let ``postfix type in tuple return type`` () =
    assertSignature
        """
module SourceParser

type SynValTyparDecls =
    | SynValTyparDecls of int * int

let inline (|ValTyparDecls|) (SynValTyparDecls (tds, b)) = (Some tds, b)
"""
        """
module SourceParser

type SynValTyparDecls = SynValTyparDecls of int * int
val inline (|ValTyparDecls|): SynValTyparDecls -> int option * int
"""

[<Test>]
let ``active pattern choice return type`` () =
    assertSignature
        """
module Colour

open System

let (|Red|Blue|Yellow|) b =
    match b with
    | 0 -> Red("hey", DateTime.Now)
    | 1 -> Blue(9., [| 'a' |])
    | _ -> Yellow [ 1uy ]
"""

        """
module Colour

open System
val (|Red|Blue|Yellow|): b: int -> Choice<string * DateTime, float * char array, byte list>
"""

[<Test>]
let ``array type`` () =
    assertSignature
        """
module A

let a = [| 0 |]
"""
        """
module A

val a: int array
"""

[<Test>]
let ``multiple postfixes`` () =
    assertSignature
        """
module A

let v = [ [| None; Some 1 |] ]
"""
        """
module A

val v: int option array list
"""

[<Test>]
let ``attributes in parameter`` () =
    assertSignature
        """
module Foo

type BAttribute() =
    inherit System.Attribute()

let a ([<B>] c: int) : int = 0
"""
        """
module Foo

type BAttribute =
    new: unit -> BAttribute
    inherit System.Attribute

val a: [<B>] c: int -> int
"""

[<Test>]
let ``attributes in parameter with type`` () =
    assertSignature
        """
module Foo

type BAttribute() =
    inherit System.Attribute()

let a ([<B>] c) : int = c + 1
"""
        """
module Foo

type BAttribute =
    new: unit -> BAttribute
    inherit System.Attribute

val a: [<B>] c: int -> int
"""

[<Test>]
let ``optional parameter in member`` () =
    assertSignature
        """
namespace X

type Meh =
    member this.Foo (?x) = defaultArg x 0
"""
        """
namespace X

[<Class>]
type Meh =
    member Foo: ?x: int -> int
"""

[<Test>]
let ``curried optional parameters in member`` () =
    assertSignature
        """
namespace X

type Meh =
    member this.Foo (?x, ?y:int) = defaultArg x 0 + defaultArg y 0
"""
        """
namespace X

[<Class>]
type Meh =
    member Foo: ?x: int * ?y: int -> int
"""

[<Test>]
let ``optional function type in member`` () =
    assertSignature
        """
module X

type Meh =
    member this.Foo (?x:int -> int) = 0
"""
        """
module X

[<Class>]
type Meh =
    member Foo: ?x: (int -> int) -> int
"""

[<Test>]
let ``typed single discrimination union`` () =
    assertSignature
        """
module A

type Folder = Folder of path: string

let private runToolListCmd (Folder workingDir: Folder) (globalFlag: bool) = ()
"""
        """
module A

type Folder = Folder of path: string
val private runToolListCmd: Folder -> globalFlag: bool -> unit
"""

[<Test>]
let ``aliased pattern parameter`` () =
    assertSignature
        """
module Hej

type DU = DU of one: string * two:int

let (|Two|) (DU.DU(one, two) as du) = two
"""
        """
module Hej

type DU = DU of one: string * two: int
val (|Two|): DU -> int
"""

[<Test>]
let ``wildcard pattern`` () =
    assertSignature
        """
module W

let (|Fst|) (a, _) = a

do
    match "", true with
    | Fst s -> printfn "%s" s
"""

        """
module W

val (|Fst|): 'a * 'b -> 'a
"""

[<Test>]
let ``tuple return type in partial active pattern`` () =
    assertSignature
        """
module A

let (|Twice|_|) (a:int) = Some (a, a) 
"""
        """
module A

val (|Twice|_|): a: int -> (int * int) option
"""

[<Test>]
let ``nested tuples`` () =
    assertSignature
        """
module T

open System

let a = ("", ('c', true)), DateTime.Now
"""
        """
module T

open System
val a: (string * (char * bool)) * DateTime
"""

[<Test>]
let ``extension type property`` () =
    assertSignature
        """
module P

type System.Object with
    member this.Range = 0
"""
        """
module P

type System.Object with

    member Range: int
"""

[<Test>]
let ``literal value should contain default value`` () =
    assertSignature
        """
module L

[<Literal>]
let MaxLength = 512
"""
        """
module L

[<Literal>]
val MaxLength: int = 512
"""
