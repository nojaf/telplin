module Telplin.Core.Tests.BindingTests

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

val a: b: string list -> int seq
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

val (|Foo|): int -> int
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
val (|Red|Blue|Yellow|): b: int -> Choice<(string * DateTime), (float * char array), byte list>
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
val (|Two|): du: DU -> int
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

val (|Fst|): a: 'a * 'b -> 'a
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

[<Test>]
let ``active pattern unwrapped in binding`` () =
    assertSignature
        """
module A

type SynTypeDefnSig =
    /// The information for a type definition in a signature
    | SynTypeDefnSig of
        typeInfo: obj *
        typeRepr: obj *
        members: obj list *
        range: obj *
        trivia: obj

let (|TypeDefnSig|) (SynTypeDefnSig(typeInfo, typeRepr, members, range, _)) = (typeInfo, typeRepr, members)
let rec meh () = ()
and fn (x:int) (TypeDefnSig(a,b,c)) = x
"""
        """
module A

type SynTypeDefnSig =
    /// The information for a type definition in a signature
    | SynTypeDefnSig of typeInfo: obj * typeRepr: obj * members: obj list * range: obj * trivia: obj

val (|TypeDefnSig|): SynTypeDefnSig -> obj * obj * obj list
val meh: unit -> unit
val fn: x: int -> SynTypeDefnSig -> int
"""

[<Test>]
let ``nested tuples should no longer be named`` () =
    assertSignature
        """
module X

let a ((x: int, y: float), z:string) = 1uy
"""
        """
module X

val a: (int * float) * z: string -> byte
"""

[<Test>]
let ``binding with unresolvable parameter that return a function type`` () =
    assertSignature
        """
module V

let f _ = (*) 3
"""
        """
module V

val f: 'a -> (int -> int)
"""

[<Test>]
let ``lambda return expression`` () =
    assertSignature
        """
module L

let f (a : string) (b : char) = fun (c : float) -> 2.0
"""
        """
module L

val f: a: string -> b: char -> c: float -> float
"""

[<Test>]
let ``function types as parameters with explicit function return type`` () =
    assertSignature
        """
module F

let f (v : int -> int) (x : int -> int) : int -> int = fun z -> z + 1
"""
        """
module F

val f: v: (int -> int) -> x: (int -> int) -> z: int -> int
"""

[<Test>]
let ``function types as parameters without explicit function return type`` () =
    assertSignature
        """
module F

let f (v : int -> int) (x : int -> int) = fun z -> z + 1
"""
        """
module F

val f: v: (int -> int) -> x: (int -> int) -> z: int -> int
"""

[<Test>]
let ``value that returns a function type`` () =
    assertSignature
        """
module V

type Node = { Index : int }
let sortChildren =
    Array.sortBy (fun ({ Index = idx }: Node) -> idx)
"""
        """
module V

type Node = { Index: int }
val sortChildren: (Node array -> Node array)
"""

[<Test>]
let ``use full type when available`` () =
    assertSignature
        """
module T

let fn (r: System.Text.RegularExpressions.Regex) = ""
"""
        """
module T

val fn: r: System.Text.RegularExpressions.Regex -> string
"""

[<Test>]
let ``use full type in tuple when available`` () =
    assertSignature
        """
module T

let fn (r: System.Text.RegularExpressions.Regex, v: string) = ""
"""
        """
module T

val fn: r: System.Text.RegularExpressions.Regex * v: string -> string
"""

[<Test>]
let ``use full type when available, with generic args`` () =
    assertSignature
        """
namespace Utils

module Dict =
    let tryGet k (d: System.Collections.Generic.IDictionary<_, _>) =
        let r, x = d.TryGetValue k
        if r then Some x else None
"""
        """
namespace Utils

module Dict =
    val tryGet: k: 'a -> d: System.Collections.Generic.IDictionary<'a, 'b> -> 'b option
"""

[<Test>]
let ``use full type when available, with post fix`` () =
    assertSignature
        """
module T

let fn (r: System.Text.RegularExpressions.Regex option) = ""
"""
        """
module T

val fn: r: System.Text.RegularExpressions.Regex option -> string
"""

[<Test>]
let ``use full type when available, for optional type`` () =
    assertSignature
        """
module T

type V() =
    member this.XY (?r: System.Text.RegularExpressions.Regex) = ""
"""
        """
module T

type V =
    new: unit -> V
    member XY: ?r: System.Text.RegularExpressions.Regex -> string
"""

[<Test>]
let ``generic constraint in binding`` () =
    assertSignature
        """
module G

let alreadyVisited<'key when 'key: not struct> () =
    let cache = System.Collections.Generic.HashSet<'key>([], HashIdentity.Reference)

    fun key ->
        if cache.Contains key then
            true
        else
            cache.Add key |> ignore
            false
"""
        """
module G

val alreadyVisited: unit -> ('key -> bool) when 'key: not struct
"""

[<Test>]
let ``implicit constraint in binding`` () =
    assertSignature
        """
module I

let memoizeBy (g: 'a -> 'c) (f: 'a -> 'b) =
    let cache =
        System.Collections.Concurrent.ConcurrentDictionary<_, _>(HashIdentity.Structural)

    fun x -> cache.GetOrAdd(Some(g x), lazy (f x)).Force()
"""
        """
module I

val memoizeBy: g: ('a -> 'c) -> f: ('a -> 'b) -> ('a -> 'b) when 'c: equality
"""

[<Test>]
let ``implicit comparison constraint in binding`` () =
    assertSignature
        """
module List2D

let minBy f = List.map (List.minBy f) >> List.minBy f
"""
        """
module List2D

val minBy: f: ('a -> 'b) -> ('a list list -> 'a) when 'b: comparison
"""

[<Test>]
let ``parameter type with a #`` () =
    assertSignature
        """
module FA

let intersperse separator (sequence: #seq<'a>) =
    seq {
      let mutable notFirst = false

      for element in sequence do
        if notFirst then
          yield separator

        yield element
        notFirst <- true
    }
"""
        """
module FA

val intersperse: separator: 'a -> sequence: #('a seq) -> 'a seq
"""

[<Test>]
let ``multiple arguments in constructor`` () =
    assertSignature
        """
module FA

type Debounce<'a>(timeout, fn) as x =

  let mailbox =
    MailboxProcessor<'a>.Start
      (fun agent ->
        let rec loop ida idb arg =
          async {
            let! r = agent.TryReceive(x.Timeout)

            match r with
            | Some arg -> return! loop ida (idb + 1) (Some arg)
            | None when ida <> idb ->
              do! fn arg.Value
              return! loop 0 0 None
            | None -> return! loop 0 0 arg
          }

        loop 0 0 None)

  /// Calls the function, after debouncing has been applied.
  member _.Bounce(arg) = mailbox.Post(arg)

  /// Timeout in ms
  member val Timeout = timeout with get, set
"""
        """
module FA

type Debounce<'a> =
    new: timeout: int * fn: ('a -> Async<unit>) -> Debounce<'a>
    /// Calls the function, after debouncing has been applied.
    member Bounce: arg: 'a -> unit
    /// Timeout in ms
    member Timeout: int with get, set
"""

[<Test>]
let ``fully typed binding`` () =
    assertSignature
        """
module F

let minus (a:int) (b:int) : int = a - b
"""
        """
module F

val minus: a: int -> b: int -> int
"""

[<Test>]
let ``member constraints`` () =
    assertSignature
        """
module Telplin

let inline sum xs = List.sum xs
"""
        """
module Telplin

val inline sum: xs: ^a list -> ^a when ^a: (static member (+): ^a * ^a -> ^a) and ^a: (static member Zero: ^a)
"""

[<Test>]
let ``array in nested type`` () =
    assertSignature
        """
module Telplin

let v = [|0|], 0
"""
        """
module Telplin

val v: int array * int
"""

[<Test>]
let ``multidimensional array`` () =
    assertSignature
        """
module Telplin

let v = array2D [ [ 1 ; 0 ] ; [ 0 ; 1 ] ]
"""
        """
module Telplin

val v: int array2d
"""

[<Test>]
let ``constraint from application of another function`` () =
    assertSignature
        """
module Telplin

let f<'T when 'T: equality> (x: 'T) = x
let g (x: 'U) : 'U = f x
"""
        """
module Telplin

val f: x: 'T -> 'T when 'T: equality
val g: x: 'U -> 'U when 'U: equality
"""

[<Test>]
let ``constraint from application of another function - large example`` () =
    assertSignature
        """
module Telplin

open System.Collections.Generic
open System.Linq

type internal Graph<'Node> = IReadOnlyDictionary<'Node, 'Node[]>

let addIfMissing<'Node when 'Node: equality> (nodes: 'Node seq) (graph: Graph<'Node>) : Graph<'Node> =
    nodes
    |> Seq.except (graph.Keys |> Seq.toArray)
    |> fun missing ->
        let toAdd = missing |> Seq.map (fun n -> KeyValuePair(n, [||])) |> Seq.toArray
        let x: KeyValuePair<'Node, 'Node[]>[] = Array.append (graph |> Seq.toArray) toAdd
        x.ToDictionary((fun (KeyValue (x, _)) -> x), (fun (KeyValue (_, v)) -> v)) :> IReadOnlyDictionary<_, _>

/// Create a reverse of the graph
let reverse (originalGraph: Graph<'Node>) : Graph<'Node> =
    originalGraph
    // Collect all edges
    |> Seq.collect (fun (KeyValue (idx, deps)) -> deps |> Array.map (fun dep -> idx, dep))
    // Group dependants of the same dependencies together
    |> Seq.groupBy snd
    // Construct reversed graph
    |> Seq.map (fun (dep, edges) -> dep, edges |> Seq.map fst |> Seq.toArray)
    |> readOnlyDict
    |> addIfMissing originalGraph.Keys
"""
        """
module Telplin

open System.Collections.Generic
open System.Linq

type internal Graph<'Node> = IReadOnlyDictionary<'Node, 'Node[]>
val addIfMissing: nodes: 'Node seq -> graph: Graph<'Node> -> Graph<'Node> when 'Node: equality
/// Create a reverse of the graph
val reverse: originalGraph: Graph<'Node> -> Graph<'Node> when 'Node: equality
"""

[<Test>]
let ``extern binding`` () =
    assertSignature
        """
module Telplin

[<System.Runtime.InteropServices.DllImport("")>]
extern int f()
"""
        """
module Telplin

[<System.Runtime.InteropServices.DllImport("")>]
val f: unit -> int
"""

[<Test>]
let ``don't re-used wildcard array types from source, 30`` () =
    assertSignature
        """
module Telplin

  /// Optimized arrays equality. ~100x faster than `array1 = array2` on strings.
  /// ~2x faster for floats
  /// ~0.8x slower for ints
  let inline areEqual (xs: 'T[]) (ys: 'T[]) =
    match xs, ys with
    | null, null -> true
    | [||], [||] -> true
    | null, _
    | _, null -> false
    | _ when xs.Length <> ys.Length -> false
    | _ ->
      let mutable break' = false
      let mutable i = 0
      let mutable result = true

      while i < xs.Length && not break' do
        if xs.[i] <> ys.[i] then
          break' <- true
          result <- false

        i <- i + 1

      result

  /// check if subArray is found in the wholeArray starting
  /// at the provided index
  let inline isSubArray (subArray: 'T[]) (wholeArray: 'T[]) index =
    if isNull subArray || isNull wholeArray then
      false
    elif subArray.Length = 0 then
      true
    elif subArray.Length > wholeArray.Length then
      false
    elif subArray.Length = wholeArray.Length then
      areEqual subArray wholeArray
    else
      let rec loop subidx idx =
        if subidx = subArray.Length then
          true
        elif subArray.[subidx] = wholeArray.[idx] then
          loop (subidx + 1) (idx + 1)
        else
          false

      loop 0 index

  /// Returns true if one array has another as its subset from index 0.
  let startsWith (prefix: _[]) (whole: _[]) = isSubArray prefix whole 0
"""
        """
module Telplin

/// Optimized arrays equality. ~100x faster than `array1 = array2` on strings.
/// ~2x faster for floats
/// ~0.8x slower for ints
val inline areEqual: xs: 'T array -> ys: 'T array -> bool when 'T: equality
/// check if subArray is found in the wholeArray starting
/// at the provided index
val inline isSubArray: subArray: 'T array -> wholeArray: 'T array -> index: int -> bool when 'T: equality
/// Returns true if one array has another as its subset from index 0.
val startsWith: prefix: 'a array -> whole: 'a array -> bool when 'a: equality
"""

[<Test>]
let ``generic argument in binding should be preserved, 32`` () =
    assertSignature
        """
module Telplin

  let isAttribute<'T> (attribute: obj) = true
"""
        """
module Telplin

val isAttribute<'T> : attribute: obj -> bool
"""

[<Test>]
let ``generic argument with constraint in binding should be preserved, 39`` () =
    assertSignature
        """
module Telplin

[<Interface>]
type Node =
    abstract Children: int array

let noa<'n when 'n :> Node> (n: 'n option) =
    match n with
    | None -> Array.empty
    | Some n -> [| n :> Node |]
"""
        """
module Telplin

[<Interface>]
type Node =
    abstract Children: int array

val noa: n: #Node option -> Node array
"""

[<Test>]
let ``hash constraint in funs type`` () =
    assertSignature
        """
module Telplin

open System
open System.Threading.Tasks

let mapWithAdditionalDependencies
    (mapping: 'a -> 'b * #seq<#IDisposable>) 
    (value: Task<'a>) : Task<'b> =
    failwith "meh"
"""
        """
module Telplin

open System
open System.Threading.Tasks

val mapWithAdditionalDependencies:
    mapping: ('a -> 'b * #('b1 seq)) -> value: Task<'a> -> Task<'b> when 'b1 :> IDisposable
"""

[<Test>]
let ``access modifier on member, 56`` () =
    assertSignature
        """
module Telplin

open System

type FSharpItemsContainer =
    member private x.AddItem(item: obj) = ()
"""
        """
module Telplin

open System

[<Class>]
type FSharpItemsContainer =
    member private AddItem: item: obj -> unit
"""

[<Test>]
let ``don't reuse parameter constraints from source, 57`` () =
    assertSignature
        """
module Telplin

open System

/// Reference equality.
let inline (==) (a: 'A when 'A: not struct) (b: 'B when 'B: not struct) =
    obj.ReferenceEquals(a, b)
"""
        """
module Telplin

open System
/// Reference equality.
val inline (==): a: 'A -> b: 'B -> bool when 'A: not struct and 'B: not struct
"""

[<Test>]
let ``property member with function return type`` () =
    assertSignature
        """
module Telplin

type Foo() =
    member x.Bar = fun (i:int) -> ""
"""
        """
module Telplin

type Foo =
    new: unit -> Foo
    member Bar: (int -> string)
"""

[<Test>]
let ``mutable value, 67`` () =
    assertSignature
        """
module X

let mutable lastDisplayContext : obj = null
"""
        """
module X

val mutable lastDisplayContext: obj
"""

[<Test>]
let ``order of generic parameters, 82`` () =
    assertSignature
        """
module Telplin

let f<'a, 'b> (x: 'b) (y: 'a) = ()
let g<'a, 'b> (x: 'b, y: 'a) = ()
"""
        """
module Telplin

val f<'a, 'b> : x: 'b -> y: 'a -> unit
val g<'a, 'b> : x: 'b * y: 'a -> unit
"""

[<Test>]
let ``private member get binding, 87`` () =
    assertSignature
        """
module Telplin

type X() =
    member private this.Y with get() = "meh"
"""
        """
module Telplin

type X =
    new: unit -> X
    member private Y: string
"""

[<Test>]
let ``a difference in accessibility leads to separate get,set members`` () =
    assertSignatureWith
        id
        false
        """
module Telplin

type T =
    struct
        member private this.X with get () : int = 1 and set (_:int) = ()
        member this.Y with set (_:int) = () and private get () = 1
        member this.Z with private set (_:int) = () and get () = 2
    end
"""
        """
module Telplin

type T =
    struct
        member private X: int with get, set
        member Y: int with set
        member private Y: int with get
        member private Z: int with set
        member Z: int with get
    end
"""

[<Test>]
let ``use accessibility from identifier pattern`` () =
    assertSignatureWith
        id
        false
        """
module Telplin

type T() =
        member private this.X with get () : int = 1 and set (_:int) = ()
        member public this.Y with set (_:int) = () and get () = 1
        member internal this.Z with set (_:int) = () and get () = 2
"""
        """
module Telplin

type T =
    new: unit -> T
    member private X: int with get, set
    member public Y: int with set, get
    member internal Z: int with set, get
"""

[<Test>]
let ``inline keyword in property, 90`` () =
    assertSignature
        """
module Meh

type Foo =
    member inline this.Item
        with get (i:int,j: char) : string = ""
        and set (i:int,j: char) (x:string) = printfn "%i %c" i j
"""
        """
module Meh

[<Class>]
type Foo =
    member inline Item: i: int * j: char -> string with get
    member inline Item: i: int * j: char -> string with set
"""

[<Test>]
let ``generic type with member with same generic parameter name, 89`` () =
    assertSignature
        """
module Telplin

type 'a Foo =
    {
        Bar: 'a array
        D: int
    }
    static member Make<'a> (array: 'a array) : 'a Foo = failwith "meh"
"""
        """
module Telplin

type 'a Foo =
    { Bar: 'a array
      D: int }

    static member Make<'a> : array: 'a array -> 'a Foo
"""
