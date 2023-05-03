﻿module Telplin.Core.Tests.TypeTests

open NUnit.Framework
open TestHelper

[<Test>]
let ``simple record`` () =
    assertSignature
        """
module A

type B = {
    Foo: string
    Bar: int
    SomethingElseButSomewhatLonger: float
}
"""
        """
module A

type B =
    { Foo: string
      Bar: int
      SomethingElseButSomewhatLonger: float }
"""

[<Test>]
let ``empty struct`` () =
    assertSignature
        """
namespace Foo

[<Struct>]
type Bar = struct end
"""
        """
namespace Foo

[<Struct>]
type Bar =
    struct
    end
"""

[<Test>]
let ``struct with value`` () =
    assertSignature
        """
namespace Foo

[<Struct>]
type Bar =
    val X : int
"""
        """
namespace Foo

[<Struct>]
type Bar =
    val X: int
"""

[<Test>]
let ``struct with constructor`` () =
    assertSignature
        """
namespace Foo

type Bar =
    struct
        val X : int
        new(x : int) = { X = x }
    end
"""
        """
namespace Foo

type Bar =
    struct
        val X: int
        new: x: int -> Bar
    end
"""

[<Test>]
let ``AbstractSlot in type definition`` () =
    assertSignature
        """
namespace Hej

type Foo =
    abstract member Bar : x : int -> y : int -> int
"""
        """
namespace Hej

type Foo =
    abstract member Bar: x: int -> y: int -> int
"""

[<Test>]
let ``abstract member with default implementation`` () =
    assertSignature
        """
namespace Telplin

type Base() =
    abstract member F: int -> int
    default this.F x = 0
"""
        """
namespace Telplin

type Base =
    new: unit -> Base
    abstract member F: int -> int
    default F: x: int -> int
"""

[<Test>]
let ``derived class with member override`` () =
    assertSignature
        """
namespace Telplin

[<AbstractClass>]
type Base() =
    abstract member F: int -> int

type T() =
    inherit Base()
    override this.F x = 1
"""
        """
namespace Telplin

[<AbstractClass>]
type Base =
    new: unit -> Base
    abstract member F: int -> int

type T =
    new: unit -> T
    inherit Base
    override F: x: int -> int
"""

[<Ignore("https://github.com/dotnet/fsharp/issues/14712")>]
[<Test>]
let ``type extension`` () =
    assertSignature
        """
module Hej

type System.Int32 with
    member i.PlusPlus () = i + 1
"""
        """
module Hej

type System.Int32 with

    member PlusPlus: unit -> int
"""

[<Test>]
let ``type with nested interface`` () =
    assertSignature
        """
namespace Hej

open System
        
type A =
    { B: int }
    interface IDisposable with
        member this.Dispose () = ()
"""
        """
namespace Hej

open System

type A =
    { B: int }

    interface IDisposable
"""

[<Test>]
let ``interface that inherits an interface`` () =
    assertSignature
        """
namespace B

open System

type A =
    interface
        inherit IDisposable
    end
"""
        """
namespace B

open System

type A =
    interface
        inherit IDisposable
    end
"""

[<Test>]
let ``static member value`` () =
    assertSignature
        """
namespace X

type State =
    {
        Files : string list
    }

    static member Empty : State = { Files = [] }
"""
        """
namespace X

type State =
    { Files: string list }

    static member Empty: State
"""

[<Test>]
let ``static getter/setter should remain static, 51`` () =
    assertSignature
        """
// This is a generated file; the original input is 'FSInteractiveSettings.txt'
namespace FSInteractiveSettings

type internal SR private() =

    static let mutable swallowResourceText = false

    /// If set to true, then all error messages will just return the filled 'holes' delimited by ',,,'s - this is for language-neutral testing (e.g. localization-invariant baselines).
    static member SwallowResourceText with get () = swallowResourceText
                                        and set (b) = swallowResourceText <- b
    // END BOILERPLATE
"""
        """
namespace FSInteractiveSettings

type internal SR =
    private new: unit -> SR
    /// If set to true, then all error messages will just return the filled 'holes' delimited by ',,,'s - this is for language-neutral testing (e.g. localization-invariant baselines).
    static member SwallowResourceText: bool with get, set
"""

[<Test>]
let ``empty class with constructor`` () =
    assertSignature
        """
namespace X

type LSPFantomasService() =
    class end
"""
        """
namespace X

type LSPFantomasService =
    class
        new: unit -> LSPFantomasService
    end
"""

[<Test>]
let ``static member of record type`` () =
    assertSignature
        """
namespace A

type ASTContext =
    {
        /// This pattern matters for formatting extern declarations
        IsCStylePattern: bool
        /// A field is rendered as union field or not
        IsUnionField: bool
        /// First type param might need extra spaces to avoid parsing errors on `<^`, `<'`, etc.
        IsFirstTypeParam: bool
    }

    static member Default =
        { IsCStylePattern = false
          IsUnionField = false
          IsFirstTypeParam = false }
"""
        """
namespace A

type ASTContext =
    {
        /// This pattern matters for formatting extern declarations
        IsCStylePattern: bool
        /// A field is rendered as union field or not
        IsUnionField: bool
        /// First type param might need extra spaces to avoid parsing errors on `<^`, `<'`, etc.
        IsFirstTypeParam: bool
    }

    static member Default: ASTContext
"""

[<Test>]
let ``construct with single argument`` () =
    assertSignature
        """
namespace FormatConfig

open System

type FormatException(msg: string) =
    inherit Exception(msg)
"""
        """
namespace FormatConfig

open System

type FormatException =
    new: msg: string -> FormatException
    inherit Exception
"""

[<Test>]
let ``member with constraint in parameter`` () =
    assertSignature
        """
module FA

open System

[<Sealed>]
type MaybeBuilder() =
  member _.Using(resource: 'T :> IDisposable, body: _ -> _ option) : _ option =
    try
      body resource
    finally
      if not <| obj.ReferenceEquals(null, box resource) then
        resource.Dispose()
"""
        """
module FA

open System

[<Sealed>]
type MaybeBuilder =
    new: unit -> MaybeBuilder
    member Using: resource: 'T * body: ('T -> 'a option) -> 'a option when 'T :> IDisposable
"""

[<Test>]
let ``member with get only`` () =
    assertSignature
        """
module FA

type X() =
    member x.Item
        with get (m: int) = ""
"""
        """
module FA

type X =
    new: unit -> X
    member Item: m: int -> string with get
"""

[<Test>]
let ``member with set only`` () =
    assertSignature
        """
module FA

type X() =
    member this.MyWriteOnlyProperty with set (value: string) = ()
"""
        """
module FA

type X =
    new: unit -> X
    member MyWriteOnlyProperty: string with set
"""

[<Test>]
let ``member with indexed get/set`` () =
    assertSignature
        """
module X

type X() =
    member x.Item
        with get (m: int) = ""
        and set (m:int) (y:string) = ()
"""
        """
module X

type X =
    new: unit -> X
    member Item: m: int -> string with get, set
"""

[<Test>]
let ``member with indexed set/get`` () =
    assertSignature
        """
module X

type X() =
    member x.Item
        with set (m:int) (y:string) = ()
        and get (m: int) = ""
"""
        """
module X

type X =
    new: unit -> X
    member Item: m: int -> string with set, get
"""

[<Test>]
let ``member with indexed get/set where the index is different for get and set`` () =
    assertSignature
        """
module X

type X() =
    member x.Item
        with get (a: int) = ""
        and set (b:int) (c:string) = ()
"""
        """
module X

type X =
    new: unit -> X
    member Item: int -> string with get, set
"""

[<Test>]
let ``member with get and unit`` () =
    assertSignature
        """
module FA

type D() =
    let mutable disableInMemoryProjectReferences = false

    member __.DisableInMemoryProjectReferences
        with get () = disableInMemoryProjectReferences
        and set (value) = disableInMemoryProjectReferences <- value
"""
        """
module FA

type D =
    new: unit -> D
    member DisableInMemoryProjectReferences: bool with get, set
"""

[<Test>]
let ``multiple constraints`` () =
    assertSignature
        """
namespace Foo

open System

[<Sealed>]
type AsyncMaybeBuilder() =
  member __.Using(resource: ('T :> IDisposable), body: _ -> Async<_ option>) : Async<_ option> =
    try
      body resource
    finally
      if not << isNull <| resource then
        resource.Dispose()
"""
        """
namespace Foo

open System

[<Sealed>]
type AsyncMaybeBuilder =
    new: unit -> AsyncMaybeBuilder
    member Using: resource: 'T * body: ('T -> Async<'a option>) -> Async<'a option> when 'T :> IDisposable and 'T: null
"""

[<Test>]
let ``generic type extension`` () =
    assertSignature
        """
module FA

open System.Collections.Concurrent

type ConcurrentDictionary<'key, 'value> with

  member x.TryFind key =
    match x.TryGetValue key with
    | true, value -> Some value
    | _ -> None
"""
        """
module FA

open System.Collections.Concurrent

type ConcurrentDictionary<'key, 'value> with

    member TryFind: key: 'key -> 'value option
"""

[<Test>]
let ``extension member when the type parameters have names other than the original`` () =
    assertSignature
        """
module Extensions

type List<'E> with

    member this.X = this.Head
"""
        """
module Extensions

type List<'E> with

    member X: 'E
"""

[<Test>]
let ``extension member with additional type parameters and nested use of original typars`` () =
    assertSignature
        """
module Telplin

type Map<'K, 'V when 'K: comparison> with

    member m.X (t: 'T) (k: 'K) = Some k, ({| n = [|k|] |}, 0)
"""
        """
module Telplin

type Map<'K, 'V when 'K: comparison> with

    member X: t: 'T -> k: 'K -> 'K option * ({| n: 'K array |} * int) when 'K: comparison
"""

[<Test>]
let ``overloads in type`` () =
    assertSignature
        """
module FA

open System

type ColumnIndentedTextWriter() =

    member __.Write(s: string) = ()

    member __.Write(s: string, [<ParamArray>] objs: obj[]) = ()

    member __.WriteLine(s: string) = ()

    member __.WriteLine(s: string, [<ParamArray>] objs: obj[]) = ()

    member x.WriteBlankLines (count:int) = ()

    member __.Indent (i:int) = ()

    member __.Unindent (i:int) = ()

    member __.Dump() = ""

    interface IDisposable with
      member __.Dispose() =
        ()
"""
        """
module FA

open System

type ColumnIndentedTextWriter =
    new: unit -> ColumnIndentedTextWriter
    member Write: s: string -> unit
    member Write: s: string * [<ParamArray>] objs: obj[] -> unit
    member WriteLine: s: string -> unit
    member WriteLine: s: string * [<ParamArray>] objs: obj[] -> unit
    member WriteBlankLines: count: int -> unit
    member Indent: i: int -> unit
    member Unindent: i: int -> unit
    member Dump: unit -> string
    interface IDisposable
"""

[<Test>]
let ``single setter`` () =
    assertSignature
        """
namespace FA

type Hej() =
    let mutable disableInMemoryProjectReferences : bool = false
    member __.DisableInMemoryProjectReferences
        with set (value) = disableInMemoryProjectReferences <- value
"""
        """
namespace FA

type Hej =
    new: unit -> Hej
    member DisableInMemoryProjectReferences: bool with set
"""

[<Test>]
let ``single indexed setter`` () =
    assertSignature
        """
namespace FA

type Hej() =
    let mutable disableInMemoryProjectReferences : bool = false
    member __.DisableInMemoryProjectReferences
        with set (idx: int) (value) = disableInMemoryProjectReferences <- value
"""
        """
namespace FA

type Hej =
    new: unit -> Hej
    member DisableInMemoryProjectReferences: idx: int -> bool with set
"""

[<Test>]
let ``non indexed get/set`` () =
    assertSignature
        """
namespace FA

type Hej() =
    let mutable disableInMemoryProjectReferences : bool = false
    member __.DisableInMemoryProjectReferences
        with get () = disableInMemoryProjectReferences
        and set (value) = disableInMemoryProjectReferences <- value
"""
        """
namespace FA

type Hej =
    new: unit -> Hej
    member DisableInMemoryProjectReferences: bool with get, set
"""

[<Test>]
let ``primary and secondary constructor`` () =
    assertSignature
        """
namespace PS

type Bar(a:int) =
    new (a:int, b:int) = Bar(a)
"""
        """
namespace PS

type Bar =
    new: a: int -> Bar
    new: a: int * b: int -> Bar
"""

[<Test>]
let ``class with C# extension method, 1`` () =
    assertSignature
        """
module Telplin

open System.Runtime.CompilerServices

[<Class>]
type Foo =
    [<Extension>]
    static member PlusOne (a:int) : int = a + 1
"""
        """
module Telplin

open System.Runtime.CompilerServices

[<Class>]
type Foo =
    [<Extension>]
    static member PlusOne: a: int -> int
"""

[<Test>]
let ``class with C# extension method, ClassAttribute`` () =
    assertSignature
        """
module Telplin

open System.Runtime.CompilerServices

[<ClassAttribute>]
type Foo =
    [<Extension>]
    static member PlusOne (a:int) : int = a + 1
"""
        """
module Telplin

open System.Runtime.CompilerServices

[<ClassAttribute>]
type Foo =
    [<Extension>]
    static member PlusOne: a: int -> int
"""

[<Test>]
let ``class with C# extension method, Microsoft.FSharp.Core.ClassAttribute`` () =
    assertSignature
        """
module Telplin

open System.Runtime.CompilerServices

[<Microsoft.FSharp.Core.Class>]
type Foo =
    [<Extension>]
    static member PlusOne (a:int) : int = a + 1
"""
        """
module Telplin

open System.Runtime.CompilerServices

[<Microsoft.FSharp.Core.Class>]
type Foo =
    [<Extension>]
    static member PlusOne: a: int -> int
"""

[<Test>]
let ``type without representation`` () =
    assertSignature
        """
namespace Telplin

[<Measure>]
type LocalPath
"""
        """
namespace Telplin

[<Measure>]
type LocalPath
"""

[<Test>]
let ``inline extension member, 31`` () =
    assertSignature
        """
module Telplin

type System.String with

  member inline x.XDoc = ""
"""
        """
module Telplin

type System.String with

    member inline XDoc: string
"""

[<Test>]
let ``val member with get,set , 33`` () =
    assertSignature
        """
module Telplin

type Debounce<'a>(timeout, fn) =

  /// Timeout in ms
  member val Timeout = timeout with get, set
"""
        """
module Telplin

type Debounce<'a> =
    new: timeout: obj * fn: obj -> Debounce<'a>
    /// Timeout in ms
    member Timeout: obj with get, set
"""

[<Test>]
let ``override val, 38`` () =
    assertSignature
        """
module Telplin

[<AbstractClass>]
type NodeBase() =
    abstract member Children: int array

type StringNode(content: string) =
    inherit NodeBase()
    member val Content = content
    override val Children = Array.empty
"""
        """
module Telplin

[<AbstractClass>]
type NodeBase =
    new: unit -> NodeBase
    abstract member Children: int array

type StringNode =
    new: content: string -> StringNode
    inherit NodeBase
    member Content: string
    override Children: int array
"""

[<Test>]
let ``basic enum`` () =
    assertSignature
        """
module Telplin

[<RequireQualifiedAccess>]
type ErrorCodes =
    | GenericError = 1
    | ProjectNotRestored = 100
    | ProjectParsingFailed = 101
    | GenericProjectError = 102
"""
        """
module Telplin

[<RequireQualifiedAccess>]
type ErrorCodes =
    | GenericError = 1
    | ProjectNotRestored = 100
    | ProjectParsingFailed = 101
    | GenericProjectError = 102
"""

[<Test>]
let ``basic delegates`` () =
    assertSignature
        """
module Telplin

type Delegate1 = delegate of (int * int) -> int
type Delegate2 = delegate of int * int -> int
"""
        """
module Telplin

type Delegate1 = delegate of (int * int) -> int
type Delegate2 = delegate of int * int -> int
"""

[<Test>]
let ``wildcard in hash constraint, 47`` () =
    assertSignature
        """
namespace FsToolkit.ErrorHandling

[<AutoOpen>]
module ResultCE =
    type ResultBuilder() =
        member inline _.Return(value: 'ok) : Result<'ok, 'error> = Ok value

[<AutoOpen>]
module ResultCEExtensions =

    type ResultBuilder with

        /// <summary>
        /// Needed to allow `for..in` and `for..do` functionality
        /// </summary>
        member inline _.Source(s: #seq<_>) : #seq<_> = s
"""
        """
namespace FsToolkit.ErrorHandling

[<AutoOpen>]
module ResultCE =
    type ResultBuilder =
        new: unit -> ResultBuilder
        member inline Return: value: 'ok -> Result<'ok, 'error>

[<AutoOpen>]
module ResultCEExtensions =
    type ResultBuilder with

        /// <summary>
        /// Needed to allow `for..in` and `for..do` functionality
        /// </summary>
        member inline Source: s: 'a -> 'a when 'a :> seq<'b>
"""
