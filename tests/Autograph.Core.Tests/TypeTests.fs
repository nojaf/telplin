module Autograph.Core.Tests.TypeTests

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
    new: string -> FormatException
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

// I'm not sure why but settings don't seem to have a signature.
// Try this out in fsi, the setter won't be printed.

[<Test>]
let ``member with get/set`` () =
    assertSignature
        """
module X

type X() =
    member x.Item
        with get (m: int) = ""
        and set (m:int) (y:int) = ()
"""
        """
module X

type X =
    new: unit -> X
    member Item: m: int -> string with get
"""
