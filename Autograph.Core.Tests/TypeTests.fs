module Autograph.Core.Tests.TypeTests

open System
open NUnit.Framework
open TestHelper

[<Test>]
let ``simple record`` () =
    mkSignature
        """
module A

type B = {
    Foo: string
    Bar: int
    SomethingElseButSomewhatLonger: float
}
"""
    |> shouldEqualWithPrepend
        """
module A

type B =
    { Foo: string
      Bar: int
      SomethingElseButSomewhatLonger: float }
"""

[<Test>]
let ``empty struct`` () =
    mkSignature
        """
namespace Foo

[<Struct>]
type Bar = struct end
"""
    |> shouldEqualWithPrepend
        """
namespace Foo

[<Struct>]
type Bar =
    struct
    end
"""

[<Test>]
let ``struct with value`` () =
    mkSignature
        """
namespace Foo

[<Struct>]
type Bar =
    val X : int
"""
    |> shouldEqualWithPrepend
        """
namespace Foo

[<Struct>]
type Bar =
    val X: int
"""

[<Test>]
let ``struct with constructor`` () =
    mkSignature
        """
namespace Foo

type Bar =
    struct
        val X : int
        new(x : int) = { X = x }
    end
"""
    |> shouldEqualWithPrepend
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
    mkSignature
        """
namespace Hej

type Foo =
    abstract member Bar : x : int -> y : int -> int
"""
    |> shouldEqualWithPrepend
        """
namespace Hej

type Foo =
    abstract member Bar: x: int -> y: int -> int
"""

[<Test>]
let ``type extension`` () =
    mkSignature
        """
module Hej

type System.Int32 with
    member i.PlusPlus () = i + 1
"""
    |> shouldEqualWithPrepend
        """
module Hej

type System.Int32 with

    member PlusPlus: unit -> int
"""

[<Test>]
let ``type with nested interface`` () =
    mkSignature
        """
namespace Hej

open System
        
type A =
    { B: int }
    interface IDisposable with
        member this.Dispose () = ()
"""
    |> shouldEqualWithPrepend
        """
namespace Hej

open System

type A =
    { B: int }

    interface IDisposable
"""

[<Test>]
let ``interface that inherits an interface`` () =
    mkSignature
        """
namespace B

type A =
    interface
        inherit IDisposable
    end
"""
    |> shouldEqualWithPrepend
        """
namespace B

type A =
    interface
        inherit IDisposable
    end
"""

[<Test>]
let ``static member value`` () =
    mkSignature
        """
namespace X

type State =
    {
        Files : string list
    }

    static member Empty : State = { Files = [] }
"""
    |> shouldEqualWithPrepend
        """
namespace X

type State =
    { Files: string list }

    static member Empty: State
"""

[<Test>]
let ``empty class with constructor`` () =
    mkSignature
        """
namespace X

type LSPFantomasService() =
    class end
"""
    |> shouldEqualWithPrepend
        """
namespace X

type LSPFantomasService =
    class
        new: unit -> LSPFantomasService
    end
"""
