module Autograph.Core.Tests.TypeTests

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
