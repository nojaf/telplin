module Telplin.Core.Tests.IncludePrivateBindingsTests

open NUnit.Framework
open TestHelper

[<Test>]
let ``private let binding is excluded`` () =
    assertSignatureWith
        id
        false
        """
module Telplin

let private a (b:string) : string = b
"""
        """
module Telplin
"""

[<Test>]
let ``private ctor is excluded`` () =
    assertSignatureWith
        id
        false
        """
module Telplin

type private T private () =
    class
    end
"""
        """
module Telplin

type private T =
    class
    end
"""

[<Test>]
let ``private ctor in struct excluded`` () =
    assertSignatureWith
        id
        false
        """
module Telplin

type Point2D =
    struct
        val X: float
        val Y: float
        private new (x: float, y: float) = { X = x; Y = y }

        member this.GetDistanceFrom(p: Point2D) =
            let dX = (p.X - this.X) ** 2.0
            let dY = (p.Y - this.Y) ** 2.0
            
            dX + dY
            |> sqrt
    end
"""
        """
module Telplin

type Point2D =
    struct
        val X: float
        val Y: float
        member GetDistanceFrom: p: Point2D -> float
    end
"""

[<Test>]
let ``private member is excluded`` () =
    assertSignatureWith
        id
        false
        """
module Telplin

type T =
    member private this.X () = 1
"""
        """
module Telplin

[<Class>]
type T =
    class
    end
"""

[<Test>]
let ``private augmentation is excluded`` () =
    assertSignatureWith
        id
        false
        """
module Telplin

open System

type String with
    member private this.V x y = x + y
"""
        """
module Telplin

open System
"""
