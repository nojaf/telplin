module Telplin.Core.Tests.ModuleOrNamespaceTests

open NUnit.Framework
open TestHelper

[<Test>]
let ``single namespace`` () =
    assertSignature
        """
namespace A

type Foo = int
"""
        """
namespace A

type Foo = int
"""

[<Test>]
let ``module abbreviation`` () =
    assertSignature
        """
namespace Root

module A =

    let a = 0

module B = A
"""
        """
namespace Root

module A =
    val a: int

module B = A
"""

[<Test>]
let ``nested module`` () =
    assertSignature
        """
namespace Company

module A =

    let a = 0
"""
        """
namespace Company

module A =
    val a: int
"""

[<Test>]
let ``module with top level attribute`` () =
    assertSignature
        """
module A

[<System.Runtime.InteropServices.DllImport("")>]
do ()
"""
        """
module A
"""

[<Test>]
let ``type in a module with ModuleSuffix is not qualified with the module name, 71`` () =
    assertSignature
        """
namespace Meh.Bar

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Telplin =
    type V =
        { X : int }
        static member Zero = { X = 0}
"""
        """
namespace Meh.Bar

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Telplin =
    type V =
        {
            X: int
        }

        static member Zero: V
"""

[<Test>]
let ``type in a module with an implicit ModuleSuffix is not qualified with the module name, 71`` () =
    assertSignature
        """
namespace Meh.Bar

type Telplin = | A

[<RequireQualifiedAccess>]
module Telplin =
    type V = { X : int }
    let zero : V = { X = 0 }
    let mk (v: V) : Telplin = A
    module Inner =
        let lst : V list * V = [], { X = 0 }
"""
        """
namespace Meh.Bar

type Telplin = | A

[<RequireQualifiedAccess>]
module Telplin =
    type V = { X: int }
    val zero: V
    val mk: v: V -> Telplin

    module Inner =
        val lst: V list * V
"""

[<Test>]
let ``nested module with nothing left in it is left out`` () =
    assertSignatureWithoutPrivate
        """
module Telplin

module Helpers =
    let private a = 1
    let private b = 2

module Outer =
    module Inner =
        let private c = 3

let x = 1
"""
        """
module Telplin

val x: int
"""

[<Test>]
let ``private nested module is left out even when it has bindings`` () =
    assertSignatureWithoutPrivate
        """
module Telplin

module private Helpers =
    let a = 1
    let b = 2

let x = Helpers.a
"""
        """
module Telplin

val x: int
"""
