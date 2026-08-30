module Telplin.Core.Tests.XmlDocTests

open NUnit.Framework
open Telplin.Core
open Telplin.Core.Tests.TestHelper

let private docRanges (code : string) =
    let resolver = TypedTree.Resolver.mkResolverForCode options false code

    (UntypedTree.Writer.mkSignature resolver code).XmlDocRanges
    |> List.map (fun r -> r.StartLine, r.EndLine)
    |> List.sort

[<Test>]
let ``docs of declarations the signature has are found, whole blocks, at every level`` () =
    let code =
        """
module A

/// A type.
type T =
    {
        /// A field.
        X : int
    }

    /// A member.
    member this.Double = this.X * 2

/// A binding,
/// over two lines.
let api (x : int) = x

module Nested =
    /// Nested.
    let inner () = 1
"""

    let expected : (int * int) list =
        [ (4, 4) ; (7, 7) ; (11, 11) ; (14, 15) ; (19, 19) ]

    Assert.That (docRanges code = expected, Is.True, $"%A{docRanges code}")

[<Test>]
let ``docs of private bindings the signature leaves out are not found`` () =
    let code =
        """
module A

/// Hidden.
let private helper (x : int) = x

/// Shown.
let api (x : int) = helper x
"""

    let expected : (int * int) list = [ (7, 7) ]
    Assert.That (docRanges code = expected, Is.True, $"%A{docRanges code}")
