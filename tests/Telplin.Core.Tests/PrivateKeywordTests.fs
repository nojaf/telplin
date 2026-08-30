module Telplin.Core.Tests.PrivateKeywordTests

open NUnit.Framework
open Telplin.Core

let private positions (code : string) =
    UntypedTree.Writer.redundantPrivateKeywords [] code
    |> List.map (fun r -> r.StartLine, r.StartColumn, r.EndColumn)

[<Test>]
let ``private on let bindings is found, at the top level and in nested modules`` () =
    let code =
        """
module A

let private helper (x: int) = x
let api (x: int) = helper x

module Nested =
    let private inner () = 1
"""

    let expected : (int * int * int) list = [ (4, 4, 11) ; (8, 8, 15) ]
    Assert.That (positions code = expected, Is.True, $"%A{positions code}")

[<Test>]
let ``private on members is left alone`` () =
    let code =
        """
module A

type T() =
    member private this.Hidden = 1
    member this.Shown = this.Hidden
"""

    Assert.That (positions code, Is.Empty)
