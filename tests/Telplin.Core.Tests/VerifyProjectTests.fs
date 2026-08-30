module Telplin.Core.Tests.VerifyProjectTests

open System.IO
open NUnit.Framework
open Telplin.Core
open Telplin.Core.Tests.TestHelper

/// A signature that hides a binding makes the compiler report it as unused (FS1182). A project
/// that promotes that warning to an error must still pass the check: the code compiled before.
[<Test>]
let ``unused binding promoted to an error by the project does not fail the check`` () =
    let implementationPath =
        Path.Combine (Path.GetTempPath (), $"%s{Path.GetRandomFileName ()}.fs")

    File.WriteAllText (
        implementationPath,
        """
module A

let hidden (x: int) = x + 1
let visible (x: int) = x - 1
"""
    )

    let options =
        { options with
            SourceFiles = [| implementationPath |]
            OtherOptions = Array.append options.OtherOptions [| "--warnon:1182" ; "--warnaserror:1182" |]
        }

    try
        let diagnostics =
            TypedTree.Resolver.typeCheckProjectWithSignatures
                options
                [
                    implementationPath,
                    """
module A

val visible: x: int -> int
"""
                ]

        Assert.That (diagnostics, Is.Empty, sprintf "%A" diagnostics)
    finally
        File.Delete implementationPath
