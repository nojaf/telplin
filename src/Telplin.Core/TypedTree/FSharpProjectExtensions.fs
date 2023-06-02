module Telplin.Core.TypedTree.FSharpProjectExtensions

open FSharp.Compiler.CodeAnalysis

type FSharpProjectOptions with

    member x.Defines : string list =
        x.OtherOptions
        |> Seq.choose (fun (option : string) ->
            if option.StartsWith "--define:" then
                Some (option.Substring 9)
            else
                None
        )
        |> Seq.toList
