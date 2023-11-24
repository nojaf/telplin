module Telplin.Core.TypedTree.FSharpProjectExtensions

open System
open System.IO
open FSharp.Compiler.CodeAnalysis

module Option =
    let orFailWith (message : string) (o : 't option) =
        match o with
        | Some v -> v
        | None -> failwith message

type FSharpProjectOptions with

    member x.Defines : string list =
        x.OtherOptions
        |> Seq.choose (fun (option : string) ->
            if option.StartsWith ("--define:", StringComparison.Ordinal) then
                Some (option.Substring 9)
            else
                None
        )
        |> Seq.toList

    /// Find the output as full path, throws an exception if the argument was not found in the OtherOptions.
    member x.Output =
        x.OtherOptions
        |> Array.tryPick (fun option ->
            if option.StartsWith ("-o:", StringComparison.Ordinal) then
                Some (option.Substring 3)
            elif option.StartsWith ("--out:", StringComparison.Ordinal) then
                Some (option.Substring 6)
            else
                None
        )
        |> Option.map (fun output -> Path.Combine (Path.GetDirectoryName x.ProjectFileName, output))
        |> Option.orFailWith $"%s{x.ProjectFileName} does not have the output argument"
