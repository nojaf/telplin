open System
open System.IO

Path.GetExtension("foo")

let isPipedInput = Console.IsInputRedirected
let fsharpFiles = set [| ".fs"; ".fsi"; ".fsx" |]

let processLine (line: string) =
    let line = line.Trim()

    if line.Contains("-o:obj") then
        let idx = line.IndexOf("-o:")
        $"-o:{__SOURCE_DIRECTORY__}\\{line.Substring(idx + 3)}"
    elif fsharpFiles.Contains(Path.GetExtension(line)) then
        $"{__SOURCE_DIRECTORY__}\\{line}"
    elif line.StartsWith("--doc:") then
        let path = line.Split([| "--doc:" |], StringSplitOptions.RemoveEmptyEntries).[0]
        $"--doc:{__SOURCE_DIRECTORY__}\\{path}"
    else
        line

if not isPipedInput then
    printfn "no input piped"
else
    let lines =
        let lines = ResizeArray<string>()
        let mutable s = ""

        while not (isNull s) do
            s <- Console.ReadLine()
            lines.Add(s)

        lines

    let compilerLines =
        lines
        |> Seq.skipWhile (fun (s: string) -> s.Trim() <> "CoreCompile:")
        |> Seq.skip 1
        |> Seq.takeWhile (fun (s: string) -> s.Trim() <> "CopyFilesToOutputDirectory:")
        |> Seq.map processLine
        |> String.concat "\n"

    printfn "piped was:\n%A" compilerLines
