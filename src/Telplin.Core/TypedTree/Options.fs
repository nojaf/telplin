module Telplin.Core.TypedTree.Options

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open FSharp.Compiler.CodeAnalysis

let fsharpFiles = set [| ".fs" ; ".fsi" ; ".fsx" |]

let isFSharpFile (file : string) =
    Seq.exists (fun (ext : string) -> file.EndsWith ext) fsharpFiles

let mkOptions (compilerArgs : string array) =
    let sourceFiles =
        compilerArgs
        |> Array.filter (fun (line : string) -> isFSharpFile line && File.Exists line)

    let otherOptions =
        compilerArgs |> Array.filter (fun line -> not (isFSharpFile line))

    {
        ProjectFileName = "Project"
        ProjectId = None
        SourceFiles = sourceFiles
        OtherOptions = otherOptions
        ReferencedProjects = [||]
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = false
        LoadTime = DateTime.Now
        UnresolvedReferences = None
        OriginalLoadReferences = []
        Stamp = None
    }

let dotnet pwd args =
    let psi = ProcessStartInfo "dotnet"
    psi.WorkingDirectory <- pwd
    psi.Arguments <- args
    psi.RedirectStandardOutput <- true
    psi.UseShellExecute <- false
    use ps = new Process ()
    ps.StartInfo <- psi
    ps.Start () |> ignore
    let output = ps.StandardOutput.ReadToEnd ()
    ps.WaitForExit ()
    output.Trim ()

// TODO: this won't work when there are wildcards in the CompileItems

let mkOptionsFromDesignTimeBuild (fsproj : string) (additionalArguments : string) =
    if not (File.Exists fsproj) then
        invalidArg (nameof fsproj) $"\"%s{fsproj}\" does not exist."

    // Move fsproj to temp folder
    let pwd = Path.Combine (Path.GetTempPath (), Guid.NewGuid().ToString "N")

    try
        let dir = DirectoryInfo pwd
        dir.Create ()

        let version = dotnet pwd "--version"

        if version <> "8.0.100-rc.2.23502.2" then
            failwith $"Expected the SDK to be 8.0.100-rc.2.23502.2 in %s{pwd}"

        let tmpFsproj = Path.Combine (pwd, Path.GetFileName fsproj)
        File.Copy (fsproj, tmpFsproj)

        let targets =
            "Restore,ResolveAssemblyReferencesDesignTime,ResolveProjectReferencesDesignTime,ResolvePackageDependenciesDesignTime,FindReferenceAssembliesForReferences,_GenerateCompileDependencyCache,_ComputeNonExistentFileProperty,BeforeBuild,BeforeCompile,CoreCompile"

        let json =
            dotnet
                pwd
                $"msbuild /t:%s{targets} /p:DesignTimeBuild=True /p:SkipCompilerExecution=True /p:ProvideCommandLineArgs=True p:DesignTimeBuild=True --getItem:FscCommandLineArgs %s{additionalArguments}"

        let jsonDocument = JsonDocument.Parse json

        let options =
            jsonDocument.RootElement
            |> fun root -> root.GetProperty("Items").GetProperty("FscCommandLineArgs").EnumerateArray ()
            |> Seq.map (fun arg -> arg.GetProperty("Identity").GetString ())
            |> Seq.toArray

        mkOptions options
    finally
        if Directory.Exists pwd then
            Directory.Delete (pwd, true)

let mkOptionsFromResponseFile responseFilePath =
    let compilerArgs = File.ReadAllLines responseFilePath
    mkOptions compilerArgs
