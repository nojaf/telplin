module Telplin.Core.TypedTree.Options

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Text.Json
open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open Telplin.Core.TypedTree.FSharpProjectExtensions

/// Stolen from https://github.com/ionide/proj-info/blob/62e15bb4ab5ac534423942012e623171601e4f95/src/Ionide.ProjInfo/Utils.fs#L7-L105
module Paths =
    let isWindows = RuntimeInformation.IsOSPlatform (OSPlatform.Windows)
    let isMac = RuntimeInformation.IsOSPlatform (OSPlatform.OSX)
    let isLinux = RuntimeInformation.IsOSPlatform (OSPlatform.Linux)

    let isUnix = isLinux || isMac

    let dotnetBinaryName = if isUnix then "dotnet" else "dotnet.exe"

    let potentialDotnetHostEnvVars =
        [
            "DOTNET_HOST_PATH", id // is a full path to dotnet binary
            "DOTNET_ROOT", (fun s -> Path.Combine (s, dotnetBinaryName)) // needs dotnet binary appended
            "DOTNET_ROOT(x86)", (fun s -> Path.Combine (s, dotnetBinaryName))
        ] // needs dotnet binary appended

    let existingEnvVarValue envVarValue =
        match envVarValue with
        | null
        | "" -> None
        | other -> Some other

    let checkExistence (f : FileInfo) = if f.Exists then Some f else None

    let tryFindFromEnvVar () =
        potentialDotnetHostEnvVars
        |> List.tryPick (fun (envVar, transformer) ->
            match Environment.GetEnvironmentVariable envVar |> existingEnvVarValue with
            | Some varValue -> transformer varValue |> FileInfo |> checkExistence
            | None -> None
        )

    let PATHSeparator = if isUnix then ':' else ';'

    let tryFindFromPATH () =
        System.Environment
            .GetEnvironmentVariable("PATH")
            .Split (PATHSeparator, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.tryPick (fun d -> Path.Combine (d, dotnetBinaryName) |> FileInfo |> checkExistence)

    let tryFindFromDefaultDirs () =
        let windowsPath = $"C:\\Program Files\\dotnet\\{dotnetBinaryName}"
        let macosPath = $"/usr/local/share/dotnet/{dotnetBinaryName}"
        let linuxPath = $"/usr/share/dotnet/{dotnetBinaryName}"

        let tryFindFile p = FileInfo p |> checkExistence

        if isWindows then tryFindFile windowsPath
        else if isMac then tryFindFile macosPath
        else if isLinux then tryFindFile linuxPath
        else None

    /// <summary>
    /// provides the path to the `dotnet` binary running this library, respecting various dotnet <see href="https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-environment-variables#dotnet_root-dotnet_rootx86%5D">environment variables</see>.
    /// Also probes the PATH and checks the default installation locations
    /// </summary>
    let dotnetRoot =
        lazy
            (tryFindFromEnvVar ()
             |> Option.orElseWith tryFindFromPATH
             |> Option.orElseWith tryFindFromDefaultDirs)

let fsharpFiles = set [| ".fs" ; ".fsi" ; ".fsx" |]

let isFSharpFile (file : string) =
    Set.exists (fun (ext : string) -> file.EndsWith (ext, StringComparison.Ordinal)) fsharpFiles

let mkOptions (projectFile : FileInfo) (compilerArgs : string array) =
    let sourceFiles =
        compilerArgs
        |> Array.choose (fun (line : string) ->
            if not (isFSharpFile line) then
                None
            else

            let fullPath = Path.Combine (projectFile.DirectoryName, line)
            if File.Exists fullPath then Some fullPath else None
        )

    let otherOptions =
        compilerArgs |> Array.filter (fun line -> not (isFSharpFile line))

    {
        ProjectFileName = projectFile.FullName
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

let dotnet (pwd : string) (args : string) : Async<string> =
    backgroundTask {
        let psi = ProcessStartInfo "dotnet"
        psi.WorkingDirectory <- pwd
        psi.Arguments <- args
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        use ps = new Process ()
        ps.StartInfo <- psi
        ps.Start () |> ignore
        let output = ps.StandardOutput.ReadToEnd ()
        let error = ps.StandardError.ReadToEnd ()
        do! ps.WaitForExitAsync ()

        if not (String.IsNullOrWhiteSpace error) then
            failwithf $"In %s{pwd}:\ndotnet %s{args} failed with\n%s{error}"

        return output.Trim ()
    }
    |> Async.AwaitTask

let mkOptionsFromDesignTimeBuildAux (msbuildBinary : string) (fsproj : FileInfo) (additionalArguments : string) =
    async {
        let targets =
            "Restore,ResolveAssemblyReferencesDesignTime,ResolveProjectReferencesDesignTime,ResolvePackageDependenciesDesignTime,FindReferenceAssembliesForReferences,_GenerateCompileDependencyCache,_ComputeNonExistentFileProperty,BeforeBuild,BeforeCompile,CoreCompile"

        let! targetFrameworkJson =
            dotnet
                fsproj.DirectoryName
                $"\"%s{msbuildBinary}\" --getProperty:TargetFrameworks --getProperty:TargetFramework"

        let targetFramework =
            let tf, tfs =
                JsonDocument.Parse targetFrameworkJson
                |> fun json -> json.RootElement.GetProperty ("Properties")
                |> fun properties ->
                    properties.GetProperty("TargetFramework").GetString (),
                    properties.GetProperty("TargetFrameworks").GetString ()

            if not (String.IsNullOrWhiteSpace tf) then
                tf
            else
                tfs.Split ';' |> Array.head

        let fscToolPath =
            Paths.dotnetRoot.Value
            |> Option.map (fun f -> $"/p:FscToolPath=\"%s{f.DirectoryName}\"")
            |> Option.defaultValue String.Empty

        let fscToolExe =
            Paths.dotnetRoot.Value
            |> Option.map (fun f -> $"/p:FscToolExe=\"%s{f.Name}\"")
            |> Option.defaultValue String.Empty

        let properties =
            [
                $"/p:TargetFramework=%s{targetFramework}"
                "/p:DesignTimeBuild=True"
                "/p:SkipCompilerExecution=True"
                "/p:ProvideCommandLineArgs=True"
                // See https://github.com/NuGet/Home/issues/13046
                "/p:RestoreUseStaticGraphEvaluation=False"
                fscToolPath
                fscToolExe
            ]
            |> List.filter (String.IsNullOrWhiteSpace >> not)
            |> String.concat " "

        let arguments =
            $"\"%s{msbuildBinary}\" /t:%s{targets} %s{properties} --getItem:FscCommandLineArgs %s{additionalArguments} -bl"

        let! json = dotnet fsproj.DirectoryName arguments
        let jsonDocument = JsonDocument.Parse json

        let options =
            jsonDocument.RootElement
                .GetProperty("Items")
                .GetProperty("FscCommandLineArgs")
                .EnumerateArray ()
            |> Seq.map (fun arg -> arg.GetProperty("Identity").GetString ())
            |> Seq.toArray

        return mkOptions fsproj options
    }

// 8.0.100 [C:\Program Files\dotnet\sdk]
let sdkListItemPattern = "^(\d+\.\d+\.\d+)\s+\[(.*?)\]$"

let tryFindDotnetEightSDKMSBuildBinary () =
    async {
        let tmp = Path.GetTempPath ()

        let! sdkList = dotnet tmp "--list-sdks"
        let sdkList = sdkList.Split ('\n', StringSplitOptions.RemoveEmptyEntries)

        return
            sdkList
            |> Array.tryPick (fun line ->
                let hasEight = line.StartsWith ("8", StringComparison.Ordinal)

                if not hasEight then
                    None
                else

                let m = Regex.Match (line, sdkListItemPattern)

                if not m.Success then
                    None
                else

                let version = m.Groups.[1].Value
                let path = m.Groups.[2].Value
                let binaryPath = Path.Combine (path, version, "MSBuild.dll")

                if not (File.Exists binaryPath) then
                    None
                else
                    Some binaryPath
            )
    }

type FullPath = string

type HighLevelFSharpProjectInfo =
    {
        FullPath : string
        ProduceReferenceAssembly : bool
        FSharpProjectReferences : Set<string>
    }

type HighLevelFSharpProjects(projects : Set<HighLevelFSharpProjectInfo>) =
    static member Empty = HighLevelFSharpProjects Set.empty

    member x.Add (project : HighLevelFSharpProjectInfo) =
        HighLevelFSharpProjects (Set.add project projects)

    member x.HasProject (fullPath : FullPath) : bool =
        Set.exists (fun p -> p.FullPath = fullPath) projects

    member x.AllProjectPaths = projects |> Seq.map (fun hp -> hp.FullPath)

    member x.TryFind (fullPath : FullPath) : HighLevelFSharpProjectInfo option =
        projects |> Seq.tryFind (fun hp -> hp.FullPath = fullPath)

let findFSharpProjectReferences (msbuildBinary : string) (fsproj : FullPath) : Async<HighLevelFSharpProjectInfo> =
    async {
        let pwd = Path.GetDirectoryName fsproj
        let! json = dotnet pwd $"\"%s{msbuildBinary}\" --getItem:ProjectReference"
        let jsonDocument = JsonDocument.Parse json

        let references =
            jsonDocument.RootElement
                .GetProperty("Items")
                .GetProperty("ProjectReference")
                .EnumerateArray ()
            |> Seq.choose (fun projectItem ->
                let fullPath = projectItem.GetProperty("FullPath").GetString ()

                if fullPath.EndsWith (".fsproj", StringComparison.Ordinal) then
                    Some fullPath
                else
                    None
            )
            |> Set.ofSeq

        let! produceReferenceAssembly = dotnet pwd $"\"%s{msbuildBinary}\" --getProperty:ProduceReferenceAssembly"

        let doesProduceReferenceAssembly =
            not (String.IsNullOrWhiteSpace produceReferenceAssembly)
            && produceReferenceAssembly
                .Trim()
                .Equals ("true", StringComparison.OrdinalIgnoreCase)

        return
            {
                FullPath = fsproj
                ProduceReferenceAssembly = doesProduceReferenceAssembly
                FSharpProjectReferences = references
            }
    }

let rec collectProjectReferences
    (msbuildBinary : string)
    (fsproj : FullPath)
    (projects : HighLevelFSharpProjects)
    : Async<HighLevelFSharpProjects>
    =
    async {
        if projects.HasProject fsproj then
            return projects
        else

        let! highLevelFSharpProjectInfo = findFSharpProjectReferences msbuildBinary fsproj
        let nextProjects = projects.Add highLevelFSharpProjectInfo

        let! combined =
            (async { return nextProjects }, highLevelFSharpProjectInfo.FSharpProjectReferences)
            ||> Set.fold (fun (projectsAsync : Async<HighLevelFSharpProjects>) referencedFullPath ->
                async {
                    let! projects = projectsAsync

                    if projects.HasProject referencedFullPath then
                        return projects
                    else
                        return! collectProjectReferences msbuildBinary referencedFullPath projects
                }
            )

        return combined
    }

let getFSharpOptionsByFullPath (projects : FSharpProjectOptions array) (fullPath : string) : FSharpProjectOptions =
    projects |> Array.find (fun fpo -> fpo.ProjectFileName = fullPath)

let mkFSharpReferencedProjects
    (allProjects : HighLevelFSharpProjects)
    (projects : FSharpProjectOptions array)
    (highLevelFSharpProjectInfo : HighLevelFSharpProjectInfo)
    =
    highLevelFSharpProjectInfo.FSharpProjectReferences
    |> Seq.map (fun refFullPath ->
        let fsharpOptions = getFSharpOptionsByFullPath projects refFullPath

        let projectOutputFile =
            let output = fsharpOptions.Output

            match allProjects.TryFind refFullPath with
            | Some { ProduceReferenceAssembly = true } ->
                let objFolder = Path.GetDirectoryName output
                Path.Combine (objFolder, "ref", Path.GetFileName output)
            | None
            | Some _ -> output

        FSharpReferencedProject.FSharpReference (projectOutputFile, fsharpOptions)
    )
    |> Seq.toArray

let mkOptionsFromDesignTimeBuild (fsproj : string) (additionalArguments : string) : Async<FSharpProjectOptions> =
    async {
        let! msbuildBinary = tryFindDotnetEightSDKMSBuildBinary ()

        match msbuildBinary with
        | None -> return failwith "Telplin requires the dotnet 8 SDK to be installed on the machine. Could not find it!"
        | Some msbuildBinary ->

        let fsproj = FileInfo fsproj

        if not fsproj.Exists then
            invalidArg (nameof fsproj) $"\"%s{fsproj.FullName}\" does not exist."

        let! allProjects = collectProjectReferences msbuildBinary fsproj.FullName HighLevelFSharpProjects.Empty

        let! allProjectOptions =
            allProjects.AllProjectPaths
            |> Seq.map (fun fullPath ->
                let fsproj = FileInfo fullPath
                mkOptionsFromDesignTimeBuildAux msbuildBinary fsproj additionalArguments
            )
            |> Async.Parallel

        let allProjectOptionWithReferences =
            allProjectOptions
            |> Array.map (fun projectOptions ->
                match allProjects.TryFind projectOptions.ProjectFileName with
                | None -> projectOptions
                | Some highLevelFSharpProjectInfo ->
                    let fsharpReferences =
                        mkFSharpReferencedProjects allProjects allProjectOptions highLevelFSharpProjectInfo

                    { projectOptions with
                        ReferencedProjects = fsharpReferences
                    }
            )

        let currentProject =
            getFSharpOptionsByFullPath allProjectOptionWithReferences fsproj.FullName

        return currentProject
    }

let mkOptionsFromResponseFile responseFilePath =
    let compilerArgs = File.ReadAllLines responseFilePath
    mkOptions (FileInfo responseFilePath) compilerArgs
