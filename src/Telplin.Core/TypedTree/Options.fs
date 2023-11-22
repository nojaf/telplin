module Telplin.Core.TypedTree.Options

open System
open System.Diagnostics
open System.IO
open System.Text.Json
open FSharp.Compiler.CodeAnalysis
open Telplin.Core.TypedTree.FSharpProjectExtensions

let fsharpFiles = set [| ".fs" ; ".fsi" ; ".fsx" |]

let isFSharpFile (file : string) =
    Set.exists (fun (ext : string) -> file.EndsWith ext) fsharpFiles

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
        psi.UseShellExecute <- false
        use ps = new Process ()
        ps.StartInfo <- psi
        ps.Start () |> ignore
        let output = ps.StandardOutput.ReadToEnd ()
        do! ps.WaitForExitAsync ()
        return output.Trim ()
    }
    |> Async.AwaitTask

let mkOptionsFromDesignTimeBuildAux (fsproj : FileInfo) (additionalArguments : string) =
    async {
        let targets =
            "Restore,ResolveAssemblyReferencesDesignTime,ResolveProjectReferencesDesignTime,ResolvePackageDependenciesDesignTime,FindReferenceAssembliesForReferences,_GenerateCompileDependencyCache,_ComputeNonExistentFileProperty,BeforeBuild,BeforeCompile,CoreCompile"

        let! json =
            dotnet
                fsproj.DirectoryName
                $"msbuild /t:%s{targets} /p:DesignTimeBuild=True /p:SkipCompilerExecution=True /p:ProvideCommandLineArgs=True --getItem:FscCommandLineArgs %s{additionalArguments}"

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

let verifyDotnetEightSDKIsPresent () =
    async {
        let tmp = Path.GetTempPath ()

        let! sdkList = dotnet tmp "--list-sdks"
        let sdkList = sdkList.Split ('\n', StringSplitOptions.RemoveEmptyEntries)

        let hasEight =
            sdkList
            |> Array.exists (fun line -> line.StartsWith ("8", StringComparison.Ordinal))

        if not hasEight then
            failwith "Telplin requires the dotnet 8 SDK to be installed on the machine. Got"
    }

type FullPath = string

type HighLevelFSharpProjectInfo =
    {
        FullPath : string
        ProduceReferenceAssembly : bool
        FSharpProjectReferences : Set<string>
    }

type HighLevelFSharpProjects private (projects : Set<HighLevelFSharpProjectInfo>) =
    static member Empty = HighLevelFSharpProjects Set.empty

    member x.Add (project : HighLevelFSharpProjectInfo) =
        HighLevelFSharpProjects (Set.add project projects)

    member x.HasProject (fullPath : FullPath) : bool =
        Set.exists (fun p -> p.FullPath = fullPath) projects

    member x.AllProjectPaths = projects |> Seq.map (fun hp -> hp.FullPath)

    member x.TryFind (fullPath : FullPath) : HighLevelFSharpProjectInfo option =
        projects |> Seq.tryFind (fun hp -> hp.FullPath = fullPath)

let findFSharpProjectReferences (fsproj : FullPath) : Async<HighLevelFSharpProjectInfo> =
    async {
        let pwd = Path.GetDirectoryName fsproj
        let! json = dotnet pwd "msbuild --getItem:ProjectReference"
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

        let! produceReferenceAssembly = dotnet pwd "msbuild --getProperty:ProduceReferenceAssembly"

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
    (fsproj : FullPath)
    (projects : HighLevelFSharpProjects)
    : Async<HighLevelFSharpProjects>
    =
    async {
        if projects.HasProject fsproj then
            return projects
        else

        let! highLevelFSharpProjectInfo = findFSharpProjectReferences fsproj
        let nextProjects = projects.Add highLevelFSharpProjectInfo

        let! combined =
            (async { return nextProjects }, highLevelFSharpProjectInfo.FSharpProjectReferences)
            ||> Seq.fold (fun (projectsAsync : Async<HighLevelFSharpProjects>) referencedFullPath ->
                async {
                    let! projects = projectsAsync

                    if projects.HasProject referencedFullPath then
                        return projects
                    else
                        let! refProjectInfo = findFSharpProjectReferences referencedFullPath
                        return projects.Add refProjectInfo
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

let mkOptionsFromDesignTimeBuild (fsproj : string) (additionalArguments : string) =
    async {
        do! verifyDotnetEightSDKIsPresent ()

        let fsproj = FileInfo fsproj

        if not fsproj.Exists then
            invalidArg (nameof fsproj) $"\"%s{fsproj.FullName}\" does not exist."

        let! allProjects = collectProjectReferences fsproj.FullName HighLevelFSharpProjects.Empty

        let! allProjectOptions =
            allProjects.AllProjectPaths
            |> Seq.map (fun fullPath ->
                let fsproj = FileInfo fullPath
                mkOptionsFromDesignTimeBuildAux fsproj additionalArguments
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
