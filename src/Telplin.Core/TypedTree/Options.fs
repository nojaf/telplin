module Telplin.Core.TypedTree.Options

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Text.Json
open FSharp.Compiler.CodeAnalysis
open Telplin.Core.TypedTree.FSharpProjectExtensions

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

            let fullPath = Path.Combine (projectFile.DirectoryName, line) |> Path.GetFullPath
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

type FullPath = string

let dotnet_msbuild (fsproj : FullPath) (args : string) : Async<string> =
    backgroundTask {
        let psi = ProcessStartInfo "dotnet"
        let pwd = Assembly.GetEntryAssembly().Location |> Path.GetDirectoryName
        psi.WorkingDirectory <- pwd
        psi.Arguments <- $"msbuild \"%s{fsproj}\" %s{args}"
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
            failwithf $"dotnet msbuild %s{args} \n in %s{pwd} failed with\n%s{error}"

        return output.Trim ()
    }
    |> Async.AwaitTask

let mkOptionsFromDesignTimeBuildAux (fsproj : FileInfo) (additionalArguments : string) =
    async {
        let! targetFrameworkJson =
            dotnet_msbuild fsproj.FullName "--getProperty:TargetFrameworks --getProperty:TargetFramework"

        let targetFramework =
            let tf, tfs =
                JsonDocument.Parse targetFrameworkJson
                |> fun json -> json.RootElement.GetProperty "Properties"
                |> fun properties ->
                    properties.GetProperty("TargetFramework").GetString (),
                    properties.GetProperty("TargetFrameworks").GetString ()

            if not (String.IsNullOrWhiteSpace tf) then
                tf
            else
                tfs.Split ';' |> Array.head

        // When CoreCompile does not need a rebuild, MSBuild will skip that target and thus will not populate the FscCommandLineArgs items.
        // To overcome this we want to force a design time build, using the NonExistentFile property helps prevent a cache hit.
        let nonExistentFile = Path.Combine ("__NonExistentSubDir__", "__NonExistentFile__")

        let properties =
            [
                "/p:Telplin=True"
                $"/p:TargetFramework=%s{targetFramework}"
                "/p:DesignTimeBuild=True"
                "/p:SkipCompilerExecution=True"
                "/p:ProvideCommandLineArgs=True"
                // See https://github.com/NuGet/Home/issues/13046
                "/p:RestoreUseStaticGraphEvaluation=False"
                // Avoid restoring with an existing lock file
                "/p:RestoreLockedMode=false"
                "/p:RestorePackagesWithLockFile=false"
                // We trick NuGet into believing there is no lock file create, if it does exist it will try and create it.
                " /p:NuGetLockFilePath=Telplin.lock"
                // Avoid skipping the CoreCompile target via this property.
                $"/p:NonExistentFile=\"%s{nonExistentFile}\""
                // https://learn.microsoft.com/en-us/nuget/reference/errors-and-warnings/nu1608
                "-warnAsMessage:NU1608"
            ]
            |> List.filter (String.IsNullOrWhiteSpace >> not)
            |> String.concat " "

        let targets =
            "ResolveAssemblyReferencesDesignTime,ResolveProjectReferencesDesignTime,ResolvePackageDependenciesDesignTime,FindReferenceAssembliesForReferences,_GenerateCompileDependencyCache,_ComputeNonExistentFileProperty,BeforeBuild,BeforeCompile,CoreCompile"

        let arguments =
            $"/restore /t:%s{targets} %s{properties} --getItem:FscCommandLineArgs %s{additionalArguments}"

        let! json = dotnet_msbuild fsproj.FullName arguments
        let jsonDocument = JsonDocument.Parse json

        let options =
            jsonDocument.RootElement.GetProperty("Items").GetProperty("FscCommandLineArgs").EnumerateArray ()
            |> Seq.map (fun arg -> arg.GetProperty("Identity").GetString ())
            |> Seq.toArray

        if Array.isEmpty options then
            return
                failwithf
                    $"Design time build for %s{fsproj.FullName} failed. CoreCompile was most likely skipped. `dotnet clean` might help here."
        else
            return mkOptions fsproj options
    }

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

let findFSharpProjectReferences (fsproj : FullPath) : Async<HighLevelFSharpProjectInfo> =
    async {
        let! json = dotnet_msbuild fsproj "--getItem:ProjectReference"
        let jsonDocument = JsonDocument.Parse json

        let references =
            jsonDocument.RootElement.GetProperty("Items").GetProperty("ProjectReference").EnumerateArray ()
            |> Seq.choose (fun projectItem ->
                let fullPath = projectItem.GetProperty("FullPath").GetString ()

                if fullPath.EndsWith (".fsproj", StringComparison.Ordinal) then
                    Some fullPath
                else
                    None
            )
            |> Set.ofSeq

        let! produceReferenceAssembly = dotnet_msbuild fsproj "--getProperty:ProduceReferenceAssembly"

        let doesProduceReferenceAssembly =
            not (String.IsNullOrWhiteSpace produceReferenceAssembly)
            && produceReferenceAssembly.Trim().Equals ("true", StringComparison.OrdinalIgnoreCase)

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
            ||> Set.fold (fun (projectsAsync : Async<HighLevelFSharpProjects>) referencedFullPath ->
                async {
                    let! projects = projectsAsync

                    if projects.HasProject referencedFullPath then
                        return projects
                    else
                        return! collectProjectReferences referencedFullPath projects
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

let mkOptionsFromDesignTimeBuildWithoutReferences
    (fsproj : string)
    (additionalArguments : string)
    : Async<FSharpProjectOptions>
    =
    async {
        let fsproj = FileInfo fsproj

        if not fsproj.Exists then
            invalidArg (nameof fsproj) $"\"%s{fsproj.FullName}\" does not exist."

        return! mkOptionsFromDesignTimeBuildAux fsproj additionalArguments
    }

let mkOptionsFromResponseFile responseFilePath =
    let compilerArgs = File.ReadAllLines responseFilePath
    mkOptions (FileInfo responseFilePath) compilerArgs
