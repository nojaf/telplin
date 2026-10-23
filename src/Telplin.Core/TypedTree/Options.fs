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

        // The dotnet CLI blocks on sending telemetry at exit, which can take seconds on a restricted network
        if not (psi.Environment.ContainsKey "DOTNET_CLI_TELEMETRY_OPTOUT") then
            psi.Environment.Add ("DOTNET_CLI_TELEMETRY_OPTOUT", "1")

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

// https://learn.microsoft.com/en-us/nuget/reference/errors-and-warnings/nu1608
// https://learn.microsoft.com/en-us/nuget/reference/errors-and-warnings/nu1510
let nugetWarningsAsMessages = "-warnAsMessage:NU1608 -warnAsMessage:NU1510"

/// Restore the project and, through it, every project it references. This runs once, on its own:
/// a restore that is given a TargetFramework writes the assets file for that framework only, and
/// a design time build needs that property.
let restore (fsproj : FullPath) (additionalArguments : string) : Async<unit> =
    let properties =
        [
            "/p:Telplin=True"
            // See https://github.com/NuGet/Home/issues/13046
            "/p:RestoreUseStaticGraphEvaluation=False"
            // Avoid restoring with an existing lock file
            "/p:RestoreLockedMode=false"
            "/p:RestorePackagesWithLockFile=false"
            // We trick NuGet into believing there is no lock file create, if it does exist it will try and create it.
            "/p:NuGetLockFilePath=Telplin.lock"
            nugetWarningsAsMessages
        ]
        |> String.concat " "

    dotnet_msbuild fsproj $"/t:Restore %s{properties} %s{additionalArguments}"
    |> Async.Ignore

/// The design time build of a project that was restored already.
let mkOptionsFromDesignTimeBuildAux (fsproj : FileInfo) (targetFramework : string) (additionalArguments : string) =
    async {
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
                // Avoid skipping the CoreCompile target via this property.
                $"/p:NonExistentFile=\"%s{nonExistentFile}\""
                nugetWarningsAsMessages
            ]
            |> String.concat " "

        let targets =
            "ResolveAssemblyReferencesDesignTime,ResolveProjectReferencesDesignTime,ResolvePackageDependenciesDesignTime,FindReferenceAssembliesForReferences,_GenerateCompileDependencyCache,_ComputeNonExistentFileProperty,BeforeBuild,BeforeCompile,CoreCompile"

        let arguments =
            $"/t:%s{targets} %s{properties} --getItem:FscCommandLineArgs %s{additionalArguments}"

        let! json = dotnet_msbuild fsproj.FullName arguments
        let jsonDocument = JsonDocument.Parse json

        let options =
            jsonDocument.RootElement.GetProperty("Items").GetProperty("FscCommandLineArgs").EnumerateArray()
            |> Seq.map (fun arg -> arg.GetProperty("Identity").GetString())
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
        /// The TargetFramework the design time build uses, the first one for a project with several.
        TargetFramework : string
        FSharpProjectReferences : Set<string>
    }

type HighLevelFSharpProjects(projects : Set<HighLevelFSharpProjectInfo>) =
    static member Empty = HighLevelFSharpProjects Set.empty

    member x.Add (project : HighLevelFSharpProjectInfo) =
        HighLevelFSharpProjects (Set.add project projects)

    member x.HasProject (fullPath : FullPath) : bool =
        Set.exists (fun p -> p.FullPath = fullPath) projects

    member x.AllProjects = projects

    member x.TryFind (fullPath : FullPath) : HighLevelFSharpProjectInfo option =
        projects |> Seq.tryFind (fun hp -> hp.FullPath = fullPath)

/// Everything the design time build needs to know about a project up front, from one evaluation.
let findFSharpProjectReferences (fsproj : FullPath) : Async<HighLevelFSharpProjectInfo> =
    async {
        let! json =
            dotnet_msbuild
                fsproj
                "--getItem:ProjectReference --getProperty:ProduceReferenceAssembly --getProperty:TargetFramework --getProperty:TargetFrameworks"

        let jsonDocument = JsonDocument.Parse json
        let properties = jsonDocument.RootElement.GetProperty "Properties"

        let references =
            jsonDocument.RootElement.GetProperty("Items").GetProperty("ProjectReference").EnumerateArray()
            |> Seq.choose (fun projectItem ->
                let fullPath = projectItem.GetProperty("FullPath").GetString()

                if fullPath.EndsWith (".fsproj", StringComparison.Ordinal) then
                    Some fullPath
                else
                    None
            )
            |> Set.ofSeq

        let produceReferenceAssembly =
            properties.GetProperty("ProduceReferenceAssembly").GetString()

        let doesProduceReferenceAssembly =
            not (String.IsNullOrWhiteSpace produceReferenceAssembly)
            && produceReferenceAssembly.Trim().Equals("true", StringComparison.OrdinalIgnoreCase)

        let targetFramework =
            let tf, tfs =
                properties.GetProperty("TargetFramework").GetString(),
                properties.GetProperty("TargetFrameworks").GetString()

            if not (String.IsNullOrWhiteSpace tf) then
                tf
            else
                tfs.Split ';' |> Array.head

        return
            {
                FullPath = fsproj
                ProduceReferenceAssembly = doesProduceReferenceAssembly
                TargetFramework = targetFramework
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

        // The walk over the references only evaluates projects, it can go while the restore runs.
        let! restored = Async.StartChild (restore fsproj.FullName additionalArguments)

        let! allProjects =
            collectProjectReferences fsproj.FullName HighLevelFSharpProjects.Empty

        do! restored

        let! allProjectOptions =
            allProjects.AllProjects
            |> Seq.map (fun project ->
                mkOptionsFromDesignTimeBuildAux (FileInfo project.FullPath) project.TargetFramework additionalArguments
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

        let! restored = Async.StartChild (restore fsproj.FullName additionalArguments)
        let! project = findFSharpProjectReferences fsproj.FullName
        do! restored
        return! mkOptionsFromDesignTimeBuildAux fsproj project.TargetFramework additionalArguments
    }

let compileItems (fsproj : string) (additionalArguments : string) : Async<string array> =
    async {
        let! json = dotnet_msbuild fsproj $"--getItem:Compile %s{additionalArguments}"
        let jsonDocument = JsonDocument.Parse json

        return
            jsonDocument.RootElement.GetProperty("Items").GetProperty("Compile").EnumerateArray()
            |> Seq.map (fun item -> Path.GetFullPath (item.GetProperty("FullPath").GetString()))
            |> Seq.toArray
    }

let mkOptionsFromResponseFile responseFilePath =
    let compilerArgs = File.ReadAllLines responseFilePath
    mkOptions (FileInfo responseFilePath) compilerArgs
