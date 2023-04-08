#r "nuget: Argu, 6.1.1"
#r "nuget: Suave, 2.6.2"
#r "nuget: FSharp.Data.Adaptive, 1.2.13"
#r "nuget: CliWrap, 3.6.0"

open System
open System.Collections.Concurrent
open System.IO
open System.Text
open System.Net
open System.Threading
open System.Threading.Tasks
open CliWrap
open CliWrap.EventStream
open FSharp.Data.Adaptive
open Suave
open Suave.Logging
open Suave.Successful
open Suave.Files
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open Suave.Filters
open Suave.Operators
open Suave.RequestErrors

let exitTask = TaskCompletionSource<unit> ()
let cts = new CancellationTokenSource ()

Console.CancelKeyPress.Add (fun _ ->
    printfn "Goodbye"
    cts.Cancel ()
    exitTask.SetResult ()
)

let dotnet pwd identifier (args : string) =
    Cli
        .Wrap("dotnet")
        .WithArguments(args)
        .WithWorkingDirectory(pwd)
        .Observe (cancellationToken = cts.Token)
    |> Observable.map (fun ev -> identifier, ev)

let serverDir = Path.Combine (__SOURCE_DIRECTORY__, "..", "server")

Observable.merge //
    (dotnet serverDir "server" "watch run")
    (dotnet __SOURCE_DIRECTORY__ "fable" "fable watch -e .js --define DEBUG")
|> Observable.subscribe (fun (identifier, ev) ->
    let identifier = identifier.PadRight 6 |> sprintf "[%s]"

    match ev with
    | :? StartedCommandEvent as startedEvent -> printfn $"%s{identifier} Started: %i{startedEvent.ProcessId}"
    | :? StandardOutputCommandEvent as stdOutEvent -> printfn $"%s{identifier}: %s{stdOutEvent.Text}"
    | :? ExitedCommandEvent as exitedEvent -> printfn $"%s{identifier} Exited: %A{exitedEvent.ExitCode}"
    | :? StandardErrorCommandEvent as stdErrEvent -> printfn $"%s{identifier} ERR: %s{stdErrEvent.Text}"
    | _ -> ()
)

let connectedClients = ConcurrentDictionary<WebSocket, unit> ()

let ws (webSocket : WebSocket) (context : HttpContext) =
    context.runtime.logger.info (Message.eventX $"New websocket connection")
    connectedClients.TryAdd (webSocket, ()) |> ignore

    socket {
        let mutable loop = true

        while loop do
            let! msg = webSocket.read ()

            match msg with
            | Text, data, true ->
                let str = Encoding.UTF8.GetString data
                context.runtime.logger.info (Message.eventX $"Received %s{str} from client")
                let response = sprintf "response to %s" str
                let byteResponse = response |> Encoding.UTF8.GetBytes |> ByteSegment
                do! webSocket.send Text byteResponse true

            | Close, _, _ ->
                context.runtime.logger.info (Message.eventX "Closing connection")
                connectedClients.TryRemove (webSocket) |> ignore
                let emptyResponse = [||] |> ByteSegment
                do! webSocket.send Close emptyResponse true
                loop <- false

            | _ -> ()
    }

let broadCastReload (msg : string) =
    let msg = msg |> Encoding.UTF8.GetBytes |> ByteSegment

    connectedClients.Keys
    |> Seq.map (fun client ->
        async {
            let! _ = client.send Text msg true
            ()
        }
    )
    |> Async.Parallel
    |> Async.Ignore
    |> Async.RunSynchronously

// CSS watcher
let cssWatcher =
    AdaptiveFile
        .GetLastWriteTime(Path.Combine (__SOURCE_DIRECTORY__, "online-tool.css"))
        .AddCallback (fun _ -> broadCastReload "online-tool.css")

// index.html watcher
let indexHtmlWatcher =
    AdaptiveFile
        .GetLastWriteTime(Path.Combine (__SOURCE_DIRECTORY__, "index.html"))
        .AddCallback (fun _ -> broadCastReload "full")

// js watcher
let jsWatcher =
    AdaptiveDirectory.GetFiles (__SOURCE_DIRECTORY__, pattern = ".*\\.js", recursive = true)
    |> ASet.map (fun file -> AdaptiveFile.GetLastWriteTime file.FullName)
    |> ASet.flattenA
    |> ASet.tryMax
    |> fun aval -> aval.AddCallback (fun _ -> broadCastReload "full")

let envs =
    OK "export const API_ROOT = \"http://localhost:8906\";"
    >=> Writers.setMimeType "text/javascript"

let server =
    choose [
        path "/ws" >=> handShake ws
        GET >=> path "/telplin/env.js" >=> envs
        GET >=> path "/" >=> file "index.html"
        GET >=> browseHome
        NOT_FOUND "Page not found."
    ]

let conf =
    { defaultConfig with
        cancellationToken = cts.Token
        homeFolder = Some __SOURCE_DIRECTORY__
        compressedFilesFolder = Some (Path.GetTempPath ())
        bindings = [ HttpBinding.create HTTP IPAddress.Loopback 8900us ]
    }

let _, suaveServer = startWebServerAsync conf server
Async.Start (suaveServer, cts.Token)

cts.Token.Register (fun _ ->
    cssWatcher.Dispose ()
    indexHtmlWatcher.Dispose ()
    jsWatcher.Dispose ()
)

exitTask.Task.Wait ()
