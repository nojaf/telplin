module WebSocketClient

open System
open Fable.Core.JsInterop
open Browser.Types
open Browser.WebSocket
open Browser.Dom
open Browser.Url

let private ws = WebSocket.Create $"ws://{window.location.host}/ws"

ws.onmessage <-
    fun (ev : MessageEvent) ->
        let data : string = !!ev.data
        console.log ("Received", data)

        if data = "full" then
            window.location.reload ()
        elif data.EndsWith (".css", StringComparison.Ordinal) then
            let linkTag = document.querySelector $"link[href*='{data}']" :?> HTMLLinkElement

            if not (isNullOrUndefined linkTag) then
                let href = URL.Create linkTag.href
                let ticks = Fable.Core.JS.Constructors.Date.Create().Ticks
                href.searchParams.set ("v", !!ticks)
                linkTag.href <- href.ToString ()

            ()

ws.onopen <- fun _ -> console.log "WebSocket opened"
window.onbeforeunload <- fun _ -> ws.close ()
