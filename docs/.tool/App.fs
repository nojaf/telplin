module Temp

open Fable.Core
open Fable.Core.JsInterop
open Elmish
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.UseElmish
open Fetch.Types
open Zanaptak.TypedCssClasses

[<Literal>]
let StyleSheet = "./online-tool.css"

importSideEffects StyleSheet
type Bootstrap = CssClasses<StyleSheet, Naming.PascalCase>

[<RequireQualifiedAccess>]
type MonacoEditorProp =
    | Height of string
    | DefaultLanguage of string
    | DefaultValue of string
    | OnChange of (string -> unit)
    | Options of obj

let inline private MonacoEditor (props : MonacoEditorProp list) : ReactElement =
    ofImport "default" "@monaco-editor/react" (keyValueList CaseRules.LowerFirst props) []

let url =
    "https://g8pt5e44zi.execute-api.eu-west-3.amazonaws.com/telplin-main-stage-230a206/telplin/signature"
//"http://localhost:8906/telplin/signature"

type Model = {
    Implementation : string
    Signature : string
    IsLoading : bool
}

type Msg =
    | UpdateImplementation of string
    | FetchSignature
    | SignatureReceived of string

let init _ =
    {
        Implementation =
            """module Telplin

let v (a:int) b = a + b
"""
        Signature = ""
        IsLoading = false
    },
    Cmd.none

let fetchSignature implementation dispatch =
    let options =
        Fetch.requestProps [
            Fetch.requestHeaders [ ContentType "text/plain" ]
            Method HttpMethod.POST
            Body !^implementation
        ]

    GlobalFetch.fetch (RequestInfo.Url url, options)
    |> Promise.bind (fun response -> response.text () |> Promise.map (fun content -> response.Status, content))
    |> Promise.iter (fun (status, content) ->
        match status with
        | 200 -> dispatch (SignatureReceived content)
        | _ -> printfn "weird response %i:\n%s" status content
    )

let update msg model =
    match msg with
    | UpdateImplementation implementation ->
        { model with
            Implementation = implementation
        },
        Cmd.none
    | FetchSignature -> { model with IsLoading = true }, Cmd.ofSub (fetchSignature model.Implementation)
    | SignatureReceived signature ->
        { model with
            IsLoading = false
            Signature = signature
        },
        Cmd.none

[<ReactComponent>]
let App () =
    let model, dispatch = React.useElmish (init, update, Array.empty)

    fragment [] [
        div [ Id "editor" ] [
            MonacoEditor [
                MonacoEditorProp.DefaultLanguage "fsharp"
                MonacoEditorProp.DefaultValue model.Implementation
                MonacoEditorProp.OnChange (UpdateImplementation >> dispatch)
            ]
        ]
        if not model.IsLoading then
            div [ Id "result" ] [
                MonacoEditor [
                    MonacoEditorProp.DefaultLanguage "fsharp"
                    MonacoEditorProp.DefaultValue model.Signature
                    MonacoEditorProp.Options {| readOnly = true |}
                ]
            ]

            div [ Id "commands" ] [
                button [
                    ClassName $"{Bootstrap.Btn} {Bootstrap.BtnPrimary}"
                    OnClick (fun ev ->
                        ev.preventDefault ()
                        dispatch FetchSignature
                    )
                ] [ str "Get signature" ]
            ]
        else
            div [ Id "loading" ] [ div [ ClassName $"{Bootstrap.SpinnerGrow} {Bootstrap.TextPrimary}" ] [] ]
    ]

let mainElement = Browser.Dom.document.querySelector "main"
ReactDom.render (App (), mainElement)
