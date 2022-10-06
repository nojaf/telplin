module Temp

open System
open Fable.Core
open Fable.Core.JsInterop
open Elmish
open Thoth.Json
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

let getUrl () =
    JsInterop.importDynamic "/telplin/env.js"
    |> Promise.map (fun env -> env?API_ROOT)
    |> Promise.catch (fun err ->
        JS.console.warn ("Could not fetch environment!", err)
        "http://localhost:8906"
    )
    |> Promise.map (sprintf "%s/telplin/signature")

[<RequireQualifiedAccess>]
type FetchSignatureResponse =
    | OK of signature : string
    | InvalidImplementation of diagnostics : Diagnostic array
    | InvalidSignature of signature : string * diagnostics : Diagnostic array
    | InternalError of string

and Diagnostic = {
    Severity : string
    Message : string
    ErrorNumber : string
    Range : Range
}

and Range = {
    StartLine : int
    StartColumn : int
    EndLine : int
    EndColumn : int
}

let decodeRange =
    Decode.object (fun get -> {
        StartLine = get.Required.Field "startLine" Decode.int
        StartColumn = get.Required.Field "startColumn" Decode.int
        EndLine = get.Required.Field "endLine" Decode.int
        EndColumn = get.Required.Field "endColumn" Decode.int
    }
    )

let decodeDiagnostic =
    Decode.object (fun get -> {
        Severity = get.Required.Field "severity" Decode.string
        Message = get.Required.Field "message" Decode.string
        ErrorNumber = get.Required.Field "errorNumber" Decode.string
        Range = get.Required.Field "range" decodeRange
    }
    )

let decodeBadResult =
    Decode.object (fun get ->
        let typeName = get.Required.Field "type" Decode.string

        match typeName with
        | "invalidImplementationFile" ->
            let diagnostics = get.Required.Field "diagnostics" (Decode.array decodeDiagnostic)
            FetchSignatureResponse.InvalidImplementation diagnostics
        | "invalidSignatureFile" ->
            let signature = get.Required.Field "signature" Decode.string
            let diagnostics = get.Required.Field "diagnostics" (Decode.array decodeDiagnostic)
            FetchSignatureResponse.InvalidSignature (signature, diagnostics)
        | other -> FetchSignatureResponse.InternalError $"Unexpected type name \"{other}\""
    )

type UrlModel = { Implementation : string }

let encodeUrlModel model =
    Encode.object [ "implementation", Encode.string model.Implementation ]

let decodeUrlModel : Decoder<UrlModel> =
    Decode.object (fun get -> {
        Implementation = get.Required.Field "implementation" Decode.string
    }
    )

type Model = {
    Implementation : string
    Signature : string
    IsLoading : bool
    Error : string
    Diagnostics : Diagnostic array
}

type Msg =
    | UpdateImplementation of string
    | FetchSignature
    | SignatureReceived of FetchSignatureResponse

let init _ =
    let urlInfo =
        OnlineTool.UrlTools.restoreModelFromUrl
            decodeUrlModel
            {
                Implementation =
                    """module Telplin

let v (a:int) b = a - b
"""
            }

    {
        Implementation = urlInfo.Implementation
        Signature = ""
        IsLoading = false
        Error = ""
        Diagnostics = Array.empty
    },
    Cmd.none

let fetchSignature implementation dispatch =
    let options =
        Fetch.requestProps [
            Fetch.requestHeaders [ ContentType "text/plain" ]
            Method HttpMethod.POST
            Body !^implementation
        ]

    getUrl ()
    |> Promise.bind (fun url -> GlobalFetch.fetch (RequestInfo.Url url, options))
    |> Promise.bind (fun response -> response.text () |> Promise.map (fun content -> response.Status, content))
    |> Promise.iter (fun (status, content) ->
        let response =
            match status with
            | 200 -> FetchSignatureResponse.OK content
            | 400 ->
                match Decode.fromString decodeBadResult content with
                | Error error -> FetchSignatureResponse.InternalError $"Could not decode the response json: {error}"
                | Ok result -> result
            | 500 -> FetchSignatureResponse.InternalError content
            | _ -> FetchSignatureResponse.InternalError $"weird response {status}:, {content}"

        dispatch (SignatureReceived response)
    )

let update msg (model : Model) =
    match msg with
    | UpdateImplementation implementation ->
        { model with
            Implementation = implementation
        },
        Cmd.none
    | FetchSignature ->
        { model with
            IsLoading = true
            Signature = ""
            Error = ""
            Diagnostics = Array.empty
        },
        Cmd.batch [
            Cmd.ofSub (fetchSignature model.Implementation)
            Cmd.ofSub (fun _ ->
                {
                    Implementation = model.Implementation
                }
                |> encodeUrlModel
                |> Encode.toString 2
                |> OnlineTool.UrlTools.updateUrlWithData
            )
        ]

    | SignatureReceived response ->
        let nextModel =
            match response with
            | FetchSignatureResponse.OK signature ->
                { model with
                    IsLoading = false
                    Signature = signature
                }
            | FetchSignatureResponse.InternalError error ->
                { model with
                    IsLoading = false
                    Error = error
                }

            | FetchSignatureResponse.InvalidImplementation diagnostics ->
                { model with
                    IsLoading = false
                    Diagnostics = diagnostics
                }
            | FetchSignatureResponse.InvalidSignature (signature, diagnostics) ->
                { model with
                    IsLoading = false
                    Signature = signature
                    Diagnostics = diagnostics
                }

        nextModel, Cmd.none

[<ReactComponent>]
let App () =
    let model, dispatch = React.useElmish (init, update, Array.empty)

    let errorPanel =
        if Array.isEmpty model.Diagnostics && String.IsNullOrWhiteSpace model.Error then
            None
        else
            let error =
                if String.IsNullOrWhiteSpace model.Error then
                    None
                else
                    div [ Class $"{Bootstrap.TextDanger} {Bootstrap.Py2} {Bootstrap.Px3}" ] [ str model.Error ]
                    |> Some

            let diagnostics =
                model.Diagnostics
                |> Array.mapi (fun idx diag ->
                    let severityClass =
                        if diag.Severity = "error" then
                            Bootstrap.TextBgDanger
                        else
                            Bootstrap.TextBgWarning

                    div [ Key !!idx ] [
                        strong [ ClassName Bootstrap.Me1 ] [
                            str
                                $"({diag.Range.StartLine}, {diag.Range.StartColumn}) ({diag.Range.EndLine},{diag.Range.EndColumn})"
                        ]
                        span [ ClassName $"{Bootstrap.Badge} {severityClass} {Bootstrap.Mx1}" ] [ str diag.Severity ]
                        span [ ClassName $"{Bootstrap.Badge} {Bootstrap.TextBgDark} {Bootstrap.Mx1}" ] [
                            str diag.ErrorNumber
                        ]
                        p [] [ str diag.Message ]
                    ]
                )

            div [ Id "error-panel" ; ClassName $"{Bootstrap.BgLight} {Bootstrap.Px2}" ] [
                ofOption error
                ofArray diagnostics
            ]
            |> Some

    let reportIssueButton =
        if String.IsNullOrWhiteSpace model.Signature || Array.isEmpty model.Diagnostics then
            None
        else
            let location = Browser.Dom.window.location

            let codeTemplate header code =
                $"#### {header}\n\n```fsharp\n{code}\n```"

            let hasDiagnostics = if Array.isEmpty model.Diagnostics then " " else "X"

            let issueTemplate =
                $"""
Issue created from [telplin-online]({location.href})

{codeTemplate "Implementation" model.Implementation}
{codeTemplate "Signature" model.Signature}

#### Problem description

**Please explain what is going wrong**

#### Extra information

- [{hasDiagnostics}] The proposed signature has problems.
- [ ] I or my company would be willing to help fix this.
"""
                |> Uri.EscapeDataString

            let uri =
                sprintf
                    "https://github.com/nojaf/telplin/issues/new?title=%s&body=%s"
                    "<Insert meaningful title>"
                    issueTemplate

            a [ Href uri ; Target "_blank" ] [
                button [ ClassName $"{Bootstrap.Btn} {Bootstrap.BtnOutlineDanger} {Bootstrap.Me2}" ] [
                    str "Report issue"
                ]
            ]
            |> Some

    fragment [] [
        div [ Id "editor" ] [
            MonacoEditor [
                MonacoEditorProp.DefaultLanguage "fsharp"
                MonacoEditorProp.DefaultValue model.Implementation
                MonacoEditorProp.OnChange (UpdateImplementation >> dispatch)
            ]
            div [ Id "info" ; ClassName $"{Bootstrap.BgLight} {Bootstrap.P3}" ] [
                str
                    "Welcome to the Telplin online tool. The goal of this tool is to report issues for scenarios where a signature file cannot be generated."
                br []
                str "Most "
                code [] [ str "System" ]
                str
                    " assemblies are present and can be used. Your code is expected to be valid inside a library project."
                br []
                str "The implementation F# code will be "
                strong [] [ str "typed checked" ]
                str " first, the signature will be "
                strong [] [ str "generated" ]
                str " and lastly "
                strong [] [ str "validated" ]
                str " against the implementation code."
                br []
                str "The initial request might take some time."
            ]
        ]
        if not model.IsLoading then
            div [ Id "result" ] [
                MonacoEditor [
                    MonacoEditorProp.DefaultLanguage "fsharp"
                    MonacoEditorProp.DefaultValue model.Signature
                    MonacoEditorProp.Height "auto"
                    MonacoEditorProp.Options {| readOnly = true |}
                ]
                ofOption errorPanel
                div [ Id "commands" ] [
                    button [
                        ClassName $"{Bootstrap.Btn} {Bootstrap.BtnPrimary}"
                        OnClick (fun ev ->
                            ev.preventDefault ()
                            dispatch FetchSignature
                        )
                    ] [ str "Get signature" ]
                    ofOption reportIssueButton
                ]
            ]
        else
            div [ Id "loading" ] [ div [ ClassName $"{Bootstrap.SpinnerGrow} {Bootstrap.TextPrimary}" ] [] ]
    ]

let mainElement = Browser.Dom.document.querySelector "main"
ReactDom.render (App (), mainElement)
