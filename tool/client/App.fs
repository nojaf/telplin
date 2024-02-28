module App

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

type Style = CssClasses<StyleSheet, Naming.PascalCase>

[<RequireQualifiedAccess>]
type MonacoEditorProp =
    | Height of string
    | DefaultLanguage of string
    | DefaultValue of string
    | OnChange of (string -> unit)
    | Options of obj

let inline private MonacoEditor (props : MonacoEditorProp list) : ReactElement =
    ofImport "default" "@monaco-editor/react" (keyValueList CaseRules.LowerFirst props) []

[<RequireQualifiedAccess>]
type Mode =
    | Telplin
    | FCS

let getUrl (mode : Mode) =
    let url = emitJsExpr () "import.meta.env.VITE_API_ROOT"

    match mode with
    | Mode.Telplin -> $"%s{url}/telplin/signature"
    | Mode.FCS -> $"%s{url}/telplin/signature?fcs"

[<RequireQualifiedAccess>]
type FetchSignatureResponse =
    | OK of signature : string
    | InvalidImplementation of diagnostics : Diagnostic array
    | InvalidSignature of signature : string * diagnostics : Diagnostic array
    | PartialSignature of signature : string * error : TelplinError array
    | InternalError of string

and Diagnostic =
    {
        Severity : string
        Message : string
        ErrorNumber : int
        Range : Range
    }

and TelplinError = { Range : Range ; Error : string }

and Range =
    {
        StartLine : int
        StartColumn : int
        EndLine : int
        EndColumn : int
    }

let decodeRange =
    Decode.object (fun get ->
        {
            StartLine = get.Required.Field "startLine" Decode.int
            StartColumn = get.Required.Field "startColumn" Decode.int
            EndLine = get.Required.Field "endLine" Decode.int
            EndColumn = get.Required.Field "endColumn" Decode.int
        }
    )

let decodeDiagnostic =
    Decode.object (fun get ->
        {
            Severity = get.Required.Field "severity" Decode.string
            Message = get.Required.Field "message" Decode.string
            ErrorNumber = get.Required.Field "errorNumber" Decode.int
            Range = get.Required.Field "range" decodeRange
        }
    )

let decodeTelplinError =
    Decode.object (fun get ->
        {
            Range = get.Required.Field "range" decodeRange
            Error = get.Required.Field "error" Decode.string
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
        | "partialSignatureFile" ->
            let signature = get.Required.Field "signature" Decode.string
            let errors = get.Required.Field "errors" (Decode.array decodeTelplinError)
            FetchSignatureResponse.PartialSignature (signature, errors)
        | other -> FetchSignatureResponse.InternalError $"Unexpected type name \"%s{other}\""
    )

type UrlModel = { Implementation : string }

let encodeUrlModel model =
    Encode.object [ "implementation", Encode.string model.Implementation ]

let decodeUrlModel : Decoder<UrlModel> =
    Decode.object (fun get ->
        {
            Implementation = get.Required.Field "implementation" Decode.string
        }
    )

type Model =
    {
        Implementation : string
        Signature : string
        IsLoading : bool
        MainError : string
        Diagnostics : Diagnostic array
        Errors : TelplinError array
        Mode : Mode
    }

type Msg =
    | UpdateImplementation of string
    | FetchSignature
    | SignatureReceived of FetchSignatureResponse
    | ChangeMode of mode : Mode

let fetchSignature (mode : Mode) (implementation : string) dispatch =
    let options =
        Fetch.requestProps [
            Fetch.requestHeaders [ ContentType "text/plain" ]
            Method HttpMethod.POST
            Body !^implementation
        ]

    let url = getUrl mode

    GlobalFetch.fetch (RequestInfo.Url url, options)
    |> Promise.bind (fun response -> response.text () |> Promise.map (fun content -> response.Status, content))
    |> Promise.iter (fun (status, content) ->
        let response =
            match status with
            | 200 -> FetchSignatureResponse.OK content
            | 400 ->
                match Decode.fromString decodeBadResult content with
                | Error error -> FetchSignatureResponse.InternalError $"Could not decode the response json: %s{error}"
                | Ok result -> result
            | 500 -> FetchSignatureResponse.InternalError content
            | _ -> FetchSignatureResponse.InternalError $"weird response %i{status}:, %s{content}"

        dispatch (SignatureReceived response)
    )

let fallbackImplementation =
    """module Telplin

let v (a:int) b = a - b
"""

let init _ =
    let urlInfo =
        OnlineTool.UrlTools.restoreModelFromUrl
            decodeUrlModel
            {
                Implementation = fallbackImplementation
            }

    let cmd =
        if urlInfo.Implementation = fallbackImplementation then
            Cmd.none
        else

        Cmd.ofEffect (fetchSignature Mode.Telplin urlInfo.Implementation)

    {
        Implementation = urlInfo.Implementation
        Signature = ""
        IsLoading = false
        MainError = ""
        Diagnostics = Array.empty
        Errors = Array.empty
        Mode = Mode.Telplin
    },
    cmd

let update msg (model : Model) =
    match msg with
    | ChangeMode mode ->
        let cmd =
            if mode = model.Mode then
                Cmd.none
            else
                Cmd.ofMsg FetchSignature

        { model with Mode = mode }, cmd
    | UpdateImplementation implementation ->
        { model with
            Implementation = implementation
        },
        Cmd.none
    | FetchSignature ->
        { model with
            IsLoading = true
            Signature = ""
            MainError = ""
            Diagnostics = Array.empty
        },
        Cmd.batch [
            Cmd.ofEffect (fetchSignature model.Mode model.Implementation)
            Cmd.ofEffect (fun _ ->
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
                    MainError = error
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
            | FetchSignatureResponse.PartialSignature (signature, errors) ->
                { model with
                    IsLoading = false
                    Signature = signature
                    Errors = errors
                }

        nextModel, Cmd.none

let telplinIssueLink model =
    let location = Browser.Dom.window.location

    let codeTemplate header code =
        $"#### %s{header}\n\n```fsharp\n%s{code}\n```"

    let hasDiagnostics = if Array.isEmpty model.Diagnostics then " " else "X"

    let issueTemplate =
        $"""
Issue created from [telplin-online](%s{location.href})

%s{codeTemplate "Implementation" model.Implementation}
%s{codeTemplate "Signature" model.Signature}

#### Problem description

**Please explain what is going wrong**

#### Extra information

- [%s{hasDiagnostics}] The proposed signature has problems.
- [ ] I or my company would be willing to help fix this.
"""
        |> Uri.EscapeDataString

    sprintf "https://github.com/nojaf/telplin/issues/new?title=%s&body=%s" "<Insert meaningful title>" issueTemplate

let fcsIssueLink (model : Model) =
    let issueTemplate =
        $"""
When generating a signature file the result is somewhat unexpected.

**Repro steps**

```fsharp
%s{model.Implementation}
```

leads to

```fsharp
%s{model.Signature}
```

**Expected behaviour**



**Actual behaviour**



**Related information**

FCS: FSharpCheckFileResults.GenerateSignature
"""
        |> Uri.EscapeDataString

    sprintf "https://github.com/dotnet/fsharp/issues/new?title=%s&body=%s" "<Insert meaningful title>" issueTemplate

let badge key range severity errorNumber message =
    div [ Key key ] [
        strong [] [
            str $"(%i{range.StartLine}, %i{range.StartColumn}) (%i{range.EndLine},%i{range.EndColumn})"
        ]
        span [ ClassName $"%s{Style.Badge} %s{severity}" ] [ str severity ]
        match errorNumber with
        | None -> ()
        | Some errorNumber -> span [ ClassName $"%s{Style.Badge}" ] [ ofInt errorNumber ]
        p [] [ str message ]
    ]

[<ReactComponent>]
let App () =
    let model, dispatch = React.useElmish (init, update, Array.empty)

    let errorPanel =
        if
            Array.isEmpty model.Diagnostics
            && Array.isEmpty model.Errors
            && String.IsNullOrWhiteSpace model.MainError
        then
            None
        else
            let mainError =
                if String.IsNullOrWhiteSpace model.MainError then
                    None
                else
                    div [ Id "error" ] [ str model.MainError ] |> Some

            let errors =
                [|
                    yield!
                        model.Diagnostics
                        |> Array.mapi (fun idx diag ->
                            badge $"diag_%i{idx}" diag.Range diag.Severity (Some diag.ErrorNumber) diag.Message
                        )
                    yield!
                        model.Errors
                        |> Array.mapi (fun idx error -> badge $"error_%i{idx}" error.Range "error" None error.Error)
                |]

            div [ Id "error-panel" ] [ ofOption mainError ; ofArray errors ] |> Some

    let reportIssueButton =
        if
            String.IsNullOrWhiteSpace model.Signature
            && Array.isEmpty model.Diagnostics
            && String.IsNullOrWhiteSpace model.MainError
        then
            None
        else
            let uri =
                match model.Mode with
                | Mode.Telplin -> telplinIssueLink model
                | Mode.FCS -> fcsIssueLink model

            a [ Href uri ; Target "_blank" ] [ button [ Id "report" ] [ str "Report issue" ] ]
            |> Some

    fragment [] [
        div [ Id "editor" ] [
            MonacoEditor [
                MonacoEditorProp.DefaultLanguage "fsharp"
                MonacoEditorProp.DefaultValue model.Implementation
                MonacoEditorProp.OnChange (UpdateImplementation >> dispatch)
            ]
        ]
        if not model.IsLoading then
            let telplinClass = if model.Mode = Mode.Telplin then "active" else ""
            let fcsClass = if model.Mode = Mode.FCS then "active" else ""

            div [ Id "result" ] [
                ul [] [
                    li [
                        ClassName telplinClass
                        OnClick (fun _ -> dispatch (ChangeMode Mode.Telplin))
                    ] [ str "Telplin" ]
                    li [ ClassName fcsClass ; OnClick (fun _ -> dispatch (ChangeMode Mode.FCS)) ] [ str "FCS" ]
                ]

                MonacoEditor [
                    MonacoEditorProp.DefaultLanguage "fsharp"
                    MonacoEditorProp.DefaultValue model.Signature
                    MonacoEditorProp.Options {| readOnly = true |}
                ]
            ]
        else
            div [ Id "loading" ] [ div [] [] ]
        footer [] [
            div [ Id "info" ] [
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
                str " against the implementation code. The initial request might take some time."
            ]
            ofOption errorPanel
            ofOption reportIssueButton
            button [
                OnClick (fun ev ->
                    ev.preventDefault ()
                    dispatch FetchSignature
                )
            ] [ str "Get signature" ]
        ]
    ]

Browser.Dom.window.addEventListener (
    "DOMContentLoaded",
    fun _ ->
        let mainElement = Browser.Dom.document.querySelector "main"
        ReactDomClient.createRoot(mainElement).render (App ())
)
