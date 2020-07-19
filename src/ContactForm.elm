module ContactForm exposing (main)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as TextArea
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox
        { init = init, update = update, view = view }


type alias OptString =
    Maybe String


type Msg
    = Name OptString
    | Email OptString
    | Message OptString
    | Submit


type alias Model =
    { formSubmissionResult : Maybe SubmissionResultMessage
    , name : OptString
    , email : OptString
    , message : OptString
    }


init : Model
init =
    { formSubmissionResult = Nothing
    , name = Nothing
    , email = Nothing
    , message = Nothing
    }


type alias SubmissionResultMessage =
    ( MessageType, String )


nullString : String
nullString =
    "<null>"


optStringToString : OptString -> String
optStringToString opt =
    case opt of
        Just text ->
            text

        Nothing ->
            nullString


maybeSubmissionResultToString : Maybe SubmissionResultMessage -> String
maybeSubmissionResultToString opt =
    case opt of
        Just msg ->
            submissionResultToString msg

        Nothing ->
            nullString


submissionResultToString : SubmissionResultMessage -> String
submissionResultToString ( msgType, msgContent ) =
    case msgType of
        INFO ->
            "( INFO, \"" ++ msgContent ++ "\" )"

        ERROR ->
            "( ERROR, \"" ++ msgContent ++ "\" )"


messageSentMessage : Maybe SubmissionResultMessage
messageSentMessage =
    Just ( INFO, "Message sent!" )


errorMessage : String -> Maybe SubmissionResultMessage
errorMessage details =
    Just ( ERROR, "An error has occured '" ++ details ++ "'" )


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name optName ->
            { model | name = optName }

        Email optEmail ->
            { model | email = optEmail }

        Message optMsg ->
            { model | message = optMsg }

        Submit ->
            { model | formSubmissionResult = errorMessage (debugModel { model | formSubmissionResult = Nothing }) }


debugModel : Model -> String
debugModel model =
    "{ name: '"
        ++ optStringToString model.name
        ++ "', email: '"
        ++ optStringToString model.email
        ++ "', message: '"
        ++ optStringToString model.message
        ++ "', result: '"
        ++ maybeSubmissionResultToString model.formSubmissionResult
        ++ "' }"


view : Model -> Html Msg
view model =
    Form.form
        []
        [ Form.group []
            (formElements model)
        ]


formElements : Model -> List (Html Msg)
formElements model =
    case getSubmissionResult model of
        Just messageElement ->
            CDN.stylesheet :: messageElement :: defaultFormElements model

        Nothing ->
            CDN.stylesheet :: defaultFormElements model


nameField : Maybe String -> Html Msg
nameField nameFieldValue =
    case nameFieldValue of
        Just value ->
            getFlowControl "Name" "Please enter your name" value (\newName -> Name (Just newName))

        Nothing ->
            getFlowControl "Name" "Please enter your name" "" (\newName -> Name (Just newName))


emailField : Maybe String -> Html Msg
emailField emailFieldValue =
    case emailFieldValue of
        Just value ->
            getFlowControl "Email" "Please enter your email (we need this to contact you back!)" value (\newName -> Email (Just newName))

        Nothing ->
            getFlowControl "Email" "Please enter your email (we need this to contact you back!)" "" (\newName -> Email (Just newName))


messageField : Maybe String -> Html Msg
messageField messageFieldValue =
    case messageFieldValue of
        Just msgValue ->
            getMessageField "messageField" "Message" "Your message goes here (how can we help you?)" msgValue

        Nothing ->
            getMessageField "messageField" "Message" "Your message goes here (how can we help you?)" ""


btnSubmit : Html Msg
btnSubmit =
    getButton "Submit"


defaultFormElements : Model -> List (Html Msg)
defaultFormElements model =
    [ nameField model.name
    , emailField model.email
    , messageField model.message
    , btnSubmit
    ]


getSubmissionResult : Model -> Maybe (Html Msg)
getSubmissionResult model =
    Maybe.map
        (\( msgType, msgText ) ->
            showMessage msgType msgText
        )
        model.formSubmissionResult


getFlowControl : String -> String -> String -> (String -> Msg) -> Html Msg
getFlowControl labelText placeholderText currentValue onInputAction =
    Form.group []
        [ Form.label [ class "col-sm-2 control-label" ] [ text labelText ]
        , Input.text [ Input.onInput onInputAction, Input.attrs [ value currentValue, class "col-sm-9", placeholder placeholderText ] ]
        ]


getMessageField : String -> String -> String -> String -> Html Msg
getMessageField inputName labelText placeholderText currentValue =
    Form.group []
        [ Form.label [ class "col-sm-2 control-label" ] [ text labelText ]
        , TextArea.textarea
            [ TextArea.id inputName
            , TextArea.value currentValue
            , TextArea.onInput (\value -> Message (Just value))
            , TextArea.rows 5
            , TextArea.attrs
                [ placeholder placeholderText
                , class "col-sm-9"
                ]
            ]
        ]


getButton : String -> Html Msg
getButton btnText =
    Button.button [ Button.primary, Button.onClick Submit ] [ text btnText ]


type MessageType
    = INFO
    | ERROR


showMessage : MessageType -> String -> Html Msg
showMessage msgType msg =
    case msgType of
        INFO ->
            Alert.simpleInfo [] [ text msg ]

        ERROR ->
            Alert.simpleDanger [] [ text msg ]
