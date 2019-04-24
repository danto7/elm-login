module Main exposing (LoginAnswer, LoginStatus(..), Model, Msg(..), init, loginAnswerDecoder, loginStatusText, main, subscriptions, update, view)

import Browser
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { username = ""
      , password = ""
      , status = Nothing
      }
    , Cmd.none
    )



-- MODEL


type LoginStatus
    = Nothing
    | LoggingIn
    | LoginSuccessfull
    | LoginFailed


type alias Model =
    { username : String
    , password : String
    , status : LoginStatus
    }



-- UPDATE


type Msg
    = ChangeUsername String
    | ChangePassword String
    | Login
    | GotLoginAnswer (Result Http.Error LoginAnswer)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUsername newUsername ->
            ( { model | username = newUsername }, Cmd.none )

        ChangePassword newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        Login ->
            ( { model | status = LoggingIn }
            , Http.get
                { url = "/login.jso"

                --, body = Http.emptyBody
                , expect = Http.expectJson GotLoginAnswer loginAnswerDecoder
                }
            )

        GotLoginAnswer result ->
            case result of
                Ok answer ->
                    if answer.ok then
                        ( { model | status = LoginSuccessfull }, Cmd.none )

                    else
                        ( { model | status = LoginFailed }, Cmd.none )

                Err _ ->
                    ( { model | status = LoginFailed }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Username", value model.username, onInput ChangeUsername ] []
        , input [ placeholder "Password", value model.password, onInput ChangePassword ] []
        , button [ onClick Login ] [ text "Login" ]
        , div [] [ text (loginStatusText model.status) ]
        ]


loginStatusText : LoginStatus -> String
loginStatusText status =
    case status of
        Nothing ->
            "Nothing"

        LoggingIn ->
            "LoggingIn"

        LoginSuccessfull ->
            "LoginSuccessfull"

        LoginFailed ->
            "LoginFailed"



-- HTTP


type alias LoginAnswer =
    { ok : Bool, msg : String }


loginAnswerDecoder : D.Decoder LoginAnswer
loginAnswerDecoder =
    D.map2 LoginAnswer
        (D.field "ok" D.bool)
        (D.field "msg" D.string)
