port module BootstrapDemo exposing (..)

import Html exposing (Html, div, text, input, h2, h4, a)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RegisterWidget
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid


-- MODEL


type alias Model =
    { register : RegisterWidget.Model }


init : ( Model, Cmd Msg )
init =
    ( { register = Tuple.first <| RegisterWidget.init "Register" }, Cmd.none )



-- MESSAGES


type Msg
    = RegisterWidgetMsg RegisterWidget.Msg



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.map RegisterWidgetMsg (RegisterWidget.view model.register)
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RegisterWidgetMsg msg ->
            ( { model | register = Tuple.first <| RegisterWidget.update msg model.register }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
