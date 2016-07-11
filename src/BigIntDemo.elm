port module BigIntDemo exposing (..)

import Html exposing (Html, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App


-- MODEL


type alias Model =
    { decimal : String
    , binary : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "0" "0b0", Cmd.none )



-- MESSAGES


type Msg
    = DecimalToJS String
    | BinaryFromJS String



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type' "number", onInput DecimalToJS ] []
        , div [] [ text model.binary ]
        ]



-- UPDATE


port decimal : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BinaryFromJS newBinary ->
            ( Model model.decimal newBinary, Cmd.none )

        DecimalToJS newDecimal ->
            ( model, decimal newDecimal )



-- SUBSCRIPTIONS


port binary : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    binary BinaryFromJS



-- MAIN


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
