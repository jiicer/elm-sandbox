port module BootstrapDemo exposing (..)

import Html exposing (Html, div, text, input, h2, h4, a)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App


-- MODEL


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "Collapsing", Cmd.none )



-- MESSAGES


type Msg
    = CollapsingText String



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h2 [] [ text "Collapsible Panel" ]
        , div [ class "panel-group" ]
            [ div [ class "panel panel-default" ]
                [ div [ class "panel-heading" ]
                    [ h4 [ class "panel-title" ] [ a [ attribute "data-toggle" "collapse", href "#collapse1" ] [ text "Collapsible panel" ] ] ]
                , div [ id "collapse1", class "panel-collapse collapse" ]
                    [ div [ class "panel-body" ]
                        [ input [ type' "text", onInput CollapsingText ] []
                        , div [ class "panel-body" ] [ text model ]
                        , div [ class "panel-footer" ] [ text "Panel Footer" ]
                        ]
                    ]
                ]
            ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CollapsingText newText ->
            ( newText, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
