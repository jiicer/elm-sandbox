module RegisterWidget exposing (..)

import Html exposing (Html, div, text, input, h2, h4, a, span)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App


-- MODEL


type alias RegisterField =
    { name : String
    , startPos : Int
    , size : Int
    }


type alias Model =
    { name : String
    , field : RegisterField
    , editable : Bool
    }


init : String -> ( Model, Cmd Msg )
init nm =
    ( Model nm (RegisterField "Field" 0 1) True
    , Cmd.none
    )



-- MESSAGES


type Msg
    = ChangeTitle String
    | EditTitle
    | ApplyTitle



-- VIEW


titleView : Bool -> String -> List (Html Msg)
titleView editable title =
    if editable == True then
        [ input [ type' "text", onInput ChangeTitle ] []
        , a [ href "#" ] [ span [ class "glyphicon glyphicon-ok-circle", onClick ApplyTitle ] [] ]
        ]
    else
        [ h4 [ class "panel-title" ] [ a [ attribute "data-toggle" "collapse", href "#collapse1" ] [ text title ] ]
        , a [ href "#" ] [ span [ class "glyphicon glyphicon-edit", onClick EditTitle ] [] ]
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h2 [] [ text "Collapsible Panel" ]
        , div [ class "panel-group" ]
            [ div [ class "panel panel-default" ]
                [ div [ class "panel-heading" ]
                    (List.append
                        (titleView
                            model.editable
                            model.name
                        )
                        [ div [ id "collapse1", class "panel-collapse collapse" ]
                            [ div [ class "panel-body" ]
                                [ input [ type' "text", onInput ChangeTitle ] []
                                , div [ class "panel-body" ] [ text model.field.name ]
                                , div [ class "panel-footer" ] [ text "Panel Footer" ]
                                ]
                            ]
                        ]
                    )
                ]
            ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTitle newText ->
            ( { model | name = newText, field = (RegisterField newText 0 1) }, Cmd.none )

        ApplyTitle ->
            ( { model | editable = False }, Cmd.none )

        EditTitle ->
            ( { model | editable = True }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN
