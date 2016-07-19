module RegisterWidget exposing (..)

import Html exposing (Html, div, text, input, h2, h4, a, span, table, tbody, thead, th, td, tr)
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
    ( Model nm (RegisterField "Field" 0 1) False
    , Cmd.none
    )



-- MESSAGES


type Msg
    = ChangeTitle String
    | EditTitle
    | ApplyTitle



-- VIEW


titleView : Bool -> String -> Int -> List (Html Msg)
titleView editable title collapseId =
    if editable == True then
        [ input [ type' "text", onInput ChangeTitle ] []
        , a [ href "#" ] [ span [ class "glyphicon glyphicon-ok-circle", onClick ApplyTitle ] [] ]
        , a [ href ("#collapse" ++ (toString collapseId)), attribute "data-toggle" "collapse" ] [ span [ class "glyphicon glyphicon-expand" ] [] ]
        ]
    else
        [ span [ class "panel-title", onClick EditTitle ] [ text title ]
        , a [ href ("#collapse" ++ (toString collapseId)), attribute "data-toggle" "collapse" ] [ span [ class "glyphicon glyphicon-expand" ] [] ]
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "panel-group" ]
            [ div [ class "panel panel-default" ]
                [ div [ class "panel-heading" ]
                    (titleView
                        model.editable
                        model.name
                        1
                    )
                ]
            , div [ id "collapse1", class "panel-collapse collapse" ]
                [ div [ class "panel-body" ]
                    [ div [ class "container" ]
                        [ div [ class "row" ]
                            [ div [ class "col-md-6" ]
                                [ table [ class "table table-bordered" ]
                                    [ thead []
                                        [ tr []
                                            [ th []
                                                [ text "Field" ]
                                            , th []
                                                [ text "Acces Type" ]
                                            , th []
                                                [ text "Description" ]
                                            ]
                                        ]
                                    , tbody []
                                        [ tr []
                                            [ td []
                                                [ text model.field.name ]
                                            , td []
                                                [ text "R/W" ]
                                            , td []
                                                [ text "Config me." ]
                                            ]
                                        , tr []
                                            [ td []
                                                [ text model.field.name ]
                                            , td []
                                                [ text "R" ]
                                            , td []
                                                [ text "Read the status." ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , div [ class "panel-body" ]
                    [ div [ class "panel panel-default" ]
                        [ div [ class "panel-heading" ]
                            (titleView
                                model.editable
                                model.name
                                2
                            )
                        ]
                    , div [ id "collapse2", class "panel-collapse collapse" ]
                        [ div [ class "panel-body" ]
                            [ div [ class "container" ]
                                [ div [ class "row" ]
                                    [ div [ class "col-md-6" ]
                                        [ table [ class "table table-bordered" ]
                                            [ thead []
                                                [ tr []
                                                    [ th []
                                                        [ text "Field" ]
                                                    , th []
                                                        [ text "Acces Type" ]
                                                    , th []
                                                        [ text "Description" ]
                                                    ]
                                                ]
                                            , tbody []
                                                [ tr []
                                                    [ td []
                                                        [ text model.field.name ]
                                                    , td []
                                                        [ text "R/W" ]
                                                    , td []
                                                        [ text "Config me." ]
                                                    ]
                                                , tr []
                                                    [ td []
                                                        [ text model.field.name ]
                                                    , td []
                                                        [ text "R" ]
                                                    , td []
                                                        [ text "Read the status." ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        , div [ class "panel-body" ]
                            [ text "Read the status." ]
                        ]
                    ]
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
