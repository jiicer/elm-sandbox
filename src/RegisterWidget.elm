module RegisterWidget exposing (..)

import Html exposing (Html, div, text, input, h2, h4, a, span, table, tbody, thead, th, td, tr, button)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App


-- MODEL


type AccessType
    = ReadWrite
    | ReadOnly


type alias RegisterField =
    { name : String
    , accessType : AccessType
    , startPos : Int
    , size : Int
    }


type alias IndexedRegisterField =
    { id : Int
    , model : RegisterField
    }


type alias Model =
    { name : String
    , field : RegisterField
    , fields : List IndexedRegisterField
    , editable : Bool
    , collapsed : Bool
    }


init : String -> ( Model, Cmd Msg )
init nm =
    ( Model nm (RegisterField "Config" ReadWrite 0 1) [] False True
    , Cmd.none
    )



-- MESSAGES


type Msg
    = ChangeTitle String
    | EditTitle
    | ApplyTitle
    | Collapse



-- VIEW


collapseGlyphicon : Bool -> String
collapseGlyphicon collapsed =
    if collapsed == True then
        "glyphicon-collapse-down"
    else
        "glyphicon-collapse-up"


titleView : Bool -> Bool -> String -> Int -> List (Html Msg)
titleView collapsed editable title collapseId =
    if editable == True then
        [ input [ type' "text", onInput ChangeTitle, onClick ApplyTitle, defaultValue title ] []
        , button [ class "btn btn-default btn-sm", type' "button", attribute "data-target" ("#collapse" ++ (toString collapseId)), attribute "data-toggle" "collapse", onClick Collapse ]
            [ span [ class ("pull-right glyphicon " ++ (collapseGlyphicon collapsed)) ] [] ]
        ]
    else
        [ span [ class "panel-title", onClick EditTitle ] [ text title ]
        , button [ class "btn btn-default btn-sm", type' "button", attribute "data-target" ("#collapse" ++ (toString collapseId)), attribute "data-toggle" "collapse", onClick Collapse ]
            [ span [ class ("pull-right glyphicon " ++ (collapseGlyphicon collapsed)) ] [] ]
        ]


viewFieldHeader : Html Msg
viewFieldHeader =
    thead []
        [ tr []
            [ th []
                [ text "Field" ]
            , th []
                [ text "Acces Type" ]
            , th []
                [ text "Description" ]
            ]
        ]


viewFieldRow : RegisterField -> Html Msg
viewFieldRow field =
    tr []
        [ td []
            [ text field.name ]
        , td []
            [ text "R/W" ]
        , td []
            [ text "Config me." ]
        , td
            [ style
                [ ( "width", "1px" )
                ]
            ]
            [ button [ class "btn btn-default btn-sm", type' "button" ] [ span [ class "glyphicon glyphicon-trash" ] [] ] ]
        ]


viewFieldBody : List RegisterField -> Html Msg
viewFieldBody fields =
    tbody [] (List.map viewFieldRow fields)


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "panel-group" ]
            [ div [ class "panel panel-default" ]
                [ div [ class "panel-heading" ]
                    (titleView model.collapsed model.editable model.name 1)
                ]
            , div [ id "collapse1", class "panel-collapse collapse" ]
                [ div [ class "panel-body" ]
                    [ div [ class "container" ]
                        [ div [ class "row" ]
                            [ div [ class "col-md-6" ]
                                [ table [ class "table table-bordered" ]
                                    [ viewFieldHeader
                                    , viewFieldBody [ model.field ]
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



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTitle newText ->
            ( { model | name = newText }, Cmd.none )

        ApplyTitle ->
            ( { model | editable = False }, Cmd.none )

        EditTitle ->
            ( { model | editable = True }, Cmd.none )

        Collapse ->
            ( { model | collapsed = not model.collapsed }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN
