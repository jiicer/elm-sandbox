module RegisterWidget exposing (..)

import Array exposing (..)
import Html exposing (Html, div, text, input, h2, h4, a, span, table, tbody, thead, th, td, tr, button)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App


-- MODEL


type AccessType
    = ReadWrite
    | ReadOnly
    | Reserved


accessTypeToString : AccessType -> String
accessTypeToString at =
    case at of
        ReadWrite ->
            "R/W"

        ReadOnly ->
            "R"

        Reserved ->
            "(N/A)"


type alias RegisterField =
    { name : String
    , accessType : AccessType
    , startPos : Int
    , size : Int
    , description : String
    }


type alias IndexedRegisterField =
    { id : Int
    , model : RegisterField
    , buttonsEnabled : Bool
    }


type alias Model =
    { name : String
    , field : RegisterField
    , fields : List IndexedRegisterField
    , reservedFields : List IndexedRegisterField
    , editable : Bool
    , collapsed : Bool
    , toolButtonsTmp : Bool
    }


init : String -> ( Model, Cmd Msg )
init nm =
    ( Model nm (RegisterField "Config" ReadWrite 0 1 "Configure me.") [] [ { id = 0, model = RegisterField "(Reserved)" Reserved 0 32 "", buttonsEnabled = False } ] False True False
    , Cmd.none
    )



-- MESSAGES


type Msg
    = ChangeTitle String
    | EditTitle
    | ApplyTitle
    | Collapse
    | EnableToolButtons Int
    | DisableToolButtons Int



-- VIEW


emptyHtml : Html msg
emptyHtml =
    span [ style [ ( "display", "none" ) ] ] []


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
            [ th [] [ text "Field" ]
            , th [] [ text "Acces Type" ]
            , th [] [ text "Description" ]
            ]
        ]


viewToolButtons : Bool -> List IndexedRegisterField -> Html Msg
viewToolButtons enabled fields =
    if enabled == True then
        td
            [ style
                [ ( "width", "100px" )
                ]
            ]
            [ button [ class "btn btn-default btn-sm", type' "button" ] [ span [ class "glyphicon glyphicon-trash" ] [] ] ]
    else
        td
            [ style
                [ ( "width", "100px" )
                , ( "visibility", "hidden" )
                ]
            ]
            [ button [ class "btn btn-default btn-sm", type' "button" ] [ span [ class "glyphicon glyphicon-trash" ] [] ] ]


viewFieldRow : RegisterField -> Int -> Html Msg -> Html Msg
viewFieldRow field id buttons =
    tr [ onMouseEnter (EnableToolButtons id), onMouseLeave (DisableToolButtons id) ]
        [ td []
            [ text field.name ]
        , td []
            [ text (accessTypeToString field.accessType) ]
        , td []
            [ text field.description ]
        , buttons
        ]


viewFieldBody : List IndexedRegisterField -> Html Msg
viewFieldBody fields =
    tbody [] (List.map (\indexedField -> viewFieldRow indexedField.model indexedField.id (viewToolButtons indexedField.buttonsEnabled fields)) fields)


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
                        [ div [ class "column" ]
                            [ div [ class "col-md-6" ]
                                [ table [ class "table table-bordered" ]
                                    [ viewFieldHeader
                                    , viewFieldBody [ { id = 0, model = model.field, buttonsEnabled = model.toolButtonsTmp } ]
                                    , viewFieldBody model.reservedFields
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

        EnableToolButtons id ->
            ( { model | toolButtonsTmp = True }, Cmd.none )

        DisableToolButtons id ->
            ( { model | toolButtonsTmp = False }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN
