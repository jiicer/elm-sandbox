module RegisterWidget exposing (..)

import Array exposing (..)
import Html exposing (Html, div, text, input, h2, h4, a, span, table, tbody, thead, th, td, tr, button)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App


-- MODEL


registerSizeInBits : Int
registerSizeInBits =
    32


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


registerFieldBitsToString : Int -> Int -> String
registerFieldBitsToString startPos size =
    if size == 1 then
        toString startPos
    else
        toString (startPos + size - 1) ++ ":" ++ (toString startPos)


type alias RegisterField =
    { name : String
    , accessType : AccessType
    , startPos : Int
    , size : Int
    , description : String
    }


type alias IndexedRegisterField =
    { model : RegisterField
    , buttonsEnabled : Bool
    , positionAndSizeEditable : Bool
    }


type alias Model =
    { name : String
    , fields : List IndexedRegisterField
    , editable : Bool
    , collapsed : Bool
    }


init : String -> ( Model, Cmd Msg )
init nm =
    ( Model nm [ { model = RegisterField "(Reserved)" Reserved 0 32 "", buttonsEnabled = False, positionAndSizeEditable = False } ] False True
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
    | InsertField Int
    | RemoveField Int
    | EditPositionAndSize Int



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
            , th [] [ text "Bits" ]
            , th [] [ text "Description" ]
            ]
        ]


viewToolButtons : IndexedRegisterField -> List IndexedRegisterField -> Html Msg
viewToolButtons field allFields =
    let
        cellAttr =
            if field.buttonsEnabled == True then
                style [ ( "width", "100px" ) ]
            else
                style [ ( "visibility", "hidden" ), ( "width", "100px" ) ]

        viewInsert =
            if (field.model.accessType == Reserved) then
                button [ class "btn btn-default btn-sm", type' "button", onClick (InsertField field.model.startPos) ] [ span [ class "glyphicon glyphicon-plus" ] [] ]
            else
                emptyHtml

        viewRemove =
            if (field.model.accessType /= Reserved) then
                button [ class "btn btn-default btn-sm", type' "button", onClick (RemoveField field.model.startPos) ] [ span [ class "glyphicon glyphicon-trash" ] [] ]
            else
                emptyHtml
    in
        td [ cellAttr ]
            [ viewRemove, viewInsert ]


viewFieldRow : RegisterField -> Html Msg -> Html Msg
viewFieldRow field buttons =
    tr [ onMouseEnter (EnableToolButtons field.startPos), onMouseLeave (DisableToolButtons field.startPos) ]
        [ td []
            [ text field.name ]
        , td []
            [ text (accessTypeToString field.accessType) ]
        , td [ onClick (EditPositionAndSize field.startPos) ]
            [ text (registerFieldBitsToString field.startPos field.size) ]
        , td []
            [ text field.description ]
        , buttons
        ]


viewFieldBody : List IndexedRegisterField -> Html Msg
viewFieldBody fields =
    tbody [] (List.map (\indexedField -> viewFieldRow indexedField.model (viewToolButtons indexedField fields)) fields)


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
                                    , viewFieldBody model.fields
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


setToolButtonsForField : List IndexedRegisterField -> Int -> Bool -> List IndexedRegisterField
setToolButtonsForField indexedFields startPos value =
    List.map
        (\indexedField ->
            if indexedField.model.startPos == startPos then
                { indexedField | buttonsEnabled = value }
            else
                indexedField
        )
        indexedFields


setPositionAndSizeEditingForField : List IndexedRegisterField -> Int -> Bool -> List IndexedRegisterField
setPositionAndSizeEditingForField indexedFields startPos value =
    List.map
        (\indexedField ->
            if indexedField.model.startPos == startPos then
                { indexedField | positionAndSizeEditable = value }
            else
                indexedField
        )
        indexedFields


enableToolButtonsForField : List IndexedRegisterField -> Int -> List IndexedRegisterField
enableToolButtonsForField indexedFields startPos =
    setToolButtonsForField indexedFields startPos True


disableToolButtonsForField : List IndexedRegisterField -> Int -> List IndexedRegisterField
disableToolButtonsForField indexedFields startPos =
    setToolButtonsForField indexedFields startPos False


fieldPrototype : Int -> RegisterField
fieldPrototype startPos =
    RegisterField "(New Field)" ReadWrite startPos 1 "(Insert Description)"


startPosSomparison a b =
    compare a.model.startPos b.model.startPos


filterReserved : List IndexedRegisterField -> List IndexedRegisterField
filterReserved fields =
    List.filter
        (\field ->
            if field.model.accessType /= Reserved then
                True
            else
                False
        )
        fields


buildGaps : Int -> List IndexedRegisterField -> List { startPos : Int, size : Int } -> List { startPos : Int, size : Int }
buildGaps expected fields result =
    let
        fieldsSorted =
            List.sortWith startPosSomparison fields
    in
        case List.head fieldsSorted of
            Just field ->
                if (field.model.startPos > expected) then
                    buildGaps (field.model.startPos + field.model.size) (List.drop 1 fieldsSorted) ({ startPos = expected, size = (field.model.startPos - expected) } :: result)
                else
                    buildGaps (field.model.startPos + field.model.size) (List.drop 1 fieldsSorted) result

            Nothing ->
                if (expected /= registerSizeInBits) then
                    { startPos = expected, size = (registerSizeInBits - expected) } :: result
                else
                    result


fillGaps : List { startPos : Int, size : Int } -> List IndexedRegisterField -> List IndexedRegisterField
fillGaps gaps fields =
    case List.head gaps of
        Just gap ->
            let
                fields' =
                    insertField (RegisterField "(Reserved)" Reserved gap.startPos gap.size "") fields
            in
                fillGaps (List.drop 1 gaps) fields'

        Nothing ->
            fields


insertField : RegisterField -> List IndexedRegisterField -> List IndexedRegisterField
insertField newField fields =
    fields ++ [ { model = newField, buttonsEnabled = False, positionAndSizeEditable = False } ]


removeField : Int -> List IndexedRegisterField -> List IndexedRegisterField
removeField startPos fields =
    List.filter
        (\indexedField ->
            if indexedField.model.startPos /= startPos then
                True
            else
                False
        )
        fields


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

        EnableToolButtons startPos ->
            ( { model | fields = enableToolButtonsForField model.fields startPos }, Cmd.none )

        DisableToolButtons startPos ->
            ( { model | fields = disableToolButtonsForField model.fields startPos }, Cmd.none )

        InsertField startPos ->
            let
                newAdded =
                    insertField (fieldPrototype startPos) model.fields

                reservedRemoved =
                    filterReserved newAdded

                reservedUpdated =
                    fillGaps (buildGaps 0 reservedRemoved []) reservedRemoved

                sorted =
                    List.sortWith startPosSomparison reservedUpdated |> List.reverse
            in
                ( { model | fields = sorted }, Cmd.none )

        RemoveField startPos ->
            let
                fields' =
                    removeField startPos model.fields

                fields'' =
                    filterReserved fields'

                fields''' =
                    fillGaps (buildGaps 0 fields'' []) fields''

                fields'''' =
                    List.sortWith startPosSomparison fields''' |> List.reverse
            in
                ( { model | fields = fields'''' }, Cmd.none )

        EditPositionAndSize startPos ->
            ( { model | fields = setPositionAndSizeEditingForField model.fields startPos True }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN
