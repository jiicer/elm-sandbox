module RegisterWidget exposing (..)

import Html exposing (Html, div, text, input, h2, h4, a, span, table, tbody, thead, th, td, tr, button, form, i)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toInt)
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup


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
    }


type alias Model =
    { name : String
    , fields : List IndexedRegisterField
    , editable : Bool
    , collapsed : Bool
    }


init : String -> ( Model, Cmd Msg )
init nm =
    ( Model nm [ { model = RegisterField "(Reserved)" Reserved 0 32 "", buttonsEnabled = False } ] False True
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
    | EditSize Int String



-- VIEW


emptyHtml : Html msg
emptyHtml =
    span [ style [ ( "display", "none" ) ] ] []


collapseGlyphicon : Bool -> String
collapseGlyphicon collapsed =
    if collapsed == True then
        "fa fa-caret-square-o-down"
    else
        "fa fa-caret-square-o-up"


titleView : Bool -> Bool -> String -> Int -> List (Html Msg)
titleView collapsed editable title collapseId =
    if editable == True then
        [ input [ type_ "text", onInput ChangeTitle, onClick ApplyTitle, defaultValue title ] []
        , button [ class "btn btn-secondary btn-sm", type_ "button", attribute "data-target" ("#collapse" ++ (toString collapseId)), attribute "data-toggle" "collapse", onClick Collapse ]
            [ span [ class (collapseGlyphicon collapsed), attribute "aria-hidden" "true" ] [] ]
        ]
    else
        [ span [ class "panel-title", onClick EditTitle ] [ text title ]
        , button [ class "btn btn-secondary btn-sm", type_ "button", attribute "data-target" ("#collapse" ++ (toString collapseId)), attribute "data-toggle" "collapse", onClick Collapse ]
            [ span [ class (collapseGlyphicon collapsed), attribute "aria-hidden" "true" ] [] ]
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


leadingReservedSpace : List IndexedRegisterField -> Int -> Int
leadingReservedSpace fields startPos =
    case List.head fields of
        Just field ->
            if (field.model.startPos == startPos) then
                if (field.model.accessType == Reserved) then
                    field.model.size
                else
                    0
            else
                leadingReservedSpace (List.drop 1 fields) startPos

        Nothing ->
            0


viewToolButtons : IndexedRegisterField -> List IndexedRegisterField -> Html Msg
viewToolButtons field allFields =
    let
        cellAttr =
            if field.buttonsEnabled == True then
                [ style [ ( "width", "200px" ) ] ]
            else
                [ style [ ( "visibility", "hidden" ), ( "width", "200px" ) ] ]

        viewInsert =
            if (field.model.accessType == Reserved) then
                ButtonGroup.buttonGroup [ ButtonGroup.small ] [ ButtonGroup.button [ Button.secondary, Button.onClick (InsertField field.model.startPos) ] [ span [ class "fa fa-plus", attribute "aria-hidden" "true" ] [] ] ]
            else
                emptyHtml

        viewRemove =
            if (field.model.accessType /= Reserved) then
                ButtonGroup.buttonGroup [ ButtonGroup.small ] [ ButtonGroup.button [ Button.secondary, Button.onClick (RemoveField field.model.startPos) ] [ span [ class "fa fa-trash", attribute "aria-hidden" "true" ] [] ] ]
            else
                emptyHtml

        viewSizeSlider =
            if (field.model.accessType /= Reserved) then
                let
                    min_ =
                        1

                    max_ =
                        field.model.size + (leadingReservedSpace allFields (field.model.startPos + field.model.size))
                in
                    if (max_ > min_) then
                        input
                            [ style [ ( "width", "100px" ) ]
                            , class "form-control"
                            , type_ "range"
                            , value (toString field.model.size)
                            , Html.Attributes.min (toString min_)
                            , Html.Attributes.max (toString max_)
                            , onInput (EditSize field.model.startPos)
                            ]
                            []
                    else
                        emptyHtml
            else
                emptyHtml
    in
        td cellAttr
            [ div [ class "input-group" ] [ viewRemove, viewInsert, viewSizeSlider ] ]


viewFieldRow : IndexedRegisterField -> Html Msg -> Html Msg
viewFieldRow field buttons =
    tr [ onMouseEnter (EnableToolButtons field.model.startPos), onMouseLeave (DisableToolButtons field.model.startPos) ]
        [ td []
            [ text field.model.name ]
        , td []
            [ text (accessTypeToString field.model.accessType) ]
        , td []
            [ text (registerFieldBitsToString field.model.startPos field.model.size) ]
        , td []
            [ text field.model.description ]
        , buttons
        ]


viewFieldBody : List IndexedRegisterField -> Html Msg
viewFieldBody fields =
    tbody [] (List.map (\indexedField -> viewFieldRow indexedField (viewToolButtons indexedField fields)) fields)


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
                            [ div [ class "col-md-9" ]
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


setFieldSize : List IndexedRegisterField -> Int -> Int -> List IndexedRegisterField
setFieldSize indexedFields startPos newSize =
    let
        sizeUpdated =
            List.map
                (\indexedField ->
                    if indexedField.model.startPos == startPos then
                        let
                            model_ =
                                indexedField.model

                            model__ =
                                { model_ | size = newSize }
                        in
                            { indexedField | model = model__ }
                    else
                        indexedField
                )
                indexedFields

        reservedRemoved =
            filterReserved sizeUpdated

        reservedUpdated =
            fillGaps (buildGaps 0 reservedRemoved []) reservedRemoved
    in
        List.sortWith startPosSomparison reservedUpdated |> List.reverse


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
                fields_ =
                    insertField (RegisterField "(Reserved)" Reserved gap.startPos gap.size "") fields
            in
                fillGaps (List.drop 1 gaps) fields_

        Nothing ->
            fields


insertField : RegisterField -> List IndexedRegisterField -> List IndexedRegisterField
insertField newField fields =
    fields ++ [ { model = newField, buttonsEnabled = False } ]


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
                fields_ =
                    removeField startPos model.fields

                fields__ =
                    filterReserved fields_

                fields___ =
                    fillGaps (buildGaps 0 fields__ []) fields__

                fields____ =
                    List.sortWith startPosSomparison fields___ |> List.reverse
            in
                ( { model | fields = fields____ }, Cmd.none )

        EditSize startPos newSize ->
            ( { model | fields = setFieldSize model.fields startPos (String.toInt (Debug.log "" newSize) |> Result.withDefault 0) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN
