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
    { id : Int
    , model : RegisterField
    , buttonsEnabled : Bool
    }


type alias Model =
    { name : String
    , field : RegisterField
    , fields : List IndexedRegisterField
    , nextId : Int
    , availableIds : List Int
    , editable : Bool
    , collapsed : Bool
    }


init : String -> ( Model, Cmd Msg )
init nm =
    ( Model nm (RegisterField "Config" ReadWrite 0 1 "Configure me.") [ { id = 0, model = RegisterField "(Reserved)" Reserved 0 32 "", buttonsEnabled = False } ] 1 [] False True
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
            else if (field.model.startPos + field.model.size < registerSizeInBits) then
                button [ class "btn btn-default btn-sm", type' "button", onClick (InsertField (field.model.startPos + field.model.size)) ] [ span [ class "glyphicon glyphicon-plus" ] [] ]
            else
                emptyHtml

        viewRemove =
            if (field.model.accessType /= Reserved) then
                button [ class "btn btn-default btn-sm", type' "button", onClick (RemoveField field.id) ] [ span [ class "glyphicon glyphicon-trash" ] [] ]
            else
                emptyHtml
    in
        td [ cellAttr ]
            [ viewRemove, viewInsert ]


viewFieldRow : RegisterField -> Int -> Html Msg -> Html Msg
viewFieldRow field id buttons =
    tr [ onMouseEnter (EnableToolButtons id), onMouseLeave (DisableToolButtons id) ]
        [ td []
            [ text field.name ]
        , td []
            [ text (accessTypeToString field.accessType) ]
        , td []
            [ text (registerFieldBitsToString field.startPos field.size) ]
        , td []
            [ text field.description ]
        , buttons
        ]


viewFieldBody : List IndexedRegisterField -> Html Msg
viewFieldBody fields =
    tbody [] (List.map (\indexedField -> viewFieldRow indexedField.model indexedField.id (viewToolButtons indexedField fields)) fields)


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
                                    , viewFieldBody [ { id = 0, model = model.field, buttonsEnabled = False } ]
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
setToolButtonsForField indexedFields id value =
    List.map
        (\indexedField ->
            if indexedField.id == id then
                { indexedField | buttonsEnabled = value }
            else
                indexedField
        )
        indexedFields


enableToolButtonsForField : List IndexedRegisterField -> Int -> List IndexedRegisterField
enableToolButtonsForField indexedFields id =
    setToolButtonsForField indexedFields id True


disableToolButtonsForField : List IndexedRegisterField -> Int -> List IndexedRegisterField
disableToolButtonsForField indexedFields id =
    setToolButtonsForField indexedFields id False


fieldPrototype : Int -> RegisterField
fieldPrototype startPos =
    RegisterField "(New Field)" ReadWrite startPos 1 "(Insert Description)"


startPosSomparison a b =
    compare a.model.startPos b.model.startPos


resizeFieldFromLsb : RegisterField -> Int -> RegisterField
resizeFieldFromLsb field reduction =
    { field
        | startPos = field.startPos + reduction
        , size = field.size - reduction
    }


reduceOverlappingField : List IndexedRegisterField -> RegisterField -> List IndexedRegisterField
reduceOverlappingField indexedFields newField =
    List.map
        (\indexedField ->
            if (List.member newField.startPos [indexedField.model.startPos..indexedField.model.startPos + indexedField.model.size - 1]) == True then
                { indexedField | model = resizeFieldFromLsb indexedField.model newField.size }
            else
                indexedField
        )
        indexedFields


insertField : RegisterField -> List IndexedRegisterField -> List Int -> Int -> ( List IndexedRegisterField, List Int, Int )
insertField newField fields usedIds nextId =
    case List.head usedIds of
        Just head ->
            let
                newIndexedField =
                    { id = head, model = newField, buttonsEnabled = False }

                fields' =
                    reduceOverlappingField fields newField

                fields'' =
                    fields' ++ [ newIndexedField ] |> List.sortWith startPosSomparison
            in
                ( fields'', List.drop 1 usedIds, nextId )

        Nothing ->
            let
                newIndexedField =
                    { id = nextId, model = newField, buttonsEnabled = False }

                fields' =
                    reduceOverlappingField fields newField

                fields'' =
                    fields' ++ [ newIndexedField ] |> List.sortWith startPosSomparison
            in
                ( fields'', usedIds, nextId + 1 )


removeField : Int -> List IndexedRegisterField -> List Int -> ( List IndexedRegisterField, List Int )
removeField id fields availableIds =
    ( List.filter
        (\indexedField ->
            if indexedField.id /= id then
                True
            else
                False
        )
        fields
    , availableIds ++ [ id ]
    )


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
            ( { model | fields = enableToolButtonsForField model.fields id }, Cmd.none )

        DisableToolButtons id ->
            ( { model | fields = disableToolButtonsForField model.fields id }, Cmd.none )

        InsertField startPos ->
            let
                ( fields', availableIds', nextId' ) =
                    insertField (fieldPrototype startPos) model.fields model.availableIds model.nextId
            in
                ( { model | fields = fields', availableIds = availableIds', nextId = nextId' }, Cmd.none )

        RemoveField id ->
            let
                ( fields', availableIds' ) =
                    removeField id model.fields model.availableIds
            in
                ( { model | fields = fields', availableIds = availableIds' }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN
