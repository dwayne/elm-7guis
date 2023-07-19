module Task.Cells.View.Sheet exposing
    ( Handlers
    , Model
    , Msg
    , UpdateOptions
    , ViewOptions
    , init
    , update
    , view
    )

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Support.Lib as Lib
import Task.Cells.Data.Cell as Cell
import Task.Cells.Data.Column as Column exposing (Column)
import Task.Cells.Data.Coord as Coord exposing (Coord)
import Task.Cells.Data.Row as Row exposing (Row)
import Task.Cells.Data.Sheet as Sheet exposing (Sheet)



-- MODEL


type Model
    = Model State


type alias State =
    { maybeEdit : Maybe Edit
    }


type alias Edit =
    { coord : Coord
    , rawInput : String
    }


type alias Handlers msg =
    { onChange : Msg -> msg
    , onInput : Coord -> String -> msg
    }


init : Model
init =
    Model initState


initState : State
initState =
    { maybeEdit = Nothing
    }



-- UPDATE


type alias UpdateOptions msg =
    { handlers : Handlers msg
    , sheet : Sheet
    }


type Msg
    = DoubleClickedCell Coord
    | FocusedInput
    | BlurredInput
    | Input String
    | PressedEsc
    | PressedEnter


update : UpdateOptions msg -> Msg -> Model -> ( Model, Cmd msg )
update { handlers, sheet } msg (Model state) =
    case msg of
        DoubleClickedCell coord ->
            let
                cell =
                    Sheet.get coord sheet
            in
            ( Model
                { state
                    | maybeEdit =
                        Just
                            { coord = coord
                            , rawInput = Cell.toEditableString cell
                            }
                }
            , Lib.focus (inputId coord) (handlers.onChange FocusedInput)
            )

        FocusedInput ->
            ( Model state
            , Cmd.none
            )

        BlurredInput ->
            ( Model { state | maybeEdit = Nothing }
            , Cmd.none
            )

        Input rawInput ->
            ( Model { state | maybeEdit = Maybe.map (\edit -> { edit | rawInput = rawInput }) state.maybeEdit }
            , Cmd.none
            )

        PressedEsc ->
            ( Model { state | maybeEdit = Nothing }
            , Cmd.none
            )

        PressedEnter ->
            ( Model { state | maybeEdit = Nothing }
            , case state.maybeEdit of
                Just { coord, rawInput } ->
                    Lib.dispatch <| handlers.onInput coord rawInput

                Nothing ->
                    Cmd.none
            )



-- VIEW


type alias ViewOptions msg =
    { handlers : Handlers msg
    , sheet : Sheet
    }


view : ViewOptions msg -> Model -> H.Html msg
view options (Model state) =
    H.table [ HA.class "sheet" ]
        [ viewColumnHeaders
        , viewRows options state
        ]


viewColumnHeaders : H.Html msg
viewColumnHeaders =
    let
        noTitle =
            H.th
                [ HA.class "sheet__th sheet__row-headers-column-header" ]
                []

        columns =
            Column.map viewColumnHeader
    in
    H.thead
        [ HA.class "sheet__thead" ]
        [ H.tr [ HA.class "sheet__tr" ] (noTitle :: columns) ]


viewColumnHeader : Column -> H.Html msg
viewColumnHeader column =
    H.th
        [ HA.class "sheet__th sheet__column-header" ]
        [ H.text <| Column.toString column ]


viewRows : ViewOptions msg -> State -> H.Html msg
viewRows options state =
    H.tbody [ HA.class "sheet__tbody" ] <| Row.map (viewRow options state)


viewRow : ViewOptions msg -> State -> Row -> H.Html msg
viewRow options state row =
    let
        header =
            H.th
                [ HA.class "sheet__th sheet__row-header" ]
                [ H.text <| Row.toString row ]

        viewCellWrapper column =
            viewCell options state (Coord column row)
    in
    H.tr [ HA.class "sheet__tr" ] (header :: Column.map viewCellWrapper)


viewCell : ViewOptions msg -> State -> Coord -> H.Html msg
viewCell { handlers, sheet } { maybeEdit } coord =
    let
        maybeBeingEdited =
            maybeEdit
                |> Maybe.andThen
                    (\edit ->
                        if edit.coord == coord then
                            Just edit

                        else
                            Nothing
                    )
    in
    H.map handlers.onChange <|
        case maybeBeingEdited of
            Just edit ->
                H.td
                    [ HA.class "sheet__td sheet__cell" ]
                    [ H.input
                        [ HA.id <| inputId coord
                        , HA.class "sheet__cell-input"
                        , HA.type_ "text"
                        , HA.spellcheck False
                        , HA.value edit.rawInput
                        , HE.onInput Input
                        , HE.onBlur BlurredInput
                        , onKey
                            { esc = PressedEsc
                            , enter = PressedEnter
                            }
                        ]
                        []
                    ]

            Nothing ->
                H.td
                    [ HA.class "sheet__td sheet__cell"
                    , HE.onDoubleClick <| DoubleClickedCell coord
                    ]
                    [ let
                        cell =
                            Sheet.get coord sheet
                      in
                      H.div
                        [ HA.class "sheet__cell-value"
                        , HA.classList
                            [ ( "sheet__cell-value--has-error"
                              , Cell.hasError cell
                              )
                            ]
                        ]
                        [ H.text <| Cell.toString cell
                        ]
                    ]


inputId : Coord -> String
inputId coord =
    "sheet__input-" ++ Coord.toString coord


onKey : { esc : msg, enter : msg } -> H.Attribute msg
onKey { esc, enter } =
    let
        decoder =
            HE.keyCode
                |> JD.andThen
                    (\code ->
                        case code of
                            13 ->
                                JD.succeed enter

                            27 ->
                                JD.succeed esc

                            _ ->
                                JD.fail "ignored"
                    )
    in
    HE.on "keydown" decoder
