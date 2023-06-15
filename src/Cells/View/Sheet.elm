module Cells.View.Sheet exposing
    ( Handlers
    , Model
    , Msg
    , UpdateOptions
    , ViewOptions
    , init
    , update
    , view
    )

import Browser.Dom as BD
import Cells.Data.Cell as Cell exposing (Cell)
import Cells.Data.Column as Column exposing (Column)
import Cells.Data.Coord as Coord exposing (Coord)
import Cells.Data.Grid as Grid exposing (Grid)
import Cells.Data.Row as Row exposing (Row)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Task



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
    , grid : Grid
    }


type Msg
    = DoubleClickedCell Coord
    | FocusedInput
    | BlurredInput
    | Input String
    | PressedEsc
    | PressedEnter


update : UpdateOptions msg -> Msg -> Model -> ( Model, Cmd msg )
update { handlers, grid } msg (Model state) =
    case msg of
        DoubleClickedCell coord ->
            let
                cell =
                    Grid.get coord grid
            in
            ( Model
                { state
                    | maybeEdit =
                        Just
                            { coord = coord
                            , rawInput = Cell.toInputString cell
                            }
                }
            , focus (inputId coord) (handlers.onChange FocusedInput)
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
                    dispatch <| handlers.onInput coord rawInput

                Nothing ->
                    Cmd.none
            )


focus : String -> msg -> Cmd msg
focus id msg =
    BD.focus id
        |> Task.attempt (always msg)


dispatch : msg -> Cmd msg
dispatch =
    Task.succeed >> Task.perform identity



-- VIEW


type alias ViewOptions msg =
    { handlers : Handlers msg
    , grid : Grid
    }


view : ViewOptions msg -> Model -> H.Html msg
view options (Model state) =
    H.div [ HA.class "sheet" ]
        [ H.table [ HA.class "sheet__table" ]
            [ viewColumnHeaders
            , viewRows options state
            ]
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
viewCell { handlers, grid } { maybeEdit } coord =
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
                        , HA.class "sheet__input"
                        , HA.type_ "text"
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
                    [ H.text <| Cell.toString <| Grid.get coord grid ]


inputId : Coord -> String
inputId coord =
    "sheet__input-" ++ Coord.toName coord


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
                                JD.fail <| "must be one of the codes 13 or 27: " ++ String.fromInt code
                    )
    in
    HE.on "keydown" decoder
