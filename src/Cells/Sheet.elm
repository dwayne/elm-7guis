module Cells.Sheet exposing
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
import Cells.Column as Column exposing (Column)
import Cells.Coord as Coord exposing (Coord)
import Cells.Row as Row exposing (Row)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Task



-- MODEL


type Model
    = Model State


type alias State =
    { maybeEdit : Maybe Edit
    }


type alias Edit =
    { coord : Coord
    , value : String
    }


type alias Handlers msg =
    { onChange : Msg -> msg
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
    }


type Msg
    = DoubleClickedCell Coord
    | FocusedInput
    | BlurredInput


update : UpdateOptions msg -> Msg -> Model -> ( Model, Cmd msg )
update options msg (Model state) =
    case msg of
        DoubleClickedCell coord ->
            ( Model { state | maybeEdit = Just { coord = coord, value = "=add(1, 2)" } }
            , focus (inputId coord) (options.handlers.onChange FocusedInput)
            )

        FocusedInput ->
            ( Model state
            , Cmd.none
            )

        BlurredInput ->
            ( Model { state | maybeEdit = Nothing }
            , Cmd.none
            )


focus : String -> msg -> Cmd msg
focus id msg =
    BD.focus id
        |> Task.attempt (always msg)



-- VIEW


type alias ViewOptions msg =
    { handlers : Handlers msg
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
viewCell { handlers } { maybeEdit } coord =
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
    case maybeBeingEdited of
        Just edit ->
            H.td
                [ HA.class "sheet__td sheet__cell" ]
                [ H.input
                    [ HA.id <| inputId coord
                    , HA.class "sheet__input"
                    , HA.type_ "text"
                    , HA.value edit.value
                    , HE.onBlur <| handlers.onChange BlurredInput
                    ]
                    []
                ]

        Nothing ->
            H.td
                [ HA.class "sheet__td sheet__cell"
                , HE.onDoubleClick <| handlers.onChange <| DoubleClickedCell coord
                ]
                []


inputId : Coord -> String
inputId coord =
    "sheet__input-" ++ Coord.toString coord
