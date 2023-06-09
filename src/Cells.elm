module Cells exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Cells.Data.Cell as Cell exposing (Cell)
import Cells.Data.Coord exposing (Coord)
import Cells.Data.SCells as SCells exposing (SCells)
import Cells.View.Sheet as Sheet exposing (Sheet)
import Html as H



-- MODEL


type alias Model =
    { scells : SCells Cell
    , sheet : Sheet
    }


init : Model
init =
    { scells = SCells.empty
    , sheet = Sheet.init
    }


sheetHandlers : Sheet.Handlers Msg
sheetHandlers =
    { onChange = ChangedSheet
    , onInput = Input
    }



-- UPDATE


type Msg
    = ChangedSheet Sheet.Msg
    | Input Coord String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedSheet sheetMsg ->
            Sheet.update { handlers = sheetHandlers, scells = model.scells } sheetMsg model.sheet
                |> Tuple.mapFirst (\sheet -> { model | sheet = sheet })

        Input coord rawInput ->
            let
                cell =
                    Cell.fromString model.scells coord rawInput
            in
            ( { model | scells = SCells.set coord cell model.scells }
            , Cmd.none
            )



-- VIEW


view : Model -> H.Html Msg
view { scells, sheet } =
    Sheet.view { handlers = sheetHandlers, scells = scells } sheet
