module Cells exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Cells.Data.Coord exposing (Coord)
import Cells.Data.SCells as SCells exposing (SCells)
import Cells.View.Sheet as Sheet exposing (Sheet)
import Html as H



-- MODEL


type alias Model =
    { scells : SCells
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

        Input coord value ->
            ( { model | scells = SCells.set coord value model.scells }
            , Cmd.none
            )



-- VIEW


view : Model -> H.Html Msg
view { scells, sheet } =
    Sheet.view { handlers = sheetHandlers, scells = scells } sheet
