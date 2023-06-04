module Cells exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Cells.Sheet as Sheet
import Html as H



-- MODEL


type alias Model =
    { sheet : Sheet.Model
    }


init : Model
init =
    { sheet = Sheet.init
    }


sheetHandlers : Sheet.Handlers Msg
sheetHandlers =
    { onChange = ChangedSheet
    }



-- UPDATE


type Msg
    = ChangedSheet Sheet.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedSheet sheetMsg ->
            Sheet.update { handlers = sheetHandlers } sheetMsg model.sheet
                |> Tuple.mapFirst (\sheet -> { model | sheet = sheet })



-- VIEW


view : Model -> H.Html Msg
view { sheet } =
    Sheet.view { handlers = sheetHandlers } sheet
