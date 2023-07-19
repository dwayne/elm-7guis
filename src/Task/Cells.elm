module Task.Cells exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Html as H
import Support.View.Frame as Frame
import Task.Cells.Data.Coord exposing (Coord)
import Task.Cells.Data.Sheet as Sheet exposing (Sheet)
import Task.Cells.View.Sheet as Sheet



-- MODEL


type alias Model =
    { sheet : Sheet
    , sheetModel : Sheet.Model
    }


init : Model
init =
    { sheet =
        Sheet.build
            [ ( "A0", "FRUIT" )
            , ( "A1", "Banana" )
            , ( "A2", "Pineapple" )
            , ( "A3", "Watermelon" )
            , ( "A4", "Grapes" )
            , ( "A5", "Mango" )
            , ( "A6", "Pawpaw" )
            , ( "B0", "PRICE PER UNIT (in TTD)" )
            , ( "B1", "=6" )
            , ( "B2", "=5" )
            , ( "B3", "=7" )
            , ( "B4", "=20" )
            , ( "B5", "=5" )
            , ( "B6", "=8" )
            , ( "C0", "UNITS" )
            , ( "C1", "=7" )
            , ( "C2", "=4" )
            , ( "C3", "=5" )
            , ( "C4", "=0.5" )
            , ( "C5", "=4" )
            , ( "C6", "=2" )
            , ( "D0", "PRICE (in TTD)" )
            , ( "D1", "=mul(B1, C1)" )
            , ( "D2", "=mul(B2, C2)" )
            , ( "D3", "=mul(B3, C3)" )
            , ( "D4", "=mul(B4, C4)" )
            , ( "D5", "=mul(B5, C5)" )
            , ( "D6", "=mul(B6, C6)" )
            , ( "C8", "TOTAL (in TTD)" )
            , ( "D8", "=sum(D1:D6)" )
            ]
    , sheetModel = Sheet.init
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
            Sheet.update { handlers = sheetHandlers, sheet = model.sheet } sheetMsg model.sheetModel
                |> Tuple.mapFirst (\sheetModel -> { model | sheetModel = sheetModel })

        Input coord rawInput ->
            ( { model | sheet = Sheet.set coord rawInput model.sheet }
            , Cmd.none
            )



-- VIEW


view : Model -> H.Html Msg
view { sheet, sheetModel } =
    Frame.view
        { modifier = Frame.Cells
        , title = "Cells"
        , body =
            Sheet.view
                { handlers = sheetHandlers
                , sheet = sheet
                }
                sheetModel
        }
