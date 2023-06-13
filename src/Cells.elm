module Cells exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Cells.Data.Cell as Cell exposing (Cell)
import Cells.Data.Coord as Coord exposing (Coord)
import Cells.Data.DirectedGraph as DirectedGraph exposing (DirectedGraph)
import Cells.Data.SCells as SCells exposing (SCells)
import Cells.View.Sheet as Sheet exposing (Sheet)
import Html as H
import Set



-- MODEL


type alias Model =
    { scells : SCells Cell
    , dependencyGraph : DirectedGraph
    , sheet : Sheet
    }


init : Model
init =
    { scells = SCells.empty
    , dependencyGraph = DirectedGraph.empty
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
                oldCell =
                    SCells.get Cell.empty coord model.scells

                oldReferences =
                    Cell.references oldCell

                newCell =
                    Cell.fromString model.scells coord rawInput

                newReferences =
                    Cell.references newCell

                dest =
                    Coord.toName coord

                edgesToBeRemoved =
                    Set.diff oldReferences newReferences
                        |> Set.map (\src -> ( src, dest ))

                edgesToBeAdded =
                    Set.diff newReferences oldReferences
                        |> Set.map (\src -> ( src, dest ))

                dependencyGraph =
                    model.dependencyGraph
                        |> DirectedGraph.removeEdges edgesToBeRemoved
                        |> DirectedGraph.addEdges edgesToBeAdded
            in
            ( refresh coord newCell dest dependencyGraph model.scells
                |> Maybe.map
                    (\scells ->
                        { model
                            | scells = scells
                            , dependencyGraph = dependencyGraph
                        }
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )


refresh : Coord -> Cell -> String -> DirectedGraph -> SCells Cell -> Maybe (SCells Cell)
refresh startCoord startCell startName dependencyGraph scells =
    DirectedGraph.tsort startName dependencyGraph
        |> Maybe.map
            (List.foldl
                (\name nextScells ->
                    let
                        coord =
                            Coord.fromSafeString name

                        refreshedCell =
                            nextScells
                                |> SCells.get Cell.empty coord
                                |> Cell.refresh nextScells
                    in
                    SCells.set coord refreshedCell nextScells
                )
                (SCells.set startCoord startCell scells)
            )



-- VIEW


view : Model -> H.Html Msg
view { scells, sheet } =
    Sheet.view { handlers = sheetHandlers, scells = scells } sheet
