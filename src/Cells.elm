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
import Cells.Data.Grid as Grid exposing (Grid)
import Cells.View.Sheet as Sheet
import Html as H
import Set



-- MODEL


type alias Model =
    { grid : Grid
    , dependencyGraph : DirectedGraph
    , sheet : Sheet.Model
    }


init : Model
init =
    { grid = Grid.empty
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
            Sheet.update { handlers = sheetHandlers, grid = model.grid } sheetMsg model.sheet
                |> Tuple.mapFirst (\sheet -> { model | sheet = sheet })

        Input coord rawInput ->
            let
                localEnv =
                    env model.grid

                oldCell =
                    localEnv coord

                oldReferences =
                    Cell.references oldCell

                newCell =
                    Cell.fromString localEnv rawInput

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
            ( refresh coord newCell dest dependencyGraph model.grid
                |> Maybe.map
                    (\grid ->
                        { model
                            | grid = grid
                            , dependencyGraph = dependencyGraph
                        }
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )


refresh : Coord -> Cell -> String -> DirectedGraph -> Grid -> Maybe Grid
refresh startCoord startCell startName dependencyGraph grid =
    DirectedGraph.tsort startName dependencyGraph
        |> Maybe.andThen List.tail
        |> Maybe.map
            (List.foldl
                (\name nextGrid ->
                    let
                        refreshedCell =
                            Cell.refresh localEnv cell

                        localEnv =
                            env nextGrid

                        cell =
                            localEnv coord

                        coord =
                            Coord.fromSafeName name
                    in
                    Grid.set coord refreshedCell nextGrid
                )
                (Grid.set startCoord startCell grid)
            )


env : Grid -> Coord -> Cell
env grid coord =
    Grid.get coord grid



-- VIEW


view : Model -> H.Html Msg
view { grid, sheet } =
    Sheet.view { handlers = sheetHandlers, grid = grid } sheet
