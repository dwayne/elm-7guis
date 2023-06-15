module Cells.Data.Sheet exposing
    ( Sheet
    , empty
    , get
    , set
    )

import Cells.Data.Cell as Cell exposing (Cell)
import Cells.Data.Coord as Coord exposing (Coord)
import Cells.Data.DirectedGraph as DirectedGraph exposing (DirectedGraph)
import Cells.Data.Grid as Grid exposing (Grid)
import Set


type Sheet
    = Sheet
        { grid : Grid
        , dependencyGraph : DirectedGraph
        }


empty : Sheet
empty =
    Sheet
        { grid = Grid.empty
        , dependencyGraph = DirectedGraph.empty
        }


get : Coord -> Sheet -> Cell
get coord (Sheet { grid }) =
    Grid.get coord grid


set : Coord -> String -> Sheet -> Sheet
set coord rawInput ((Sheet { grid, dependencyGraph }) as sheet) =
    let
        localEnv =
            env grid

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

        newDependencyGraph =
            dependencyGraph
                |> DirectedGraph.removeEdges edgesToBeRemoved
                |> DirectedGraph.addEdges edgesToBeAdded
    in
    refresh coord newCell dest newDependencyGraph grid
        |> Maybe.map
            (\newGrid ->
                Sheet
                    { grid = newGrid
                    , dependencyGraph = newDependencyGraph
                    }
            )
        |> Maybe.withDefault sheet


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
