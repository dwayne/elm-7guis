module Task.Cells.Data.Grid exposing (Grid, empty, get, set)

import Dict exposing (Dict)
import Task.Cells.Data.Cell as Cell exposing (Cell)
import Task.Cells.Data.Coord as Coord exposing (Coord)


type Grid
    = Grid (Dict String Cell)


empty : Grid
empty =
    Grid Dict.empty


set : Coord -> Cell -> Grid -> Grid
set coord cell (Grid cells) =
    let
        name =
            Coord.toString coord
    in
    Grid <| Dict.insert name cell cells


get : Coord -> Grid -> Cell
get coord (Grid cells) =
    let
        name =
            Coord.toString coord
    in
    Dict.get name cells
        |> Maybe.withDefault Cell.empty
