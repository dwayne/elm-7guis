module Cells.Data.Grid exposing (Grid, empty, get, set)

import Cells.Data.Cell as Cell exposing (Cell)
import Cells.Data.Coord as Coord exposing (Coord)
import Dict exposing (Dict)


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
