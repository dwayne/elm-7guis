module Cells.Data.SCells exposing (SCells, empty, get, set)

import Cells.Data.Cell as Cell exposing (Cell)
import Cells.Data.Coord as Coord exposing (Coord)
import Dict exposing (Dict)


type SCells
    = SCells (Dict String Cell)


empty : SCells
empty =
    SCells Dict.empty


set : Coord -> Cell -> SCells -> SCells
set coord cell (SCells cells) =
    let
        name =
            Coord.toName coord
    in
    SCells <| Dict.insert name cell cells


get : Coord -> SCells -> Cell
get coord (SCells cells) =
    let
        name =
            Coord.toName coord
    in
    Dict.get name cells
        |> Maybe.withDefault Cell.empty
