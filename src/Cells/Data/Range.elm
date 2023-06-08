module Cells.Data.Range exposing (Range)

import Cells.Data.Coord exposing (Coord)


type alias Range =
    { start : Coord
    , end : Coord
    }
