module Cells.Data.Coord exposing (Coord, toName)

import Cells.Data.Column as Column exposing (Column)
import Cells.Data.Row as Row exposing (Row)


type alias Coord =
    { column : Column
    , row : Row
    }


toName : Coord -> String
toName { column, row } =
    Column.toString column ++ Row.toString row
