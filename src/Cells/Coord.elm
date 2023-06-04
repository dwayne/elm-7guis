module Cells.Coord exposing (Coord, toString)

import Cells.Column as Column exposing (Column)
import Cells.Row as Row exposing (Row)


type alias Coord =
    { column : Column
    , row : Row
    }


toString : Coord -> String
toString { column, row } =
    Column.toString column ++ Row.toString row
