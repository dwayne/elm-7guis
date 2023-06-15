module Cells.Data.Coord exposing (Coord, first, fromSafeString, toString)

import Cells.Data.Column as Column exposing (Column)
import Cells.Data.Row as Row exposing (Row)


type alias Coord =
    { column : Column
    , row : Row
    }


first : Coord
first =
    Coord Column.first Row.first


fromSafeString : String -> Coord
fromSafeString s =
    case String.uncons s of
        Just ( c, t ) ->
            Coord
                (Column.fromSafeString <| String.fromChar c)
                (Row.fromSafeString t)

        Nothing ->
            first


toString : Coord -> String
toString { column, row } =
    Column.toString column ++ Row.toString row
