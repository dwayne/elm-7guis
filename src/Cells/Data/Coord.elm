module Cells.Data.Coord exposing (Coord, fromSafeString, toName)

import Cells.Data.Column as Column exposing (Column)
import Cells.Data.Row as Row exposing (Row)


type alias Coord =
    { column : Column
    , row : Row
    }


fromSafeString : String -> Coord
fromSafeString s =
    case String.uncons s of
        Just ( c, t ) ->
            Coord
                (Column.fromSafeString <| String.fromChar c)
                (Row.fromSafeString t)

        Nothing ->
            Coord Column.first Row.first


toName : Coord -> String
toName { column, row } =
    Column.toString column ++ Row.toString row
