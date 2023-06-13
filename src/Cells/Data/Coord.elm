module Cells.Data.Coord exposing (Coord, fromSafeName, toName)

import Cells.Data.Column as Column exposing (Column)
import Cells.Data.Row as Row exposing (Row)


type alias Coord =
    { column : Column
    , row : Row
    }


fromSafeName : String -> Coord
fromSafeName name =
    case String.uncons name of
        Just ( c, t ) ->
            Coord
                (Column.fromSafeString <| String.fromChar c)
                (Row.fromSafeString t)

        Nothing ->
            Coord Column.first Row.first


toName : Coord -> String
toName { column, row } =
    Column.toString column ++ Row.toString row
