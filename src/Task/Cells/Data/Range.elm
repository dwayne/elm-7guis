module Task.Cells.Data.Range exposing (Range, expand, fromSafeString, toString)

import Task.Cells.Data.Column as Column
import Task.Cells.Data.Coord as Coord exposing (Coord)
import Task.Cells.Data.Row as Row


type alias Range =
    { start : Coord
    , end : Coord
    }


fromSafeString : String -> Range
fromSafeString s =
    case String.split ":" s of
        [ start, end ] ->
            Range (Coord.fromSafeString start) (Coord.fromSafeString end)

        _ ->
            Range Coord.first Coord.first


expand : Range -> List Coord
expand { start, end } =
    let
        startColumn =
            Column.toInt start.column

        startRow =
            Row.toInt start.row

        endColumn =
            Column.toInt end.column

        endRow =
            Row.toInt end.row

        columns =
            List.range startColumn endColumn
                |> List.map Column.fromSafeInt

        rows =
            List.range startRow endRow
                |> List.map Row.fromSafeInt

        coordsForColumn c =
            List.map (Coord c) rows
    in
    List.concatMap coordsForColumn columns


toString : Range -> String
toString { start, end } =
    Coord.toString start ++ ":" ++ Coord.toString end
