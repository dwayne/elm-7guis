module Cells.Data.Range exposing (Range, expand)

import Cells.Data.Column as Column
import Cells.Data.Coord exposing (Coord)
import Cells.Data.Row as Row


type alias Range =
    { start : Coord
    , end : Coord
    }


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
