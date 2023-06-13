module Cells.Data.Formula.AST exposing
    ( Expr(..)
    , Formula(..)
    )

import Cells.Data.Coord exposing (Coord)
import Cells.Data.Range exposing (Range)


type Formula
    = Expr Expr
    | Text String


type Expr
    = Number Float
    | Cell Coord
    | Range Range
    | Application String (List Expr)
