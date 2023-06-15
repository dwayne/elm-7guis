module Test.Cells.Data.Formula.Parser exposing (suite)

import Cells.Data.Coord as Coord
import Cells.Data.Formula.AST exposing (Expr(..), Formula(..))
import Cells.Data.Formula.Parser as P
import Cells.Data.Range as Range
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Parser"
        [ textSuite
        , exprSuite
        ]


textSuite : Test
textSuite =
    describe "Text"
        [ parse "" <|
            Just <|
                Text ""
        , parse "This is text." <|
            Just <|
                Text "This is text."
        ]


exprSuite : Test
exprSuite =
    describe "Expr"
        [ parse "=5" <|
            Just <|
                Expr (Number 5)
        , parse "=A0" <|
            Just <|
                Expr (Cell (Coord.fromSafeString "A0"))
        , parse "=B0:B9" <|
            Just <|
                Expr (Range (Range.fromSafeString "B0:B9"))
        , parse "=add(A0,A1)" <|
            Just <|
                Expr
                    (Application "add"
                        [ Cell (Coord.fromSafeString "A0")
                        , Cell (Coord.fromSafeString "A1")
                        ]
                    )
        ]


parse : String -> Maybe Formula -> Test
parse input maybeFormula =
    test ("input = \"" ++ input ++ "\"") <|
        \_ ->
            P.parse input
                |> Result.toMaybe
                |> Expect.equal maybeFormula
