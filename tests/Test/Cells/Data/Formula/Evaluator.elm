module Test.Cells.Data.Formula.Evaluator exposing (suite)

import Cells.Data.Coord as Coord
import Cells.Data.Formula.Evaluator as E
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Evaluator"
        [ evalStringSuite
        ]


evalStringSuite : Test
evalStringSuite =
    describe "evalString"
        [ evalString "This is text." <|
            Ok 0
        , evalString "=5" <|
            Ok 5
        , evalString "=A0" <|
            Ok 8
        , evalString "=add(2, 7)" <|
            Ok 9
        , evalString "=sub(19, add(2, 7))" <|
            Ok 10
        , evalString "=mul(3, 5)" <|
            Ok 15
        , evalString "=div(100, 5)" <|
            Ok 20
        , evalString "=mod(A0, 2)" <|
            Ok 0
        , evalString "=sum(A1:Z1)" <|
            Ok 26
        , evalString "=prod(1, 2, 3, 4, 5)" <|
            Ok 120
        , evalString "=x" <|
            Err SyntaxError
        , evalString "=div(1, 0)" <|
            Err <|
                RuntimeError E.DivisionByZero
        ]


type Error
    = SyntaxError
    | RuntimeError E.RuntimeError


evalString : String -> Result Error Int -> Test
evalString rawInput expected =
    let
        env coord =
            case Coord.toName coord of
                "A0" ->
                    8

                _ ->
                    1
    in
    test rawInput <|
        \_ ->
            let
                actual =
                    case E.evalString env rawInput of
                        E.Formula { value } ->
                            Ok <| truncate value

                        E.RuntimeError { error } ->
                            Err <| RuntimeError error

                        E.SyntaxError _ ->
                            Err SyntaxError
            in
            Expect.equal expected actual
