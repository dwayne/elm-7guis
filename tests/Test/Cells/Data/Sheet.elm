module Test.Cells.Data.Sheet exposing (suite)

import Cells.Data.Cell as Cell
import Cells.Data.Coord as Coord
import Cells.Data.Sheet as Sheet exposing (Sheet)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Sheet"
        [ examplesSuite
        , changePropagationSuite
        ]


examplesSuite : Test
examplesSuite =
    describe "examples"
        [ factorialSuite
        , fibonacciSuite
        ]


factorialSuite : Test
factorialSuite =
    let
        sheet =
            Sheet.build
                [ ( "A0", "=0" )
                , ( "B0", "=1" )
                , ( "A1", "=1" )
                , ( "B1", "=1" )
                , ( "A2", "=2" )
                , ( "B2", "=mul(A2, B1)" )
                , ( "A3", "=3" )
                , ( "B3", "=mul(A3, B2)" )
                , ( "A4", "=4" )
                , ( "B4", "=mul(A4, B3)" )
                , ( "A5", "=5" )
                , ( "B5", "=mul(A5, B4)" )
                , ( "A6", "=6" )
                , ( "B6", "=mul(A6, B5)" )
                , ( "A7", "=7" )
                , ( "B7", "=mul(A7, B6)" )
                , ( "A8", "=8" )
                , ( "B8", "=mul(A8, B7)" )
                , ( "A9", "=9" )
                , ( "B9", "=mul(A9, B8)" )
                , ( "A10", "=10" )
                , ( "B10", "=mul(A10, B9)" )
                , ( "C10", "=prod(A1:A10)" )
                ]
    in
    describe "factorial"
        [ test "B10 = 10!" <|
            \_ ->
                get "B10" sheet
                    |> Expect.equal "3628800"
        , test "C10 = 10!" <|
            \_ ->
                get "C10" sheet
                    |> Expect.equal "3628800"
        ]


fibonacciSuite : Test
fibonacciSuite =
    let
        sheet =
            Sheet.build
                [ ( "F0", "=0" )
                , ( "F1", "=1" )
                , ( "F2", "=add(F1, F0)" )
                , ( "F3", "=add(F2, F1)" )
                , ( "F4", "=add(F3, F2)" )
                , ( "F5", "=add(F4, F3)" )
                , ( "F6", "=add(F5, F4)" )
                , ( "F7", "=add(F6, F5)" )
                , ( "F8", "=add(F7, F6)" )
                , ( "F9", "=add(F8, F7)" )
                , ( "F10", "=add(F9, F8)" )
                ]
    in
    describe "fibonacci"
        [ test "F10 = 55" <|
            \_ ->
                get "F10" sheet
                    |> Expect.equal "55"
        , test "F10 = 123" <|
            \_ ->
                sheet
                    -- Change the Fibonacci sequence into the Lucas sequence
                    |> Sheet.set (Coord.fromSafeString "F0") "=2"
                    |> get "F10"
                    |> Expect.equal "123"
        ]


changePropagationSuite : Test
changePropagationSuite =
    let
        sheet =
            Sheet.build
                [ ( "A0", "=1" )
                , ( "A1", "=add(A0,A0)" )
                , ( "B1", "=add(A0,A0)" )
                , ( "A2", "=sum(A1:B1)" )
                , ( "B2", "=sum(A1:B1)" )
                , ( "C2", "=sum(A1:B1)" )
                , ( "A3", "=sum(A2:C2)" )
                , ( "B3", "=sum(A2:C2)" )
                , ( "C3", "=sum(A2:C2)" )
                , ( "D3", "=sum(A2:C2)" )
                , ( "A4", "=sum(A3:D3)" )
                , ( "B4", "=sum(A3:D3)" )
                , ( "C4", "=sum(A3:D3)" )
                , ( "D4", "=sum(A3:D3)" )
                , ( "E4", "=sum(A3:D3)" )
                , ( "A5", "=sum(A4:E4)" )
                , ( "B5", "=sum(A4:E4)" )
                , ( "C5", "=sum(A4:E4)" )
                , ( "D5", "=sum(A4:E4)" )
                , ( "E5", "=sum(A4:E4)" )
                , ( "F5", "=sum(A4:E4)" )
                , ( "A6", "=sum(A5:F5)" )
                , ( "B6", "=sum(A5:F5)" )
                , ( "C6", "=sum(A5:F5)" )
                , ( "D6", "=sum(A5:F5)" )
                , ( "E6", "=sum(A5:F5)" )
                , ( "F6", "=sum(A5:F5)" )
                , ( "G6", "=sum(A5:F5)" )
                , ( "A7", "=sum(A6:G6)" )
                , ( "B7", "=sum(A6:G6)" )
                , ( "C7", "=sum(A6:G6)" )
                , ( "D7", "=sum(A6:G6)" )
                , ( "E7", "=sum(A6:G6)" )
                , ( "F7", "=sum(A6:G6)" )
                , ( "G7", "=sum(A6:G6)" )
                , ( "H7", "=sum(A6:G6)" )
                , ( "A8", "=sum(A0:H7)" )
                ]
    in
    test "change propagation" <|
        \_ ->
            sheet
                |> Sheet.set (Coord.fromSafeString "A0") "=5"
                |> get "A8"
                |> Expect.equal (String.fromInt <| 5 + 2 * 10 + 3 * 20 + 4 * 60 + 5 * 240 + 6 * 1200 + 7 * 7200 + 8 * 50400)


get : String -> Sheet -> String
get coordAsString =
    Cell.toString << Sheet.get (Coord.fromSafeString coordAsString)
