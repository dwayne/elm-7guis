module Test.Task.Crud.Selection exposing (suite)

import Expect
import Task.Crud.Selection as Selection
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Selection"
        [ miscSuite
        , mapSelectedSuite
        , filterSuite
        , reverseSuite
        ]


miscSuite : Test
miscSuite =
    describe "miscellaneous examples"
        [ let
            todaysMenu =
                [ Burrito, ChickenWrap, TacoSalad ]
                    |> Selection.fromList
                    |> Selection.select Burrito
          in
          describe "NoRedInk/list-selection usage"
            [ test "example 1" <|
                \_ ->
                    Selection.selected todaysMenu
                        |> Expect.equal (Just Burrito)
            , test "example 2" <|
                \_ ->
                    todaysMenu
                        |> Selection.select DonerKebab
                        |> Selection.selected
                        -- Their version returns `Just Burrito`.
                        |> Expect.equal Nothing
            , test "example 3" <|
                \_ ->
                    todaysMenu
                        |> Selection.deselect
                        |> Selection.selected
                        |> Expect.equal Nothing
            ]
        , describe "https://github.com/NoRedInk/list-selection/issues/5"
            [ test "eriktim" <|
                \_ ->
                    Selection.fromList [ 1, 2, 3 ]
                        |> Selection.select 2
                        |> Selection.selectBy (\x -> x > 3)
                        |> Selection.selected
                        -- The 2nd selection failed so it returns `Nothing`.
                        |> Expect.equal Nothing
            ]
        ]


type Lunch
    = Burrito
    | ChickenWrap
    | TacoSalad
    | DonerKebab


mapSelectedSuite : Test
mapSelectedSuite =
    describe "mapSelected"
        [ test "without duplicates" <|
            \_ ->
                Selection.fromList [ 1, 2, 3 ]
                    |> Selection.select 2
                    |> Selection.mapSelected { selected = (*) 2, rest = identity }
                    |> Selection.toList
                    |> Expect.equal [ 1, 4, 3 ]
        , test "with duplicates" <|
            \_ ->
                Selection.fromList [ 1, 2, 3, 2, 5, 2 ]
                    |> Selection.select 2
                    |> Selection.mapSelected { selected = (*) 2, rest = identity }
                    |> Selection.toList
                    |> Expect.equal [ 1, 4, 3, 2, 5, 2 ]
        ]


filterSuite : Test
filterSuite =
    describe "filter"
        [ test "example 1" <|
            \_ ->
                Selection.fromList [ 1, 2, 3, 4, 5 ]
                    |> Selection.select 5
                    |> Selection.filter (\n -> modBy 2 n == 0)
                    |> Selection.toListWithSelection
                    |> Expect.equal [ ( False, 2 ), ( False, 4 ) ]
        , test "example 2" <|
            \_ ->
                Selection.fromList [ 1, 2, 3, 4, 5 ]
                    |> Selection.select 3
                    |> Selection.filter (\n -> modBy 2 n == 1)
                    |> Selection.toListWithSelection
                    |> Expect.equal [ ( False, 1 ), ( True, 3 ), ( False, 5 ) ]
        , test "example 3" <|
            \_ ->
                Selection.fromList [ 1, 2, 2, 3, 3, 3, 4 ]
                    |> Selection.select 3
                    |> Selection.filter (\n -> modBy 2 n == 1)
                    |> Selection.toListWithSelection
                    |> Expect.equal [ ( False, 1 ), ( True, 3 ), ( False, 3 ), ( False, 3 ) ]
        ]


reverseSuite : Test
reverseSuite =
    describe "reverse"
        [ test "example 1" <|
            \_ ->
                Selection.fromList [ 1, 2, 3, 4, 5 ]
                    |> Selection.reverse
                    |> Selection.toList
                    |> Expect.equal [ 5, 4, 3, 2, 1 ]
        , test "example 2" <|
            \_ ->
                Selection.fromList [ 1, 2, 3, 4, 5 ]
                    |> Selection.select 1
                    |> Selection.reverse
                    |> Selection.toListWithSelection
                    |> Expect.equal [ ( False, 5 ), ( False, 4 ), ( False, 3 ), ( False, 2 ), ( True, 1 ) ]
        , test "example 3" <|
            \_ ->
                Selection.fromList [ 1, 2, 3, 4, 5 ]
                    |> Selection.select 2
                    |> Selection.reverse
                    |> Selection.toListWithSelection
                    |> Expect.equal [ ( False, 5 ), ( False, 4 ), ( False, 3 ), ( True, 2 ), ( False, 1 ) ]
        , test "example 4" <|
            \_ ->
                Selection.fromList [ 1, 2, 3, 4, 5 ]
                    |> Selection.select 3
                    |> Selection.reverse
                    |> Selection.toListWithSelection
                    |> Expect.equal [ ( False, 5 ), ( False, 4 ), ( True, 3 ), ( False, 2 ), ( False, 1 ) ]
        , test "example 5" <|
            \_ ->
                Selection.fromList [ 1, 2, 3, 4, 5 ]
                    |> Selection.select 4
                    |> Selection.reverse
                    |> Selection.toListWithSelection
                    |> Expect.equal [ ( False, 5 ), ( True, 4 ), ( False, 3 ), ( False, 2 ), ( False, 1 ) ]
        , test "example 6" <|
            \_ ->
                Selection.fromList [ 1, 2, 3, 4, 5 ]
                    |> Selection.select 5
                    |> Selection.reverse
                    |> Selection.toListWithSelection
                    |> Expect.equal [ ( True, 5 ), ( False, 4 ), ( False, 3 ), ( False, 2 ), ( False, 1 ) ]
        ]
