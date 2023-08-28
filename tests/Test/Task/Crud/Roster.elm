module Test.Task.Crud.Roster exposing (suite)

import Expect exposing (Expectation)
import Task.Crud.Person as Person
import Task.Crud.Roster as Roster exposing (Roster)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Roster"
        [ fromListSuite
        , addSuite
        , selectSuite
        , updateSuite
        ]


fromListSuite : Test
fromListSuite =
    describe "fromList"
        [ test "example 1" <|
            \_ ->
                Roster.fromList
                    [ ( "Rupert", "Smith" )
                    , ( "", "Czaplicki" )
                    , ( "  Hayleigh ", "    Thompson   " )
                    , ( " ", "  " )
                    , ( "Jack ", "Leow" )
                    , ( "Rupert", "Smith " )
                    , ( "Georges", " Boris" )
                    , ( " Martin", "Janiczek" )
                    , ( "Joaquin", "" )
                    , ( " Hayleigh  ", "   Thompson    " )
                    , ( "Peter", "Gao" )
                    ]
                    |> toFilteredListWithSelection ""
                    |> Expect.equal
                        [ ( False, { id = 1, firstName = "Rupert", lastName = "Smith" } )
                        , ( False, { id = 2, firstName = "Hayleigh", lastName = "Thompson" } )
                        , ( False, { id = 3, firstName = "Jack", lastName = "Leow" } )
                        , ( False, { id = 4, firstName = "Georges", lastName = "Boris" } )
                        , ( False, { id = 5, firstName = "Martin", lastName = "Janiczek" } )
                        , ( False, { id = 6, firstName = "Joaquin", lastName = "" } )
                        , ( False, { id = 7, firstName = "Peter", lastName = "Gao" } )
                        ]
        ]


addSuite : Test
addSuite =
    describe "add"
        [ test "when the name is valid" <|
            \_ ->
                Roster.fromList [ ( "Rupert", "Smith" ) ]
                    |> Roster.add "  Hayleigh " "    Thompson   "
                    |> Maybe.map (toFilteredListWithSelection "")
                    |> Maybe.withDefault []
                    |> Expect.equal
                        [ ( False, { id = 2, firstName = "Hayleigh", lastName = "Thompson" } )
                        , ( False, { id = 1, firstName = "Rupert", lastName = "Smith" } )
                        ]
        , test "when the first name is empty" <|
            \_ ->
                Roster.fromList [ ( "Rupert", "Smith" ) ]
                    |> Roster.add "" "Czaplicki"
                    |> Expect.equal Nothing
        , test "when the name already exists" <|
            \_ ->
                Roster.fromList [ ( "Rupert", "Smith" ) ]
                    |> Roster.add "Rupert" "Smith "
                    |> Expect.equal Nothing
        ]


selectSuite : Test
selectSuite =
    describe "select"
        [ test "when the id exists" <|
            \_ ->
                Roster.fromList
                    [ ( "Evan", "Czaplicki" )
                    , ( "Richard", "Feldman" )
                    ]
                    |> Roster.select 1
                    |> Maybe.map
                        (\( person, roster ) ->
                            ( Person.getData person
                            , toFilteredListWithSelection "" roster
                            )
                        )
                    |> Expect.equal
                        (Just
                            ( { id = 1, firstName = "Evan", lastName = "Czaplicki" }
                            , [ ( True, { id = 1, firstName = "Evan", lastName = "Czaplicki" } )
                              , ( False, { id = 2, firstName = "Richard", lastName = "Feldman" } )
                              ]
                            )
                        )
        , test "when the id does not exist" <|
            \_ ->
                Roster.fromList
                    [ ( "Evan", "Czaplicki" )
                    , ( "Richard", "Feldman" )
                    ]
                    |> Roster.select 3
                    |> Expect.equal Nothing
        ]


updateSuite : Test
updateSuite =
    describe "update"
        [ test "when the name is valid" <|
            \_ ->
                Roster.fromList
                    [ ( "Jack ", "Leow" )
                    , ( "Rupert", "Smith" )
                    , ( "Georges", "Boris" )
                    , ( "Martin", "Janiczek" )
                    ]
                    |> Roster.select 3
                    |> Maybe.andThen (Tuple.second >> Roster.update "Richard" "Feldman")
                    |> Maybe.map (toFilteredListWithSelection "")
                    |> Maybe.withDefault []
                    |> Expect.equal
                        [ ( False, { id = 1, firstName = "Jack", lastName = "Leow" } )
                        , ( False, { id = 2, firstName = "Rupert", lastName = "Smith" } )
                        , ( True, { id = 3, firstName = "Richard", lastName = "Feldman" } )
                        , ( False, { id = 4, firstName = "Martin", lastName = "Janiczek" } )
                        ]
        , test "when the first name is empty" <|
            \_ ->
                Roster.fromList [ ( "Rupert", "Smith" ) ]
                    |> Roster.select 1
                    |> Maybe.andThen (Tuple.second >> Roster.update "" "Czaplicki")
                    |> Expect.equal Nothing
        , test "when the name already exists" <|
            \_ ->
                Roster.fromList
                    [ ( "Georges", "Boris" )
                    , ( "Martin", "Janiczek" )
                    ]
                    |> Roster.select 2
                    |> Maybe.andThen (Tuple.second >> Roster.update "Georges" "Boris")
                    |> Expect.equal Nothing
        ]


toFilteredListWithSelection : String -> Roster -> List ( Bool, Person.Data )
toFilteredListWithSelection prefix =
    Roster.toFilteredListWithSelection prefix >> List.map (Tuple.mapSecond Person.getData)
