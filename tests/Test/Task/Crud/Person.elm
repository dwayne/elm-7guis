module Test.Task.Crud.Person exposing (suite)

import Expect
import Task.Crud.Person as Person exposing (Person)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Person"
        [ newSuite
        , updateSuite
        , toSearchTermSuite
        , toStringSuite
        ]


newSuite : Test
newSuite =
    describe "new"
        [ testNew "when both first and last name are non-empty"
            { id = 1
            , rawFirstName = "Evan"
            , rawLastName = "Czaplicki"
            , maybeData =
                Just
                    { id = 1
                    , firstName = "Evan"
                    , lastName = "Czaplicki"
                    }
            }
        , testNew "when the names have leading and/or trailing spaces"
            { id = 1
            , rawFirstName = " Evan  "
            , rawLastName = "   Czaplicki    "
            , maybeData =
                Just
                    { id = 1
                    , firstName = "Evan"
                    , lastName = "Czaplicki"
                    }
            }
        , testNew "when the last name is empty"
            { id = 2
            , rawFirstName = "Richard"
            , rawLastName = " "
            , maybeData =
                Just
                    { id = 2
                    , firstName = "Richard"
                    , lastName = ""
                    }
            }
        , testNew "when the first name is empty"
            { id = 3
            , rawFirstName = " "
            , rawLastName = "Feldman"
            , maybeData = Nothing
            }
        ]


testNew :
    String
    ->
        { id : Int
        , rawFirstName : String
        , rawLastName : String
        , maybeData : Maybe Person.Data
        }
    -> Test
testNew description { id, rawFirstName, rawLastName, maybeData } =
    test description <|
        \_ ->
            Person.new id rawFirstName rawLastName
                |> Maybe.map Person.getData
                |> Expect.equal maybeData


updateSuite : Test
updateSuite =
    describe "update"
        [ test "it doesn't change the id" <|
            \_ ->
                let
                    maybeDillon =
                        Person.new 42 "Dillon" "Kearns"

                    expected =
                        Just
                            { id = 42
                            , firstName = "Jeroen"
                            , lastName = "Engels"
                            }
                in
                maybeDillon
                    |> Maybe.andThen (Person.update " Jeroen  " "   Engels    ")
                    |> Maybe.map Person.getData
                    |> Expect.equal expected
        ]


toSearchTermSuite : Test
toSearchTermSuite =
    let
        testToSearchTerm =
            testStringify Person.toSearchTerm
    in
    describe "toSearchTerm"
        [ testToSearchTerm "example 1"
            { rawFirstName = "Tessa"
            , rawLastName = "Kelly"
            , result = "kellytessa"
            }
        , testToSearchTerm "example 2"
            { rawFirstName = " Tessa  "
            , rawLastName = "   Kelly    "
            , result = "kellytessa"
            }
        , testToSearchTerm "example 3"
            { rawFirstName = "Tessa"
            , rawLastName = " "
            , result = "tessa"
            }
        ]


toStringSuite : Test
toStringSuite =
    let
        testToString =
            testStringify Person.toString
    in
    describe "toString"
        [ testToString "example 1"
            { rawFirstName = "Jared"
            , rawLastName = "Smith"
            , result = "Smith, Jared"
            }
        , testToString "example 2"
            { rawFirstName = " Jared  "
            , rawLastName = "   Smith    "
            , result = "Smith, Jared"
            }
        , testToString "example 3"
            { rawFirstName = "Jared"
            , rawLastName = " "
            , result = "Jared"
            }
        ]


testStringify :
    (Person -> String)
    -> String
    ->
        { rawFirstName : String
        , rawLastName : String
        , result : String
        }
    -> Test
testStringify toString description { rawFirstName, rawLastName, result } =
    test description <|
        \_ ->
            let
                maybePerson =
                    Person.new 1 rawFirstName rawLastName
            in
            maybePerson
                |> Maybe.map toString
                |> Expect.equal (Just result)
