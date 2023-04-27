module Crud.Roster exposing (Roster, empty, add, filter)


import Crud.Person as Person exposing (Person)


type Roster
    = Roster
        { nextId : Int
        , people : List Person
        }


empty : Roster
empty =
    Roster
        { nextId = 1
        , people = []
        }


add : String -> String -> Roster -> Roster
add rawFirstName rawLastName (Roster { nextId, people } as roster) =
    case Person.create nextId rawFirstName rawLastName of
        Just person ->
            Roster
                { nextId = nextId + 1
                , people = person :: people
                }

        Nothing ->
            roster


filter : String -> Roster -> List Person
filter rawPrefix (Roster { people }) =
    let
        prefix =
            rawPrefix
                |> String.trim
                |> String.toLower
    in
    people
        |> List.filter (Person.toFullName >> String.toLower >> String.contains prefix)
        |> List.reverse
