module Crud.Roster exposing (Roster, empty, fromList, add, filter)


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


fromList : List (String, String) -> Roster
fromList =
    List.foldl
        (\(rawFirstName, rawLastName) roster ->
            add rawFirstName rawLastName roster
                |> Maybe.withDefault roster
        )
        empty


add : String -> String -> Roster -> Maybe Roster
add rawFirstName rawLastName (Roster { nextId, people } as roster) =
    Person.create nextId rawFirstName rawLastName
        |> Maybe.map
            (\person ->
                Roster
                    { nextId = nextId + 1
                    , people = person :: people
                    }
            )


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
