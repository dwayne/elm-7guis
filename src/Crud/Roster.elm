module Crud.Roster exposing
    ( Roster, empty, fromList
    , add
    , filter
    , select, selected
    , updateSelected
    , deleteSelected
    )


import Crud.Person as Person exposing (Person)
import Crud.Selection as Selection exposing (Selection)


type Roster
    = Roster
        { nextId : Int
        , people : Selection Person
        }


empty : Roster
empty =
    Roster
        { nextId = 1
        , people = Selection.empty
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
                    , people = Selection.cons person people
                    }
            )


filter : String -> Roster -> List (Bool, Person)
filter rawPrefix (Roster { people }) =
    let
        prefix =
            rawPrefix
                |> String.trim
                |> String.toLower
    in
    people
        |> Selection.mapSelected
            { selected = Tuple.pair True
            , rest = Tuple.pair False
            }
        |> Selection.toList
        |> List.filter
            (\(_, person) ->
                person
                    |> Person.toFullName
                    |> String.toLower
                    |> String.contains prefix
            )
        |> List.reverse


select : Int -> Roster -> Roster
select id (Roster state) =
    Roster
        { state
        | people =
            Selection.selectBy (Person.toId >> ((==) id)) state.people
        }


selected : Roster -> Maybe Person
selected (Roster { people }) =
    Selection.selected people


updateSelected : String -> String -> Roster -> Roster
updateSelected rawFirstName rawLastName (Roster state) =
    Roster
        { state
        | people =
            Selection.mapSelected
                { selected =
                    \person ->
                        case Person.update rawFirstName rawLastName person of
                            Just updatedPerson ->
                                updatedPerson

                            Nothing ->
                                person
                , rest = identity
                }
                state.people
        }


deleteSelected : Roster -> Roster
deleteSelected (Roster state) =
    Roster
        { state
        | people =
            state.people
                |> Selection.mapSelected
                    { selected = always Nothing
                    , rest = Just
                    }
                |> Selection.toList
                |> List.filterMap identity
                |> Selection.fromList
        }
