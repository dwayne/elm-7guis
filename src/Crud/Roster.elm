module Crud.Roster exposing
    ( Roster, empty, fromList
    , add, update, delete
    , select, selected
    , filter
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
    List.foldr
        (\(rawFirstName, rawLastName) roster ->
            add rawFirstName rawLastName roster
                |> Maybe.withDefault roster
        )
        empty


add : String -> String -> Roster -> Maybe Roster
add rawFirstName rawLastName (Roster { nextId, people } as roster) =
    Person.create nextId rawFirstName rawLastName
        |> Maybe.andThen
            (\person ->
                if isMember person people then
                    Nothing

                else
                    Just <|
                        Roster
                            { nextId = nextId + 1
                            , people = Selection.cons person people
                            }
            )


update : String -> String -> Roster -> Roster
update rawFirstName rawLastName (Roster state) =
    Roster
        { state
        | people =
            Selection.mapSelected
                { selected =
                    \person ->
                        case Person.update rawFirstName rawLastName person of
                            Just updatedPerson ->
                                if isMember updatedPerson state.people then
                                    person

                                else
                                    updatedPerson

                            Nothing ->
                                person
                , rest = identity
                }
                state.people
        }


isMember : Person -> Selection Person -> Bool
isMember person people =
    let
        newFirstAndLastName =
            Person.toFirstAndLastName person

        firstAndLastNames =
            people
                |> Selection.toList
                |> List.map Person.toFirstAndLastName
    in
    List.member newFirstAndLastName firstAndLastNames


delete : Roster -> Roster
delete (Roster state) =
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
