module Task.Crud.Roster exposing
    ( Roster
    , add
    , delete
    , deselect
    , filter
    , fromList
    , select
    , selected
    , update
    )

import Task.Crud.Person as Person exposing (Person)
import Task.Crud.Selection as Selection exposing (Selection)


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


fromList : List ( String, String ) -> Roster
fromList =
    List.foldr
        (\( rawFirstName, rawLastName ) roster ->
            add rawFirstName rawLastName roster
                |> Maybe.withDefault roster
        )
        empty


add : String -> String -> Roster -> Maybe Roster
add rawFirstName rawLastName (Roster { nextId, people }) =
    Person.new nextId rawFirstName rawLastName
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


update : String -> String -> Roster -> Maybe Roster
update rawFirstName rawLastName (Roster r) =
    Selection.selected r.people
        |> Maybe.andThen (Person.update rawFirstName rawLastName)
        |> Maybe.andThen
            (\updatedPerson ->
                if isMember updatedPerson r.people then
                    Nothing

                else
                    Just <|
                        Roster
                            { r
                                | people =
                                    Selection.mapSelected
                                        { selected = always updatedPerson
                                        , rest = identity
                                        }
                                        r.people
                            }
            )


isMember : Person -> Selection Person -> Bool
isMember person people =
    let
        newFirstAndLastName =
            Person.getFirstAndLastName person

        firstAndLastNames =
            people
                |> Selection.toList
                |> List.map Person.getFirstAndLastName
    in
    List.member newFirstAndLastName firstAndLastNames


delete : Roster -> Roster
delete (Roster r) =
    Roster { r | people = Selection.removeSelected r.people }


select : Int -> Roster -> Maybe ( Person, Roster )
select id (Roster r) =
    let
        people =
            Selection.selectBy (Person.getId >> (==) id) r.people
    in
    people
        |> Selection.selected
        |> Maybe.map (\person -> ( person, Roster { r | people = people } ))


selected : Roster -> Maybe Person
selected (Roster { people }) =
    Selection.selected people


deselect : Roster -> Roster
deselect (Roster r) =
    Roster { r | people = Selection.deselect r.people }


filter : String -> Roster -> List ( Bool, Person )
filter rawPrefix (Roster { people }) =
    let
        prefix =
            rawPrefix
                |> String.trim
                |> String.toLower
    in
    people
        |> Selection.filter (Person.getLastName >> String.toLower >> String.startsWith prefix)
        |> Selection.toListWithSelection
