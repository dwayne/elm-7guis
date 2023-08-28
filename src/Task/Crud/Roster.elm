module Task.Crud.Roster exposing
    ( Roster
    , add
    , delete
    , deselect
    , fromList
    , select
    , selected
    , toFilteredListWithSelection
    , update
    )

import Task.Crud.Person as Person exposing (Person)
import Task.Crud.Selection as Selection exposing (Selection)


type Roster
    = Roster
        { nextId : Int
        , people : Selection Person
        }


firstId : Int
firstId =
    1


empty : Roster
empty =
    Roster
        { nextId = firstId
        , people = Selection.empty
        }


fromList : List ( String, String ) -> Roster
fromList =
    fromListHelper firstId Selection.empty


fromListHelper : Int -> Selection Person -> List ( String, String ) -> Roster
fromListHelper nextId people names =
    case names of
        [] ->
            Roster
                { nextId = nextId
                , people = Selection.reverse people
                }

        ( rawFirstName, rawLastName ) :: restNames ->
            case addHelper rawFirstName rawLastName nextId people of
                Just ( newNextId, newPeople ) ->
                    fromListHelper newNextId newPeople restNames

                Nothing ->
                    fromListHelper nextId people restNames


add : String -> String -> Roster -> Maybe Roster
add rawFirstName rawLastName (Roster { nextId, people }) =
    addHelper rawFirstName rawLastName nextId people
        |> Maybe.map
            (\( newNextId, newPeople ) ->
                Roster
                    { nextId = newNextId
                    , people = newPeople
                    }
            )


addHelper : String -> String -> Int -> Selection Person -> Maybe ( Int, Selection Person )
addHelper rawFirstName rawLastName id people =
    Person.new id rawFirstName rawLastName
        |> Maybe.andThen
            (\person ->
                if isMember person people then
                    Nothing

                else
                    Just
                        ( id + 1
                        , Selection.cons person people
                        )
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
isMember person =
    let
        newFirstAndLastName =
            Person.getFirstAndLastName person

        isSamePerson otherPerson =
            newFirstAndLastName == Person.getFirstAndLastName otherPerson
    in
    Selection.toList >> List.any isSamePerson


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


toFilteredListWithSelection : String -> Roster -> List ( Bool, Person )
toFilteredListWithSelection rawPrefix (Roster { people }) =
    let
        prefix =
            rawPrefix
                |> String.trim
                |> String.toLower
    in
    people
        |> Selection.filter (Person.toSearchTerm >> String.startsWith prefix)
        |> Selection.toListWithSelection
