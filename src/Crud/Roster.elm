module Crud.Roster exposing
    ( Roster, empty, fromList
    , add
    , filter
    , select, selected
    )


import Crud.Person as Person exposing (Person)


type Roster
    = Roster
        { nextId : Int
        , people : Selection
        }


empty : Roster
empty =
    Roster
        { nextId = 1
        , people = selectionEmpty
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
                    , people = selectionCons person people
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
        |> selectionToList
        |> List.filter (Person.toFullName >> String.toLower >> String.contains prefix)
        |> List.reverse


select : Int -> Roster -> Roster
select id (Roster state) =
    Roster { state | people = selectionSelect id state.people }


selected : Roster -> Maybe Person
selected (Roster { people }) =
    selectionSelected people


-- SELECTION


type Selection
    = Selection (List Person) (Maybe Person) (List Person)


selectionEmpty : Selection
selectionEmpty =
    Selection [] Nothing []


selectionCons : Person -> Selection -> Selection
selectionCons person (Selection front maybeSel back) =
    Selection (person :: front) maybeSel back


selectionSelect : Int -> Selection -> Selection
selectionSelect id =
    selectionToList >> selectionSelectHelper [] id


selectionSelectHelper : List Person -> Int -> List Person -> Selection
selectionSelectHelper front id people =
    case people of
        [] ->
            Selection (List.reverse front) Nothing []

        person :: restPeople ->
            if Person.toId person == id then
                Selection (List.reverse front) (Just person) restPeople

            else
                selectionSelectHelper (person :: front) id restPeople


selectionSelected : Selection -> Maybe Person
selectionSelected (Selection _ maybeSel _) =
    maybeSel


selectionToList : Selection -> List Person
selectionToList (Selection front maybeSel back) =
    List.concat
        [ front
        , case maybeSel of
            Just sel ->
                [ sel ]

            Nothing ->
                []
        , back
        ]
