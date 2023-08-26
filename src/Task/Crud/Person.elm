module Task.Crud.Person exposing
    ( Data
    , Person
    , getData
    , getFirstAndLastName
    , getId
    , new
    , toSearchTerm
    , toString
    , update
    )


type Person
    = Person Data


type alias Data =
    { id : Int
    , firstName : String
    , lastName : String
    }


new : Int -> String -> String -> Maybe Person
new id rawFirstName rawLastName =
    let
        firstName =
            String.trim rawFirstName
    in
    if String.isEmpty firstName then
        Nothing

    else
        Just <|
            Person
                { id = id
                , firstName = firstName
                , lastName = String.trim rawLastName
                }


update : String -> String -> Person -> Maybe Person
update rawFirstName rawLastName (Person { id }) =
    new id rawFirstName rawLastName


getData : Person -> Data
getData (Person data) =
    data


getId : Person -> Int
getId (Person { id }) =
    id


getFirstAndLastName : Person -> { firstName : String, lastName : String }
getFirstAndLastName (Person { firstName, lastName }) =
    { firstName = firstName
    , lastName = lastName
    }


toSearchTerm : Person -> String
toSearchTerm (Person { firstName, lastName }) =
    String.toLower <| lastName ++ firstName


toString : Person -> String
toString (Person { firstName, lastName }) =
    if String.isEmpty lastName then
        firstName

    else
        lastName ++ ", " ++ firstName
