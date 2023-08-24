module Task.Crud.Person exposing
    ( Person
    , getFirstAndLastName
    , getId
    , getLastName
    , new
    , toString
    , update
    )


type Person
    = Person
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


getId : Person -> Int
getId (Person { id }) =
    id


getFirstAndLastName : Person -> { firstName : String, lastName : String }
getFirstAndLastName (Person { firstName, lastName }) =
    { firstName = firstName
    , lastName = lastName
    }


getLastName : Person -> String
getLastName (Person { lastName }) =
    lastName


toString : Person -> String
toString (Person { firstName, lastName }) =
    if String.isEmpty lastName then
        firstName

    else
        lastName ++ ", " ++ firstName
