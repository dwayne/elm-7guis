module Crud.Person exposing (Person, create, toId, toFullName, toString)


type Person
    = Person
        { id : Int
        , firstName : String
        , lastName : String
        }


create : Int -> String -> String -> Maybe Person
create id rawFirstName rawLastName =
    let
        firstName =
            String.trim rawFirstName

        lastName =
            String.trim rawLastName
    in
    if String.isEmpty firstName then
        Nothing

    else
        Just <|
            Person
                { id = id
                , firstName = firstName
                , lastName = lastName
                }


toId : Person -> Int
toId (Person { id }) =
    id


toFullName : Person -> String
toFullName (Person { firstName, lastName }) =
    if String.isEmpty lastName then
        firstName

    else
        firstName ++ " " ++ lastName

toString : Person -> String
toString (Person { firstName, lastName }) =
    if String.isEmpty lastName then
        firstName

    else
        lastName ++ ", " ++ firstName
