module FlightBooker.Date exposing
    ( Date
    , today
    , fromString
    , isLaterThan
    , toString
    )


import Char
import Date as JDate
import FlightBooker.Parser as P
import Parser as P exposing ((|.), (|=))
import Task


type Date
    = Date JDate.Date


today : (Date -> msg) -> Cmd msg
today toMsg =
    JDate.today
        |> Task.perform (toMsg << Date)


fromString : String -> Maybe Date
fromString s =
    case P.run dateParser s of
        Ok iso ->
            case JDate.fromIsoString iso of
                Ok date ->
                    Just <| Date date

                Err _ ->
                    Nothing

        Err _ ->
            Nothing


dateParser : P.Parser String
dateParser =
    P.succeed (\dd mm yyyy -> yyyy ++ "-" ++ mm ++ "-" ++ dd)
        |= P.chompExactly 2 Char.isDigit
        |. P.chompIf ((==) '.')
        |= P.chompExactly 2 Char.isDigit
        |. P.chompIf ((==) '.')
        |= P.chompExactly 4 Char.isDigit
        |. P.end


isLaterThan : Date -> Date -> Bool
isLaterThan (Date date1) (Date date2) =
    JDate.compare date2 date1 /= LT


toString : Date -> String
toString (Date date) =
    let
        dd =
            JDate.day date
                |> String.fromInt
                |> String.padLeft 2 '0'

        mm =
            JDate.monthNumber date
                |> String.fromInt
                |> String.padLeft 2 '0'

        yyyy =
            JDate.year date
                |> String.fromInt
                |> String.padLeft 4 '0'
    in
    dd ++ "." ++ mm ++ "." ++ yyyy
