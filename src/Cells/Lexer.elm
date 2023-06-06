module Cells.Lexer exposing (decimal, parseString)

import Char
import Parser as P exposing ((|.), Parser)


parseString : String -> Result (List P.DeadEnd) Float
parseString =
    P.run decimal


decimal : Parser Float
decimal =
    P.getChompedString chompDecimal
        |> P.andThen
            (\s ->
                case String.toFloat s of
                    Just f ->
                        P.succeed f

                    Nothing ->
                        P.problem <| "improperly formatted float string: " ++ s
            )


chompDecimal : Parser ()
chompDecimal =
    let
        minusSign =
            P.chompIf ((==) '-')

        zeroOrMoreDigits =
            P.chompWhile Char.isDigit

        oneOrMoreDigits =
            P.succeed ()
                |. P.chompIf Char.isDigit
                |. zeroOrMoreDigits

        decimalPart =
            P.succeed ()
                |. P.chompIf ((==) '.')
                |. zeroOrMoreDigits
    in
    P.succeed ()
        |. optional minusSign
        |. oneOrMoreDigits
        |. optional decimalPart


optional : Parser () -> Parser ()
optional p =
    P.oneOf
        [ p
        , P.succeed ()
        ]
