module Cells.Lexer exposing (decimal, parseString)

import Char
import Parser as P exposing ((|.), Parser)


type AST
    = Number Float
    | Text String


parseString : String -> Result (List P.DeadEnd) AST
parseString =
    P.run ast


ast : Parser AST
ast =
    P.oneOf [ P.map Number decimal, P.map Text text ]


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


text : Parser String
text =
    P.getChompedString chompText


chompText : Parser ()
chompText =
    P.succeed ()
        |. P.chompIf ((/=) '=')
        |. P.chompWhile (always True)
