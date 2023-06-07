module Cells.Lexer exposing (parseString)

import Char
import Parser as P exposing ((|.), Parser)


type AST
    = Number Float
    | Text String


parseString : String -> Result (List P.DeadEnd) String
parseString =
    P.run identifier


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

        decimalPart =
            P.succeed ()
                |. P.chompIf ((==) '.')
                |. zeroOrMoreDigits
    in
    P.succeed ()
        |. optional minusSign
        |. oneOrMoreDigits
        |. optional decimalPart


zeroOrMoreDigits : Parser ()
zeroOrMoreDigits =
    P.chompWhile Char.isDigit


oneOrMoreDigits : Parser ()
oneOrMoreDigits =
    P.succeed ()
        |. P.chompIf Char.isDigit
        |. zeroOrMoreDigits


optional : Parser () -> Parser ()
optional p =
    P.oneOf
        [ p
        , P.succeed ()
        ]


text : Parser String
text =
    P.getChompedString <|
        P.succeed ()
            |. P.chompIf ((/=) '=')
            |. P.chompWhile (always True)


identifier : Parser String
identifier =
    P.getChompedString <|
        P.succeed ()
            |. P.chompIf isLetterOrUnderscore
            |. P.chompWhile isLetterOrDigitOrUnderscore


isLetterOrUnderscore : Char -> Bool
isLetterOrUnderscore c =
    Char.isAlpha c || c == '_'


isLetterOrDigitOrUnderscore : Char -> Bool
isLetterOrDigitOrUnderscore c =
    Char.isAlphaNum c || c == '_'
