module Cells.Lexer exposing
    ( coord
    , decimal
    , identifier
    , parens
    , spaces
    , symbol
    , text
    )

import Cells.Data.Column as Column exposing (Column)
import Cells.Data.Coord exposing (Coord)
import Cells.Data.Row as Row exposing (Row)
import Char
import Parser as P exposing ((|.), (|=), Parser)


text : Parser String
text =
    P.getChompedString chompText


chompText : Parser ()
chompText =
    P.succeed ()
        |. P.chompIf ((/=) '=')
        |. chompAny


chompAny : Parser ()
chompAny =
    P.chompWhile (always True)


decimal : Parser Float
decimal =
    chompDecimal
        |> P.mapChompedString (\s _ -> String.toFloat s |> Maybe.withDefault 0)
        |> lexeme


chompDecimal : Parser ()
chompDecimal =
    let
        minusSign =
            P.chompIf ((==) '-')

        decimalPart =
            P.succeed ()
                |. P.chompIf ((==) '.')
                |. chompZeroOrMoreDigits
    in
    P.succeed ()
        |. chompOptional minusSign
        |. chompOneOrMoreDigits
        |. chompOptional decimalPart


chompOneOrMoreDigits : Parser ()
chompOneOrMoreDigits =
    P.succeed ()
        |. P.chompIf Char.isDigit
        |. chompZeroOrMoreDigits


chompZeroOrMoreDigits : Parser ()
chompZeroOrMoreDigits =
    P.chompWhile Char.isDigit


chompOptional : Parser () -> Parser ()
chompOptional p =
    P.oneOf
        [ p
        , P.succeed ()
        ]


coord : Parser Coord
coord =
    lexeme <|
        P.succeed Coord
            |= column
            |= row


column : Parser Column
column =
    P.chompIf Char.isUpper
        |> P.mapChompedString (\s _ -> Column.fromSafeString s)


row : Parser Row
row =
    chompRow
        |> P.mapChompedString (\s _ -> Row.fromSafeString s)


chompRow : Parser ()
chompRow =
    P.oneOf
        [ P.chompIf ((==) '0')
        , P.succeed ()
            |. P.chompIf isNonZeroDigit
            |. chompOptional (P.chompIf Char.isDigit)
        ]


isNonZeroDigit : Char -> Bool
isNonZeroDigit c =
    Char.isDigit c && c /= '0'


identifier : Parser String
identifier =
    chompIdentifier
        |> P.getChompedString
        |> lexeme


chompIdentifier : Parser ()
chompIdentifier =
    P.succeed ()
        |. P.chompIf isLetterOrUnderscore
        |. P.chompWhile isLetterOrDigitOrUnderscore


isLetterOrUnderscore : Char -> Bool
isLetterOrUnderscore c =
    Char.isAlpha c || c == '_'


isLetterOrDigitOrUnderscore : Char -> Bool
isLetterOrDigitOrUnderscore c =
    Char.isAlphaNum c || c == '_'


parens : Parser a -> Parser (List a)
parens item =
    between "(" item ")"


between : String -> Parser a -> String -> Parser (List a)
between start item end =
    lexeme <|
        P.sequence
            { start = start
            , separator = ","
            , end = end
            , spaces = spaces
            , item = item
            , trailing = P.Forbidden
            }


symbol : String -> Parser ()
symbol =
    P.symbol >> lexeme


lexeme : Parser a -> Parser a
lexeme p =
    P.succeed identity
        |= p
        |. spaces


spaces : Parser ()
spaces =
    P.spaces
