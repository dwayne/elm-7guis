module Task.Cells.Data.Formula.Lexer exposing
    ( coord
    , decimal
    , identifier
    , parens
    , spaces
    , symbol
    , text
    )

import Char
import Parser as P exposing ((|.), (|=), Parser)
import Task.Cells.Data.Column as Column exposing (Column)
import Task.Cells.Data.Coord exposing (Coord)
import Task.Cells.Data.Row as Row exposing (Row)


text : Parser String
text =
    P.getChompedString chompAll


decimal : Parser Float
decimal =
    chompDecimal
        |> P.mapChompedString (\s _ -> String.toFloat s |> Maybe.withDefault 0)
        |> lexeme


chompDecimal : Parser ()
chompDecimal =
    let
        decimalPart =
            P.succeed ()
                |. P.chompIf ((==) '.')
                |. P.chompWhile Char.isDigit
    in
    P.succeed ()
        |. chompOptional ((==) '-')
        |. chompOneOrMore Char.isDigit
        |. chompZeroOrOne decimalPart


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
            |. chompOptional Char.isDigit
        ]


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



-- CHARACTER PREDICATES


isNonZeroDigit : Char -> Bool
isNonZeroDigit c =
    Char.isDigit c && c /= '0'


isLetterOrUnderscore : Char -> Bool
isLetterOrUnderscore c =
    Char.isAlpha c || c == '_'


isLetterOrDigitOrUnderscore : Char -> Bool
isLetterOrDigitOrUnderscore c =
    Char.isAlphaNum c || c == '_'



-- CHOMPERS


chompAll : Parser ()
chompAll =
    P.chompWhile (always True)


chompOptional : (Char -> Bool) -> Parser ()
chompOptional isGood =
    P.oneOf
        [ P.chompIf isGood
        , P.succeed ()
        ]


chompZeroOrOne : Parser () -> Parser ()
chompZeroOrOne p =
    P.oneOf
        [ p
        , P.succeed ()
        ]


chompOneOrMore : (Char -> Bool) -> Parser ()
chompOneOrMore isGood =
    P.succeed ()
        |. P.chompIf isGood
        |. P.chompWhile isGood
