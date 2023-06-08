module Cells.Parser exposing (parse)

import Cells.AST as AST exposing (Expr, Formula)
import Cells.Data.Range exposing (Range)
import Cells.Lexer as L
import Parser as P exposing ((|.), (|=), Parser)


parse : String -> Result (List P.DeadEnd) Formula
parse =
    P.run formula


formula : Parser Formula
formula =
    P.getChompedString L.spaces
        |> P.andThen
            (\leadingSpaces ->
                P.succeed identity
                    |= P.oneOf
                        [ P.succeed AST.Expr
                            |. L.symbol "="
                            |= expr
                        , L.text
                            |> P.map (\s -> AST.Text <| leadingSpaces ++ s)
                        ]
                    |. P.end
            )


expr : Parser Expr
expr =
    P.oneOf
        [ number
        , rangeOrCell
        , application
        ]


number : Parser Expr
number =
    L.decimal
        |> P.map AST.Number


rangeOrCell : Parser Expr
rangeOrCell =
    L.coord
        |> P.andThen
            (\start ->
                P.oneOf
                    [ P.succeed (\end -> AST.Range <| Range start end)
                        |. L.symbol ":"
                        |= L.coord
                    , P.succeed <| AST.Cell start
                    ]
            )


application : Parser Expr
application =
    P.succeed AST.Application
        |= L.identifier
        |= L.parens (P.lazy (\_ -> expr))
