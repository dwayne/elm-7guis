module Task.Cells.Data.Formula.Parser exposing (parse)

import Parser as P exposing ((|.), (|=), Parser)
import Task.Cells.Data.Formula.AST as AST exposing (Expr, Formula)
import Task.Cells.Data.Formula.Lexer as L
import Task.Cells.Data.Range exposing (Range)


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
        , P.backtrackable application
        , rangeOrCell
        ]


number : Parser Expr
number =
    L.decimal
        |> P.map AST.Number


application : Parser Expr
application =
    P.succeed AST.Application
        |= L.identifier
        |= L.parens (P.lazy (\_ -> expr))


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
