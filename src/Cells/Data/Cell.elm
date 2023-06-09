module Cells.Data.Cell exposing
    ( Cell
    , empty
    , fromString
    , toInputString
    , toString
    )

import Cells.AST as AST
import Cells.Data.Coord exposing (Coord)
import Cells.Data.Range exposing (Range)
import Cells.Data.SCells as SCells exposing (SCells)
import Cells.Parser as P
import Parser exposing (DeadEnd)


type Cell
    = Cell
        { coord : Coord
        , value : Value
        }


type Value
    = Empty
    | Text String
    | Expr
        { rawInput : String
        , expr : AST.Expr
        , result : Result RuntimeError Float
        }
    | SyntaxError
        { rawInput : String
        , deadEnds : List DeadEnd
        }


type RuntimeError
    = TopLevelRange Range
    | IdentifierNotFound String


empty : Coord -> Cell
empty coord =
    Cell
        { coord = coord
        , value = Empty
        }


fromString : SCells Cell -> Coord -> String -> Cell
fromString scells coord rawInput =
    let
        value =
            case P.parse rawInput of
                Ok (AST.Text t) ->
                    Text t

                Ok (AST.Expr expr) ->
                    Expr
                        { rawInput = rawInput
                        , expr = expr
                        , result = evaluate scells expr
                        }

                Err deadEnds ->
                    SyntaxError
                        { rawInput = rawInput
                        , deadEnds = deadEnds
                        }
    in
    Cell
        { coord = coord
        , value = value
        }


evaluate : SCells Cell -> AST.Expr -> Result RuntimeError Float
evaluate scells expr =
    case expr of
        AST.Number x ->
            Ok x

        AST.Cell coord ->
            Ok <| toFloat <| SCells.get empty coord scells

        AST.Range range ->
            Err <| TopLevelRange range

        AST.Application identifier _ ->
            -- TODO: Evaluate application.
            Err <| IdentifierNotFound identifier


toFloat : Cell -> Float
toFloat (Cell { value }) =
    case value of
        Expr { result } ->
            Result.withDefault 0 result

        _ ->
            0


toInputString : Cell -> String
toInputString (Cell { value }) =
    case value of
        Empty ->
            ""

        Text t ->
            t

        Expr { rawInput } ->
            rawInput

        SyntaxError { rawInput } ->
            rawInput


toString : Cell -> String
toString (Cell { value }) =
    case value of
        Empty ->
            ""

        Text t ->
            t

        Expr { result } ->
            result
                |> Result.map String.fromFloat
                |> Result.withDefault "#RuntimeError#"

        SyntaxError _ ->
            "#SyntaxError#"
