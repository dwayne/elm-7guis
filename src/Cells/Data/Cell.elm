module Cells.Data.Cell exposing
    ( Cell
    , Env
    , empty
    , fromString
    , references
    , refresh
    , toInputString
    , toString
    )

import Cells.Data.Coord as Coord exposing (Coord)
import Cells.Data.Formula.AST as AST
import Cells.Data.Formula.Evaluator as E
import Cells.Data.Formula.Parser as P
import Cells.Data.Range as Range exposing (Range)
import Dict exposing (Dict)
import Parser exposing (DeadEnd)
import Set exposing (Set)


type Cell
    = Cell Content


type Content
    = Empty
    | NonEmpty
        { rawInput : String
        , value : E.Value
        }


type alias Env =
    Coord -> Cell


empty : Cell
empty =
    Cell Empty


fromString : Env -> String -> Cell
fromString env rawInput =
    Cell <|
        NonEmpty
            { rawInput = rawInput
            , value = E.evalString (env >> toFloat) rawInput
            }


references : Cell -> Set String
references (Cell content) =
    case content of
        Empty ->
            Set.empty

        NonEmpty { value } ->
            case value of
                E.Formula { formula } ->
                    referencesForFormula formula

                E.SyntaxError _ ->
                    Set.empty


referencesForFormula : AST.Formula -> Set String
referencesForFormula formula =
    case formula of
        AST.Text _ ->
            Set.empty

        AST.Expr expr ->
            referencesForExpr expr


referencesForExpr : AST.Expr -> Set String
referencesForExpr expr =
    case expr of
        AST.Number _ ->
            Set.empty

        AST.Cell coord ->
            Set.singleton <| Coord.toName coord

        AST.Range range ->
            range
                |> Range.expand
                |> List.map Coord.toName
                |> Set.fromList

        AST.Application _ exprs ->
            exprs
                |> List.map referencesForExpr
                |> List.foldl Set.union Set.empty


refresh : Env -> Cell -> Cell
refresh env ((Cell content) as cell) =
    case content of
        Empty ->
            cell

        NonEmpty { rawInput, value } ->
            case value of
                E.Formula { formula } ->
                    Cell <|
                        NonEmpty
                            { rawInput = rawInput
                            , value =
                                E.Formula
                                    { formula = formula
                                    , result = E.evalFormula (env >> toFloat) formula
                                    }
                            }

                E.SyntaxError _ ->
                    cell


toFloat : Cell -> Float
toFloat =
    map
        { onEmpty = 0
        , onFormula = \_ _ -> identity
        , onSyntaxError = \_ _ -> 0
        , onRuntimeError = \_ _ -> 0
        }


toInputString : Cell -> String
toInputString =
    map
        { onEmpty = ""
        , onFormula = \rawInput _ _ -> rawInput
        , onSyntaxError = \rawInput _ -> rawInput
        , onRuntimeError = \rawInput _ -> rawInput
        }


toString : Cell -> String
toString =
    map
        { onEmpty = ""
        , onFormula =
            \_ formula x ->
                case formula of
                    AST.Text t ->
                        t

                    AST.Expr _ ->
                        String.fromFloat x
        , onSyntaxError = \_ _ -> "#SyntaxError#"
        , onRuntimeError = \_ _ -> "#RuntimeError"
        }


map :
    { onEmpty : a
    , onFormula : String -> AST.Formula -> Float -> a
    , onSyntaxError : String -> List DeadEnd -> a
    , onRuntimeError : String -> E.RuntimeError -> a
    }
    -> Cell
    -> a
map { onEmpty, onFormula, onSyntaxError, onRuntimeError } (Cell content) =
    case content of
        Empty ->
            onEmpty

        NonEmpty { rawInput, value } ->
            case value of
                E.Formula { formula, result } ->
                    case result of
                        Ok x ->
                            onFormula rawInput formula x

                        Err runtimeError ->
                            onRuntimeError rawInput runtimeError

                E.SyntaxError deadEnds ->
                    onSyntaxError rawInput deadEnds
