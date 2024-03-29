module Task.Cells.Data.Cell exposing
    ( Cell
    , Env
    , empty
    , fromString
    , hasError
    , references
    , refresh
    , toEditableString
    , toString
    )

import Parser exposing (DeadEnd)
import Set exposing (Set)
import Task.Cells.Data.Coord as Coord exposing (Coord)
import Task.Cells.Data.Formula.AST as AST
import Task.Cells.Data.Formula.Evaluator as E
import Task.Cells.Data.Range as Range


type Cell
    = Cell Content


type Content
    = Empty
    | NonEmpty
        { rawInput : String
        , answer : E.Answer
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
            , answer = E.evalString (env >> toFloat) rawInput
            }


hasError : Cell -> Bool
hasError =
    withCell
        { onEmpty = False
        , onFormula = always False
        , onRuntimeError = always True
        , onSyntaxError = always True
        }


references : Cell -> Set String
references =
    withCell
        { onEmpty = Set.empty
        , onFormula = referencesForFormula << .formula
        , onRuntimeError = referencesForFormula << .formula
        , onSyntaxError = always Set.empty
        }


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
            Set.singleton <| Coord.toString coord

        AST.Range range ->
            range
                |> Range.expand
                |> List.map Coord.toString
                |> Set.fromList

        AST.Application _ exprs ->
            exprs
                |> List.map referencesForExpr
                |> List.foldl Set.union Set.empty


refresh : Env -> Cell -> Cell
refresh env cell =
    let
        refreshedCell { rawInput, formula } =
            Cell <|
                NonEmpty
                    { rawInput = rawInput
                    , answer = E.evalFormula (env >> toFloat) formula
                    }
    in
    withCell
        { onEmpty = cell
        , onFormula = refreshedCell
        , onRuntimeError = refreshedCell
        , onSyntaxError = always cell
        }
        cell


toFloat : Cell -> Float
toFloat =
    withCell
        { onEmpty = 0
        , onFormula = .value
        , onRuntimeError = always 0
        , onSyntaxError = always 0
        }


toEditableString : Cell -> String
toEditableString =
    withCell
        { onEmpty = ""
        , onFormula = formulaToString << .formula
        , onRuntimeError = formulaToString << .formula
        , onSyntaxError = .rawInput
        }


formulaToString : AST.Formula -> String
formulaToString formula =
    case formula of
        AST.Text t ->
            t

        AST.Expr expr ->
            "=" ++ exprToString expr


exprToString : AST.Expr -> String
exprToString expr =
    case expr of
        AST.Number x ->
            String.fromFloat x

        AST.Cell coord ->
            Coord.toString coord

        AST.Range range ->
            Range.toString range

        AST.Application identifier exprs ->
            let
                commaSepExprs =
                    exprs
                        |> List.map exprToString
                        |> List.intersperse ", "
                        |> String.concat
            in
            identifier ++ "(" ++ commaSepExprs ++ ")"


toString : Cell -> String
toString =
    withCell
        { onEmpty = ""
        , onFormula =
            \{ formula, value } ->
                case formula of
                    AST.Text t ->
                        t

                    AST.Expr _ ->
                        String.fromFloat value
        , onRuntimeError = always "Runtime Error"
        , onSyntaxError = always "Syntax Error"
        }


withCell :
    { onEmpty : a
    , onFormula :
        { rawInput : String
        , formula : AST.Formula
        , value : Float
        }
        -> a
    , onRuntimeError :
        { rawInput : String
        , formula : AST.Formula
        , error : E.RuntimeError
        }
        -> a
    , onSyntaxError :
        { rawInput : String
        , deadEnds : List DeadEnd
        }
        -> a
    }
    -> Cell
    -> a
withCell { onEmpty, onFormula, onSyntaxError, onRuntimeError } (Cell content) =
    case content of
        Empty ->
            onEmpty

        NonEmpty { rawInput, answer } ->
            case answer of
                E.Formula { formula, value } ->
                    onFormula
                        { rawInput = rawInput
                        , formula = formula
                        , value = value
                        }

                E.RuntimeError { formula, error } ->
                    onRuntimeError
                        { rawInput = rawInput
                        , formula = formula
                        , error = error
                        }

                E.SyntaxError deadEnds ->
                    onSyntaxError
                        { rawInput = rawInput
                        , deadEnds = deadEnds
                        }
