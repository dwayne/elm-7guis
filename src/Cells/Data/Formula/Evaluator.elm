module Cells.Data.Formula.Evaluator exposing
    ( Env
    , RuntimeError(..)
    , Value(..)
    , evalExpr
    , evalFormula
    , evalString
    )

import Cells.Data.Coord as Coord exposing (Coord)
import Cells.Data.Formula.AST as AST
import Cells.Data.Formula.Parser as P
import Cells.Data.Range as Range exposing (Range)
import Dict exposing (Dict)
import Parser exposing (DeadEnd)


type alias Env =
    Coord -> Float


type Value
    = Formula
        { formula : AST.Formula
        , result : Result RuntimeError Float
        }
    | SyntaxError (List DeadEnd)


type RuntimeError
    = TopLevelRange Range
    | IdentifierNotFound String
    | WrongNumberOfArguments
        String
        { expected : Int
        , given : Int
        }
    | DivisionByZero


evalString : Env -> String -> Value
evalString env rawInput =
    case P.parse rawInput of
        Ok formula ->
            Formula
                { formula = formula
                , result = evalFormula env formula
                }

        Err deadEnds ->
            SyntaxError deadEnds


evalFormula : Env -> AST.Formula -> Result RuntimeError Float
evalFormula env formula =
    case formula of
        AST.Text _ ->
            Ok 0

        AST.Expr expr ->
            evalExpr env expr


evalExpr : Env -> AST.Expr -> Result RuntimeError Float
evalExpr env expr =
    case expr of
        AST.Number x ->
            Ok x

        AST.Cell coord ->
            Ok <| env coord

        AST.Range range ->
            Err <| TopLevelRange range

        AST.Application identifier exprs ->
            case Dict.get identifier builtinFunctions of
                Just f ->
                    evalArgs env exprs
                        |> Result.andThen f

                Nothing ->
                    Err <| IdentifierNotFound identifier


evalArgs : Env -> List AST.Expr -> Result RuntimeError (List Float)
evalArgs env =
    concatMapSequence (evalArg env)


concatMapSequence : (a -> Result e (List b)) -> List a -> Result e (List b)
concatMapSequence =
    concatMapSequenceHelper []


concatMapSequenceHelper : List b -> (a -> Result e (List b)) -> List a -> Result e (List b)
concatMapSequenceHelper acc f list =
    case list of
        [] ->
            Ok <| List.reverse acc

        a :: restList ->
            case f a of
                Ok bs ->
                    concatMapSequenceHelper (bs ++ acc) f restList

                err ->
                    err


evalArg : Env -> AST.Expr -> Result RuntimeError (List Float)
evalArg env expr =
    case expr of
        AST.Range range ->
            range
                |> Range.expand
                |> List.map env
                |> Ok

        _ ->
            evalExpr env expr
                |> Result.map List.singleton


builtinFunctions : Dict String (List Float -> Result RuntimeError Float)
builtinFunctions =
    Dict.fromList
        [ ( "add"
          , \args ->
                case args of
                    x :: y :: [] ->
                        Ok <| x + y

                    _ ->
                        Err <| WrongNumberOfArguments "add" { expected = 2, given = List.length args }
          )
        , ( "sub"
          , \args ->
                case args of
                    x :: y :: [] ->
                        Ok <| x - y

                    _ ->
                        Err <| WrongNumberOfArguments "sub" { expected = 2, given = List.length args }
          )
        , ( "mul"
          , \args ->
                case args of
                    x :: y :: [] ->
                        Ok <| x * y

                    _ ->
                        Err <| WrongNumberOfArguments "mul" { expected = 2, given = List.length args }
          )
        , ( "div"
          , \args ->
                case args of
                    x :: y :: [] ->
                        if y /= 0 then
                            Ok <| x / y

                        else
                            Err DivisionByZero

                    _ ->
                        Err <| WrongNumberOfArguments "div" { expected = 2, given = List.length args }
          )
        , ( "mod"
          , \args ->
                case args of
                    x :: y :: [] ->
                        let
                            m =
                                truncate x

                            n =
                                truncate y
                        in
                        if n /= 0 then
                            Ok <| Basics.toFloat (m |> modBy n)

                        else
                            Err DivisionByZero

                    _ ->
                        Err <| WrongNumberOfArguments "mod" { expected = 2, given = List.length args }
          )
        , ( "sum"
          , Ok << List.sum
          )
        , ( "prod"
          , Ok << List.product
          )
        ]
