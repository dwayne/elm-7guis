module Cells.Data.Cell exposing
    ( Cell
    , empty
    , fromString
    , toInputString
    , toString
    )

import Cells.AST as AST
import Cells.Data.Coord exposing (Coord)
import Cells.Data.Range as Range exposing (Range)
import Cells.Data.SCells as SCells exposing (SCells)
import Cells.Parser as P
import Dict exposing (Dict)
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
    | WrongNumberOfArguments
        String
        { expected : Int
        , given : Int
        }
    | DivisionByZero


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
            Ok <| evaluateCoord scells coord

        AST.Range range ->
            Err <| TopLevelRange range

        AST.Application identifier exprs ->
            case Dict.get identifier builtinFunctions of
                Just f ->
                    evaluateArgs scells exprs
                        |> Result.andThen f

                Nothing ->
                    Err <| IdentifierNotFound identifier


evaluateCoord : SCells Cell -> Coord -> Float
evaluateCoord scells coord =
    toFloat <| SCells.get empty coord scells


evaluateArgs : SCells Cell -> List AST.Expr -> Result RuntimeError (List Float)
evaluateArgs scells =
    concatMapSequence (evaluateArg scells)


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


evaluateArg : SCells Cell -> AST.Expr -> Result RuntimeError (List Float)
evaluateArg scells expr =
    case expr of
        AST.Range range ->
            range
                |> Range.expand
                |> List.map (evaluateCoord scells)
                |> Ok

        _ ->
            evaluate scells expr
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
