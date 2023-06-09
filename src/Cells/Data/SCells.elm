module Cells.Data.SCells exposing (SCells, empty, get, set)

import Cells.Data.Coord as Coord exposing (Coord)
import Dict exposing (Dict)


type SCells a
    = SCells (Dict String a)


empty : SCells a
empty =
    SCells Dict.empty


set : Coord -> a -> SCells a -> SCells a
set coord a (SCells cells) =
    let
        name =
            Coord.toName coord
    in
    SCells <| Dict.insert name a cells


get : (Coord -> a) -> Coord -> SCells a -> a
get toDefault coord (SCells cells) =
    let
        name =
            Coord.toName coord
    in
    Dict.get name cells
        |> Maybe.withDefault (toDefault coord)
