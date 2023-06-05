module Cells.Data.SCells exposing (SCells, empty, set, get)


import Cells.Data.Coord as Coord exposing (Coord)
import Dict exposing (Dict)


type SCells
    = SCells (Dict String String)


empty : SCells
empty =
    SCells Dict.empty


set : Coord -> String -> SCells -> SCells
set coords value (SCells cells) =
    let
        name =
            Coord.toName coords
    in
    SCells <| Dict.insert name value cells


get : Coord -> SCells -> String
get coords (SCells cells) =
    let
        name =
            Coord.toName coords
    in
    Dict.get name cells
      |> Maybe.withDefault ""
