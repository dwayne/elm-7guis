module Cells.Row exposing
    ( Row
    , fromInt
    , map
    , toInt
    , toString
    )


type Row
    = Row Int


minN : Int
minN =
    0


maxN : Int
maxN =
    99


fromInt : Int -> Row
fromInt =
    Row << toSafeN


toSafeN : Int -> Int
toSafeN n =
    min (max n minN) maxN


map : (Row -> a) -> List a
map f =
    List.map (f << Row) range


range : List Int
range =
    List.range minN maxN


toInt : Row -> Int
toInt (Row n) =
    n


toString : Row -> String
toString (Row n) =
    String.fromInt n
