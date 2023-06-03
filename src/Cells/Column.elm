module Cells.Column exposing
    ( Column
    , fromInt
    , map
    , toInt
    , toString
    )


type Column
    = Column Int


minN : Int
minN =
    0


maxN : Int
maxN =
    25


fromInt : Int -> Column
fromInt =
    Column << toSafeN


toSafeN : Int -> Int
toSafeN n =
    min (max n minN) maxN


map : (Column -> a) -> List a
map f =
    List.map (f << Column) range


range : List Int
range =
    List.range minN maxN


toInt : Column -> Int
toInt (Column n) =
    n


toString : Column -> String
toString (Column n) =
    codeForA
        + n
        |> Char.fromCode
        |> String.fromChar


codeForA : Int
codeForA =
    Char.toCode 'A'
