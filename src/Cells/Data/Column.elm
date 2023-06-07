module Cells.Data.Column exposing
    ( Column
    , fromInt
    , fromString
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
    -- TODO: Rename to fromSafeInt.
    Column << toSafeN


fromString : String -> Maybe Column
fromString s =
    String.uncons s
        |> Maybe.andThen
            (\( c, t ) ->
                if Char.isUpper c && String.isEmpty t then
                    Just <| Column <| Char.toCode c - codeForA

                else
                    Nothing
            )


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
