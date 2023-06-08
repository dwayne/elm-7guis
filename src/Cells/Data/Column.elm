module Cells.Data.Column exposing
    ( Column
    , first
    , fromInt
    , fromSafeString
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


first : Column
first =
    Column minN


fromInt : Int -> Column
fromInt =
    -- TODO: Rename to fromSafeInt.
    Column << toSafeN


toSafeN : Int -> Int
toSafeN n =
    min (max n minN) maxN


fromSafeString : String -> Column
fromSafeString s =
    String.uncons s
        |> Maybe.map
            (\( c, t ) ->
                if Char.isUpper c && String.isEmpty t then
                    Column <| Char.toCode c - codeForA

                else
                    first
            )
        |> Maybe.withDefault first


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
