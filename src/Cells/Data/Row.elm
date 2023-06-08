module Cells.Data.Row exposing
    ( Row
    , first
    , fromInt
    , fromSafeString
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


first : Row
first =
    Row minN


fromInt : Int -> Row
fromInt =
    -- TODO: Rename to fromSafeInt.
    Row << toSafeN


toSafeN : Int -> Int
toSafeN n =
    min (max n minN) maxN


fromSafeString : String -> Row
fromSafeString s =
    String.toInt s
        |> Maybe.map
            (\n ->
                if minN <= n && n <= maxN then
                    Row n

                else
                    first
            )
        |> Maybe.withDefault first


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
