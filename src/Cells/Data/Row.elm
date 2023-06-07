module Cells.Data.Row exposing
    ( Row
    , fromInt
    , fromString
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
    -- TODO: Rename to fromSafeInt.
    Row << toSafeN


fromString : String -> Maybe Row
fromString s =
    String.toInt s
        |> Maybe.andThen
            (\n ->
                if minN <= n && n <= maxN then
                    Just <| Row n

                else
                    Nothing
            )


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
