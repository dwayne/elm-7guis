module CircleDrawer.Diameter exposing
    ( Diameter
    , fromInt
    , fromSafeInt
    , max
    , min
    , toFloat
    , toString
    )


type Diameter
    = Diameter Int


min : Diameter
min =
    Diameter 2


max : Diameter
max =
    Diameter 100


fromInt : Int -> Maybe Diameter
fromInt n =
    if n >= 2 && n <= 100 then
        Just <| Diameter n

    else
        Nothing


fromSafeInt : Int -> Diameter
fromSafeInt n =
    if n >= 2 && n <= 100 then
        Diameter n

    else
        Diameter 2


toFloat : Diameter -> Float
toFloat (Diameter n) =
    Basics.toFloat n


toString : Diameter -> String
toString (Diameter n) =
    String.fromInt n
