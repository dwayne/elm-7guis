module Task.CircleDrawer.Data.Diameter exposing
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


minN : Int
minN =
    2


maxN : Int
maxN =
    100


min : Diameter
min =
    Diameter minN


max : Diameter
max =
    Diameter maxN


fromSafeInt : Int -> Diameter
fromSafeInt =
    fromInt >> Maybe.withDefault min


fromInt : Int -> Maybe Diameter
fromInt n =
    if n >= minN && n <= maxN then
        Just <| Diameter n

    else
        Nothing


toFloat : Diameter -> Float
toFloat (Diameter n) =
    Basics.toFloat n


toString : Diameter -> String
toString (Diameter n) =
    String.fromInt n
