module Task.Timer.Duration exposing
    ( Duration
    , fromInt
    , fromString
    , toFloat
    , toInt
    , toString
    )


type Duration
    = Duration Int


fromInt : Int -> Duration
fromInt =
    Duration << max 0


fromString : String -> Maybe Duration
fromString =
    Maybe.map fromInt << String.toInt


toInt : Duration -> Int
toInt (Duration n) =
    n


toFloat : Duration -> Float
toFloat (Duration n) =
    Basics.toFloat n


toString : Duration -> String
toString (Duration n) =
    String.fromInt n
