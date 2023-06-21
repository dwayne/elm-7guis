module Task.Timer.Duration exposing
    ( Duration
    , fromString
    , halfOfMax
    , max
    , toFloat
    , toString
    )


type Duration
    = Duration Int


max : Duration
max =
    Duration 30000


halfOfMax : Duration
halfOfMax =
    Duration 15000


fromString : String -> Maybe Duration
fromString =
    Maybe.map fromInt << String.toInt


fromInt : Int -> Duration
fromInt =
    Duration << Basics.max 0


toFloat : Duration -> Float
toFloat (Duration n) =
    Basics.toFloat n


toString : Duration -> String
toString (Duration n) =
    String.fromInt n
