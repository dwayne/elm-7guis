module Timer.Duration exposing
    ( Duration, fromInt, fromString
    , toInt, toString
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


toString : Duration -> String
toString (Duration n) =
    String.fromInt n
