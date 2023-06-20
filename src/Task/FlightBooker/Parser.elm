module Task.FlightBooker.Parser exposing (chompExactly)

import Parser as P exposing ((|.), Parser)


chompExactly : Int -> (Char -> Bool) -> Parser String
chompExactly n =
    chompExactlyHelper n >> P.getChompedString


chompExactlyHelper : Int -> (Char -> Bool) -> Parser ()
chompExactlyHelper n pred =
    if n == 0 then
        P.succeed ()

    else
        P.succeed ()
            |. P.chompIf pred
            |. chompExactly (n - 1) pred
