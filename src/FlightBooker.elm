module FlightBooker exposing (main)

import Browser
import Task.FlightBooker exposing (Model, Msg, init, update, view)


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
