module Counter exposing (main)

import Browser
import Task.Counter exposing (Model, Msg, init, update, view)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
