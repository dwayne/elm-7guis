module Cells exposing (main)

import Browser
import Task.Cells exposing (Model, Msg, init, update, view)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
