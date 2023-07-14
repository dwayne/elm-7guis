module Support.Lib exposing (dispatch, focus)

import Browser.Dom as BD
import Task


dispatch : msg -> Cmd msg
dispatch =
    Task.succeed >> Task.perform identity


focus : String -> msg -> Cmd msg
focus id msg =
    BD.focus id
        |> Task.attempt (always msg)
