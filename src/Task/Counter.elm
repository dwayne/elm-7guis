module Task.Counter exposing (Model, Msg, init, update, view)

import Html as H
import Html.Events as HE



-- MODEL


type Model
    = Model Int


init : Model
init =
    Model 0



-- UPDATE


type Msg
    = ClickedIncrement


update : Msg -> Model -> Model
update msg (Model n) =
    case msg of
        ClickedIncrement ->
            Model <| n + 1



-- VIEW


view : Model -> H.Html Msg
view (Model n) =
    H.div []
        [ H.text <| String.fromInt n
        , H.button [ HE.onClick ClickedIncrement ] [ H.text "Count" ]
        ]
