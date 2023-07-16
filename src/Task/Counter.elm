module Task.Counter exposing (Model, Msg, init, update, view)

import Html as H
import Html.Attributes as HA
import Support.View.Button as Button
import Support.View.Frame as Frame



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
    Frame.view
        { modifier = Frame.Default
        , title = "Counter"
        , body =
            H.div [ HA.class "counter" ]
                [ H.output [] [ H.text <| String.fromInt n ]
                , Button.view
                    { type_ = Button.Button ClickedIncrement
                    , text = "Count"
                    }
                ]
        }
