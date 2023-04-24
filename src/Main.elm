module Main exposing (main)


import Browser
import Counter
import Html as H
import Html.Attributes as HA


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


-- MODEL


type alias Model =
    { counter : Counter.Model
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { counter = Counter.init
      }
    , Cmd.none
    )


-- UPDATE


type Msg
    = ChangedCounter Counter.Msg


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ChangedCounter counterMsg ->
            ( { model | counter = Counter.update counterMsg model.counter }
            , Cmd.none
            )


-- VIEW


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.h1 [] [ H.text "7GUIs in Elm" ]
        , H.p []
            [ H.text "This is a live version of an implementation ("
            , H.a
                [ HA.href "https://github.com/dwayne/elm-7guis"
                , HA.target "_blank"
                ]
                [ H.text "source" ]
            , H.text ") of "
            , H.a
                [ HA.href "https://eugenkiss.github.io/7guis/"
                , HA.target "_blank"
                ]
                [ H.text "7GUIs" ]
            , H.text " with "
            , H.a
                [ HA.href "https://elm-lang.org/"
                , HA.target "_blank"
                ]
                [ H.text "Elm" ]
            , H.text "."
            ]
        , viewCounter model.counter
        ]


viewCounter : Counter.Model -> H.Html Msg
viewCounter counter =
    H.div []
        [ H.h2 [] [ H.text "Counter" ]
        , Counter.view counter
            |> H.map ChangedCounter
        ]
