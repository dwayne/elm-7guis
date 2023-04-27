module Timer exposing (Model, init, Msg, update, view)


import Timer.Duration as Duration exposing (Duration)
import Html as H
import Html.Attributes as HA
import Html.Events as HE


-- CONSTANTS


maxMillis : Int
maxMillis =
    30000


maxDuration : Duration
maxDuration =
    Duration.fromInt maxMillis


-- MODEL


type alias Model =
    { duration : Duration
    }


init : Model
init =
    { duration = Duration.fromInt <| maxMillis // 2
    }


-- UPDATE


type Msg
    = InputDuration String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputDuration s ->
            case Duration.fromString s of
                Just duration ->
                    { model | duration = duration }

                Nothing ->
                    model


-- VIEW


view : Model -> H.Html Msg
view { duration } =
    let
        elapsedTime =
            String.fromInt <| Duration.toInt duration // 3
    in
    H.div []
        [ H.div []
            [ H.text "Elapsed Time: "
            , H.meter
                [ HA.min "0"
                , HA.max <| Duration.toString duration
                , HA.value elapsedTime
                ]
                []
            ]
        , H.div [] [ H.text elapsedTime ]
        , H.div []
            [ H.text "Duration: "
            , H.input
                [ HA.type_ "range"
                , HA.min "0"
                , HA.max <| Duration.toString maxDuration
                , HA.value <| Duration.toString duration
                , HE.onInput InputDuration
                ]
                []
            ]
        , H.div []
            [ H.button
                [ HA.type_ "button"
                ]
                [ H.text "Reset" ]
            ]
        ]
