module Task.Timer exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events as BE
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Task.Timer.Duration as Duration exposing (Duration)



-- MODEL


type alias Model =
    { duration : Duration
    , elapsedTime : Float
    }


init : Model
init =
    { duration = Duration.halfOfMax
    , elapsedTime = 0
    }



-- UPDATE


type Msg
    = InputDuration Duration
    | NewDelta Float
    | ClickedReset


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputDuration duration ->
            { model | duration = duration }

        NewDelta delta ->
            let
                elapsedTime =
                    model.elapsedTime
                        + delta
                        |> clamp 0 max

                max =
                    Duration.toFloat model.duration
            in
            { model | elapsedTime = elapsedTime }

        ClickedReset ->
            { model | elapsedTime = 0 }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { duration, elapsedTime } =
    if elapsedTime < Duration.toFloat duration then
        BE.onAnimationFrameDelta NewDelta

    else
        Sub.none



-- VIEW


view : Model -> H.Html Msg
view { duration, elapsedTime } =
    H.div []
        [ H.div []
            [ H.text "Elapsed Time: "
            , H.meter
                [ HA.min "0"
                , HA.max <| Duration.toString duration
                , HA.value <| String.fromFloat elapsedTime
                ]
                []
            ]
        , viewElapsedTime elapsedTime
        , H.div []
            [ H.text "Duration: "
            , H.input
                [ HA.type_ "range"
                , HA.min "0"
                , HA.max <| Duration.toString Duration.max
                , HA.value <| Duration.toString duration
                , onDurationInput InputDuration
                ]
                []
            ]
        , H.div []
            [ H.button
                [ HA.type_ "button"
                , HE.onClick ClickedReset
                ]
                [ H.text "Reset" ]
            ]
        ]


onDurationInput : (Duration -> msg) -> H.Attribute msg
onDurationInput toMsg =
    let
        decoder =
            HE.targetValue
                |> JD.andThen
                    (\value ->
                        case Duration.fromString value of
                            Just duration ->
                                JD.succeed duration

                            Nothing ->
                                JD.fail "ignored"
                    )
                |> JD.map toMsg
    in
    HE.on "input" decoder


viewElapsedTime : Float -> H.Html msg
viewElapsedTime elapsedTime =
    let
        wholePart =
            elapsedTime
                / 1000
                |> floor

        decimalPart =
            elapsedTime
                / 100
                |> floor
                |> modBy 10

        seconds =
            String.join ""
                [ String.fromInt wholePart
                , "."
                , String.fromInt decimalPart
                , "s"
                ]
    in
    H.div [] [ H.text seconds ]
