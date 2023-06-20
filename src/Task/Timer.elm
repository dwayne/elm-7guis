module Task.Timer exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events as BE
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Task.Timer.Duration as Duration exposing (Duration)



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
    , elapsedTime : Float
    }


init : Model
init =
    { duration = Duration.fromInt <| maxMillis // 2
    , elapsedTime = 0
    }



-- UPDATE


type Msg
    = InputDuration String
    | NewDelta Float
    | ClickedReset


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputDuration s ->
            case Duration.fromString s of
                Just duration ->
                    { model | duration = duration }

                Nothing ->
                    model

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
                , HA.max <| Duration.toString maxDuration
                , HA.value <| Duration.toString duration
                , HE.onInput InputDuration
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
