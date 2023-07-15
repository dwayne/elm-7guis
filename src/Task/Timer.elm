module Task.Timer exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events as BE
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Support.View.Button as Button
import Support.View.Control as Control
import Support.View.Frame as Frame
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
    Frame.view
        { modifier = Frame.Default
        , title = "Timer"
        , body =
            H.div [ HA.class "timer" ]
                [ viewField
                    { id = "elapsedTime"
                    , text = "Elapsed Time:"
                    , toField = \id -> viewElapsedTime id duration elapsedTime
                    }
                , viewOutput "elapsedTime" elapsedTime
                , viewField
                    { id = "duration"
                    , text = "Duration:"
                    , toField = \id -> viewDuration id duration
                    }
                , Button.view
                    { type_ = Button.Button <| Just ClickedReset
                    , text = "Reset"
                    }
                ]
        }


viewField :
    { id : String
    , text : String
    , toField : String -> H.Html msg
    }
    -> H.Html msg
viewField { id, text, toField } =
    H.div [ HA.class "timer__field" ]
        [ Control.viewLabel
            { for = id
            , text = text
            }
        , toField id
        ]


viewElapsedTime : String -> Duration -> Float -> H.Html msg
viewElapsedTime id duration elapsedTime =
    H.meter
        [ HA.id id
        , HA.min "0"
        , HA.max <| Duration.toString duration
        , HA.value <| String.fromFloat elapsedTime
        ]
        []


viewDuration : String -> Duration -> H.Html Msg
viewDuration id duration =
    H.input
        [ HA.id id
        , HA.type_ "range"
        , HA.step "1"
        , HA.min "0"
        , HA.max <| Duration.toString Duration.max
        , HA.value <| Duration.toString duration
        , onDurationInput InputDuration
        ]
        []


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


viewOutput : String -> Float -> H.Html msg
viewOutput for elapsedTime =
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
            String.concat
                [ String.fromInt wholePart
                , "."
                , String.fromInt decimalPart
                , "s"
                ]
    in
    H.output [ HA.for for ] [ H.text seconds ]
