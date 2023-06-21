module Task.CircleDrawer.Lib.Html.Events exposing
    ( onInputDiameter
    , onMainButtonClick
    , onMouseMove
    , onSecondaryButtonClick
    )

import Html as H
import Html.Events as HE
import Json.Decode as JD
import Task.CircleDrawer.Data.Diameter as Diameter exposing (Diameter)
import Task.CircleDrawer.Data.Position exposing (Position)


onMainButtonClick : (Position -> msg) -> H.Attribute msg
onMainButtonClick toMsg =
    let
        decoder =
            buttonDecoder
                |> JD.andThen
                    (\button ->
                        if button == 0 then
                            JD.map toMsg positionDecoder

                        else
                            JD.fail "ignored"
                    )
    in
    HE.on "click" decoder


onSecondaryButtonClick : (Position -> msg) -> H.Attribute msg
onSecondaryButtonClick toMsg =
    let
        decoder =
            buttonDecoder
                |> JD.andThen
                    (\button ->
                        if button == 2 then
                            JD.map
                                (\position -> ( toMsg position, True ))
                                positionDecoder

                        else
                            JD.fail "ignored"
                    )
    in
    HE.preventDefaultOn "contextmenu" decoder


onMouseMove : (Position -> msg) -> H.Attribute msg
onMouseMove toMsg =
    HE.on "mousemove" (JD.map toMsg positionDecoder)


onInputDiameter : (Diameter -> msg) -> H.Attribute msg
onInputDiameter toMsg =
    let
        decoder =
            HE.targetValue
                |> JD.andThen
                    (String.toInt
                        >> Maybe.andThen Diameter.fromInt
                        >> Maybe.map (JD.succeed << toMsg)
                        >> Maybe.withDefault (JD.fail "ignored")
                    )
    in
    HE.on "input" decoder


buttonDecoder : JD.Decoder Int
buttonDecoder =
    JD.field "button" JD.int


positionDecoder : JD.Decoder Position
positionDecoder =
    JD.map4
        (\pageX pageY offsetLeft offsetTop ->
            let
                x =
                    pageX - offsetLeft

                y =
                    pageY - offsetTop
            in
            Position x y
        )
        (JD.field "pageX" JD.int)
        (JD.field "pageY" JD.int)
        (JD.at [ "currentTarget", "offsetLeft" ] JD.int)
        (JD.at [ "currentTarget", "offsetTop" ] JD.int)
