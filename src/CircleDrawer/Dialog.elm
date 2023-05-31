module CircleDrawer.Dialog exposing
    ( Config
    , Dialog
    , Msg
    , Options
    , open
    , update
    , view
    )

import Browser.Dom as BD
import CircleDrawer.Html.Attributes as HA
import CircleDrawer.Position exposing (Position)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Process
import Task


type alias Config msg =
    { onClose : msg
    , onChange : Msg -> msg
    }


open : Config msg -> String -> Cmd msg
open { onChange } htmlId =
    "dialog-"
        ++ htmlId
        |> BD.focus
        |> Task.attempt (always Focus)
        |> Cmd.map onChange


type Msg
    = Focus


update : Msg -> Cmd msg
update msg =
    case msg of
        Focus ->
            Cmd.none


type alias Dialog msg =
    { htmlId : String
    , block : H.Html msg
    , position : Position
    }


type alias Options msg =
    { viewport : H.Html msg
    }


view : Options msg -> Config msg -> Maybe (Dialog msg) -> H.Html msg
view { viewport } { onClose } maybeDialog =
    case maybeDialog of
        Just { htmlId, block, position } ->
            H.div
                [ HA.class "dialog-wrapper" ]
                [ viewport
                , H.div
                    [ HA.id <| "dialog-background-" ++ htmlId
                    , currentTargetOnClick onClose
                    , HA.class "dialog-background"
                    ]
                    [ H.div
                        [ HA.id <| "dialog-" ++ htmlId
                        , HA.tabindex -1
                        , HA.class "dialog"
                        , HA.customProperties
                            [ ( "dialog-x", String.fromInt position.x ++ "px" )
                            , ( "dialog-y", String.fromInt position.y ++ "px" )
                            ]
                        ]
                        [ block ]
                    ]
                ]

        Nothing ->
            viewport


currentTargetOnClick : msg -> H.Attribute msg
currentTargetOnClick msg =
    let
        decoder =
            JD.map2 Tuple.pair
                (JD.at [ "currentTarget", "id" ] JD.string)
                (JD.at [ "target", "id" ] JD.string)
                |> JD.andThen
                    (\( currentTargetId, targetId ) ->
                        if currentTargetId == targetId then
                            JD.succeed msg

                        else
                            JD.fail "ignore click"
                    )
    in
    HE.on "click" decoder
