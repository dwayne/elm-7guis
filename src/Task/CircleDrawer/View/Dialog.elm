module Task.CircleDrawer.View.Dialog exposing
    ( Dialog
    , Handlers
    , Msg
    , ViewOptions
    , open
    , update
    , view
    )

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Support.Lib as Lib
import Task.CircleDrawer.Data.Position exposing (Position)
import Task.CircleDrawer.Lib.Html.Attributes as HA


type alias Handlers msg =
    { onClose : msg
    , onChange : Msg -> msg
    }


open : String -> Handlers msg -> Cmd msg
open htmlId { onChange } =
    Lib.focus ("dialog-" ++ htmlId) Focus
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


type alias ViewOptions msg =
    { viewport : H.Html msg
    , handlers : Handlers msg
    }


view : ViewOptions msg -> Maybe (Dialog msg) -> H.Html msg
view { viewport, handlers } maybeDialog =
    case maybeDialog of
        Just { htmlId, block, position } ->
            H.div
                [ HA.class "dialog-wrapper" ]
                [ viewport
                , H.div
                    [ HA.id <| "dialog-background-" ++ htmlId
                    , currentTargetOnClick handlers.onClose
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
                            JD.fail "ignored"
                    )
    in
    HE.on "click" decoder
