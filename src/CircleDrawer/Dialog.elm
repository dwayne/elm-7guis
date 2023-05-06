module CircleDrawer.Dialog exposing
    ( Config
    , open
    , update
    , Msg
    , Dialog
    , Options
    , view
    )


import Browser.Dom as BD
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import CircleDrawer.Html.Attributes as HA
import CircleDrawer.Position exposing (Position)
import Process
import Task


type alias Config msg =
    { onClose : msg
    , onChange : Msg -> msg
    }


open : Config msg -> String -> Cmd msg
open { onChange } htmlId =
    Process.sleep 0
        |> Task.andThen (always <| BD.focus <| toDialogId htmlId)
        |> Task.attempt (always Focus)
        |> Cmd.map onChange


type Msg
    = Focus
    | Blur
    | Close


update : Config msg -> Msg -> Cmd msg
update { onClose, onChange } msg =
    case msg of
        Focus ->
            Cmd.none

        Blur ->
            Process.sleep 250
                |> Task.attempt (always onClose)

        Close ->
            dispatch onClose


dispatch : msg -> Cmd msg
dispatch =
    Task.succeed >> Task.perform identity


type alias Dialog msg =
    { htmlId : String
    , block : H.Html msg
    , position : Position
    }


type alias Options msg =
    { viewport : H.Html msg
    }


view : Options msg -> Config msg -> Maybe (Dialog msg) -> H.Html msg
view { viewport } { onChange } maybeDialog =
    case maybeDialog of
        Just { htmlId, block, position } ->
            H.div
                [ HA.class "dialog-wrapper" ]
                [ viewport
                , H.div
                    [ HA.class "dialog-background" ]
                    [ H.div
                        [ HA.id <| toDialogId htmlId
                        , HA.tabindex -1
                        , HA.class "dialog"
                        , HA.customProperties
                            [ ( "dialog-x", String.fromInt position.x ++ "px" )
                            , ( "dialog-y", String.fromInt position.y ++ "px" )
                            ]
                        , HE.onBlur (onChange Blur)
                        ]
                        [ block ]
                    ]
                ]

        Nothing ->
            viewport


toDialogId : String -> String
toDialogId =
    (++) "dialog-"
