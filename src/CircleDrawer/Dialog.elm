module CircleDrawer.Dialog exposing
    ( Dialog
    , Options, view
    )


import Html as H
import Html.Attributes as HA
import CircleDrawer.Html.Attributes as HA
import CircleDrawer.Position exposing (Position)


type alias Dialog msg =
    { block : H.Html msg
    , position : Position
    }


type alias Options msg =
    { viewport : H.Html msg
    }


view : Options msg -> Maybe (Dialog msg) -> H.Html msg
view { viewport } maybeDialog =
    case maybeDialog of
        Just { block, position } ->
            H.div
                [ HA.class "dialog-wrapper" ]
                [ viewport
                , H.div
                    [ HA.class "dialog-background" ]
                    [ H.div
                        [ HA.class "dialog"
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
