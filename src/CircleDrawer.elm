module CircleDrawer exposing (view)


import Html as H
import Html.Attributes as HA


view : H.Html msg
view =
    H.div []
        [ H.div []
            [ H.button [ HA.type_ "button" ] [ H.text "Undo" ]
            , H.button [ HA.type_ "button" ] [ H.text "Redo" ]
            ]
        , H.div [ HA.class "canvas" ] []
        ]
