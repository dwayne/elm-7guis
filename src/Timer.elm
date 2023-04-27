module Timer exposing (view)


import Html as H
import Html.Attributes as HA


view : H.Html msg
view =
    H.div []
        [ H.div []
            [ H.text "Elapsed Time: "
            , H.meter
                [ HA.min "0"
                , HA.max "15000"
                , HA.value "5300"
                ]
                []
            ]
        , H.div [] [ H.text "5.3s" ]
        , H.div []
            [ H.text "Duration: "
            , H.input
                [ HA.type_ "range"
                , HA.min "0"
                , HA.max "30000"
                , HA.value "15000"
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
