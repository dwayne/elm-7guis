module Crud exposing (view)


import Html as H
import Html.Attributes as HA


view : H.Html msg
view =
    H.div []
        [ H.div []
            [ H.text "Filter prefix: "
            , H.input
                [ HA.type_ "text"
                ]
                []
            ]
        , H.div []
            [ H.div []
                [ H.select
                    [ HA.size 2
                    ]
                    [ H.option [ HA.value "1" ] [ H.text "Emil, Hans" ]
                    , H.option [ HA.value "2" ] [ H.text "Mustermann, Max" ]
                    , H.option [ HA.value "3" ] [ H.text "Tisch, Roman" ]
                    ]
                ]
            , H.div []
                [ H.div []
                    [ H.label [] [ H.text "Name: " ]
                    , H.input
                        [ HA.type_ "text" ]
                        []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Surname: " ]
                    , H.input
                        [ HA.type_ "text" ]
                        []
                    ]
                ]
            ]
        , H.div []
            [ H.button [ HA.type_ "button" ] [ H.text "Create" ]
            , H.button [ HA.type_ "button" ] [ H.text "Update" ]
            , H.button [ HA.type_ "button" ] [ H.text "Delete" ]
            ]
        ]
