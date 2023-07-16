module Support.View.Button exposing
    ( Options
    , Type(..)
    , view
    )

import Html as H
import Html.Attributes as HA
import Html.Events as HE


type Type msg
    = Button msg
    | Disabled
    | Submit


type alias Options msg =
    { type_ : Type msg
    , text : String
    }


view : Options msg -> H.Html msg
view { type_, text } =
    let
        attrs =
            baseAttrs ++ additionalAttrs

        baseAttrs =
            [ HA.class "button"
            ]

        additionalAttrs =
            case type_ of
                Button onClick ->
                    [ HA.type_ "button"
                    , HE.onClick onClick
                    ]

                Disabled ->
                    [ HA.type_ "button"
                    , HA.disabled True
                    ]

                Submit ->
                    [ HA.type_ "submit"
                    ]
    in
    H.button attrs [ H.text text ]
