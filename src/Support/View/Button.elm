module Support.View.Button exposing
    ( Options
    , Type(..)
    , view
    )

import Html as H
import Html.Attributes as HA
import Html.Events as HE


type Type
    = Button
    | Submit


type alias Options msg =
    { type_ : Type
    , maybeOnClick : Maybe msg
    , text : String
    }


view : Options msg -> H.Html msg
view { type_, maybeOnClick, text } =
    H.button
        [ HA.class "button"
        , HA.type_ <|
            case type_ of
                Button ->
                    "button"

                Submit ->
                    "submit"
        , case maybeOnClick of
            Just onClick ->
                HE.onClick onClick

            Nothing ->
                HA.disabled True
        ]
        [ H.text text ]
