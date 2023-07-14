module Support.View.Control exposing
    ( InputStatus(..)
    , ViewInputOptions
    , ViewLabelOptions
    , viewInput
    , viewLabel
    )

import Html as H
import Html.Attributes as HA
import Html.Events as HE


type alias ViewLabelOptions =
    { for : String
    , text : String
    }


viewLabel : ViewLabelOptions -> H.Html msg
viewLabel { for, text } =
    H.label [ HA.for for ] [ H.text text ]


type alias ViewInputOptions msg =
    { id : String
    , status : InputStatus
    , value : String
    , maybeOnInput : Maybe (String -> msg)
    }


type InputStatus
    = Normal
    | HasWarning
    | HasError


viewInput : ViewInputOptions msg -> H.Html msg
viewInput { id, status, value, maybeOnInput } =
    H.input
        [ HA.type_ "text"
        , HA.id id
        , HA.class "input"
        , HA.class <|
            case status of
                Normal ->
                    ""

                HasWarning ->
                    "input--has-warning"

                HasError ->
                    "input--has-error"
        , HA.value value
        , case maybeOnInput of
            Just onInput ->
                HE.onInput onInput

            Nothing ->
                HA.disabled True
        ]
        []
