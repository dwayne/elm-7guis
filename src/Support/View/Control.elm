module Support.View.Control exposing
    ( InputStatus(..)
    , ViewInputOptions
    , ViewLabelOptions
    , ViewSelectOptions
    , viewInput
    , viewLabel
    , viewSelect
    )

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD


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


type alias ViewSelectOptions option msg =
    { id : String
    , fromString : String -> Maybe option
    , onInput : option -> msg
    , toValue : option -> String
    , toText : option -> String
    , options : List ( Bool, option )
    }


viewSelect : ViewSelectOptions option msg -> H.Html msg
viewSelect { id, fromString, onInput, toValue, toText, options } =
    let
        decoder =
            HE.targetValue
                |> JD.andThen
                    (\value ->
                        case fromString value of
                            Just option ->
                                JD.succeed option

                            Nothing ->
                                JD.fail "ignored"
                    )
                |> JD.map onInput

        toOption ( isSelected, option ) =
            H.option
                [ HA.value <| toValue option
                , HA.selected isSelected
                ]
                [ H.text <| toText option ]
    in
    H.select
        [ HA.id id
        , HA.class "select"
        , HE.on "input" decoder
        ]
    <|
        List.map toOption options
