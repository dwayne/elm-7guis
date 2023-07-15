module Support.View.Frame exposing (Modifier(..), view)

import Html as H
import Html.Attributes as HA


type alias Options msg =
    { modifier : Modifier
    , title : String
    , body : H.Html msg
    }


type Modifier
    = Default
    | FlightBooker


view : Options msg -> H.Html msg
view { modifier, title, body } =
    H.div
        [ HA.class "frame"
        , HA.class <|
            case modifier of
                Default ->
                    ""

                FlightBooker ->
                    "frame--flight-booker"
        ]
        [ H.h2 [ HA.class "frame__title" ] [ H.text title ]
        , H.div [ HA.class "frame__body" ] [ body ]
        ]
