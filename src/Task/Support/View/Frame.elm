module Task.Support.View.Frame exposing (view)

import Html as H
import Html.Attributes as HA


view : String -> H.Html msg -> H.Html msg
view title body =
    H.div [ HA.class "frame" ]
        [ H.h2 [ HA.class "frame__title" ] [ H.text title ]
        , H.div [ HA.class "frame__body" ] [ body ]
        ]
