module Support.Html.Attributes exposing (none)

import Html as H
import Html.Attributes as HA


none : H.Attribute msg
none =
    HA.classList []
