module CircleDrawer.Html.Attributes exposing (customProperties)


import Html as H
import Html.Attributes as HA


customProperties : List (String, String) -> H.Attribute msg
customProperties =
    List.map (\(name, value) -> "--" ++ name ++ ": " ++ value)
        >> String.join "; "
        >> HA.attribute "style"
