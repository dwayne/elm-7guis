module Task.CircleDrawer.Lib.Html.Attributes exposing (attrList, customProperties)

import Html as H
import Html.Attributes as HA


attrList : List ( H.Attribute msg, Bool ) -> List (H.Attribute msg)
attrList =
    List.filterMap
        (\( attr, keep ) ->
            if keep then
                Just attr

            else
                Nothing
        )


customProperties : List ( String, String ) -> H.Attribute msg
customProperties =
    List.map (\( name, value ) -> "--" ++ name ++ ": " ++ value)
        >> String.join "; "
        >> HA.attribute "style"
