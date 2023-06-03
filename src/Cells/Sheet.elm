module Cells.Sheet exposing (view)

import Cells.Column as Column exposing (Column)
import Cells.Row as Row exposing (Row)
import Html as H
import Html.Attributes as HA


view : H.Html msg
view =
    H.div [ HA.class "sheet" ]
        [ H.table [ HA.class "sheet__table" ]
            [ viewColumnHeaders
            , viewRows
            ]
        ]


viewColumnHeaders : H.Html msg
viewColumnHeaders =
    let
        noTitle =
            H.th
                [ HA.class "sheet__th sheet__row-headers-column-header" ]
                []

        columns =
            Column.map viewColumnHeader
    in
    H.thead
        [ HA.class "sheet__thead" ]
        [ H.tr [ HA.class "sheet__tr" ] (noTitle :: columns) ]


viewColumnHeader : Column -> H.Html msg
viewColumnHeader column =
    H.th
        [ HA.class "sheet__th sheet__column-header" ]
        [ H.text <| Column.toString column ]


viewRows : H.Html msg
viewRows =
    H.tbody [ HA.class "sheet__tbody" ] <| Row.map viewRow


viewRow : Row -> H.Html msg
viewRow row =
    let
        header =
            H.th
                [ HA.class "sheet__th sheet__row-header" ]
                [ H.text <| Row.toString row ]
    in
    H.tr [ HA.class "sheet__tr" ] (header :: Column.map (viewCell row))


viewCell : Row -> Column -> H.Html msg
viewCell _ _ =
    H.td [ HA.class "sheet__td sheet__cell" ] []
