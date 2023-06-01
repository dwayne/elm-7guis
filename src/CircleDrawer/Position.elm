module CircleDrawer.Position exposing (Position, toString)


type alias Position =
    { x : Int
    , y : Int
    }


toString : Position -> String
toString { x, y } =
    "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"
