module Task.CircleDrawer.Data.Circle exposing (Circle, findById, findClosest)

import Task.CircleDrawer.Data.Diameter as Diameter exposing (Diameter)
import Task.CircleDrawer.Data.Position exposing (Position)
import Task.CircleDrawer.Lib.List as List


type alias Circle =
    { id : Int
    , position : Position
    , diameter : Diameter
    }


findById : Int -> List Circle -> Maybe Circle
findById id =
    List.findBy (.id >> (==) id)


findClosest : Position -> List Circle -> Maybe Circle
findClosest position circles =
    findClosestHelper Nothing position circles


findClosestHelper : Maybe ( Circle, Float ) -> Position -> List Circle -> Maybe Circle
findClosestHelper closest position circles =
    case circles of
        [] ->
            Maybe.map Tuple.first closest

        circle :: restCircles ->
            let
                d =
                    distanceBetween position circle

                r =
                    Diameter.toFloat circle.diameter / 2

                newClosest =
                    if d > r then
                        closest

                    else
                        case closest of
                            Nothing ->
                                Just ( circle, d )

                            Just ( _, minD ) ->
                                if d < minD then
                                    Just ( circle, d )

                                else
                                    closest
            in
            findClosestHelper newClosest position restCircles


distanceBetween : Position -> Circle -> Float
distanceBetween { x, y } { position } =
    sqrt <| sqr (x - position.x) + sqr (y - position.y)


sqr : Int -> Float
sqr n =
    toFloat <| n * n
