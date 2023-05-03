module CircleDrawer exposing (Model, init, view, Msg, update)


import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD


-- CONSTANTS


defaultDiameter : Int
defaultDiameter =
    30


-- MODEL


type alias Model =
    { id : Int
    , circles : List Circle
    , selectedId : Maybe Int
    }


type alias Circle =
    { id : Int
    , position : Position
    , diameter : Int
    }


type alias Position =
    { x : Int
    , y : Int
    }


init : Model
init =
    { id = 0
    , circles = []
    , selectedId = Nothing
    }


-- UPDATE


type Msg
    = ClickedCanvas Position
    | MovedMouse Position


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedCanvas position ->
            let
                circle =
                    Circle model.id position defaultDiameter
            in
            { model
            | id = model.id + 1
            , circles = circle :: model.circles
            , selectedId = Just model.id
            }

        MovedMouse position ->
            { model | selectedId = findClosestCircle position model.circles }


findClosestCircle : Position -> List Circle -> Maybe Int
findClosestCircle position circles =
    findClosestCircleHelper Nothing position circles


findClosestCircleHelper : Maybe (Int, Float) -> Position -> List Circle -> Maybe Int
findClosestCircleHelper closest position circles =
    case circles of
        [] ->
            Maybe.map Tuple.first closest

        circle :: restCircles ->
            let
                d =
                    distanceBetween position circle

                r =
                    toFloat circle.diameter / 2

                newClosest =
                    if d > r then
                        closest

                    else
                        case closest of
                            Nothing ->
                                Just (circle.id, d)

                            Just (minId, minD) ->
                                if d < minD then
                                    Just (circle.id, d)

                                else
                                    closest
            in
            findClosestCircleHelper newClosest position restCircles


distanceBetween : Position -> Circle -> Float
distanceBetween { x, y } { position } =
    sqrt <| sqr (x - position.x) + sqr (y - position.y)


sqr : Int -> Float
sqr n =
    toFloat <| n * n


-- VIEW


view : Model -> H.Html Msg
view { circles, selectedId } =
    H.div []
        [ H.div []
            [ H.button [ HA.type_ "button" ] [ H.text "Undo" ]
            , H.button [ HA.type_ "button" ] [ H.text "Redo" ]
            ]
        , circles
            |> List.reverse
            |> List.map (viewCircle selectedId)
            |> H.div
                [ HA.class "canvas"
                , onClick ClickedCanvas
                , onMouseMove MovedMouse
                ]
        ]


viewCircle : Maybe Int -> Circle -> H.Html msg
viewCircle selectedId { id, position, diameter } =
    H.div
        [ HA.classList
            [ ( "circle", True )
            , ( "circle--selected", Just id == selectedId )
            ]
        , customProperties
            [ ( "circle-x", String.fromInt position.x ++ "px" )
            , ( "circle-y", String.fromInt position.y ++ "px" )
            , ( "circle-diameter", String.fromInt diameter ++ "px" )
            ]
        ]
        []


customProperties : List (String, String) -> H.Attribute msg
customProperties =
    HA.attribute "style"
        << String.join "; "
        << List.map (\(name, value) -> "--" ++ name ++ ": " ++ value)


onClick : (Position -> msg) -> H.Attribute msg
onClick toMsg =
    HE.on "click" (JD.map toMsg positionDecoder)


onMouseMove : (Position -> msg) -> H.Attribute msg
onMouseMove toMsg =
    HE.on "mousemove" (JD.map toMsg positionDecoder)


positionDecoder : JD.Decoder Position
positionDecoder =
    JD.map4
        (\pageX pageY offsetLeft offsetTop ->
            let
                x =
                    pageX - offsetLeft

                y =
                    pageY - offsetTop
            in
            Position x y
        )
        (JD.field "pageX" JD.int)
        (JD.field "pageY" JD.int)
        (JD.at [ "currentTarget", "offsetLeft" ] JD.int)
        (JD.at [ "currentTarget", "offsetTop" ] JD.int)
