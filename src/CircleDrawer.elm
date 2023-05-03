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


-- VIEW


view : Model -> H.Html Msg
view { circles, selectedId } =
    H.div []
        [ H.div []
            [ H.button [ HA.type_ "button" ] [ H.text "Undo" ]
            , H.button [ HA.type_ "button" ] [ H.text "Redo" ]
            ]
        , H.div
            [ HA.class "canvas"
            , onClick ClickedCanvas
            ]
            <| List.map (viewCircle selectedId) circles
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
    let
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
                (JD.at [ "target", "offsetLeft" ] JD.int)
                (JD.at [ "target", "offsetTop" ] JD.int)
    in
    HE.on "click" (JD.map toMsg positionDecoder)
