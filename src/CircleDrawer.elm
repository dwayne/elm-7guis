module CircleDrawer exposing (Model, init, view, Msg, update)


import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD


-- MODEL


type alias Model =
    ()


init : Model
init =
    ()


-- UPDATE


type Msg
    = ClickedCanvas Position


type alias Position =
    { x : Int
    , y : Int
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedCanvas { x, y } ->
            model
                |> Debug.log ("x = " ++ Debug.toString x ++ ", y = " ++ Debug.toString y)


-- VIEW


view : Model -> H.Html Msg
view _ =
    H.div []
        [ H.div []
            [ H.button [ HA.type_ "button" ] [ H.text "Undo" ]
            , H.button [ HA.type_ "button" ] [ H.text "Redo" ]
            ]
        , H.div
            [ HA.class "canvas"
            , onClick ClickedCanvas
            ]
            []
        ]


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
