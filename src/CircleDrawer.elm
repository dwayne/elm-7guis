module CircleDrawer exposing (Model, init, view, Msg, update)


import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import CircleDrawer.UndoManager as UndoManager


-- CONSTANTS


defaultDiameter : Int
defaultDiameter =
    30


-- MODEL


type alias Model =
    { id : Int
    , circles : List Circle
    , selectedId : Maybe Int
    , undoManager : UndoManager
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


type alias UndoManager =
    UndoManager.UndoManager Undo Redo


type Undo
    = RemoveCircle (List Circle)


type Redo
    = AddCircle (List Circle)


init : Model
init =
    { id = 0
    , circles = []
    , selectedId = Nothing
    , undoManager = UndoManager.empty
    }


-- UPDATE


type Msg
    = ClickedCanvas Position
    | MovedMouse Position
    | MouseLeftCanvas
    | ClickedUndo
    | ClickedRedo


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedCanvas position ->
            let
                circle =
                    Circle model.id position defaultDiameter

                circles =
                    circle :: model.circles
            in
            { model
            | id = model.id + 1
            , circles = circles
            , selectedId = Just model.id
            , undoManager =
                UndoManager.add
                    { undo = RemoveCircle model.circles
                    , redo = AddCircle circles
                    }
                    model.undoManager
            }

        MovedMouse position ->
            { model | selectedId = findClosestCircle position model.circles }

        MouseLeftCanvas ->
            { model | selectedId = Nothing }

        ClickedUndo ->
            model.undoManager
                |> UndoManager.undo
                |> Maybe.map
                    (\(undo, undoManager) ->
                        case undo of
                            RemoveCircle circles ->
                                { model
                                | circles = circles
                                , undoManager = undoManager
                                }
                    )
                |> Maybe.withDefault model

        ClickedRedo ->
            model.undoManager
                |> UndoManager.redo
                |> Maybe.map
                    (\(redo, undoManager) ->
                        case redo of
                            AddCircle circles ->
                                { model
                                | circles = circles
                                , undoManager = undoManager
                                }
                    )
                |> Maybe.withDefault model


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
view { circles, selectedId, undoManager } =
    H.div []
        [ viewUndoRedo undoManager
        , viewCanvas selectedId circles
        ]


viewUndoRedo : UndoManager -> H.Html Msg
viewUndoRedo undoManager =
    H.div []
        [ let
            isEnabled =
                UndoManager.canUndo undoManager

            isDisabled =
                not isEnabled

            attrs =
                attrList
                    [ ( HA.type_ "button", True )
                    , ( HA.disabled isDisabled, True )
                    , ( HE.onClick ClickedUndo, isEnabled )
                    ]
          in
          H.button attrs [ H.text "Undo" ]
        , let
            isEnabled =
                UndoManager.canRedo undoManager

            isDisabled =
                not isEnabled

            attrs =
                attrList
                    [ ( HA.type_ "button", True )
                    , ( HA.disabled isDisabled, True )
                    , ( HE.onClick ClickedRedo, isEnabled )
                    ]
          in
          H.button attrs [ H.text "Redo" ]
        ]


attrList : List (H.Attribute msg, Bool) -> List (H.Attribute msg)
attrList =
    List.filterMap
        (\(attr, keep) ->
            if keep then
                Just attr

            else
                Nothing
        )


viewCanvas : Maybe Int -> List Circle -> H.Html Msg
viewCanvas selectedId =
    List.reverse
        >> List.map (viewCircle selectedId)
        >> H.div
            [ HA.class "canvas"
            , onClick ClickedCanvas
            , onMouseMove MovedMouse
            , HE.onMouseLeave MouseLeftCanvas
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
