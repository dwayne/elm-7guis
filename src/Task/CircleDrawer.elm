module Task.CircleDrawer exposing (Model, Msg, init, update, view)

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Task.CircleDrawer.Data.Diameter as Diameter exposing (Diameter)
import Task.CircleDrawer.Data.Position as Position exposing (Position)
import Task.CircleDrawer.Data.UndoManager as UndoManager
import Task.CircleDrawer.Lib.Html.Attributes as HA
import Task.CircleDrawer.View.Dialog as Dialog



-- CONSTANTS


defaultDiameter : Diameter
defaultDiameter =
    Diameter.fromSafeInt 30


dialogHandlers : Dialog.Handlers Msg
dialogHandlers =
    { onClose = ClosedDialog
    , onChange = ChangedDialog
    }



-- MODEL


type alias Model =
    { id : Int
    , circles : List Circle
    , selection : Selection
    , undoManager : UndoManager
    }


type alias Circle =
    { id : Int
    , position : Position
    , diameter : Diameter
    }


findCircleById : Int -> List Circle -> Maybe Circle
findCircleById id circles =
    case circles of
        [] ->
            Nothing

        circle :: restCircles ->
            if circle.id == id then
                Just circle

            else
                findCircleById id restCircles


type Selection
    = None
    | Hovered Int
    | Selected Int Position Mode


type Mode
    = Menu
    | AdjustDiameter (List Circle) Diameter


selectionToId : Selection -> Maybe Int
selectionToId selection =
    case selection of
        None ->
            Nothing

        Hovered id ->
            Just id

        Selected id _ _ ->
            Just id


isSelected : Selection -> Bool
isSelected selection =
    case selection of
        Selected _ _ _ ->
            True

        _ ->
            False


mapSelection :
    { none : a
    , hovered : Int -> a
    , selected : Int -> Position -> Mode -> a
    }
    -> Selection
    -> a
mapSelection { none, hovered, selected } selection =
    case selection of
        None ->
            none

        Hovered id ->
            hovered id

        Selected id position mode ->
            selected id position mode


mapSelected :
    { selected : Int -> Position -> Mode -> a
    , other : a
    }
    -> Selection
    -> a
mapSelected { selected, other } =
    mapSelection
        { none = other
        , hovered = always other
        , selected = selected
        }


type alias UndoManager =
    UndoManager.UndoManager Undo Redo


type Undo
    = RemoveCircle (List Circle)
    | UndoAdjustDiameter (List Circle)


type Redo
    = AddCircle (List Circle)
    | RedoAdjustDiameter (List Circle)


init : Model
init =
    { id = 0
    , circles = []
    , selection = None
    , undoManager = UndoManager.empty
    }



-- UPDATE


type Msg
    = MainButtonClickedCanvas Position
    | SecondaryButtonClickedCanvas Position
    | MovedMouse Position
    | MouseLeftCanvas
    | ClickedAdjustDiameter
    | InputDiameter Diameter
    | MouseUpAfterInputDiameter
    | ClickedUndo
    | ClickedRedo
    | ClosedDialog
    | ChangedDialog Dialog.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MainButtonClickedCanvas position ->
            mapSelection
                { none =
                    let
                        circle =
                            Circle model.id position defaultDiameter

                        circles =
                            circle :: model.circles
                    in
                    ( { model
                        | id = model.id + 1
                        , circles = circles
                        , selection = Hovered model.id
                        , undoManager =
                            UndoManager.add
                                { undo = RemoveCircle model.circles
                                , redo = AddCircle circles
                                }
                                model.undoManager
                      }
                    , Cmd.none
                    )
                , hovered =
                    \_ ->
                        ( model
                        , Cmd.none
                        )
                , selected =
                    \_ _ _ ->
                        ( model
                        , Cmd.none
                        )
                }
                model.selection

        SecondaryButtonClickedCanvas position ->
            mapSelection
                { none =
                    ( model
                    , Cmd.none
                    )
                , hovered =
                    \id ->
                        ( { model | selection = Selected id position Menu }
                        , Dialog.open "menu" dialogHandlers
                        )
                , selected =
                    \_ _ _ ->
                        ( model
                        , Cmd.none
                        )
                }
                model.selection

        MovedMouse position ->
            ( if isSelected model.selection then
                model

              else
                case findClosestCircle position model.circles of
                    Just id ->
                        { model | selection = Hovered id }

                    Nothing ->
                        { model | selection = None }
            , Cmd.none
            )

        MouseLeftCanvas ->
            ( if isSelected model.selection then
                model

              else
                { model | selection = None }
            , Cmd.none
            )

        ClickedAdjustDiameter ->
            ( mapSelected
                { selected =
                    \id position mode ->
                        case mode of
                            Menu ->
                                case findCircleById id model.circles of
                                    Just { diameter } ->
                                        { model
                                            | selection = Selected id position (AdjustDiameter model.circles diameter)
                                        }

                                    Nothing ->
                                        model

                            _ ->
                                model
                , other = model
                }
                model.selection
            , Cmd.none
            )

        InputDiameter newDiameter ->
            ( mapSelected
                { selected =
                    \id position mode ->
                        case mode of
                            AdjustDiameter initialCircles _ ->
                                { model
                                    | circles =
                                        List.map
                                            (\circle ->
                                                if circle.id == id then
                                                    { circle | diameter = newDiameter }

                                                else
                                                    circle
                                            )
                                            model.circles
                                    , selection = Selected id position (AdjustDiameter initialCircles newDiameter)
                                }

                            _ ->
                                model
                , other = model
                }
                model.selection
            , Cmd.none
            )

        MouseUpAfterInputDiameter ->
            ( mapSelected
                { selected =
                    \id position mode ->
                        case mode of
                            AdjustDiameter initialCircles finalDiameter ->
                                { model
                                    | selection = Selected id position (AdjustDiameter model.circles finalDiameter)
                                    , undoManager =
                                        UndoManager.add
                                            { undo = UndoAdjustDiameter initialCircles
                                            , redo = RedoAdjustDiameter model.circles
                                            }
                                            model.undoManager
                                }

                            _ ->
                                model
                , other = model
                }
                model.selection
            , Cmd.none
            )

        ClickedUndo ->
            ( model.undoManager
                |> UndoManager.undo
                |> Maybe.map
                    (\( undo, undoManager ) ->
                        case undo of
                            RemoveCircle circles ->
                                { model
                                    | circles = circles
                                    , undoManager = undoManager
                                }

                            UndoAdjustDiameter circles ->
                                { model
                                    | circles = circles
                                    , undoManager = undoManager
                                }
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )

        ClickedRedo ->
            ( model.undoManager
                |> UndoManager.redo
                |> Maybe.map
                    (\( redo, undoManager ) ->
                        case redo of
                            AddCircle circles ->
                                { model
                                    | circles = circles
                                    , undoManager = undoManager
                                }

                            RedoAdjustDiameter circles ->
                                { model
                                    | circles = circles
                                    , undoManager = undoManager
                                }
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )

        ClosedDialog ->
            ( { model | selection = None }
            , Cmd.none
            )

        ChangedDialog dialogMsg ->
            ( model
            , Dialog.update dialogMsg
            )


findClosestCircle : Position -> List Circle -> Maybe Int
findClosestCircle position circles =
    findClosestCircleHelper Nothing position circles


findClosestCircleHelper : Maybe ( Int, Float ) -> Position -> List Circle -> Maybe Int
findClosestCircleHelper closest position circles =
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
                                Just ( circle.id, d )

                            Just ( minId, minD ) ->
                                if d < minD then
                                    Just ( circle.id, d )

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
view { circles, selection, undoManager } =
    let
        ( isEnabled, activeId, maybeDialog ) =
            case selection of
                None ->
                    ( True
                    , Nothing
                    , Nothing
                    )

                Hovered id ->
                    ( True
                    , Just id
                    , Nothing
                    )

                Selected id position mode ->
                    ( False
                    , Just id
                    , case mode of
                        Menu ->
                            Just
                                { htmlId = "menu"
                                , block =
                                    H.button
                                        [ HE.onClick ClickedAdjustDiameter ]
                                        [ H.text "Adjust Diameter" ]
                                , position = position
                                }

                        AdjustDiameter _ diameter ->
                            Just
                                { htmlId = "adjustDiameter"
                                , block =
                                    H.div []
                                        [ H.p [] [ H.text <| "Adjust Diameter of circle at " ++ Position.toString position ]
                                        , H.input
                                            [ HA.type_ "range"
                                            , HA.min <| Diameter.toString Diameter.min
                                            , HA.max <| Diameter.toString Diameter.max
                                            , HA.value <| Diameter.toString diameter
                                            , onInputDiameter InputDiameter
                                            , HE.onMouseUp MouseUpAfterInputDiameter
                                            ]
                                            []
                                        ]
                                , position = position
                                }
                    )
    in
    H.div []
        [ viewUndoRedo isEnabled undoManager
        , Dialog.view
            { viewport = viewCanvas activeId circles
            , handlers = dialogHandlers
            }
            maybeDialog
        ]


viewUndoRedo : Bool -> UndoManager -> H.Html Msg
viewUndoRedo isEnabled undoManager =
    H.div []
        [ viewButton
            { isEnabled = isEnabled && UndoManager.canUndo undoManager
            , text = "Undo"
            , onClick = ClickedUndo
            }
        , viewButton
            { isEnabled = isEnabled && UndoManager.canRedo undoManager
            , text = "Redo"
            , onClick = ClickedRedo
            }
        ]


viewButton :
    { isEnabled : Bool
    , text : String
    , onClick : msg
    }
    -> H.Html msg
viewButton options =
    let
        isDisabled =
            not options.isEnabled

        attrs =
            attrList
                [ ( HA.type_ "button", True )
                , ( HA.disabled isDisabled, True )
                , ( HE.onClick options.onClick, options.isEnabled )
                ]
    in
    H.button attrs [ H.text options.text ]


attrList : List ( H.Attribute msg, Bool ) -> List (H.Attribute msg)
attrList =
    List.filterMap
        (\( attr, keep ) ->
            if keep then
                Just attr

            else
                Nothing
        )


viewCanvas : Maybe Int -> List Circle -> H.Html Msg
viewCanvas activeId =
    List.reverse
        >> List.map (viewCircle activeId)
        >> H.div
            [ HA.class "canvas"
            , onMainButtonClick MainButtonClickedCanvas
            , onSecondaryButtonClick SecondaryButtonClickedCanvas
            , onMouseMove MovedMouse
            , HE.onMouseLeave MouseLeftCanvas
            ]


viewCircle : Maybe Int -> Circle -> H.Html msg
viewCircle activeId { id, position, diameter } =
    H.div
        [ HA.classList
            [ ( "circle", True )
            , ( "circle--selected", Just id == activeId )
            ]
        , HA.customProperties
            [ ( "circle-x", String.fromInt position.x ++ "px" )
            , ( "circle-y", String.fromInt position.y ++ "px" )
            , ( "circle-diameter", Diameter.toString diameter ++ "px" )
            ]
        ]
        []


onMainButtonClick : (Position -> msg) -> H.Attribute msg
onMainButtonClick toMsg =
    let
        decoder =
            JD.field "button" JD.int
                |> JD.andThen
                    (\button ->
                        if button == 0 then
                            JD.map toMsg positionDecoder

                        else
                            JD.fail "unknown click"
                    )
    in
    HE.on "click" decoder


onSecondaryButtonClick : (Position -> msg) -> H.Attribute msg
onSecondaryButtonClick toMsg =
    let
        decoder =
            JD.field "button" JD.int
                |> JD.andThen
                    (\button ->
                        if button == 2 then
                            JD.map
                                (\position -> ( toMsg position, True ))
                                positionDecoder

                        else
                            JD.fail "unknown click"
                    )
    in
    HE.preventDefaultOn "contextmenu" decoder


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


onInputDiameter : (Diameter -> msg) -> H.Attribute msg
onInputDiameter toMsg =
    let
        diameterDecoder =
            JD.at [ "target", "value" ] JD.string
                |> JD.andThen
                    (\s ->
                        String.toInt s
                            |> Maybe.andThen Diameter.fromInt
                            |> Maybe.map JD.succeed
                            |> Maybe.withDefault (JD.fail "invalid diameter")
                    )
    in
    HE.on "input" (JD.map toMsg diameterDecoder)
