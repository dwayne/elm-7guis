module Task.CircleDrawer exposing (Model, Msg, init, update, view)

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Support.View.Button as Button
import Support.View.Control as Control
import Support.View.Frame as Frame
import Task.CircleDrawer.Data.Circle as Circle exposing (Circle)
import Task.CircleDrawer.Data.Diameter as Diameter exposing (Diameter)
import Task.CircleDrawer.Data.Position as Position exposing (Position)
import Task.CircleDrawer.Data.UndoManager as UndoManager
import Task.CircleDrawer.Lib.Html.Attributes as HA
import Task.CircleDrawer.Lib.Html.Events as HE
import Task.CircleDrawer.View.Dialog as Dialog



-- MODEL


type alias Model =
    { id : Int
    , circles : List Circle
    , selection : Selection
    , undoManager : UndoManager
    }


type Selection
    = None
    | Hovered Int
    | Selected Int Position Mode


type Mode
    = Menu
    | AdjustDiameter (List Circle) Diameter


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


defaultDiameter : Diameter
defaultDiameter =
    Diameter.fromSafeInt 30


dialogHandlers : Dialog.Handlers Msg
dialogHandlers =
    { onClose = ClosedDialog
    , onChange = ChangedDialog
    }



-- UPDATE


type Msg
    = MainButtonClicked Position
    | SecondaryButtonClicked Position
    | MouseMoved Position
    | MouseLeft
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
        MainButtonClicked position ->
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

        SecondaryButtonClicked position ->
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

        MouseMoved position ->
            ( if isSelected model.selection then
                model

              else
                case Circle.findClosest position model.circles of
                    Just { id } ->
                        { model | selection = Hovered id }

                    Nothing ->
                        { model | selection = None }
            , Cmd.none
            )

        MouseLeft ->
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
                                case Circle.findById id model.circles of
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
                                    Button.view
                                        { type_ = Button.Button ClickedAdjustDiameter
                                        , text = "Adjust Diameter"
                                        }
                                , position = position
                                }

                        AdjustDiameter _ diameter ->
                            Just
                                { htmlId = "adjust-diameter"
                                , block =
                                    Frame.view
                                        { modifier = Frame.Default
                                        , title = "Adjust Diameter"
                                        , body =
                                            H.div [ HA.class "adjust-diameter" ]
                                                [ Control.viewLabel
                                                    { for = "diameter"
                                                    , text = "Adjust diameter of circle at " ++ Position.toString position ++ "."
                                                    }
                                                , H.input
                                                    [ HA.type_ "range"
                                                    , HA.step "1"
                                                    , HA.min <| Diameter.toString Diameter.min
                                                    , HA.max <| Diameter.toString Diameter.max
                                                    , HA.value <| Diameter.toString diameter
                                                    , HE.onInputDiameter InputDiameter
                                                    , HE.onMouseUp MouseUpAfterInputDiameter
                                                    ]
                                                    []
                                                ]
                                        }
                                , position = position
                                }
                    )
    in
    Frame.view
        { modifier = Frame.Default
        , title = "Circle Drawer"
        , body =
            H.div [ HA.class "circle-drawer" ]
                [ viewUndoRedo isEnabled undoManager
                , H.div [ HA.class "canvas" ]
                    [ Dialog.view
                        { viewport = viewCanvasLayer activeId circles
                        , handlers = dialogHandlers
                        , maybeDialog = maybeDialog
                        }
                    ]
                ]
        }


viewUndoRedo : Bool -> UndoManager -> H.Html Msg
viewUndoRedo isEnabled undoManager =
    H.div [ HA.class "circle-drawer__undoredo" ]
        [ viewButton
            { isEnabled = isEnabled && UndoManager.canUndo undoManager
            , onClick = ClickedUndo
            , text = "Undo"
            }
        , viewButton
            { isEnabled = isEnabled && UndoManager.canRedo undoManager
            , onClick = ClickedRedo
            , text = "Redo"
            }
        ]


viewButton :
    { isEnabled : Bool
    , onClick : msg
    , text : String
    }
    -> H.Html msg
viewButton { isEnabled, onClick, text } =
    Button.view
        { type_ =
            if isEnabled then
                Button.Button onClick

            else
                Button.Disabled
        , text = text
        }


viewCanvasLayer : Maybe Int -> List Circle -> H.Html Msg
viewCanvasLayer activeId =
    List.map (viewCircle activeId)
        >> List.reverse
        >> H.div
            [ HA.class "canvas__layer"
            , HE.onMainButtonClick MainButtonClicked
            , HE.onSecondaryButtonClick SecondaryButtonClicked
            , HE.onMouseMove MouseMoved
            , HE.onMouseLeave MouseLeft
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
