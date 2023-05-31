module CircleDrawer exposing (Model, Msg, init, update, view)

import CircleDrawer.Dialog as Dialog
import CircleDrawer.Diameter as Diameter exposing (Diameter)
import CircleDrawer.Html.Attributes as HA
import CircleDrawer.Position exposing (Position)
import CircleDrawer.UndoManager as UndoManager
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD



-- CONSTANTS


defaultDiameter : Diameter
defaultDiameter =
    Diameter.fromSafeInt 30


dialogConfig : Dialog.Config Msg
dialogConfig =
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


type Selection
    = None
    | Hovered Int
    | Selected Int Position


selectionToId : Selection -> Maybe Int
selectionToId selection =
    case selection of
        None ->
            Nothing

        Hovered id ->
            Just id

        Selected id _ ->
            Just id


isSelected : Selection -> Bool
isSelected selection =
    case selection of
        Selected _ _ ->
            True

        _ ->
            False


mapSelection :
    { none : a
    , hovered : Int -> a
    , selected : Int -> Position -> a
    }
    -> Selection
    -> a
mapSelection { none, hovered, selected } selection =
    case selection of
        None ->
            none

        Hovered id ->
            hovered id

        Selected id position ->
            selected id position


mapSelected :
    { selected : Int -> Position -> a
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


type Redo
    = AddCircle (List Circle)


init : Model
init =
    { id = 0
    , circles = []
    , selection = None
    , undoManager = UndoManager.empty
    }



-- UPDATE


type Msg
    = ClickedCanvas Position
    | MovedMouse Position
    | MouseLeftCanvas
    | ClickedUndo
    | ClickedRedo
    | ClosedDialog
    | ChangedDialog Dialog.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCanvas position ->
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
                    \id ->
                        ( { model | selection = Selected id position }
                        , Dialog.open dialogConfig "menu"
                        )
                , selected = always <| always ( model, Cmd.none )
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

                Selected id position ->
                    ( False
                    , Just id
                    , Just
                        { htmlId = "menu"
                        , block = H.button [] [ H.text "Adjust Diameter" ]
                        , position = position
                        }
                    )
    in
    H.div []
        [ viewUndoRedo isEnabled undoManager
        , Dialog.view
            { viewport = viewCanvas activeId circles
            }
            dialogConfig
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
            , onClick ClickedCanvas
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
