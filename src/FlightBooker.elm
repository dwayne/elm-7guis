module FlightBooker exposing (Model, Msg, init, update, view)

import FlightBooker.Date as Date exposing (Date)
import Html as H
import Html.Attributes as HA
import Html.Events as HE



-- MODEL


type Model
    = Loading
    | Loaded State


type alias State =
    { flight : Flight
    , start : Field
    , end : Field
    }


type Flight
    = OneWay
    | Return


type Field
    = Valid Date
    | Invalid String


init : ( Model, Cmd Msg )
init =
    ( Loading
    , Date.today <| LoadingMsg << GotDate
    )



-- UPDATE


type Msg
    = LoadingMsg LoadingMsg
    | LoadedMsg LoadedMsg


type LoadingMsg
    = GotDate Date


type LoadedMsg
    = InputFlight String
    | InputStart String
    | InputEnd String
    | Submitted


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( msg, model ) of
        ( LoadingMsg loadingMsg, Loading ) ->
            ( updateLoading loadingMsg
            , Cmd.none
            )

        ( LoadedMsg loadedMsg, Loaded state ) ->
            updateLoaded loadedMsg state

        _ ->
            ( model
            , Cmd.none
            )


updateLoading : LoadingMsg -> Model
updateLoading msg =
    case msg of
        GotDate date ->
            Loaded
                { flight = OneWay
                , start = Valid date
                , end = Valid date
                }


updateLoaded : LoadedMsg -> State -> ( Model, Cmd msg )
updateLoaded msg ({ flight, start, end } as state) =
    case msg of
        InputFlight value ->
            ( Loaded
                { state
                    | flight =
                        case value of
                            "one-way" ->
                                OneWay

                            "return" ->
                                Return

                            _ ->
                                flight
                }
            , Cmd.none
            )

        InputStart rawInput ->
            ( Loaded
                { state
                    | start =
                        case Date.fromString rawInput of
                            Just startDate ->
                                Valid startDate

                            Nothing ->
                                Invalid rawInput
                }
            , Cmd.none
            )

        InputEnd rawInput ->
            ( case state.flight of
                OneWay ->
                    Loaded state

                Return ->
                    Loaded
                        { state
                            | end =
                                case Date.fromString rawInput of
                                    Just endDate ->
                                        Valid endDate

                                    Nothing ->
                                        Invalid rawInput
                        }
            , Cmd.none
            )

        Submitted ->
            ( Loaded state
            , mapBookable
                --
                -- TODO: Tell JavaScript to show the message in an alert.
                --
                { onOneWay =
                    \startDate ->
                        Cmd.none
                            |> Debug.log ("You have booked a one-way flight for " ++ Date.toString startDate ++ ".")
                , onReturn =
                    \startDate endDate ->
                        Cmd.none
                            |> Debug.log ("You have booked a return flight from " ++ Date.toString startDate ++ " to " ++ Date.toString endDate ++ ".")
                , default = Cmd.none
                }
                flight
                start
                end
            )



-- VIEW


view : Model -> H.Html Msg
view model =
    case model of
        Loading ->
            H.text "Loading..."

        Loaded { flight, start, end } ->
            H.form
                [ HE.onSubmit <| LoadedMsg Submitted ]
                [ viewFlight flight
                , viewStart start
                , viewEnd flight end
                , viewSubmitButton flight start end
                ]


viewFlight : Flight -> H.Html Msg
viewFlight flight =
    H.select [ HE.onInput <| LoadedMsg << InputFlight ] <|
        List.map (viewFlightOption flight)
            [ OneWay
            , Return
            ]


viewFlightOption : Flight -> Flight -> H.Html msg
viewFlightOption selected current =
    H.option
        [ HA.value <| flightToValue current
        , HA.selected <| selected == current
        ]
        [ H.text <| flightToText current ]


flightToValue : Flight -> String
flightToValue flight =
    case flight of
        OneWay ->
            "one-way"

        Return ->
            "return"


flightToText : Flight -> String
flightToText flight =
    case flight of
        OneWay ->
            "one-way flight"

        Return ->
            "return flight"


viewStart : Field -> H.Html Msg
viewStart =
    viewInput <| Just <| LoadedMsg << InputStart


viewEnd : Flight -> Field -> H.Html Msg
viewEnd flight =
    viewInput <|
        case flight of
            OneWay ->
                Nothing

            Return ->
                Just <| LoadedMsg << InputEnd


viewInput : Maybe (String -> msg) -> Field -> H.Html msg
viewInput maybeOnInput field =
    let
        ( value, backgroundColor ) =
            case field of
                Valid date ->
                    ( Date.toString date
                    , "initial"
                    )

                Invalid s ->
                    ( s
                    , "coral"
                    )
    in
    case maybeOnInput of
        Just onInput ->
            H.input
                [ HA.type_ "text"
                , HA.value value
                , HA.style "background-color" backgroundColor
                , HE.onInput onInput
                ]
                []

        Nothing ->
            H.input
                [ HA.type_ "text"
                , HA.value value
                , HA.disabled True
                ]
                []


viewSubmitButton : Flight -> Field -> Field -> H.Html Msg
viewSubmitButton flight start end =
    let
        attrs =
            if isBookable flight start end then
                [ HA.type_ "submit"
                ]

            else
                [ HA.type_ "button"
                , HA.disabled True
                ]
    in
    H.button attrs [ H.text "Book" ]


isBookable : Flight -> Field -> Field -> Bool
isBookable =
    mapBookable
        { onOneWay = always True
        , onReturn =
            \startDate endDate ->
                endDate |> Date.isLaterThan startDate
        , default = False
        }


mapBookable :
    { onOneWay : Date -> a
    , onReturn : Date -> Date -> a
    , default : a
    }
    -> Flight
    -> Field
    -> Field
    -> a
mapBookable { onOneWay, onReturn, default } flight start end =
    case ( flight, start, end ) of
        ( OneWay, Valid startDate, _ ) ->
            onOneWay startDate

        ( Return, Valid startDate, Valid endDate ) ->
            onReturn startDate endDate

        _ ->
            default
