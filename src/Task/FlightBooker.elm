module Task.FlightBooker exposing (Model, Msg, init, update, view)

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Task.FlightBooker.Date as Date exposing (Date)
import Task.FlightBooker.Port as Port



-- MODEL


type Model
    = Loading
    | Loaded Booking


type alias Booking =
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
    = InputFlight Flight
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

        ( LoadedMsg loadedMsg, Loaded booking ) ->
            updateLoaded loadedMsg booking

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


updateLoaded : LoadedMsg -> Booking -> ( Model, Cmd msg )
updateLoaded msg booking =
    case msg of
        InputFlight flight ->
            ( Loaded { booking | flight = flight }
            , Cmd.none
            )

        InputStart rawInput ->
            ( Loaded
                { booking
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
            ( case booking.flight of
                OneWay ->
                    Loaded booking

                Return ->
                    Loaded
                        { booking
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
            ( Loaded booking
            , mapBooking
                { onOneWay =
                    \startDate ->
                        Port.alert <| "You have booked a one-way flight for " ++ Date.toString startDate ++ "."
                , onReturn =
                    \startDate endDate ->
                        Port.alert <| "You have booked a return flight from " ++ Date.toString startDate ++ " to " ++ Date.toString endDate ++ "."
                , default = Cmd.none
                }
                booking
            )



-- VIEW


view : Model -> H.Html Msg
view model =
    case model of
        Loading ->
            H.text "Loading..."

        Loaded ({ flight, start, end } as booking) ->
            H.map LoadedMsg <|
                H.form
                    [ HE.onSubmit Submitted ]
                    [ viewFlight flight
                    , viewStart start
                    , viewEnd flight end
                    , viewSubmitButton booking
                    ]


viewFlight : Flight -> H.Html LoadedMsg
viewFlight flight =
    H.select [ onFlightInput InputFlight ] <|
        List.map (viewFlightOption flight)
            [ OneWay
            , Return
            ]


onFlightInput : (Flight -> msg) -> H.Attribute msg
onFlightInput toMsg =
    let
        decoder =
            HE.targetValue
                |> JD.andThen
                    (\value ->
                        case value of
                            "one-way" ->
                                JD.succeed OneWay

                            "return" ->
                                JD.succeed Return

                            _ ->
                                JD.fail "ignored"
                    )
                |> JD.map toMsg
    in
    HE.on "input" decoder


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


viewStart : Field -> H.Html LoadedMsg
viewStart =
    viewInput <| Just InputStart


viewEnd : Flight -> Field -> H.Html LoadedMsg
viewEnd flight =
    viewInput <|
        case flight of
            OneWay ->
                Nothing

            Return ->
                Just InputEnd


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


viewSubmitButton : Booking -> H.Html msg
viewSubmitButton booking =
    let
        attrs =
            if isBookable booking then
                [ HA.type_ "submit"
                ]

            else
                [ HA.type_ "button"
                , HA.disabled True
                ]
    in
    H.button attrs [ H.text "Book" ]


isBookable : Booking -> Bool
isBookable =
    mapBooking
        { onOneWay = always True
        , onReturn =
            \startDate endDate ->
                endDate |> Date.isLaterThan startDate
        , default = False
        }


mapBooking :
    { onOneWay : Date -> a
    , onReturn : Date -> Date -> a
    , default : a
    }
    -> Booking
    -> a
mapBooking { onOneWay, onReturn, default } { flight, start, end } =
    case ( flight, start, end ) of
        ( OneWay, Valid startDate, _ ) ->
            onOneWay startDate

        ( Return, Valid startDate, Valid endDate ) ->
            onReturn startDate endDate

        _ ->
            default
