module Task.FlightBooker exposing (Model, Msg, init, update, view)

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Support.Lib as Lib
import Support.View.Button as Button
import Support.View.Control as Control
import Support.View.Frame as Frame
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


isBookable : Booking -> Bool
isBookable =
    withBooking
        { onOneWay = always True
        , onReturn = always (always True)
        , default = False
        }


withBooking :
    { onOneWay : Date -> a
    , onReturn : Date -> Date -> a
    , default : a
    }
    -> Booking
    -> a
withBooking { onOneWay, onReturn, default } { flight, start, end } =
    case ( flight, start, end ) of
        ( OneWay, Valid startDate, _ ) ->
            onOneWay startDate

        ( Return, Valid startDate, Valid endDate ) ->
            if endDate |> Date.isLaterThan startDate then
                onReturn startDate endDate

            else
                default

        _ ->
            default


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
    = FocusFlight
    | InputFlight Flight
    | InputStart String
    | InputEnd String
    | Submitted


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoadingMsg loadingMsg, Loading ) ->
            updateLoading loadingMsg

        ( LoadedMsg loadedMsg, Loaded booking ) ->
            updateLoaded loadedMsg booking

        _ ->
            ( model
            , Cmd.none
            )


updateLoading : LoadingMsg -> ( Model, Cmd Msg )
updateLoading msg =
    case msg of
        GotDate date ->
            ( Loaded
                { flight = OneWay
                , start = Valid date
                , end = Valid date
                }
            , Lib.focus "oneWayOrReturn" <| LoadedMsg FocusFlight
            )


updateLoaded : LoadedMsg -> Booking -> ( Model, Cmd msg )
updateLoaded msg booking =
    case msg of
        FocusFlight ->
            ( Loaded booking
            , Cmd.none
            )

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
            , withBooking
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
                Frame.view
                    { modifier = Frame.FlightBooker
                    , title = "Flight Booker"
                    , body =
                        H.form
                            [ HA.class "flight-booker"
                            , HE.onSubmit Submitted
                            ]
                            [ viewField
                                { id = "oneWayOrReturn"
                                , text = "One way or return:"
                                , toField = \id -> viewFlight id flight
                                }
                            , viewField
                                { id = "departure"
                                , text = "Departure date (format: DD.MM.YYYY):"
                                , toField = \id -> viewStart id start
                                }
                            , viewField
                                { id = "return"
                                , text = "Return date (format: DD.MM.YYYY):"
                                , toField = \id -> viewEnd id flight end
                                }
                            , viewSubmitButton booking
                            ]
                    }


viewField :
    { id : String
    , text : String
    , toField : String -> H.Html msg
    }
    -> H.Html msg
viewField { id, text, toField } =
    H.div [ HA.class "flight-booker__field" ]
        [ Control.viewLabel
            { for = id
            , text = text
            }
        , toField id
        ]


viewFlight : String -> Flight -> H.Html LoadedMsg
viewFlight id flight =
    let
        toOption current =
            ( flight == current
            , current
            )
    in
    Control.viewSelect
        { id = id
        , fromString = flightFromString
        , onInput = InputFlight
        , toValue = flightToValue
        , toText = flightToText
        , options = List.map toOption [ OneWay, Return ]
        }


flightFromString : String -> Maybe Flight
flightFromString s =
    case s of
        "one-way" ->
            Just OneWay

        "return" ->
            Just Return

        _ ->
            Nothing


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


viewStart : String -> Field -> H.Html LoadedMsg
viewStart id =
    viewInput id <| Just InputStart


viewEnd : String -> Flight -> Field -> H.Html LoadedMsg
viewEnd id flight =
    viewInput id <|
        case flight of
            OneWay ->
                Nothing

            Return ->
                Just InputEnd


viewInput : String -> Maybe (String -> msg) -> Field -> H.Html msg
viewInput id maybeOnInput field =
    let
        ( status, value ) =
            case field of
                Valid date ->
                    ( Control.Normal
                    , Date.toString date
                    )

                Invalid s ->
                    ( Control.HasError
                    , s
                    )
    in
    Control.viewInput
        { id = id
        , status = status
        , value = value
        , maybeOnInput = maybeOnInput
        }


viewSubmitButton : Booking -> H.Html msg
viewSubmitButton booking =
    Button.view
        { type_ =
            if isBookable booking then
                Button.Submit

            else
                Button.Disabled
        , text = "Book"
        }
