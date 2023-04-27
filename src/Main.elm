module Main exposing (main)


import Browser
import Counter
import FlightBooker
import Html as H
import Html.Attributes as HA
import TemperatureConverter
import Timer


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL


type alias Model =
    { counter : Counter.Model
    , temperatureConverter : TemperatureConverter.Model
    , flightBooker : FlightBooker.Model
    , timer : Timer.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( flightBooker, flightBookerCmd ) =
            FlightBooker.init
    in
    ( { counter = Counter.init
      , temperatureConverter = TemperatureConverter.init
      , flightBooker = flightBooker
      , timer = Timer.init
      }
    , Cmd.map ChangedFlightBooker flightBookerCmd
    )


-- UPDATE


type Msg
    = ChangedCounter Counter.Msg
    | ChangedTemperatureConverter TemperatureConverter.Msg
    | ChangedFlightBooker FlightBooker.Msg
    | ChangedTimer Timer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedCounter counterMsg ->
            ( { model | counter = Counter.update counterMsg model.counter }
            , Cmd.none
            )

        ChangedTemperatureConverter temperatureConverterMsg ->
            ( { model | temperatureConverter = TemperatureConverter.update temperatureConverterMsg model.temperatureConverter }
            , Cmd.none
            )

        ChangedFlightBooker flightBookerMsg ->
            let
                ( flightBooker, flightBookerCmd ) =
                    FlightBooker.update flightBookerMsg model.flightBooker
            in
            ( { model | flightBooker = flightBooker }
            , Cmd.map ChangedFlightBooker flightBookerCmd
            )

        ChangedTimer timerMsg ->
            ( { model | timer = Timer.update timerMsg model.timer }
            , Cmd.none
            )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { timer } =
    Timer.subscriptions timer
        |> Sub.map ChangedTimer


-- VIEW


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.h1 [] [ H.text "7GUIs in Elm" ]
        , H.p []
            [ H.text "This is a live version of an implementation ("
            , H.a
                [ HA.href "https://github.com/dwayne/elm-7guis"
                , HA.target "_blank"
                ]
                [ H.text "source" ]
            , H.text ") of "
            , H.a
                [ HA.href "https://eugenkiss.github.io/7guis/"
                , HA.target "_blank"
                ]
                [ H.text "7GUIs" ]
            , H.text " with "
            , H.a
                [ HA.href "https://elm-lang.org/"
                , HA.target "_blank"
                ]
                [ H.text "Elm" ]
            , H.text "."
            ]
        , viewCounter model.counter
        , viewTemperatureConverter model.temperatureConverter
        , viewFlightBooker model.flightBooker
        , viewTimer model.timer
        ]


viewCounter : Counter.Model -> H.Html Msg
viewCounter counter =
    H.div []
        [ H.h2 [] [ H.text "Counter" ]
        , Counter.view counter
            |> H.map ChangedCounter
        ]


viewTemperatureConverter : TemperatureConverter.Model -> H.Html Msg
viewTemperatureConverter temperatureConverter =
    H.div []
        [ H.h2 [] [ H.text "Temperature Converter" ]
        , TemperatureConverter.view temperatureConverter
            |> H.map ChangedTemperatureConverter
        ]


viewFlightBooker : FlightBooker.Model -> H.Html Msg
viewFlightBooker flightBooker =
    H.div []
        [ H.h2 [] [ H.text "Flight Booker" ]
        , FlightBooker.view flightBooker
            |> H.map ChangedFlightBooker
        ]


viewTimer : Timer.Model -> H.Html Msg
viewTimer timer =
    H.div []
        [ H.h2 [] [ H.text "Timer" ]
        , Timer.view timer
            |> H.map ChangedTimer
        ]
