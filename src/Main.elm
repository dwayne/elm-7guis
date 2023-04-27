module Main exposing (main)


import Browser
import Counter
import Crud
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
    , crud : Crud.Model
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
      , crud = Crud.init
      }
    , Cmd.map ChangedFlightBooker flightBookerCmd
    )


-- UPDATE


type Msg
    = ChangedCounter Counter.Msg
    | ChangedTemperatureConverter TemperatureConverter.Msg
    | ChangedFlightBooker FlightBooker.Msg
    | ChangedTimer Timer.Msg
    | ChangedCrud Crud.Msg


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

        ChangedCrud crudMsg ->
            ( { model | crud = Crud.update crudMsg model.crud }
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
        , model.counter
            |> Counter.view
            |> H.map ChangedCounter
            |> viewTask "Counter"
        , model.temperatureConverter
            |> TemperatureConverter.view
            |> H.map ChangedTemperatureConverter
            |> viewTask "Temperature Converter"
        , model.flightBooker
            |> FlightBooker.view
            |> H.map ChangedFlightBooker
            |> viewTask "Flight Booker"
        , model.timer
            |> Timer.view
            |> H.map ChangedTimer
            |> viewTask "Timer"
        , model.crud
            |> Crud.view
            |> H.map ChangedCrud
            |> viewTask "CRUD"
        ]


viewTask : String -> H.Html msg -> H.Html msg
viewTask name html =
    H.div []
        [ H.h2 [] [ H.text name ]
        , html
        ]
