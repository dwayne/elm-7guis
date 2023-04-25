module TemperatureConverter exposing (Model, init, Msg, update, view)


import Html as H
import Html.Attributes as HA
import Html.Events as HE
import TemperatureConverter.Temperature as Temperature


-- MODEL


type alias Model =
    { celsius : Field
    , fahrenheit : Field
    }


type Field
    = Initial String
    | Valid String
    | Invalid String


init : Model
init =
    { celsius = Initial ""
    , fahrenheit = Initial ""
    }


-- UPDATE


type Msg
    = InputCelsius String
    | InputFahrenheit String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputCelsius rawInput ->
            let
                cleanInput =
                    String.trim rawInput
            in
            if String.isEmpty cleanInput then
                { model | celsius = Initial rawInput }

            else
                case Temperature.fromCelsius cleanInput of
                    Just tempC ->
                        { model
                        | celsius = Valid rawInput
                        , fahrenheit =
                            case Temperature.toFahrenheit tempC of
                                Ok valueF ->
                                    Valid valueF

                                Err s ->
                                    Invalid s
                        }

                    Nothing ->
                        { model | celsius = Invalid rawInput }

        InputFahrenheit rawInput ->
            let
                cleanInput =
                    String.trim rawInput
            in
            if String.isEmpty cleanInput then
                { model | fahrenheit = Initial rawInput }

            else
                case Temperature.fromFahrenheit cleanInput of
                    Just tempF ->
                        { model
                        | celsius =
                            case Temperature.toCelsius tempF of
                                Ok valueC ->
                                    Valid valueC

                                Err s ->
                                    Invalid s
                        , fahrenheit = Valid rawInput
                        }

                    Nothing ->
                        { model | fahrenheit = Invalid rawInput }


-- VIEW


view : Model -> H.Html Msg
view { celsius, fahrenheit } =
    H.div []
        [ viewField celsius fahrenheit InputCelsius
        , H.label [] [ H.text "Celsius = " ]
        , viewField fahrenheit celsius InputFahrenheit
        , H.label [] [ H.text "Fahrenheit" ]
        ]


viewField : Field -> Field -> (String -> msg) -> H.Html msg
viewField primary secondary onInput =
    H.input
        [ HA.type_ "text"
        , HA.value <| getValue primary
        , HA.style "background-color" <| getBackgroundColor primary secondary
        , HE.onInput onInput
        ]
        []


getValue : Field -> String
getValue field =
    case field of
        Initial s ->
            s

        Valid s ->
            s

        Invalid s ->
            s


getBackgroundColor : Field -> Field -> String
getBackgroundColor primary secondary =
    case ( primary, secondary ) of
        ( Invalid _, _ ) ->
            "coral"

        ( _, Invalid _ ) ->
            "lightgray"

        _ ->
            "initial"
