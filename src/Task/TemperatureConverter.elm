module Task.TemperatureConverter exposing (Model, Msg, init, update, view)

import Html as H
import Html.Attributes as HA
import Support.Lib as Lib
import Support.View.Control as Control
import Support.View.Frame as Frame
import Task.TemperatureConverter.Temperature as Temperature



-- MODEL


type alias Model =
    { celsius : Field
    , fahrenheit : Field
    }


type Field
    = Initial String
    | Valid String
    | Invalid String


init : ( Model, Cmd Msg )
init =
    ( { celsius = Initial ""
      , fahrenheit = Initial ""
      }
    , Lib.focus "celsius" FocusCelsius
    )



-- UPDATE


type Msg
    = FocusCelsius
    | InputCelsius String
    | InputFahrenheit String


update : Msg -> Model -> Model
update msg model =
    case msg of
        FocusCelsius ->
            model

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
    Frame.view "Temperature Converter" <|
        H.div [ HA.class "temperature-converter" ]
            [ viewTemperature
                { primary = celsius
                , secondary = fahrenheit
                , id = "celsius"
                , text = "°C"
                , onInput = InputCelsius
                }
            , viewEq
            , viewTemperature
                { primary = fahrenheit
                , secondary = celsius
                , id = "fahrenheit"
                , text = "°F"
                , onInput = InputFahrenheit
                }
            ]


viewTemperature :
    { primary : Field
    , secondary : Field
    , id : String
    , text : String
    , onInput : String -> msg
    }
    -> H.Html msg
viewTemperature { primary, secondary, id, text, onInput } =
    H.div [ HA.class "temperature-converter__temperature" ]
        [ Control.viewInput
            { id = id
            , status = getStatus primary secondary
            , value = getValue primary
            , maybeOnInput = Just onInput
            }
        , Control.viewLabel
            { for = id
            , text = text
            }
        ]


getStatus : Field -> Field -> Control.InputStatus
getStatus primary secondary =
    case ( primary, secondary ) of
        ( Invalid _, _ ) ->
            Control.HasError

        ( _, Invalid _ ) ->
            Control.HasWarning

        _ ->
            Control.Normal


getValue : Field -> String
getValue field =
    case field of
        Initial s ->
            s

        Valid s ->
            s

        Invalid s ->
            s


viewEq : H.Html msg
viewEq =
    H.span [ HA.class "temperature-converter__eq" ] [ H.text "=" ]
