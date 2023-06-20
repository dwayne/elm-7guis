module Task.TemperatureConverter.Temperature exposing
    ( Temperature
    , fromCelsius
    , fromFahrenheit
    , toCelsius
    , toFahrenheit
    )


type Temperature
    = Celsius Float
    | Fahrenheit Float


fromCelsius : String -> Maybe Temperature
fromCelsius =
    String.toFloat >> Maybe.andThen (fromFloat Celsius)


fromFahrenheit : String -> Maybe Temperature
fromFahrenheit =
    String.toFloat >> Maybe.andThen (fromFloat Fahrenheit)


fromFloat : (Float -> Temperature) -> Float -> Maybe Temperature
fromFloat toTemperature f =
    if isFinite f then
        Just <| toTemperature f

    else
        Nothing


toCelsius : Temperature -> Result String String
toCelsius t =
    case t of
        Celsius c ->
            Ok <| String.fromFloat c

        Fahrenheit f ->
            let
                c =
                    ftoc f
            in
            String.fromFloat c
                |> (if isFinite c then
                        Ok

                    else
                        Err
                   )


toFahrenheit : Temperature -> Result String String
toFahrenheit t =
    case t of
        Celsius c ->
            let
                f =
                    ctof c
            in
            String.fromFloat f
                |> (if isFinite f then
                        Ok

                    else
                        Err
                   )

        Fahrenheit f ->
            Ok <| String.fromFloat f


isFinite : Float -> Bool
isFinite f =
    not (isInfinite f || isNaN f)


ftoc : Float -> Float
ftoc f =
    (f - 32) * (5 / 9)


ctof : Float -> Float
ctof c =
    c * (9 / 5) + 32
