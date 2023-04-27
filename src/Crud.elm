module Crud exposing (Model, init, Msg, update, view)


import Crud.Person as Person exposing (Person)
import Crud.Roster as Roster exposing (Roster)
import Html as H
import Html.Attributes as HA
import Html.Events as HE


-- MODEL


type alias Model =
    { prefix : String
    , roster : Roster
    }


init : Model
init =
    { prefix = ""
    , roster =
        Roster.empty
            |> Roster.add "Hans" "Emil"
            |> Roster.add "Max" "Mustermann"
            |> Roster.add "Roman" "Tisch"
    }


-- UPDATE


type Msg
    = InputPrefix String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputPrefix prefix ->
            { model | prefix = prefix }


-- VIEW


view : Model -> H.Html Msg
view { prefix, roster } =
    H.div []
        [ H.div []
            [ H.text "Filter prefix: "
            , H.input
                [ HA.type_ "text"
                , HA.value prefix
                , HE.onInput InputPrefix
                ]
                []
            ]
        , H.div []
            [ H.div [] [ viewRoster prefix roster ]
            , H.div []
                [ H.div []
                    [ H.label [] [ H.text "Name: " ]
                    , H.input
                        [ HA.type_ "text" ]
                        []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Surname: " ]
                    , H.input
                        [ HA.type_ "text" ]
                        []
                    ]
                ]
            ]
        , H.div []
            [ H.button [ HA.type_ "button" ] [ H.text "Create" ]
            , H.button [ HA.type_ "button" ] [ H.text "Update" ]
            , H.button [ HA.type_ "button" ] [ H.text "Delete" ]
            ]
        ]


viewRoster : String -> Roster -> H.Html msg
viewRoster prefix roster =
    let
        people =
            Roster.filter prefix roster

        viewPerson person =
            H.option
                [ HA.value <| String.fromInt <| Person.toId person ]
                [ H.text <| Person.toString person ]
    in
    H.select [ HA.size 2 ] <|
        List.map viewPerson people
