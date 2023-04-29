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
    , firstName : String
    , lastName : String
    }


init : Model
init =
    { prefix = ""
    , roster =
        Roster.fromList
            [ ("Hans", "Emil")
            , ("Max", "Mustermann")
            , ("Roman", "Tisch")
            ]
    , firstName = ""
    , lastName = ""
    }


-- UPDATE


type Msg
    = InputPrefix String
    | InputId String
    | InputFirstName String
    | InputLastName String
    | ClickedCreate
    | ClickedUpdate


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputPrefix prefix ->
            { model | prefix = prefix }

        InputId idAsString ->
            case String.toInt idAsString of
                Just id ->
                    let
                        roster =
                            Roster.select id model.roster
                    in
                    case Roster.selected roster of
                        Just person ->
                            let
                                ( firstName, lastName ) =
                                    ( Person.toFirstName person
                                    , Person.toLastName person
                                    )
                            in
                            { model
                            | roster = roster
                            , firstName = firstName
                            , lastName = lastName
                            }

                        Nothing ->
                            model

                Nothing ->
                    model

        InputFirstName firstName ->
            { model | firstName = firstName }

        InputLastName lastName ->
            { model | lastName = lastName }

        ClickedCreate ->
            model.roster
                |> Roster.add model.firstName model.lastName
                |> Maybe.map
                    (\roster ->
                        { model
                        | roster = roster
                        , firstName = ""
                        , lastName = ""
                        }
                    )
                |> Maybe.withDefault model

        ClickedUpdate ->
            { model
            | roster =
                Roster.updateSelected
                    model.firstName
                    model.lastName
                    model.roster
            }


-- VIEW


view : Model -> H.Html Msg
view { prefix, roster, firstName, lastName } =
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
                        [ HA.type_ "text"
                        , HA.value firstName
                        , HE.onInput InputFirstName
                        ]
                        []
                    ]
                , H.div []
                    [ H.label [] [ H.text "Surname: " ]
                    , H.input
                        [ HA.type_ "text"
                        , HA.value lastName
                        , HE.onInput InputLastName
                        ]
                        []
                    ]
                ]
            ]
        , H.div []
            [ H.button
                [ HA.type_ "button"
                , HE.onClick ClickedCreate
                ]
                [ H.text "Create" ]
            , H.button
                [ HA.type_ "button"
                , HE.onClick ClickedUpdate
                ]
                [ H.text "Update" ]
            , H.button [ HA.type_ "button" ] [ H.text "Delete" ]
            ]
        ]


viewRoster : String -> Roster -> H.Html Msg
viewRoster prefix roster =
    let
        people =
            Roster.filter prefix roster

        viewPerson (isSelected, person) =
            H.option
                [ HA.value <| String.fromInt <| Person.toId person
                , HA.selected isSelected
                ]
                [ H.text <| Person.toString person ]
    in
    H.select [ HA.size 2, HE.onInput InputId ] <|
        List.map viewPerson people
