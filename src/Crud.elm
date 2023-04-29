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
    | ClickedDelete


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputPrefix prefix ->
            { model | prefix = prefix }

        InputId idAsString ->
            case String.toInt idAsString of
                Just id ->
                    { model | roster = Roster.select id model.roster }

                Nothing ->
                    model

        InputFirstName firstName ->
            { model | firstName = firstName }

        InputLastName lastName ->
            { model | lastName = lastName }

        ClickedCreate ->
            case Roster.add model.firstName model.lastName model.roster of
                Just roster ->
                    { model | roster = roster }

                Nothing ->
                    model

        ClickedUpdate ->
            { model
            | roster =
                Roster.update model.firstName model.lastName model.roster
            }

        ClickedDelete ->
            { model | roster = Roster.delete model.roster }


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
        , let
            isDisabled =
                Roster.selected roster == Nothing
          in
            H.div []
                [ H.button
                    [ HA.type_ "button"
                    , HE.onClick ClickedCreate
                    ]
                    [ H.text "Create" ]
                , H.button
                    [ HA.type_ "button"
                    , HA.disabled isDisabled
                    , HE.onClick ClickedUpdate
                    ]
                    [ H.text "Update" ]
                , H.button
                    [ HA.type_ "button"
                    , HA.disabled isDisabled
                    , HE.onClick ClickedDelete
                    ]
                    [ H.text "Delete" ]
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
