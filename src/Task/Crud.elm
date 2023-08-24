module Task.Crud exposing (Model, Msg, init, update, view)

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Support.Lib as Lib
import Support.View.Button as Button
import Support.View.Control as Control
import Support.View.Frame as Frame
import Task.Crud.Person as Person
import Task.Crud.Roster as Roster exposing (Roster)



-- MODEL


type alias Model =
    { prefix : String
    , roster : Roster
    , firstName : String
    , lastName : String
    }


init : ( Model, Cmd Msg )
init =
    ( { prefix = ""
      , roster =
            Roster.fromList
                [ ( "Evan", "Czaplicki" )
                , ( "Jeroen", "Engels" )
                , ( "Richard", "Feldman" )
                , ( "Matthew", "Griffith" )
                , ( "Dillon", "Kearns" )
                , ( "Tessa", "Kelly" )
                , ( "Simon", "Lydell" )
                , ( "Wolfgang", "Schuster" )
                , ( "Jared", "Smith" )
                , ( "Tereza", "Sokol" )
                ]
      , firstName = ""
      , lastName = ""
      }
    , Lib.focus "name" FocusName
    )



-- UPDATE


type Msg
    = FocusName
    | InputPrefix String
    | InputId String
    | InputFirstName String
    | InputLastName String
    | ClickedCreate
    | ClickedUpdate
    | ClickedDelete


update : Msg -> Model -> Model
update msg model =
    case msg of
        FocusName ->
            model

        InputPrefix prefix ->
            { model | prefix = prefix }

        InputId idAsString ->
            String.toInt idAsString
                |> Maybe.andThen (\id -> Roster.select id model.roster)
                |> Maybe.map
                    (\( person, roster ) ->
                        let
                            { firstName, lastName } =
                                Person.getFirstAndLastName person
                        in
                        { model
                            | roster = roster
                            , firstName = firstName
                            , lastName = lastName
                        }
                    )
                |> Maybe.withDefault model

        InputFirstName firstName ->
            { model | firstName = firstName }

        InputLastName lastName ->
            { model | lastName = lastName }

        ClickedCreate ->
            Roster.add model.firstName model.lastName model.roster
                |> Maybe.map (clear model)
                |> Maybe.withDefault model

        ClickedUpdate ->
            Roster.update model.firstName model.lastName model.roster
                |> Maybe.map (clear model)
                |> Maybe.withDefault model

        ClickedDelete ->
            { model
                | roster = Roster.delete model.roster
                , firstName = ""
                , lastName = ""
            }


clear : Model -> Roster -> Model
clear model roster =
    { model
        | roster = Roster.deselect roster
        , firstName = ""
        , lastName = ""
    }



-- VIEW


view : Model -> H.Html Msg
view { prefix, roster, firstName, lastName } =
    Frame.view
        { modifier = Frame.Default
        , title = "CRUD"
        , body =
            H.div [ HA.class "crud" ]
                [ H.div [ HA.class "crud__filter-area" ]
                    [ H.div [ HA.class "crud__filter" ]
                        [ Control.viewLabel
                            { for = "filter"
                            , text = "Filter prefix:"
                            }
                        , Control.viewInput
                            { id = "filter"
                            , status = Control.Normal
                            , value = prefix
                            , maybeOnInput = Just InputPrefix
                            }
                        ]
                    ]
                , H.div [ HA.class "crud__listbox-area" ]
                    [ viewRoster prefix roster
                    ]
                , H.div [ HA.class "crud__names-area" ]
                    [ H.div [ HA.class "crud__names" ]
                        [ Control.viewLabel
                            { for = "name"
                            , text = "Name:"
                            }
                        , Control.viewInput
                            { id = "name"
                            , status = Control.Normal
                            , value = firstName
                            , maybeOnInput = Just InputFirstName
                            }
                        , Control.viewLabel
                            { for = "surname"
                            , text = "Surname:"
                            }
                        , Control.viewInput
                            { id = "surname"
                            , status = Control.Normal
                            , value = lastName
                            , maybeOnInput = Just InputLastName
                            }
                        ]
                    ]
                , let
                    isCreateDisabled =
                        String.isEmpty <| String.trim firstName

                    isUpdateDisabled =
                        Roster.selected roster == Nothing

                    isDeleteDisabled =
                        isUpdateDisabled
                  in
                  H.div [ HA.class "crud__buttons-area" ]
                    [ H.div [ HA.class "crud__buttons" ]
                        [ Button.view
                            { type_ =
                                if isCreateDisabled then
                                    Button.Disabled

                                else
                                    Button.Button ClickedCreate
                            , text = "Create"
                            }
                        , Button.view
                            { type_ =
                                if isUpdateDisabled then
                                    Button.Disabled

                                else
                                    Button.Button ClickedUpdate
                            , text = "Update"
                            }
                        , Button.view
                            { type_ =
                                if isDeleteDisabled then
                                    Button.Disabled

                                else
                                    Button.Button ClickedDelete
                            , text = "Delete"
                            }
                        ]
                    ]
                ]
        }


viewRoster : String -> Roster -> H.Html Msg
viewRoster prefix roster =
    let
        people =
            Roster.filter prefix roster

        viewPerson ( isSelected, person ) =
            H.option
                [ HA.value <| String.fromInt <| Person.getId person
                , HA.selected isSelected
                ]
                [ H.text <| Person.toString person ]
    in
    H.select
        [ HA.class "select"
        , HA.size 2
        , HE.onInput InputId
        ]
    <|
        List.map viewPerson people
