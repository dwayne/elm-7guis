module Cells.Data.DirectedGraph exposing
    ( DirectedGraph
    , addEdges
    , empty
    , removeEdges
    , tsort
    )

import Dict exposing (Dict)
import Set exposing (Set)


type DirectedGraph
    = DirectedGraph AdjSet


type alias AdjSet =
    Dict String (Set String)


empty : DirectedGraph
empty =
    DirectedGraph Dict.empty


type alias Edge =
    ( String, String )


addEdges : Set Edge -> DirectedGraph -> DirectedGraph
addEdges edges (DirectedGraph adj) =
    DirectedGraph <| addEdgesHelper edges adj


addEdgesHelper : Set Edge -> AdjSet -> AdjSet
addEdgesHelper edges adj =
    Set.foldl addEdge adj edges


addEdge : Edge -> AdjSet -> AdjSet
addEdge ( u, v ) =
    Dict.update u (addVertex v)


addVertex : String -> Maybe (Set String) -> Maybe (Set String)
addVertex v maybeSet =
    case maybeSet of
        Nothing ->
            Just <| Set.singleton v

        Just vertices ->
            Just <| Set.insert v vertices


removeEdges : Set Edge -> DirectedGraph -> DirectedGraph
removeEdges edges (DirectedGraph adj) =
    DirectedGraph <| removeEdgesHelper edges adj


removeEdgesHelper : Set Edge -> AdjSet -> AdjSet
removeEdgesHelper edges adj =
    Set.foldl removeEdge adj edges


removeEdge : Edge -> AdjSet -> AdjSet
removeEdge ( u, v ) =
    Dict.update u (removeVertex v)


removeVertex : String -> Maybe (Set String) -> Maybe (Set String)
removeVertex v maybeSet =
    case maybeSet of
        Nothing ->
            Nothing

        Just vertices ->
            let
                newVertices =
                    Set.remove v vertices
            in
            if Set.isEmpty newVertices then
                Nothing

            else
                Just newVertices


tsort : String -> DirectedGraph -> Maybe (List String)
tsort u (DirectedGraph adj) =
    let
        { maybeResult } =
            visit { marks = Dict.empty, maybeResult = Just [] } u adj
    in
    maybeResult
        |> Maybe.andThen List.tail


type alias VisitState =
    { marks : Dict String Mark
    , maybeResult : Maybe (List String)
    }


type Mark
    = Temporary
    | Permanent


visit : VisitState -> String -> AdjSet -> VisitState
visit state u adj =
    case state.maybeResult of
        Just _ ->
            case Dict.get u state.marks of
                Nothing ->
                    let
                        vertices =
                            Dict.get u adj
                                |> Maybe.withDefault Set.empty

                        finalState =
                            Set.foldl
                                (\v nextState -> visit nextState v adj)
                                { state | marks = Dict.insert u Temporary state.marks }
                                vertices
                    in
                    case finalState.maybeResult of
                        Just result ->
                            { finalState
                                | marks = Dict.insert u Permanent finalState.marks
                                , maybeResult = Just <| u :: result
                            }

                        Nothing ->
                            finalState

                Just Temporary ->
                    { state | maybeResult = Nothing }

                Just Permanent ->
                    state

        Nothing ->
            state
