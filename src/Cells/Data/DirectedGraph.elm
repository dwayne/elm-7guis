module Cells.Data.DirectedGraph exposing
    ( DirectedGraph
    , Edge
    , Vertex
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
    Dict Vertex (Set Vertex)


type alias Vertex =
    String


type alias Edge =
    ( Vertex, Vertex )


empty : DirectedGraph
empty =
    DirectedGraph Dict.empty


addEdges : Set Edge -> DirectedGraph -> DirectedGraph
addEdges edges (DirectedGraph adj) =
    DirectedGraph <| addEdgesHelper edges adj


addEdgesHelper : Set Edge -> AdjSet -> AdjSet
addEdgesHelper edges adj =
    Set.foldl addEdge adj edges


addEdge : Edge -> AdjSet -> AdjSet
addEdge ( u, v ) =
    Dict.update u (addVertex v)


addVertex : Vertex -> Maybe (Set Vertex) -> Maybe (Set Vertex)
addVertex v maybeVertices =
    Just <|
        case maybeVertices of
            Nothing ->
                Set.singleton v

            Just vertices ->
                Set.insert v vertices


removeEdges : Set Edge -> DirectedGraph -> DirectedGraph
removeEdges edges (DirectedGraph adj) =
    DirectedGraph <| removeEdgesHelper edges adj


removeEdgesHelper : Set Edge -> AdjSet -> AdjSet
removeEdgesHelper edges adj =
    Set.foldl removeEdge adj edges


removeEdge : Edge -> AdjSet -> AdjSet
removeEdge ( u, v ) =
    Dict.update u (removeVertex v)


removeVertex : Vertex -> Maybe (Set Vertex) -> Maybe (Set Vertex)
removeVertex v =
    Maybe.andThen
        (\vertices ->
            let
                newVertices =
                    Set.remove v vertices
            in
            if Set.isEmpty newVertices then
                Nothing

            else
                Just newVertices
        )


tsort : Vertex -> DirectedGraph -> Maybe (List Vertex)
tsort u (DirectedGraph adj) =
    visit u adj (Just initVisitState)
        |> Maybe.map .vertices


type alias VisitState =
    { marks : Dict Vertex Mark
    , vertices : List Vertex
    }


type Mark
    = Temporary
    | Permanent


initVisitState : VisitState
initVisitState =
    VisitState Dict.empty []


visit : Vertex -> AdjSet -> Maybe VisitState -> Maybe VisitState
visit u adj =
    Maybe.andThen
        (\state ->
            case Dict.get u state.marks of
                Nothing ->
                    let
                        maybeFinalState =
                            Set.foldl
                                (\v -> Maybe.andThen (Just >> visit v adj))
                                maybeStartState
                                adjVertices

                        maybeStartState =
                            Just { state | marks = Dict.insert u Temporary state.marks }

                        adjVertices =
                            Dict.get u adj
                                |> Maybe.withDefault Set.empty
                    in
                    maybeFinalState
                        |> Maybe.map
                            (\finalState ->
                                { finalState
                                    | marks = Dict.insert u Permanent finalState.marks
                                    , vertices = u :: finalState.vertices
                                }
                            )

                Just Temporary ->
                    Nothing

                Just Permanent ->
                    Just state
        )
