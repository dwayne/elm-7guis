module Test.Task.Cells.Data.DirectedGraph exposing (suite)

import Expect
import Set
import Task.Cells.Data.DirectedGraph as DirectedGraph exposing (DirectedGraph, Edge, Vertex)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "DirectedGraph"
        [ tsortSuite
        ]


tsortSuite : Test
tsortSuite =
    describe "tsort"
        [ tsort "a" [] <|
            Just <|
                [ "a" ]
        , tsort "a" [ ( "a", "b" ), ( "a", "c" ), ( "b", "c" ) ] <|
            Just <|
                [ "a", "b", "c" ]
        , tsort "a" [ ( "a", "b" ), ( "a", "c" ), ( "c", "b" ) ] <|
            Just <|
                [ "a", "c", "b" ]
        , tsort "a" [ ( "a", "b" ), ( "a", "c" ), ( "b", "c" ), ( "c", "b" ) ] <|
            Nothing
        ]


tsort : Vertex -> List Edge -> Maybe (List Vertex) -> Test
tsort u edges maybeVertices =
    test ("G = " ++ edgesToString edges ++ ", start = " ++ u) <|
        \_ ->
            DirectedGraph.tsort u (fromEdges edges)
                |> Expect.equal maybeVertices


fromEdges : List Edge -> DirectedGraph
fromEdges edges =
    DirectedGraph.addEdges (Set.fromList edges) DirectedGraph.empty


edgesToString : List Edge -> String
edgesToString edges =
    let
        contents =
            edges
                |> List.map edgeToString
                |> List.intersperse ", "
                |> String.concat

        edgeToString ( u, v ) =
            "(" ++ u ++ ", " ++ v ++ ")"
    in
    "[" ++ contents ++ "]"
