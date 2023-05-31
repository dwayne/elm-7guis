module CircleDrawer.UndoManager exposing
    ( Edit
    , UndoManager
    , add
    , canRedo
    , canUndo
    , empty
    , redo
    , undo
    )


type UndoManager u r
    = UndoManager (List (Edit u r)) (List (Edit u r))


type alias Edit u r =
    { undo : u
    , redo : r
    }


empty : UndoManager u r
empty =
    UndoManager [] []


add : Edit u r -> UndoManager u r -> UndoManager u r
add edit (UndoManager past future) =
    UndoManager (edit :: past) []


canUndo : UndoManager u r -> Bool
canUndo (UndoManager past _) =
    not <| List.isEmpty past


canRedo : UndoManager u r -> Bool
canRedo (UndoManager _ future) =
    not <| List.isEmpty future


undo : UndoManager u r -> Maybe ( u, UndoManager u r )
undo ((UndoManager past future) as undoManager) =
    case past of
        [] ->
            Nothing

        edit :: restEdits ->
            Just
                ( edit.undo
                , UndoManager restEdits (edit :: future)
                )


redo : UndoManager u r -> Maybe ( r, UndoManager u r )
redo (UndoManager past future) =
    case future of
        [] ->
            Nothing

        edit :: restEdits ->
            Just
                ( edit.redo
                , UndoManager (edit :: past) restEdits
                )
