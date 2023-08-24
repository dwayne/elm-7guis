module Task.Crud.Selection exposing
    ( Selection
    , cons
    , deselect
    , empty
    , filter
    , mapSelected
    , removeSelected
    , selectBy
    , selected
    , toList
    , toListWithSelection
    )


type Selection a
    = Selection (List a) (Maybe a) (List a)


empty : Selection a
empty =
    Selection [] Nothing []


cons : a -> Selection a -> Selection a
cons a (Selection front maybeSel back) =
    Selection (a :: front) maybeSel back


selectBy : (a -> Bool) -> Selection a -> Selection a
selectBy isSelected =
    toList >> selectHelper [] isSelected


selectHelper : List a -> (a -> Bool) -> List a -> Selection a
selectHelper front isSelected list =
    case list of
        [] ->
            Selection (List.reverse front) Nothing []

        a :: restList ->
            if isSelected a then
                Selection (List.reverse front) (Just a) restList

            else
                selectHelper (a :: front) isSelected restList


selected : Selection a -> Maybe a
selected (Selection _ maybeSel _) =
    maybeSel


deselect : Selection a -> Selection a
deselect ((Selection front maybeSel back) as selection) =
    case maybeSel of
        Just a ->
            Selection front Nothing (a :: back)

        Nothing ->
            selection


removeSelected : Selection a -> Selection a
removeSelected (Selection front _ back) =
    Selection front Nothing back


mapSelected : { selected : a -> b, rest : a -> b } -> Selection a -> Selection b
mapSelected mappers (Selection front maybeSel back) =
    Selection
        (List.map mappers.rest front)
        (Maybe.map mappers.selected maybeSel)
        (List.map mappers.rest back)


filter : (a -> Bool) -> Selection a -> Selection a
filter isGood (Selection front maybeSel back) =
    let
        newMaybeSel =
            maybeSel
                |> Maybe.andThen
                    (\sel ->
                        if isGood sel then
                            maybeSel

                        else
                            Nothing
                    )
    in
    Selection
        (List.filter isGood front)
        newMaybeSel
        (List.filter isGood back)


toList : Selection a -> List a
toList (Selection front maybeSel back) =
    List.concat
        [ front
        , case maybeSel of
            Just sel ->
                [ sel ]

            Nothing ->
                []
        , back
        ]


toListWithSelection : Selection a -> List ( Bool, a )
toListWithSelection =
    mapSelected
        { selected = Tuple.pair True
        , rest = Tuple.pair False
        }
        >> toList
