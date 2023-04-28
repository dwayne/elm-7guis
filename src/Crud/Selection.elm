module Crud.Selection exposing
    ( Selection, empty, cons
    , selectBy, selected
    , toList
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
selectBy satisfiesCondition =
    toList >> selectHelper [] satisfiesCondition


selectHelper : List a -> (a -> Bool) -> List a -> Selection a
selectHelper front satisfiesCondition list =
    case list of
        [] ->
            Selection (List.reverse front) Nothing []

        a :: restList ->
            if satisfiesCondition a then
                Selection (List.reverse front) (Just a) restList

            else
                selectHelper (a :: front) satisfiesCondition restList


selected : Selection a -> Maybe a
selected (Selection _ maybeSel _) =
    maybeSel


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
