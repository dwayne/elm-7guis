module Task.CircleDrawer.Lib.List exposing (findBy)


findBy : (a -> Bool) -> List a -> Maybe a
findBy isNeedle list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if isNeedle x then
                Just x

            else
                findBy isNeedle xs
