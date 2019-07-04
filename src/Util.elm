module Util exposing (findInGrid, padList, padToLongest)

import Grid
import List
import Maybe


padList : Int -> a -> List a -> List a
padList l x xs =
    let
        missing =
            l - List.length xs
    in
    if missing > 0 then
        xs ++ List.repeat missing x

    else
        xs


padToLongest : a -> List (List a) -> List (List a)
padToLongest x xs =
    case
        List.map List.length xs
            |> List.maximum
    of
        Maybe.Just l ->
            List.map (padList l x) xs

        Nothing ->
            xs


findInGrid : (a -> Bool) -> Grid.Grid a -> Maybe ( Int, Int )
findInGrid f grid =
    Grid.indexedMap
        (\x y v ->
            if f v then
                Just ( x, y )

            else
                Nothing
        )
        grid
        |> Grid.foldl
            (\current new ->
                case new of
                    Maybe.Nothing ->
                        current

                    Maybe.Just ( x, y ) ->
                        Maybe.Just ( x, y )
            )
            Nothing
