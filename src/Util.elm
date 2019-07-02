module Util exposing (padList, padToLongest)

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
