module Util exposing (padList)

import List


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
