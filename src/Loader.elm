module Loader exposing (loadLevels)

import Array
import Board
import Grid
import Tuple
import Util


parseField : Char -> Board.Field
parseField c =
    case c of
        ' ' ->
            []

        '.' ->
            [ Board.Target ]

        '#' ->
            [ Board.Wall ]

        '$' ->
            [ Board.Crate ]

        '@' ->
            [ Board.Player ]

        '*' ->
            [ Board.Crate, Board.Target ]

        '+' ->
            [ Board.Player, Board.Target ]

        _ ->
            []


boardFromList : List (List Board.Field) -> Board.Board
boardFromList list =
    let
        grid =
            Grid.fromList list
                |> Maybe.withDefault (Grid.repeat 0 0 [])
    in
    { grid = grid
    , playerPos =
        Util.findInGrid (\x -> List.any ((==) Board.Player) x) grid
            |> Maybe.withDefault ( 0, 0 )
    }


parseLine :
    String
    -> ( List (List (List Board.Field)), List (List Board.Field) )
    -> ( List (List (List Board.Field)), List (List Board.Field) )
parseLine line ( loaded, current ) =
    if String.startsWith line ";" then
        ( loaded ++ [ current ], [] )

    else
        ( loaded, List.map parseField (String.toList line) :: current )


loadLevels : String -> Array.Array Board.Board
loadLevels data =
    String.lines data
        |> List.foldl parseLine ( [], [] )
        |> Tuple.first
        |> List.map (Util.padToLongest [])
        |> List.map boardFromList
        |> Array.fromList
