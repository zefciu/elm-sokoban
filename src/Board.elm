module Board exposing
    ( Board
    , Field
    , GameObject(..)
    , canMove
    , canPush
    , getField
    , isCrate
    , isFree
    , isNonBlockingObject
    , move
    )

import Grid
import Keyboard.Arrows


type GameObject
    = Player
    | Crate
    | Wall
    | Target


isNonBlockingObject : GameObject -> Bool
isNonBlockingObject o =
    case o of
        Target ->
            True

        _ ->
            False


type alias Field =
    List GameObject


type alias FieldGrid =
    Grid.Grid Field


type alias Board =
    { playerPos : ( Int, Int )
    , grid : Grid.Grid Field
    }


addArrows : ( Int, Int ) -> Keyboard.Arrows.Arrows -> ( Int, Int )
addArrows ( x, y ) arrows =
    ( x + arrows.x, y - arrows.y )


getField : ( Int, Int ) -> Board -> Field
getField ( x, y ) board =
    Grid.get ( x, y ) board.grid
        |> Maybe.withDefault [ Wall ]


isFree : Board -> ( Int, Int ) -> Bool
isFree board ( x, y ) =
    getField ( x, y ) board
        |> List.all isNonBlockingObject


isCrate : Board -> ( Int, Int ) -> Bool
isCrate board ( x, y ) =
    getField ( x, y ) board
        |> List.any ((==) Crate)


canPush : Keyboard.Arrows.Arrows -> Board -> Bool
canPush arrows board =
    let
        pushFrom =
            addArrows board.playerPos arrows

        pushTo =
            addArrows pushFrom arrows
    in
    isCrate board pushFrom
        && isFree board pushTo


canMove : Keyboard.Arrows.Arrows -> Board -> Bool
canMove arrows board =
    (addArrows board.playerPos arrows
        |> isFree board
    )
        || canPush arrows board


removeObject : ( Int, Int ) -> GameObject -> FieldGrid -> FieldGrid
removeObject ( x, y ) o grid =
    let
        value =
            Grid.get ( x, y ) grid
                |> Maybe.map (List.filter ((/=) o))
                |> Maybe.withDefault []
    in
    Grid.set ( x, y ) value grid


putObject : ( Int, Int ) -> GameObject -> FieldGrid -> FieldGrid
putObject ( x, y ) o grid =
    let
        value =
            Grid.get ( x, y ) grid
                |> Maybe.map ((::) o)
                |> Maybe.withDefault []
    in
    Grid.set ( x, y ) value grid


moveObject : ( Int, Int ) -> ( Int, Int ) -> GameObject -> FieldGrid -> FieldGrid
moveObject ( x1, y1 ) ( x2, y2 ) o grid =
    removeObject ( x1, y1 ) o grid
        |> putObject ( x2, y2 ) o


move : Board -> Keyboard.Arrows.Arrows -> Board
move board arrows =
    let
        destination =
            addArrows board.playerPos arrows

        crate =
            isCrate board destination

        pushTo =
            addArrows destination arrows
    in
    { board
        | grid =
            (if crate then
                moveObject destination pushTo Crate board.grid

             else
                board.grid
            )
                |> moveObject board.playerPos destination Player
        , playerPos = destination
    }
