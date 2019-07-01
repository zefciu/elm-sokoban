module Main exposing (main)

import Array
import Browser
import Grid
import Html
import Html.Attributes
import Keyboard
import Keyboard.Arrows
import List exposing (range)
import Maybe


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


type alias Board =
    Grid.Grid Field


type alias Model =
    { pressedKeys : List Keyboard.Key
    , playerPos : ( Int, Int )
    , board : Board
    }


type Msg
    = KeyMsg Keyboard.Msg


viewField : Field -> Html.Html msg
viewField field =
    Html.td
        [ Html.Attributes.class <|
            case field of
                [ Player ] ->
                    "player"

                [ Crate ] ->
                    "crate"

                [ Wall ] ->
                    "wall"

                [ Target ] ->
                    "target"

                [ Crate, Target ] ->
                    "crate-target"

                [ Player, Target ] ->
                    "player"

                [] ->
                    "floor"

                _ ->
                    "unknown"
        ]
        []


viewRow : Array.Array Field -> Html.Html msg
viewRow row =
    Html.tr [] <| Array.toList <| Array.map viewField row


view : Model -> Html.Html msg
view model =
    Grid.rows model.board
        |> Array.map viewRow
        |> Array.toList
        |> Html.table []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map KeyMsg Keyboard.subscriptions


init : {} -> ( Model, Cmd msg )
init _ =
    ( { pressedKeys = []
      , playerPos = ( 0, 1 )
      , board =
            Grid.fromList
                [ [ [ Wall ], [], [] ]
                , [ [ Player ], [ Crate ], [ Target ] ]
                , [ [], [], [] ]
                ]
                |> Maybe.withDefault (Grid.repeat 0 0 [])
      }
    , Cmd.none
    )


processKey model keyMsg =
    { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }


getField : ( Int, Int ) -> Model -> Field
getField ( x, y ) model =
    Grid.get ( x, y ) model.board
        |> Maybe.withDefault [ Wall ]


isFree : Model -> ( Int, Int ) -> Bool
isFree model ( x, y ) =
    getField ( x, y ) model
        |> List.all isNonBlockingObject


isCrate : Model -> ( Int, Int ) -> Bool
isCrate model ( x, y ) =
    getField ( x, y ) model
        |> List.any ((==) Crate)


canPush : Keyboard.Arrows.Arrows -> Model -> Bool
canPush arrows model =
    let
        pushFrom =
            addArrows model.playerPos arrows

        pushTo =
            addArrows pushFrom arrows
    in
    isCrate model pushFrom
        && isFree model pushTo


canMove : Keyboard.Arrows.Arrows -> Model -> Bool
canMove arrows model =
    (addArrows model.playerPos arrows
        |> isFree model
    )
        || canPush arrows model


addArrows : ( Int, Int ) -> Keyboard.Arrows.Arrows -> ( Int, Int )
addArrows ( x, y ) arrows =
    ( x + arrows.x, y - arrows.y )


removeObject : ( Int, Int ) -> GameObject -> Board -> Board
removeObject ( x, y ) o board =
    let
        value =
            Grid.get ( x, y ) board
                |> Maybe.map (List.filter ((/=) o))
                |> Maybe.withDefault []
    in
    Grid.set ( x, y ) value board


putObject : ( Int, Int ) -> GameObject -> Board -> Board
putObject ( x, y ) o board =
    let
        value =
            Grid.get ( x, y ) board
                |> Maybe.map ((::) o)
                |> Maybe.withDefault []
    in
    Grid.set ( x, y ) value board


moveObject : ( Int, Int ) -> ( Int, Int ) -> GameObject -> Board -> Board
moveObject ( x1, y1 ) ( x2, y2 ) o board =
    removeObject ( x1, y1 ) o board
        |> putObject ( x2, y2 ) o


move : Model -> Keyboard.Arrows.Arrows -> Model
move model arrows =
    let
        destination =
            addArrows model.playerPos arrows

        crate =
            isCrate model destination

        pushTo =
            addArrows destination arrows
    in
    { model
        | board =
            (if crate then
                moveObject destination pushTo Crate model.board

             else
                model.board
            )
                |> moveObject model.playerPos destination Player
        , playerPos = destination
    }


processMove : Model -> ( Model, Cmd msg )
processMove model =
    let
        arrows =
            Keyboard.Arrows.arrows model.pressedKeys
    in
    if canMove arrows model then
        ( move model arrows, Cmd.none )

    else
        ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        KeyMsg keyMsg ->
            processKey model keyMsg
                |> processMove


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
