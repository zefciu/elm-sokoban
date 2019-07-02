module Main exposing (main)

import Array
import Board
import Browser
import Grid
import Html
import Html.Attributes
import Keyboard
import Keyboard.Arrows
import List exposing (range)
import Maybe


type alias Model =
    { pressedKeys : List Keyboard.Key
    , board : Board.Board
    }


type Msg
    = KeyMsg Keyboard.Msg


viewField : Board.Field -> Html.Html msg
viewField field =
    Html.td
        [ Html.Attributes.class <|
            case field of
                [ Board.Player ] ->
                    "player"

                [ Board.Crate ] ->
                    "crate"

                [ Board.Wall ] ->
                    "wall"

                [ Board.Target ] ->
                    "target"

                [ Board.Crate, Board.Target ] ->
                    "crate-target"

                [ Board.Player, Board.Target ] ->
                    "player"

                [] ->
                    "floor"

                _ ->
                    "unknown"
        ]
        []


viewRow : Array.Array Board.Field -> Html.Html msg
viewRow row =
    Html.tr [] <| Array.toList <| Array.map viewField row


view : Model -> Html.Html msg
view model =
    Grid.rows model.board.grid
        |> Array.map viewRow
        |> Array.toList
        |> Html.table []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map KeyMsg Keyboard.subscriptions


init : {} -> ( Model, Cmd msg )
init _ =
    ( { pressedKeys = []
      , board =
            { grid =
                Grid.fromList
                    [ [ [ Board.Wall ], [], [] ]
                    , [ [ Board.Player ], [ Board.Crate ], [ Board.Target ] ]
                    , [ [], [], [] ]
                    ]
                    |> Maybe.withDefault (Grid.repeat 0 0 [])
            , playerPos = ( 0, 1 )
            }
      }
    , Cmd.none
    )


processKey model keyMsg =
    { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }


processMove : Model -> ( Model, Cmd msg )
processMove model =
    let
        arrows =
            Keyboard.Arrows.arrows model.pressedKeys
    in
    if Board.canMove arrows model.board then
        ( { model | board = Board.move model.board arrows }, Cmd.none )

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
