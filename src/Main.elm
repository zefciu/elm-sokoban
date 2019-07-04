module Main exposing (main)

import Array
import Board
import Browser
import Grid
import Html
import Html.Attributes
import Http
import Keyboard
import Keyboard.Arrows
import List exposing (range)
import Loader
import Maybe


levelSetUrl =
    "/alberto.txt"


type ModelState
    = Initial
    | Playing


type alias Model =
    { pressedKeys : List Keyboard.Key
    , state : ModelState
    , board : Board.Board
    , levels : Array.Array Board.Board
    , currentLevel : Int
    }


newModel : Model
newModel =
    { pressedKeys = []
    , state = Initial
    , levels = Array.empty
    , currentLevel = 0
    , board = Board.empty
    }


type Msg
    = KeyMsg Keyboard.Msg
    | LevelSetLoaded (Result Http.Error String)


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


viewRow : Array.Array Board.Field -> Html.Html Msg
viewRow row =
    Html.tr [] <| Array.toList <| Array.map viewField row


view : Model -> Html.Html Msg
view model =
    Grid.rows model.board.grid
        |> Array.map viewRow
        |> Array.toList
        |> Html.table []


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Initial ->
            Sub.none

        Playing ->
            Sub.map KeyMsg Keyboard.subscriptions


init : {} -> ( Model, Cmd Msg )
init _ =
    ( newModel
    , Http.get
        { url = levelSetUrl
        , expect = Http.expectString LevelSetLoaded
        }
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

        LevelSetLoaded (Ok data) ->
            let
                levels =
                    Loader.loadLevels data
            in
            ( { model
                | levels = Loader.loadLevels data
                , state = Playing
                , currentLevel = 0
                , board =
                    Array.get 0 levels
                        |> Maybe.withDefault Board.empty
              }
            , Cmd.none
            )

        LevelSetLoaded (Err message) ->
            ( { model
                | levels = Array.empty
                , state = Playing
                , currentLevel = 0
                , board = Board.empty
              }
            , Cmd.none
            )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
