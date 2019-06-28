module Main exposing (main)

import Array
import Browser
import Html
import List exposing (range)


type GameObject
    = Player
    | Crate
    | Wall
    | Target


type alias Field =
    List GameObject


type alias Model =
    Array.Array (Array.Array Field)


viewField : Field -> Html.Html msg
viewField field =
    Html.td []
        [ Html.text <|
            case field of
                [ Player ] ->
                    "p"

                [ Crate ] ->
                    "c"

                [ Wall ] ->
                    "w"

                [ Target ] ->
                    "t"

                [ Crate, Target ] ->
                    "C"

                _ ->
                    "!"
        ]


viewRow : Array.Array Field -> Html.Html msg
viewRow row =
    Html.tr [] <| Array.toList <| Array.map viewField row


view : Model -> Html.Html msg
view model =
    Html.table [] <| Array.toList <| Array.map viewRow model


init =
    Array.fromList
        [ Array.fromList [ [ Wall ], [], [] ]
        , Array.fromList [ [ Player ], [ Crate ], [ Target ] ]
        , Array.fromList [ [], [], [] ]
        ]


main =
    Browser.sandbox { init = init, view = view, update = identity }
