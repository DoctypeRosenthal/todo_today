module Color exposing (..)

import Element exposing (Color, toRgb)


toStr : Color -> String
toStr color =
    let
        { red, green, blue } =
            toRgb color

        lst =
            [ red, green, blue ]
                |> List.map String.fromFloat
                |> String.join ","
    in
    "rgb(" ++ lst ++ ")"
