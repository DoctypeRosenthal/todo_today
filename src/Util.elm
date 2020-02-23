module Util exposing (..)

import Time


type Location
    = Location String


location : String -> Location
location =
    Location


onlyUpdateX : x -> (x -> x) -> List x -> List x
onlyUpdateX x function list =
    List.map
        (\el ->
            if el == x then
                function x

            else
                el
        )
        list


type alias ID =
    Int


getNextId : List { a | id : ID } -> Time.Posix -> ID
getNextId list posix =
    let
        nowInMs =
            Time.posixToMillis posix
    in
    if List.any (\{ id } -> id == nowInMs) list then
        nowInMs + 1

    else
        nowInMs
