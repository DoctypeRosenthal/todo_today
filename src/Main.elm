module Main exposing (..)

import Browser
import Html exposing (Html, div, img)
import Html.Attributes exposing (src)
import ToDo exposing (ToDo)



---- MODEL ----


type alias Model =
    {}


initModel : Model
initModel =
    {}


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



---- UPDATE ----


type Msg
    = ZoomIn
    | ZoomOut
    | AddToDo
    | RemoveToDo ToDo
    | ToDoMsg ToDo.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
