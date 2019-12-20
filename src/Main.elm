module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes as HtmlAttr exposing (src, style)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , listView mockList
        ]


type Minute
    = Minute Int


minute : Int -> Minute
minute int =
    Minute int


type alias Todo =
    { start : Minute
    , duration : Minute
    , title : String
    }


minuteToStr : Minute -> String
minuteToStr (Minute m) =
    String.fromInt m


todoView : Todo -> Html Msg
todoView { title, duration, start } =
    Html.li
        [ style "border" "1px solid black" ]
        [ Html.input [ HtmlAttr.value title ] []
        , Html.input [ HtmlAttr.value <| minuteToStr duration ] []
        ]


listView : List Todo -> Html Msg
listView list =
    Html.ul
        []
        (List.map todoView list)


mockTodo =
    { start = minute 0
    , duration = minute 5
    , title = "bla"
    }


mockList : List Todo
mockList =
    List.repeat 5 mockTodo



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
