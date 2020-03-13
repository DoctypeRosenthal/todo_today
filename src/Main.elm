module Main exposing (..)

import Browser
import CustomTime exposing (FiveMinuteBasedTime, Hour)
import Date
import DayPlan exposing (DayPlan)
import Html exposing (Html, div, img)
import Html.Attributes exposing (src)
import Task
import Time exposing (Month(..), Posix, Zone, utc)
import Util exposing (Location, getNextId, location, onlyUpdateX)



---- MODEL ----


type alias Start =
    Hour


type alias End =
    Hour


type alias Model =
    { home : Location
    , locationHistory : List Location
    , timeZone : Time.Zone
    , now : Time.Posix
    , zoom : ( Start, End )
    , workTime : ( Start, End )
    , plans : List DayPlan
    , currentPlan : Maybe DayPlan
    }


initModel : Model
initModel =
    { home = location "Köln"
    , timeZone = utc
    , locationHistory = []
    , now = Time.millisToPosix 0
    , zoom = ( 8, 23 )
    , workTime = ( 8, 22 )
    , plans = []
    , currentPlan = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Task.perform AdjustTimeZone Time.here )



---- UPDATE ----


type Msg
    = SetHome Location
    | AdjustTimeZone Time.Zone
    | UpdateNow Posix
    | Zoom ( Start, End )
    | SetWorkTime ( Start, End )
    | CreatePlan
    | LoadPlan DayPlan
    | RemovePlan DayPlan
    | RemindUserToIncreaseSpareTime
    | PersistState
    | UpdateDayPlan DayPlan.Msg DayPlan


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetHome location ->
            ( { model | home = location }, Cmd.none )

        UpdateNow posix ->
            ( { model | now = posix }, Cmd.none )

        Zoom hourInterval ->
            ( { model | zoom = hourInterval }, Cmd.none )

        SetWorkTime interval ->
            ( { model | workTime = interval }, Cmd.none )

        CreatePlan ->
            let
                nextId =
                    getNextId model.plans model.now
            in
            ( { model
                | plans = DayPlan.new model.timeZone model.now nextId :: model.plans
              }
            , Cmd.none
            )

        LoadPlan dayPlan ->
            ( { model | currentPlan = Just dayPlan }, Cmd.none )

        RemovePlan dayPlan ->
            ( { model
                | currentPlan =
                    if model.currentPlan == Just dayPlan then
                        Nothing

                    else
                        Just dayPlan
                , plans = List.filter (\x -> x /= dayPlan) model.plans
              }
            , Cmd.none
            )

        UpdateDayPlan planMsg dayPlan ->
            ( { model
                | plans = onlyUpdateX dayPlan (DayPlan.update planMsg) model.plans
              }
            , Cmd.none
            )

        RemindUserToIncreaseSpareTime ->
            ( model, Cmd.none )

        PersistState ->
            ( model, Cmd.none )

        AdjustTimeZone zone ->
            ( { model
                | timeZone = zone
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    -- see https://keep.google.com/u/0/#home for design idea
    div []
        [ Html.h1 [] [ Html.text "ToDos" ]
        , Html.input [ Html.Attributes.value "Ich bin eine Searchbar! 👻" ] []
        , Html.button [] [ Html.text "Neuer Tagesplan +" ]
        , Html.div []
            (Html.h5 [] [ Html.text "Pinned" ]
                :: List.map DayPlan.view model.plans
            )
        , Html.div []
            (Html.h5 [] [ Html.text "Others" ]
                :: List.map DayPlan.view model.plans
            )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 UpdateNow



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
