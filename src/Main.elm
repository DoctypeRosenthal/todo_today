module Main exposing (..)

import Browser
import CustomTime exposing (FiveMinuteBasedTime, Hour)
import DayPlan exposing (DayPlan)
import Html exposing (Html, div)
import Html.Attributes
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
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
    , nextPlanTitle : String
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
    , nextPlanTitle = ""
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
    | CreatePlan String
    | LoadPlan DayPlan
    | CloseCurrentPlan
    | RemovePlan DayPlan
    | RemindUserToIncreaseSpareTime
    | PersistState
    | SetNextPlanTitle String
    | UpdateDayPlan DayPlan DayPlan.Msg
    | NoOp


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

        CreatePlan title ->
            if title == "" then
                ( model, Cmd.none )

            else
                let
                    nextId =
                        getNextId model.plans model.now

                    newPlan =
                        DayPlan.new model.timeZone model.now nextId title

                    ( nextModel, _ ) =
                        update (SetNextPlanTitle "") { model | plans = newPlan :: model.plans }
                in
                update (LoadPlan newPlan) nextModel

        LoadPlan dayPlan ->
            ( { model | currentPlan = Just dayPlan }, Cmd.none )

        CloseCurrentPlan ->
            ( { model | currentPlan = Nothing }, Cmd.none )

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

        UpdateDayPlan dayPlan planMsg ->
            ( { model
                | plans = onlyUpdateX dayPlan (DayPlan.update planMsg) model.plans
              }
            , Cmd.none
            )

        SetNextPlanTitle title ->
            ( { model | nextPlanTitle = title }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        pinnedPlans =
            List.filter .isPinnedToTop model.plans

        otherPlans =
            List.filter (not << .isPinnedToTop) model.plans
    in
    -- see https://keep.google.com/u/0/#home for design idea
    div []
        [ Html.h1 [] [ Html.text "ToDos" ]
        , viewNextDayPlanTitle model.nextPlanTitle
        , Html.div []
            (Html.h5 [] [ Html.text "Pinned" ]
                :: List.map dayPlans pinnedPlans
            )
        , Html.div []
            (Html.h5 [] [ Html.text "Others" ]
                :: List.map dayPlans otherPlans
            )
        , case model.currentPlan of
            Just plan ->
                modalView plan

            Nothing ->
                Html.text ""
        ]


modalView : DayPlan -> Html Msg
modalView plan =
    Html.div [ Html.Attributes.class "editor" ]
        [ Html.div [ Html.Attributes.class "editor__inner" ]
            [ Html.div [ Html.Attributes.class "editor__top-bar" ]
                [ Html.b [ Html.Attributes.class "editor__title" ] [ Html.text plan.title ]
                , Html.button [ Html.Attributes.class "editor__close", onClick CloseCurrentPlan ] [ Html.text "Schließen" ]
                ]
            ]
        ]


viewNextDayPlanTitle : String -> Html Msg
viewNextDayPlanTitle title =
    Html.input
        [ Html.Attributes.placeholder "New Dayplan..."
        , Html.Attributes.value title
        , onInput SetNextPlanTitle
        , on "keydown" (executeOnEnter (CreatePlan title))
        ]
        []


executeOnEnter : Msg -> Decode.Decoder Msg
executeOnEnter msg =
    Decode.map
        (\str ->
            if str == "Enter" then
                msg

            else
                NoOp
        )
        (Decode.field "key" Decode.string)


dayPlans : DayPlan.DayPlan -> Html Msg
dayPlans dayPlan =
    Html.map (UpdateDayPlan dayPlan) (DayPlan.view dayPlan)



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
