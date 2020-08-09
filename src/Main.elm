module Main exposing (End, Model, Msg(..), Start, dayPlansView, executeOnEnter, init, initModel, main, subscriptions, update, view, viewNextDayPlanTitle)

import Browser
import DayPlan
import Html exposing (Html, div)
import Html.Attributes
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Task
import Tick exposing (Hour, Tick)
import Time exposing (Month(..), Posix, Zone, utc)
import ToDo
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
    , plans : List DayPlan.DayPlan
    , nextPlanTitle : String
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
    | RemovePlan DayPlan.DayPlan
    | RemindUserToIncreaseSpareTime
    | PersistState
    | SetNewPlanInputValue String
    | UpdateDayPlan DayPlan.DayPlan DayPlan.Msg -- Reihenfolge so besser wegen message mapping via partial application
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
                        getNextId (List.map Tuple.second model.plans) model.now

                    newPlan =
                        DayPlan.new model.timeZone model.now nextId title

                    newToDo =
                        ToDo.new nextId (Tick.fromPosix model.timeZone model.now) model.home

                    planWithTodo =
                        DayPlan.update (DayPlan.AddToDo newToDo) newPlan

                    nextModel =
                        { model | plans = planWithTodo :: model.plans }
                in
                update (SetNewPlanInputValue "") nextModel

        RemovePlan dayPlan ->
            ( { model | plans = List.filter ((/=) dayPlan) model.plans }
            , Cmd.none
            )

        RemindUserToIncreaseSpareTime ->
            ( model, Cmd.none )

        PersistState ->
            ( model, Cmd.none )

        AdjustTimeZone zone ->
            ( { model | timeZone = zone }
            , Cmd.none
            )

        UpdateDayPlan dayPlan planMsg ->
            case planMsg of
                DayPlan.RemoveMe ->
                    update (RemovePlan dayPlan) model

                _ ->
                    ( { model
                        | plans = onlyUpdateX dayPlan (DayPlan.update planMsg) model.plans
                      }
                    , Cmd.none
                    )

        SetNewPlanInputValue title ->
            ( { model | nextPlanTitle = title }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        pinnedPlans =
            List.filter (Tuple.second >> .isPinnedToTop) model.plans

        otherPlans =
            List.filter (Tuple.second >> .isPinnedToTop >> not) model.plans
    in
    -- see https://keep.google.com/u/0/#home for design idea
    div []
        [ Html.h1 [] [ Html.text "ToDo today" ]
        , viewNextDayPlanTitle model.nextPlanTitle
        , Html.div []
            (Html.h5 [] [ Html.text "Pinned" ]
                :: List.map dayPlansView pinnedPlans
            )
        , Html.div []
            (Html.h5 [] [ Html.text "Others" ]
                :: List.map dayPlansView otherPlans
            )
        ]


viewNextDayPlanTitle : String -> Html Msg
viewNextDayPlanTitle title =
    Html.input
        [ Html.Attributes.placeholder "New Dayplan..."
        , Html.Attributes.value title
        , onInput SetNewPlanInputValue
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


dayPlansView : DayPlan.DayPlan -> Html Msg
dayPlansView dayPlan =
    Html.map (UpdateDayPlan dayPlan) (DayPlan.render dayPlan)



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
