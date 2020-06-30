module DayPlan exposing (DayPlan, Msg(..), Now, new, update, view)

import CustomTime exposing (to5MinutesBasedDayTime)
import Date exposing (Date)
import Element as Color exposing (Color)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Time
import ToDo exposing (ToDo)
import Util exposing (ID, Location, getNextId, onlyUpdateX)



-- MODEL


type alias DayPlan =
    { id : ID
    , title : String
    , color : Color
    , createdAt : Date
    , lastUsedAt : Date
    , todos : List ToDo
    , isPinnedToTop : Bool
    , isEditingTitle : Bool
    }


new : Time.Zone -> Time.Posix -> ID -> String -> DayPlan
new timeZone posix id title =
    let
        today =
            Date.fromPosix timeZone posix
    in
    { id = id
    , title = title
    , color = Color.rgb 255 255 255
    , createdAt = today
    , lastUsedAt = today
    , todos = []
    , isPinnedToTop = False
    , isEditingTitle = False
    }



-- UPDATE


type alias Now =
    Time.Posix


type Msg
    = SetTitle String
    | SetColor Color
    | TogglePinning
    | SetLastUsedAt Date
    | AddToDo Time.Zone Now Location
    | UpdateToDo ToDo.Msg ToDo
    | RemoveToDo ToDo
    | ToggleIsEditingTitle


update : Msg -> DayPlan -> DayPlan
update msg dayPlan =
    case msg of
        SetTitle string ->
            { dayPlan | title = string }

        SetColor color ->
            { dayPlan | color = color }

        TogglePinning ->
            { dayPlan | isPinnedToTop = not dayPlan.isPinnedToTop }

        SetLastUsedAt date ->
            { dayPlan | lastUsedAt = date }

        AddToDo timeZone now location ->
            let
                id =
                    getNextId dayPlan.todos now

                startTime =
                    to5MinutesBasedDayTime timeZone now

                newTodo =
                    ToDo.new id startTime location
            in
            { dayPlan | todos = newTodo :: dayPlan.todos }

        UpdateToDo toDoMsg toDo ->
            { dayPlan
                | todos = onlyUpdateX toDo (ToDo.update toDoMsg) dayPlan.todos
            }

        RemoveToDo toDo ->
            { dayPlan
                | todos = List.filter (\x -> x /= toDo) dayPlan.todos
            }

        ToggleIsEditingTitle ->
            { dayPlan | isEditingTitle = not dayPlan.isEditingTitle }



-- VIEW


view : DayPlan -> Html Msg
view dayplan =
    Html.div [ Html.Attributes.class "dayplan" ]
        [ if dayplan.isEditingTitle then
            Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.value dayplan.title
                , Html.Events.onInput SetTitle
                , Html.Events.onBlur ToggleIsEditingTitle
                ]
                []

          else
            Html.span
                [ onDoubleClick ToggleIsEditingTitle ]
                [ Html.text dayplan.title
                , Html.text " zuletzt benutzt:"
                , Html.text <| Date.toIsoString dayplan.lastUsedAt
                ]
        , Html.button
            [ onClick TogglePinning
            , if dayplan.isPinnedToTop then
                Html.Attributes.class "pinned"

              else
                Html.Attributes.class "unpinned"
            ]
            []
        ]
