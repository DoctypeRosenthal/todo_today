module DayPlan exposing (Model, Msg(..), Now, ViewModel, new, update, view)

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


type alias Model =
    { id : ID
    , title : String
    , color : String
    , createdAt : Date
    , lastUsedAt : Date
    , todos : List ToDo
    , isPinnedToTop : Bool
    }


type alias ViewModel =
    { dayPlan : Model
    , isEditingTitle : Bool
    , isColorPickerVisible : Bool
    }


new : Time.Zone -> Time.Posix -> ID -> String -> ViewModel
new timeZone posix id title =
    let
        today =
            Date.fromPosix timeZone posix
    in
    { dayPlan =
        { id = id
        , title = title
        , color = "yellow"
        , createdAt = today
        , lastUsedAt = today
        , todos = []
        , isPinnedToTop = False
        }
    , isEditingTitle = False
    , isColorPickerVisible = False
    }



-- UPDATE


type alias Now =
    Time.Posix


type Msg
    = SetTitle String
    | SetColor String
    | TogglePinning
    | SetLastUsedAt Date
    | AddToDo Time.Zone Now Location
    | UpdateToDo ToDo.Msg ToDo
    | RemoveToDo ToDo
    | ToggleIsEditingTitle
    | ToggleColorPicker


updateDayPlan : Msg -> Model -> Model
updateDayPlan msg dayPlan =
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

        _ ->
            dayPlan


update : Msg -> ViewModel -> ViewModel
update msg model =
    case msg of
        ToggleIsEditingTitle ->
            { model | isEditingTitle = not model.isEditingTitle }

        ToggleColorPicker ->
            { model | isColorPickerVisible = not model.isColorPickerVisible }

        _ ->
            { model | dayPlan = updateDayPlan msg model.dayPlan }



-- VIEW


view : ViewModel -> Html Msg
view ({ dayPlan } as viewModel) =
    Html.div [ Html.Attributes.class ("dayplan " ++ dayPlan.color) ]
        [ if viewModel.isEditingTitle then
            Html.input
                [ Html.Attributes.type_ "text"
                , Html.Attributes.value dayPlan.title
                , Html.Events.onInput SetTitle
                , Html.Events.onBlur ToggleIsEditingTitle
                ]
                []

          else
            Html.span
                [ onDoubleClick ToggleIsEditingTitle ]
                [ Html.text dayPlan.title
                , Html.text " zuletzt benutzt:"
                , Html.text <| Date.toIsoString dayPlan.lastUsedAt
                ]
        , Html.button
            [ onClick TogglePinning
            , if dayPlan.isPinnedToTop then
                Html.Attributes.class "pinned"

              else
                Html.Attributes.class "unpinned"
            ]
            []
        , Html.button
            [ Html.Attributes.class "dayplan__color-picker-btn", onClick ToggleColorPicker ]
            [ colorPicker viewModel.isColorPickerVisible dayPlan.color ]
        ]


colorPicker : Bool -> String -> Html Msg
colorPicker isVisible selectedColor =
    Html.div
        [ Html.Attributes.class
            ("dayplan__color-picker "
                ++ (if isVisible then
                        "visible"

                    else
                        ""
                   )
            )
        ]
        ([ "red", "blue", "yellow" ] |> List.map toColorBtn)


toColorBtn : String -> Html Msg
toColorBtn colorClassName =
    Html.button
        [ Html.Attributes.class colorClassName, Html.Events.onClick <| SetColor colorClassName ]
        []
