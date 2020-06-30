module DayPlan exposing (Model, Msg(..), Now, ViewModel, new, update, view)

import CustomTime exposing (to5MinutesBasedDayTime)
import Date exposing (Date)
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
    | UpdateToDo ToDo ToDo.Msg
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

        UpdateToDo toDo toDoMsg ->
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
        [ Html.div [ Html.Attributes.class "dayplan__header" ]
            [ Html.h3
                [ Html.Attributes.class "dayplan__title", onDoubleClick ToggleIsEditingTitle ]
                [ if viewModel.isEditingTitle then
                    Html.input
                        [ Html.Attributes.type_ "text"
                        , Html.Attributes.value dayPlan.title
                        , Html.Events.onInput SetTitle
                        , Html.Events.onBlur ToggleIsEditingTitle
                        ]
                        []

                  else
                    Html.text dayPlan.title
                , Html.button
                    [ onClick TogglePinning
                    , if dayPlan.isPinnedToTop then
                        Html.Attributes.class "pinned"

                      else
                        Html.Attributes.class "unpinned"
                    ]
                    []
                ]
            , Html.div [ Html.Attributes.class "dayplan__subtitle" ]
                [ Html.text " zuletzt benutzt: "
                , Html.text <| Date.format "dd.M.y" dayPlan.lastUsedAt
                ]
            ]
        , Html.div
            [ Html.Attributes.class "dayplan__main" ]
            (List.map todosView dayPlan.todos)
        , Html.div [ Html.Attributes.class "dayplan__footer" ]
            [ Html.button
                [ Html.Attributes.class "dayplan__color-picker-btn", onClick ToggleColorPicker ]
                [ colorPicker viewModel.isColorPickerVisible ]
            ]
        ]


todosView : ToDo -> Html Msg
todosView todo =
    Html.map (UpdateToDo todo) (ToDo.view todo)


colorPicker : Bool -> Html Msg
colorPicker isVisible =
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
