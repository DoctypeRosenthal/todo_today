module DayPlan exposing (DayPlan, Msg(..), Now, new, render, update)

import Date exposing (Date)
import FiveMinutBasedTime exposing (fromPosix)
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


type alias View =
    { isEditingTitle : Bool
    , isColorPickerVisible : Bool
    }


type alias DayPlan =
    ( View, Model )


new : Time.Zone -> Time.Posix -> ID -> String -> DayPlan
new timeZone posix id title =
    let
        today =
            Date.fromPosix timeZone posix
    in
    ( { isEditingTitle = False
      , isColorPickerVisible = False
      }
    , { id = id
      , title = title
      , color = "yellow"
      , createdAt = today
      , lastUsedAt = today
      , todos = []
      , isPinnedToTop = False
      }
    )



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


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        SetTitle string ->
            { model | title = string }

        SetColor color ->
            { model | color = color }

        TogglePinning ->
            { model | isPinnedToTop = not model.isPinnedToTop }

        SetLastUsedAt date ->
            { model | lastUsedAt = date }

        AddToDo timeZone now location ->
            let
                id =
                    getNextId model.todos now

                startTime =
                    fromPosix timeZone now

                newTodo =
                    ToDo.new id startTime location
            in
            { model | todos = newTodo :: model.todos }

        UpdateToDo toDo toDoMsg ->
            { model
                | todos = onlyUpdateX toDo (ToDo.update toDoMsg) model.todos
            }

        RemoveToDo toDo ->
            { model
                | todos = List.filter (\x -> x /= toDo) model.todos
            }

        _ ->
            model


updateView : Msg -> View -> View
updateView msg view =
    case msg of
        ToggleIsEditingTitle ->
            { view | isEditingTitle = not view.isEditingTitle }

        ToggleColorPicker ->
            { view | isColorPickerVisible = not view.isColorPickerVisible }

        _ ->
            view


update : Msg -> DayPlan -> DayPlan
update msg ( view, model ) =
    ( updateView msg view, updateModel msg model )



-- VIEW


render : DayPlan -> Html Msg
render ( view, model ) =
    Html.div [ Html.Attributes.class ("dayplan " ++ model.color) ]
        [ Html.div [ Html.Attributes.class "dayplan__header" ]
            [ Html.h3
                [ Html.Attributes.class "dayplan__title", onDoubleClick ToggleIsEditingTitle ]
                [ if view.isEditingTitle then
                    Html.input
                        [ Html.Attributes.type_ "text"
                        , Html.Attributes.value model.title
                        , Html.Events.onInput SetTitle
                        , Html.Events.onBlur ToggleIsEditingTitle
                        ]
                        []

                  else
                    Html.text model.title
                , Html.button
                    [ onClick TogglePinning
                    , if model.isPinnedToTop then
                        Html.Attributes.class "pinned"

                      else
                        Html.Attributes.class "unpinned"
                    ]
                    []
                ]
            , Html.div [ Html.Attributes.class "dayplan__subtitle" ]
                [ Html.text " zuletzt benutzt: "
                , Html.text <| Date.format "dd.M.y" model.lastUsedAt
                ]
            ]
        , Html.div
            [ Html.Attributes.class "dayplan__main" ]
            (List.map renderTodos model.todos)
        , Html.div [ Html.Attributes.class "dayplan__footer" ]
            [ Html.button
                [ Html.Attributes.class "dayplan__color-picker-btn", onClick ToggleColorPicker ]
                [ colorPicker view.isColorPickerVisible ]
            ]
        ]


renderTodos : ToDo -> Html Msg
renderTodos todo =
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
