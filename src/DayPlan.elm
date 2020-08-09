module DayPlan exposing (DayPlan, Msg(..), Now, default, new, render, renderEditor, update)

import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Tick exposing (fromPosix)
import Time exposing (Month(..))
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
    , isEditing : Bool
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
      , isEditing = False
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


default : DayPlan
default =
    ( { isEditingTitle = False
      , isColorPickerVisible = False
      , isEditing = True
      }
    , { id = 0
      , title = "Test Dayplan"
      , color = "yellow"
      , createdAt = Date.fromCalendarDate 18 Sep 20
      , lastUsedAt = Date.fromCalendarDate 18 Sep 20
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
    | AddToDo ToDo
    | UpdateToDo ToDo ToDo.Msg
    | RemoveToDo ToDo
    | ToggleIsEditingTitle
    | ToggleColorPicker
    | ToggleEditing Bool
    | RemoveMe


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

        AddToDo newTodo ->
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

        ToggleEditing isEditing ->
            { view | isEditing = isEditing }

        _ ->
            view


update : Msg -> DayPlan -> DayPlan
update msg ( view, model ) =
    ( updateView msg view, updateModel msg model )



-- VIEW


render : DayPlan -> Html Msg
render (( view, model ) as plan) =
    if view.isEditing then
        renderEditor plan

    else
        renderPreview plan


renderPreview : DayPlan -> Html Msg
renderPreview ( view, model ) =
    Html.div [ Html.Attributes.class ("dayplan " ++ model.color) ]
        [ Html.div [ Html.Attributes.class "dayplan__header" ]
            [ Html.h3
                [ Html.Attributes.class "dayplan__title", onDoubleClick ToggleIsEditingTitle ]
                (if view.isEditingTitle then
                    [ Html.input
                        [ Html.Attributes.type_ "text"
                        , Html.Attributes.value model.title
                        , Html.Events.onInput SetTitle
                        , Html.Events.onBlur ToggleIsEditingTitle
                        ]
                        []
                    ]

                 else
                    [ Html.text model.title
                    , Html.button
                        [ onClick TogglePinning
                        , if model.isPinnedToTop then
                            Html.Attributes.class "pinned"

                          else
                            Html.Attributes.class "unpinned"
                        ]
                        []
                    ]
                )
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
            , Html.button
                [ Html.Attributes.class "dayplan__delete-btn", onClick RemoveMe ]
                []
            ]
        ]



-- TODO: Maybe this can be integrated into the main render function?


renderEditor : DayPlan -> Html Msg
renderEditor ( view, model ) =
    Html.div [ Html.Attributes.class "editor" ]
        [ Html.div [ Html.Attributes.class "editor__background" ] []
        , Html.div [ Html.Attributes.class "editor__inner" ]
            [ Html.div [ Html.Attributes.class "editor__top-bar" ]
                [ Html.b [ Html.Attributes.class "editor__title" ] [ Html.text model.title ]
                , Html.button [ Html.Attributes.class "editor__close" ] [ Html.text "Schließen" ]
                ]
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
