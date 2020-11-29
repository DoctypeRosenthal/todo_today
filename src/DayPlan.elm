module DayPlan exposing (DayPlan, Msg(..), Now, default, new, render, renderEditor, update)

import Date exposing (Date)
import Html exposing (Html, div, h1)
import Html.Attributes as Attr exposing (class, style)
import Html.Events as Event exposing (onBlur, onClick, onDoubleClick, onInput)
import Html.Events.Extra.Pointer as Pointer
import Tick exposing (Tick)
import Time exposing (Month(..))
import ToDo exposing (ToDo)
import Util exposing (ID, Location, getNextId, onlyUpdateX)



-- MODEL


type alias Model =
    { title : String
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
    , activeTick : Maybe Tick
    }


type alias DayPlan =
    ( View, Model )


new : Time.Zone -> Time.Posix -> String -> DayPlan
new timeZone posix title =
    let
        today =
            Date.fromPosix timeZone posix
    in
    ( { isEditingTitle = False
      , isColorPickerVisible = False
      , isEditing = False
      , activeTick = Nothing
      }
    , { title = title
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
      , activeTick = Nothing
      }
    , { title = "Test Dayplan"
      , color = "yellow"
      , createdAt = Date.fromCalendarDate 18 Sep 20
      , lastUsedAt = Date.fromCalendarDate 18 Sep 20
      , todos = [ ToDo.default ]
      , isPinnedToTop = False
      }
    )



-- MSG


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
    | SetActiveTick (Maybe Tick)
    | ClickOnActiveTick
    | RemoveMe
    | NoOp



-- UPDATE


type alias Now =
    Time.Posix


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

        SetActiveTick maybeTick ->
            { view | activeTick = maybeTick }

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
    div [ class ("dayplan " ++ model.color) ]
        [ div [ class "dayplan__header" ]
            [ Html.h3
                [ class "dayplan__title"
                , onDoubleClick <|
                    if view.isEditingTitle then
                        NoOp

                    else
                        ToggleIsEditingTitle
                ]
                (if view.isEditingTitle then
                    [ Html.input
                        [ Attr.type_ "text"
                        , Attr.value model.title
                        , onInput SetTitle
                        , onBlur ToggleIsEditingTitle
                        ]
                        []
                    ]

                 else
                    [ Html.text model.title
                    , Html.button
                        [ onClick TogglePinning
                        , if model.isPinnedToTop then
                            class "pinned"

                          else
                            class "unpinned"
                        ]
                        []
                    ]
                )
            , div [ class "dayplan__subtitle" ]
                [ Html.text " zuletzt benutzt: "
                , Html.text <| Date.format "dd.M.y" model.lastUsedAt
                ]
            ]
        , div
            [ class "dayplan__main" ]
            (List.map renderTodosPreview model.todos)
        , div [ class "dayplan__footer" ]
            [ Html.button
                [ class "dayplan__color-picker-btn", onClick ToggleColorPicker ]
                [ colorPicker view.isColorPickerVisible ]
            , Html.button
                [ class "dayplan__delete-btn", onClick RemoveMe ]
                []
            ]
        ]



-- EDITOR
-- TODO: Maybe this can be integrated into the main render function?


getCompleteWorkingTime : List ToDo -> Float
getCompleteWorkingTime toDos =
    toDos
        |> List.map (.interval >> Tick.intervalLen >> Tick.float)
        |> List.sum


renderAmountWorkingTime : List ToDo -> Html Msg
renderAmountWorkingTime toDos =
    let
        workingTime =
            String.fromFloat <| getCompleteWorkingTime toDos / 60.0
    in
    div [ class "progress-bar" ]
        [ div
            [ class "progress-bar__progress"
            , style "width" (workingTime ++ "%")
            ]
            [ Html.text <| workingTime ++ "h" ]
        ]


renderEditor : DayPlan -> Html Msg
renderEditor (( view, model ) as plan) =
    let
        listLenStr =
            String.fromInt << List.length

        totalTodoCount =
            listLenStr model.todos

        doneTodoCount =
            (listLenStr << List.filter .isDone) model.todos
    in
    div [ class "editor" ]
        [ div [ class "editor__background" ] []
        , div [ class "editor__inner" ]
            [ div [ class "editor__sidebar" ]
                [ Html.h3
                    [ class "editor__close-btn", onClick NoOp ]
                    []
                , Html.h3
                    [ class "editor__title"
                    , onDoubleClick <|
                        if view.isEditingTitle then
                            NoOp

                        else
                            ToggleIsEditingTitle
                    ]
                    (if view.isEditingTitle then
                        [ Html.input
                            [ Attr.type_ "text"
                            , Attr.autofocus True
                            , Attr.value model.title
                            , onInput SetTitle
                            , onBlur ToggleIsEditingTitle
                            ]
                            []
                        ]

                     else
                        [ Html.text model.title
                        ]
                    )
                , div [ class "dayplan__subtitle" ]
                    [ Html.text " zuletzt benutzt: "
                    , Html.text <| Date.format "dd.M.y" model.lastUsedAt
                    ]
                , div [ class "dayplan__progress" ]
                    [ Html.div [] [ Html.text "Arbeitszeit:" ]
                    , renderAmountWorkingTime model.todos
                    , Html.text <| totalTodoCount ++ " ToDos, " ++ doneTodoCount ++ " erledigt"
                    ]
                , Html.button [ class "editor__delete-btn", onClick RemoveMe ] [ Html.text "Löschen" ]
                ]
            , renderTimeline view.activeTick model.todos
            ]
        ]


renderTodosPreview : ToDo -> Html Msg
renderTodosPreview todo =
    Html.map (UpdateToDo todo) (ToDo.renderPreview todo)


colorPicker : Bool -> Html Msg
colorPicker isVisible =
    div
        [ class
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
        [ class colorClassName, onClick <| SetColor colorClassName ]
        []



-- TIMELINE


g : Float -> Float
g number =
    if number >= 0 then
        number

    else
        0


f : Int -> Int -> Float
f activeTick x =
    -0.0000001 * toFloat (x - activeTick) ^ 4 + 1


zoomTicks : Maybe Tick -> List Float
zoomTicks activeTick =
    case activeTick of
        Just tick ->
            List.map (Tick.toInt >> f (Tick.toInt tick) >> g) Tick.fullDay

        Nothing ->
            List.map (always 0) Tick.fullDay


todoView : Maybe Tick -> ToDo -> Html Msg
todoView activeTick todo =
    Html.map (UpdateToDo todo) (ToDo.render activeTick todo)


renderTimeline : Maybe Tick -> List ToDo -> Html Msg
renderTimeline activeTick todos =
    let
        ticksZooms =
            zoomTicks activeTick

        numberOfTicks =
            toFloat <| List.length ticksZooms

        tickHeights : List Float
        tickHeights =
            List.map (\scalar -> 100.0 / numberOfTicks + scalar) ticksZooms

        renderedTicks =
            List.map2 (Tick.render SetActiveTick) Tick.fullDay ticksZooms

        separator =
            div
                [ class "timeline__separator"
                , style "grid-row" ("1 / " ++ String.fromFloat numberOfTicks)
                ]
                []
    in
    div
        [ class "timeline"
        , style "grid-template-rows" (String.join " " <| List.map (\x -> String.fromFloat x ++ "%") tickHeights)

        --, Pointer.onOut (always Nothing >> SetActiveTick)
        ]
        (renderedTicks ++ [ separator ] ++ List.map (todoView activeTick) todos)
