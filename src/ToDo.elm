module ToDo exposing (End, Msg(..), Start, ToDo, default, new, render, renderPreview, update)

import Color
import Element as Color exposing (Color)
import Html exposing (Html, div, iframe)
import Html.Attributes exposing (class, classList, src, style, type_)
import Html.Events exposing (onClick)
import Tick exposing (Tick)
import Util exposing (ID, Location)



-- MODEL


type alias Start =
    Tick


type alias End =
    Tick


type alias ToDo =
    { title : String
    , isDone : Bool
    , interval : ( Start, End )
    , location : Location
    , color : Color
    }


new : Tick -> Location -> ToDo
new startTick location =
    { title = "New ToDo"
    , isDone = False
    , interval = ( startTick, Tick.plus startTick <| Tick.fromInt 10 )
    , location = location
    , color = Color.rgb 3 169 244
    }


default : ToDo
default =
    { title = "New ToDo"
    , isDone = True
    , interval = ( Tick.fromInt <| 12 * 12, Tick.fromInt <| 13 * 12 )
    , location = Util.location "Köln"
    , color = Color.rgb 3 169 244
    }



-- UPDATE


type Msg
    = SetTitle String
    | ToggleIsDone
    | SetStartTime Tick
    | SetEndTime Tick
    | SetLocation Location
    | SetColor Color


update : Msg -> ToDo -> ToDo
update msg todo =
    case msg of
        SetTitle string ->
            { todo | title = string }

        ToggleIsDone ->
            { todo | isDone = not todo.isDone }

        SetStartTime nextStart ->
            let
                ( _, end ) =
                    todo.interval

                nextInterval =
                    if Tick.ge nextStart end then
                        ( nextStart, Tick.plus nextStart Tick.single )

                    else
                        ( nextStart, end )
            in
            { todo | interval = nextInterval }

        SetEndTime nextEnd ->
            let
                ( start, end ) =
                    todo.interval
            in
            if Tick.ge start end then
                -- ending cannot be earlier than start
                todo

            else
                { todo | interval = ( start, nextEnd ) }

        SetLocation location ->
            { todo | location = location }

        SetColor color ->
            { todo | color = color }



-- VIEW


renderPreview : ToDo -> Html Msg
renderPreview toDo =
    Html.div [] [ Html.text "a default todo" ]


render : Maybe Tick -> ToDo -> Html Msg
render maybeActiveTick { isDone, interval, color, title } =
    let
        ( startTick, endTick ) =
            interval

        isHovered =
            case maybeActiveTick of
                Just activeTick ->
                    Tick.within startTick endTick (Tick.plus activeTick Tick.single)

                Nothing ->
                    False

        tickIndexStr =
            String.fromInt << Tick.index
    in
    div
        [ classList [ ( "todo", True ), ( "todo--hover", isHovered ) ]
        , style "grid-row" (tickIndexStr startTick ++ " / " ++ tickIndexStr endTick)
        , style "background" <| Color.toStr color
        , onClick ToggleIsDone
        ]
        [ Html.label
            [ classList [ ( "checkbox", True ), ( "checkbox--checked", isDone ) ], onClick ToggleIsDone ]
            [ Html.input [ type_ "checkbox" ] []
            , Html.div
                [ class "checkbox__icon"
                , style "color" <| Color.toStr color
                ]
                []
            ]
        , Html.span [ class "todo__time" ] [ Html.text "11:45" ]
        , Html.text title
        , todoDetailCardView
        ]


todoDetailCardView : Html Msg
todoDetailCardView =
    Html.div
        [ class "todo-detail-card" ]
        [ Html.div [ class "todo-detail-card__map" ]
            [ iframe [ src "https://maps.google.com/maps?q=K%C3%B6ln&t=&z=13&ie=UTF8&iwloc=&output=embed" ] []
            ]
        , Html.div [ class "todo-detail-card__actions" ]
            [ Html.button [ class "btn btn--text btn--color-picker" ] []
            , Html.button [ class "btn btn--text btn--trash" ] []
            ]
        , Html.h2 [ class "todo-detail-card__title" ] [ Html.div [ class "todo-detail-card__" ] [], Html.text "LALALA" ]
        , Html.div [ class "todo-detail-card__location" ] [ Html.text "Köln" ]
        , Html.div [ class "todo-detail-card__time" ] [ Html.text "11:50 - 12:30" ]
        , Html.div [ class "todo-detail-card__description" ] [ Html.text "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet." ]
        , Html.button [ class "btn btn--check" ] [ Html.text "ERLEDIGT" ]
        ]
