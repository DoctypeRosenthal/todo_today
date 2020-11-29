module ToDo exposing (End, Msg(..), Start, ToDo, default, new, render, renderPreview, update)

import Element as Color exposing (Color)
import Html exposing (Html, div)
import Html.Attributes exposing (classList, style)
import Tick exposing (Tick)
import Util exposing (ID, Location)



-- MODEL


type alias Start =
    Tick


type alias End =
    Tick


type alias ToDo =
    { id : ID
    , title : String
    , isDone : Bool
    , interval : ( Start, End )
    , location : Location
    , color : Color
    }


new : ID -> Tick -> Location -> ToDo
new id startTick location =
    { id = id
    , title = "New ToDo"
    , isDone = False
    , interval = ( startTick, Tick.plus startTick <| Tick.fromInt 10 )
    , location = location
    , color = Color.rgb 255 255 255
    }


default : ToDo
default =
    { id = 0
    , title = "New ToDo"
    , isDone = False
    , interval = ( Tick.fromInt <| 12 * 12, Tick.fromInt <| 13 * 12 )
    , location = Util.location "Köln"
    , color = Color.rgb 255 255 255
    }



-- UPDATE


type Msg
    = SetTitle String
    | ToggleIsDone Bool
    | SetStartTime Tick
    | SetEndTime Tick
    | SetLocation Location
    | SetColor Color


update : Msg -> ToDo -> ToDo
update msg todo =
    case msg of
        SetTitle string ->
            { todo | title = string }

        ToggleIsDone bool ->
            { todo | isDone = bool }

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
render maybeActiveTick { interval, color, title } =
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
        , style "background" <| "blue"
        ]
        [ Html.text title ]
