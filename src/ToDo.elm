module ToDo exposing (End, Msg(..), Start, ToDo, new, update, view)

import Element as Color exposing (Color)
import FiveMinutBasedTime exposing (FiveMinuteBasedTime, fiveMinutes)
import Html exposing (Html)
import Util exposing (ID, Location)



-- MODEL


type alias Start =
    FiveMinuteBasedTime


type alias End =
    FiveMinuteBasedTime


type alias ToDo =
    { id : ID
    , title : String
    , isDone : Bool
    , interval : ( Start, End )
    , location : Location
    , color : Color
    }


new : ID -> FiveMinuteBasedTime -> Location -> ToDo
new id startTime location =
    { id = id
    , title = "New ToDo"
    , isDone = False
    , interval = ( startTime, FiveMinutBasedTime.add startTime fiveMinutes )
    , location = location
    , color = Color.rgb 255 255 255
    }



-- UPDATE


type Msg
    = SetTitle String
    | ToggleIsDone Bool
    | SetStartTime FiveMinuteBasedTime
    | SetEndTime FiveMinuteBasedTime
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
                    if FiveMinutBasedTime.ge nextStart end then
                        ( nextStart, FiveMinutBasedTime.add nextStart fiveMinutes )

                    else
                        ( nextStart, end )
            in
            { todo | interval = nextInterval }

        SetEndTime nextEnd ->
            let
                ( start, end ) =
                    todo.interval
            in
            if FiveMinutBasedTime.ge start end then
                -- ending cannot be earlier than start
                todo

            else
                { todo | interval = ( start, nextEnd ) }

        SetLocation location ->
            { todo | location = location }

        SetColor color ->
            { todo | color = color }



-- VIEW


view : ToDo -> Html Msg
view toDo =
    Html.div [] [ Html.text "a default todo" ]
