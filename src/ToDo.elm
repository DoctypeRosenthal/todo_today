module ToDo exposing (End, Msg(..), Start, ToDo, new, update, view)

import Element as Color exposing (Color)
import Html exposing (Html)
import Tick as Time exposing (Tick, oneTick)
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
new id startTime location =
    { id = id
    , title = "New ToDo"
    , isDone = False
    , interval = ( startTime, Time.add startTime <| Time.fromInt 10 )
    , location = location
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
                    if Time.ge nextStart end then
                        ( nextStart, Time.add nextStart oneTick )

                    else
                        ( nextStart, end )
            in
            { todo | interval = nextInterval }

        SetEndTime nextEnd ->
            let
                ( start, end ) =
                    todo.interval
            in
            if Time.ge start end then
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
