module ToDo exposing (..)

import Element exposing (Color)
import Time exposing (FiveMinuteBasedTime)
import Util exposing (ID, Location)



-- MODEL


type alias ToDo =
    { id : ID
    , title : String
    , isDone : Bool
    , startTime : FiveMinuteBasedTime
    , duration : FiveMinuteBasedTime
    , location : Location
    , color : Color
    }



-- UPDATE


type Msg
    = SetTitle String
    | ToggleIsDone Bool
    | SetStartTime FiveMinuteBasedTime
    | SetDuration FiveMinuteBasedTime
    | SetLocation Location
    | SetColor Color


update : Msg -> ToDo -> ToDo
update msg todo =
    todo
