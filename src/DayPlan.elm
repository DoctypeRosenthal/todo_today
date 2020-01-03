module DayPlan exposing (..)

import Element exposing (Color)
import Set exposing (Set)
import Time exposing (Date)
import ToDo exposing (ToDo)
import Util exposing (ID)



-- MODEL


type alias DayPlan =
    { id : ID -- for persistence
    , title : String
    , color : Color
    , createdAt : Date
    , lastUsedAt : Date
    , todos : Set ToDo
    }



-- UPDATE


type Msg
    = SetTitle String
    | SetColor Color
    | SetLastUsedAt Date
    | AddToDo
    | UpdateToDo ToDo.Msg ToDo
    | RemoveToDo ToDo


update : Msg -> DayPlan -> DayPlan
update msg dayPlan =
    dayPlan
