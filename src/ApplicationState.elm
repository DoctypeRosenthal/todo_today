module ApplicationState exposing (..)

import DayPlan exposing (DayPlan)
import Time exposing (FiveMinuteBasedTime, Hour, TimeInMinutes)
import Util exposing (Location)



-- MODEL


type alias VisibleHourInterval =
    ( Hour, Hour )


type alias Zoom =
    VisibleHourInterval


type alias AppState =
    { home : Location
    , now : TimeInMinutes
    , zoom : Zoom
    , dayIsDone : FiveMinuteBasedTime
    , plans : List DayPlan
    }



-- UPDATE


type Msg
    = SetHome Location
    | UpdateNow TimeInMinutes
    | Zoom Zoom
    | SetDayIsDone FiveMinuteBasedTime
    | CreatePlan
    | LoadPlan DayPlan
    | RemovePlan DayPlan


update : Msg -> AppState -> AppState
update msg state =
    state
