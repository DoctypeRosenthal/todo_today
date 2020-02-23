module CustomTime exposing (..)

import Time


type alias Hour =
    Int


type alias Minute =
    Int


type FiveMinuteBasedTime
    = FiveMinuteBasedTime Minute


add : FiveMinuteBasedTime -> FiveMinuteBasedTime -> FiveMinuteBasedTime
add (FiveMinuteBasedTime a) (FiveMinuteBasedTime b) =
    FiveMinuteBasedTime (a + b)


ge : FiveMinuteBasedTime -> FiveMinuteBasedTime -> Bool
ge (FiveMinuteBasedTime a) (FiveMinuteBasedTime b) =
    a >= b


fiveMinutes : FiveMinuteBasedTime
fiveMinutes =
    FiveMinuteBasedTime 5


to5MinutesBasedDayTime : Time.Zone -> Time.Posix -> FiveMinuteBasedTime
to5MinutesBasedDayTime zone posix =
    let
        nowInMinutes =
            minutesSinceDayStart zone posix

        mod =
            modBy 5 nowInMinutes
    in
    FiveMinuteBasedTime <|
        if mod <= 2 then
            nowInMinutes - mod + 0

        else
            nowInMinutes - mod + 5


minutesSinceDayStart : Time.Zone -> Time.Posix -> Minute
minutesSinceDayStart zone posix =
    Time.toHour zone posix * 60 + Time.toMinute zone posix
