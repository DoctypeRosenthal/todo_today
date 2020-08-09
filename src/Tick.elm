module Tick exposing (..)

import Time


type alias Hour =
    Int


type alias Minute =
    Int



-- The smallest time unit used by the App


type Tick
    = Tick Minute


add : Tick -> Tick -> Tick
add (Tick a) (Tick b) =
    Tick (a + b)


ge : Tick -> Tick -> Bool
ge (Tick a) (Tick b) =
    a >= b


base : Int
base =
    5


multipleOfBase : Int -> Int
multipleOfBase int =
    int - remainderBy base int


fromInt : Int -> Tick
fromInt =
    Tick << multipleOfBase


oneTick : Tick
oneTick =
    Tick base


fromPosix : Time.Zone -> Time.Posix -> Tick
fromPosix zone posix =
    let
        nowInMinutes =
            minutesSinceDayStart zone posix

        mod =
            modBy base nowInMinutes
    in
    Tick <|
        if mod <= 2 then
            nowInMinutes - mod + 0

        else
            nowInMinutes - mod + base


minutesSinceDayStart : Time.Zone -> Time.Posix -> Minute
minutesSinceDayStart zone posix =
    Time.toHour zone posix * 60 + Time.toMinute zone posix
