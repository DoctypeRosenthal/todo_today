module Tick exposing (..)

import CustomTime as Time exposing (Minute)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Html.Events.Extra.Pointer as Pointer exposing (Event)
import Time



-- The smallest time unit used by the App


type Tick
    = Tick Minute


plus : Tick -> Tick -> Tick
plus (Tick a) (Tick b) =
    Tick (a + b)


minus : Tick -> Tick -> Tick
minus (Tick a) (Tick b) =
    Tick (a - b)


ge : Tick -> Tick -> Bool
ge (Tick a) (Tick b) =
    a >= b


le : Tick -> Tick -> Bool
le (Tick a) (Tick b) =
    a <= b


intervalLen : ( Tick, Tick ) -> Tick
intervalLen ( start, end ) =
    minus end start


within : Tick -> Tick -> Tick -> Bool
within lower upper x =
    le lower x && le x upper


toString : Tick -> String
toString (Tick t) =
    String.fromInt t


base : Int
base =
    5


multipleOfBase : Int -> Int
multipleOfBase int =
    int - remainderBy base int


fromInt : Int -> Tick
fromInt =
    Tick << multipleOfBase


single : Tick
single =
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


index : Tick -> Int
index (Tick int) =
    int // base


toInt : Tick -> Int
toInt (Tick int) =
    int


float : Tick -> Float
float =
    toFloat << toInt


minutesSinceDayStart : Time.Zone -> Time.Posix -> Minute
minutesSinceDayStart zone posix =
    Time.toHour zone posix * 60 + Time.toMinute zone posix


minutesPerHour =
    60


dayStartsAt =
    8


wokeHours =
    15


ticksPerHour =
    12


numberOfTicks =
    wokeHours * ticksPerHour + 1


fullDay =
    List.range 0 numberOfTicks
        |> List.map ((*) 5 >> fromInt)



-- VIEW


render : (Maybe Tick -> msg) -> Tick -> Float -> Html msg
render onHover tick scalar =
    let
        myTime =
            float tick / minutesPerHour + dayStartsAt

        isFullHour =
            (toFloat << round) myTime == myTime

        tickNumberView =
            if not isFullHour then
                []

            else
                [ div [ class "tick__number" ] [ Html.text <| String.fromFloat myTime ]
                ]

        tickLineCssValue =
            String.fromFloat
                (if isFullHour then
                    1

                 else
                    scalar
                )

        tickLine =
            div
                [ class "tick__line"
                , style "opacity" tickLineCssValue
                , style "transform" <| "scaleX(" ++ tickLineCssValue ++ ")"
                ]
                []
    in
    div
        [ Html.Attributes.class "tick"
        , Pointer.onOver (always (Just tick) >> onHover)

        --, onClick ClickOnActiveTick
        ]
        (tickNumberView ++ [ tickLine ])
