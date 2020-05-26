module Force exposing (Force, new, value)

import Ease exposing (inCubic)
import Time exposing (Posix)


type alias Force =
    { startTime : Posix
    , endTime : Posix
    }


new : Posix -> Force
new posix =
    { startTime = posix
    , endTime = Time.millisToPosix (Time.posixToMillis posix + 250)
    }


value : Posix -> Force -> Float
value posix force =
    if Time.posixToMillis posix <= Time.posixToMillis force.startTime then
        inCubic 0.0

    else if Time.posixToMillis posix >= Time.posixToMillis force.endTime then
        inCubic 1.0

    else
        inCubic
            (toFloat (Time.posixToMillis posix - Time.posixToMillis force.startTime)
                / toFloat (Time.posixToMillis force.endTime - Time.posixToMillis force.startTime)
            )
