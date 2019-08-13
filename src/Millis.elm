module Millis exposing (Millis, formatDate, hereCmd, nowCmd)

import Date
import Task
import Time


type alias Millis =
    Int


nowCmd : (Millis -> msg) -> Cmd msg
nowCmd f =
    Time.now |> Task.map Time.posixToMillis |> Task.perform f


hereCmd : (Time.Zone -> msg) -> Cmd msg
hereCmd f =
    Time.here |> Task.perform f


formatDate : String -> Time.Zone -> Millis -> String
formatDate fmtStr zone millis =
    millis
        |> Time.millisToPosix
        |> Date.fromPosix zone
        |> Date.format fmtStr
