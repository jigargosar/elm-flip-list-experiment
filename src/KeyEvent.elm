module KeyEvent exposing (onEnter)

import Html exposing (Attribute)
import Html.Events
import Json.Decode as JD


onEnter : a -> Attribute a
onEnter tagger =
    Html.Events.on "keydown"
        (JD.field "key" JD.string
            |> JD.andThen
                (\key ->
                    case key of
                        "Enter" ->
                            JD.succeed tagger

                        _ ->
                            JD.fail "Not Interested"
                )
        )
