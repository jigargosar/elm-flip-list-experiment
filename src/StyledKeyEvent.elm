module StyledKeyEvent exposing (onEnter)

import Html.Styled exposing (Attribute)
import Html.Styled.Events
import Json.Decode as JD


onEnter : a -> Attribute a
onEnter tagger =
    Html.Styled.Events.on "keydown"
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
