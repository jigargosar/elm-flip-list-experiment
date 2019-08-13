module HtmlStyledExtra exposing (empty, viewIf, viewMaybe, viewUnless)

-- VIEW HELPERS

import Html.Styled exposing (Html, text)
import Maybe.Extra


viewIf bool v =
    if bool then
        v

    else
        text ""


viewUnless bool v =
    viewIf (not bool) v


empty : Html msg
empty =
    text ""


viewMaybe : (a -> Html msg) -> Maybe a -> Html msg
viewMaybe fn =
    Maybe.Extra.unwrap empty fn
