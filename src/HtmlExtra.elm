module HtmlExtra exposing (empty, viewIf, viewUnless)

-- VIEW HELPERS

import Html exposing (Html, text)


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
