module Errors exposing (Errors, detailView, empty, fromStrings, mapErrorList, prependDecodeError, prependString, viewError)

import Html.Styled exposing (Html, div, li, ol, text)
import Html.Styled.Attributes exposing (class)
import HtmlStyledExtra
import Json.Decode as JD


type alias Error =
    String


type Errors
    = Errors (List Error)


empty : Errors
empty =
    [] |> fromStrings


fromStrings : List String -> Errors
fromStrings errors =
    errors |> Errors


prependString : String -> Errors -> Errors
prependString error =
    mapErrorList (\errors -> error :: errors)


mapErrorList : (List Error -> List Error) -> Errors -> Errors
mapErrorList fn (Errors errors) =
    fn errors |> Errors


prependDecodeError : JD.Error -> Errors -> Errors
prependDecodeError error =
    prependString (JD.errorToString error)


detailView : Errors -> Html msg
detailView (Errors errors) =
    HtmlStyledExtra.viewUnless (errors |> List.isEmpty) <|
        div [ class "vs3" ]
            [ div [ class "ttu tracked" ] [ text "Errors:" ]
            , ol [ class "vs3" ] (List.map viewError errors)
            ]


viewError : Error -> Html msg
viewError error =
    li [] [ text error ]
