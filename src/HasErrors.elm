module HasErrors exposing (HasErrors, detailView, prependDecodeError, prependString)

import Errors exposing (Errors)
import Html.Styled exposing (Html)
import Json.Decode as JD


type alias HasErrors a =
    { a | errors : Errors }


prependString : String -> HasErrors a -> HasErrors a
prependString error =
    mapErrors (Errors.prependString error)


mapErrors : (Errors -> Errors) -> HasErrors a -> HasErrors a
mapErrors fn model =
    { model | errors = fn model.errors }


prependDecodeError : JD.Error -> HasErrors a -> HasErrors a
prependDecodeError error =
    mapErrors (Errors.prependDecodeError error)


detailView : HasErrors a -> Html msg
detailView { errors } =
    Errors.detailView errors
