module HtmlStyledEvent exposing (onDomIdClicked, targetDomIdDecoder, targetDomIdEqDecoder)

import Html.Styled.Events exposing (on)
import Json.Decode as JD exposing (Decoder)


targetDomIdDecoder : Decoder String
targetDomIdDecoder =
    JD.at [ "target", "id" ] JD.string


targetDomIdEqDecoder : String -> a -> Decoder a
targetDomIdEqDecoder domId tagger =
    targetDomIdDecoder
        |> JD.andThen
            (\targetDomId ->
                if targetDomId == domId then
                    JD.succeed tagger

                else
                    JD.fail ("Not Clicked on:" ++ domId)
            )


onDomIdClicked targetDomId tagger =
    on "click" (targetDomIdEqDecoder targetDomId tagger)
