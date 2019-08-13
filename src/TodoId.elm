module TodoId exposing (TodoId, decoder, encoder)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


type alias TodoId =
    String


decoder : Decoder TodoId
decoder =
    JD.string


encoder : TodoId -> Value
encoder =
    JE.string
