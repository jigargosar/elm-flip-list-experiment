module AuthState exposing (AuthState(..), UID, User, decoder, encoder, initial, view)

import Html.Styled exposing (Html, div, text)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)


type alias UID =
    String


type alias User =
    { displayName : String
    , email : String
    , uid : UID
    }


userDecoder : Decoder User
userDecoder =
    JD.succeed User
        |> JDP.optional "displayName" JD.string ""
        |> JDP.optional "email" JD.string ""
        |> JDP.required "uid" JD.string


userEncoder : User -> Value
userEncoder { displayName, email, uid } =
    JE.object
        [ ( "displayName", JE.string displayName )
        , ( "email", JE.string email )
        , ( "uid", JE.string uid )
        ]


type AuthState
    = Unknown
    | SignedIn User
    | NotSignedIn


initial : AuthState
initial =
    Unknown


view : AuthState -> Html msg
view model =
    div [] [ text (toDebugString model) ]


toDebugString : AuthState -> String
toDebugString model =
    case model of
        Unknown ->
            "Unknown"

        SignedIn user ->
            "SignedIn"

        NotSignedIn ->
            "NotSignedIn"


decoder : Decoder AuthState
decoder =
    JD.oneOf
        [ JD.string
            |> JD.andThen
                (\str ->
                    case str of
                        "Unknown" ->
                            JD.succeed Unknown

                        "NotSignedIn" ->
                            JD.succeed NotSignedIn

                        _ ->
                            JD.fail ("Invalid AuthState String: " ++ str)
                )
        , JD.null NotSignedIn
        , userDecoder |> JD.map SignedIn
        ]


encoder : AuthState -> Value
encoder model =
    case model of
        Unknown ->
            JE.string "Unknown"

        SignedIn user ->
            userEncoder user

        NotSignedIn ->
            JE.string "NotSignedIn"
