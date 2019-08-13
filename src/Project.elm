module Project exposing
    ( Project
    , ProjectList
    , decoder
    , encoder
    , filterActive
    , listDecoder
    , listEncoder
    , new
    , setModifiedAt
    )

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JD
import Json.Encode as JE exposing (Value)
import Millis exposing (Millis)
import ProjectId exposing (ProjectId)


type alias Project =
    { id : ProjectId
    , title : String
    , sortIdx : Int
    , deleted : Bool
    , createdAt : Int
    , modifiedAt : Int
    }


type alias ProjectList =
    List Project


decoder : Decoder Project
decoder =
    JD.succeed Project
        |> JD.required "id" JD.string
        |> JD.required "title" JD.string
        |> JD.required "sortIdx" JD.int
        |> JD.optional "deleted" JD.bool False
        |> JD.required "createdAt" JD.int
        |> JD.required "modifiedAt" JD.int


listDecoder : Decoder ProjectList
listDecoder =
    JD.list decoder


encoder : Project -> Value
encoder { id, title, sortIdx, deleted, createdAt, modifiedAt } =
    JE.object
        [ ( "id", JE.string id )
        , ( "title", JE.string title )
        , ( "sortIdx", JE.int sortIdx )
        , ( "deleted", JE.bool deleted )
        , ( "createdAt", JE.int createdAt )
        , ( "modifiedAt", JE.int modifiedAt )
        ]


new : Millis -> Value
new now =
    { id = ""
    , title = ""
    , sortIdx = 0
    , deleted = False
    , createdAt = now
    , modifiedAt = now
    }
        |> encoder


setModifiedAt now todo =
    { todo | modifiedAt = now }


listEncoder : ProjectList -> Value
listEncoder =
    JE.list encoder


filterActive =
    List.filter (.deleted >> not)
