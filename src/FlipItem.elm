module FlipItem exposing (FlipItem, Id, fetch, strId)

import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP


type alias Id =
    Int


type alias FlipItem =
    { id : Id
    , title : String
    , done : Bool
    }


decoder : Decoder FlipItem
decoder =
    JD.succeed FlipItem
        |> JDP.required "id" JD.int
        |> JDP.required "title" JD.string
        |> JDP.required "completed" JD.bool


listDecoder : Decoder (List FlipItem)
listDecoder =
    JD.list decoder


type alias HttpResult a =
    Result Http.Error a


fetch : (HttpResult (List FlipItem) -> msg) -> Cmd msg
fetch tagger =
    Http.get
        { url = "http://jsonplaceholder.typicode.com/todos"
        , expect = Http.expectJson tagger listDecoder
        }


strId fi =
    fi.id
        |> String.fromInt
