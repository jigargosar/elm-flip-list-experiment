module FlipItem exposing (FlipItem, Id, fetch, viewAnimatingKeyed)

import Css
import Html.Styled exposing (Html, button, div, text)
import Html.Styled.Attributes as A exposing (class, css)
import Html.Styled.Events exposing (on, onClick)
import Html.Styled.Keyed as K
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP


type alias Id =
    String


type alias Idx =
    Int


type alias FlipItem =
    { id : Id
    , idx : Idx
    , title : String
    , done : Bool
    }


decoder : Decoder FlipItem
decoder =
    JD.succeed FlipItem
        |> JDP.required "id" (JD.int |> JD.map String.fromInt)
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


viewAnimatingKeyed : String -> Css.Style -> FlipItem -> ( String, Html msg )
viewAnimatingKeyed domId animCss fi =
    ( fi.id
    , div
        [ class "absolute bg-black-80 white ba br-pill lh-copy pv1"
        , class "ph3"
        , A.id domId

        --        , class "fixed"
        , css
            [ animCss
            ]
        ]
        [ text <| fi.id ++ ": " ++ fi.title ]
    )
