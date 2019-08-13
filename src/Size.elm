module Size exposing (Size, decoder, encoder, fromViewport, initial, onBrowserResize)

import Browser.Events
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)


type alias Size =
    { width : Int, height : Int }


initial : Size
initial =
    { width = 0, height = 0 }


decoder : Decoder Size
decoder =
    JD.succeed Size
        |> JDP.required "width" JD.int
        |> JDP.required "height" JD.int


encoder : Size -> Value
encoder { width, height } =
    JE.object
        [ ( "width", JE.int width )
        , ( "height", JE.int height )
        ]


onBrowserResize : (Size -> msg) -> Sub msg
onBrowserResize tagger =
    Browser.Events.onResize (\w h -> Size w h |> tagger)


fromViewport : { a | width : Float, height : Float } -> Size
fromViewport vp =
    Size (round vp.width) (round vp.height)
