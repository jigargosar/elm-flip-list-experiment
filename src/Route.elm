module Route exposing (Route(..), fromUrl, toFlipDemoUrl)

import Url exposing (Url)
import Url.Builder as B
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string, top)


type Route
    = NotFound Url
    | FlipDemo


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map FlipDemo top
        , map FlipDemo (s "flip-demo")
        ]


fromUrl : Url -> Route
fromUrl url =
    parse routeParser url
        |> Maybe.withDefault (NotFound url)


toFlipDemoUrl : String
toFlipDemoUrl =
    B.absolute [ "flip-demo" ] []
