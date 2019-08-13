module Icons exposing (RGBA, circleOutline)

import Html.Styled exposing (Html)
import Svg.Styled exposing (Svg)
import Svg.Styled.Attributes as A


circleOutline : Int -> RGBA -> Html msg
circleOutline =
    p "M256,48C141.1,48,48,141.1,48,256s93.1,208,208,208c114.9,0,208-93.1,208-208S370.9,48,256,48zM256,446.7c-105.1,0-190.7-85.5-190.7-190.7c0-105.1,85.5-190.7,190.7-190.7c105.1,0,190.7,85.5,190.7,190.7C446.7,361.1,361.1,446.7,256,446.7z"


svg : Int -> List (Svg msg) -> Html msg
svg size =
    Svg.Styled.svg
        [ A.version "1.1"
        , A.x "0px"
        , A.y "0px"
        , A.width (String.fromInt size)
        , A.height (String.fromInt size)
        , A.viewBox "0 0 512 512"
        , A.enableBackground "new 0 0 512 512"
        ]


p : String -> Int -> RGBA -> Html msg
p d size color =
    svg size
        [ Svg.Styled.path
            [ A.d d
            , A.fill (fill color)
            ]
            []
        ]


fill : RGBA -> String
fill { red, green, blue, alpha } =
    let
        ( colorSpace, values ) =
            if 0 <= alpha && alpha < 1 then
                ( "rgba"
                , [ red |> toColorString
                  , green |> toColorString
                  , blue |> toColorString
                  , alpha |> toAlphaString
                  ]
                )

            else
                ( "rgb"
                , [ red |> toColorString
                  , green |> toColorString
                  , blue |> toColorString
                  ]
                )
    in
    colorSpace ++ "(" ++ String.join "," values ++ ")"


toColorString : Float -> String
toColorString value =
    value
        |> (*) 255
        |> clamp 0 255
        |> String.fromFloat
        |> String.left 5


toAlphaString : Float -> String
toAlphaString value =
    value
        |> clamp 0 1
        |> String.fromFloat
        |> String.left 5


type alias RGBA =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }
