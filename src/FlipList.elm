module FlipList exposing (FlipList, Msg, empty, init, subscriptions, update, view)

import BasicsExtra exposing (callWith)
import Browser.Dom as Dom exposing (Element)
import Browser.Events
import Css exposing (fixed, height, left, position, px, top, width)
import Css.Transitions as Transitions exposing (transition)
import Dict exposing (Dict)
import FlipItem exposing (FlipItem)
import Html.Styled exposing (Html, button, div, text)
import Html.Styled.Attributes as A exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as K
import Http
import Maybe.Extra
import Random
import Random.List
import Result exposing (Result)
import Result.Extra
import Task exposing (Task)
import UpdateExtra exposing (pure)


type alias Lists =
    { from : List FlipItem, to : List FlipItem }


type alias Measurement =
    { from : FIClientRectById
    , to : FIClientRectById
    }


type FlipList
    = Initial (List FlipItem)
    | Measuring Lists
    | Starting Lists Measurement
    | Animating Lists Measurement


type alias HttpResult a =
    Result Http.Error a


type Msg
    = NoOp
    | GotFlipItems (HttpResult (List FlipItem))
    | OnShuffle
    | GotRandomShuffled (List FlipItem)
    | GotMeasurement (Result Dom.Error Measurement)
    | OnPlay


empty : FlipList
empty =
    Initial []


init : ( FlipList, Cmd Msg )
init =
    ( empty
    , FlipItem.fetch GotFlipItems
    )


type alias Return =
    ( FlipList, Cmd Msg )


subscriptions : FlipList -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model of
            Starting _ _ ->
                Browser.Events.onAnimationFrame (\_ -> OnPlay)

            _ ->
                Sub.none
        ]


update : Msg -> FlipList -> Return
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotFlipItems res ->
            res
                |> Result.Extra.unpack onHttpError onGotFIList
                |> callWith model

        OnShuffle ->
            onShuffle model

        GotRandomShuffled fl ->
            onGotShuffled fl model

        GotMeasurement res ->
            res
                |> Result.Extra.unpack onDomError onGotMeasurement
                |> callWith model

        OnPlay ->
            case model of
                Starting lists measurement ->
                    pure (Animating lists measurement)

                _ ->
                    pure model


type alias ClientRect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias FIClientRect =
    ( FlipItem.Id, ClientRect )


type alias FIClientRectById =
    Dict FlipItem.Id ClientRect


getFIClientRect : String -> FlipItem -> Task Dom.Error FIClientRect
getFIClientRect idPrefix fi =
    let
        domId =
            idPrefix ++ "-" ++ FlipItem.strId fi
    in
    Dom.getElement domId
        |> Task.map (\el -> ( fi.id, el.element ))


getFIClientRectById : String -> List FlipItem -> Task Dom.Error FIClientRectById
getFIClientRectById idPrefix fiList =
    fiList
        |> List.map (getFIClientRect idPrefix)
        |> Task.sequence
        |> Task.map Dict.fromList


onGotShuffled shuffled model =
    case model of
        Initial fl ->
            let
                from =
                    fl

                to =
                    shuffled

                flipDomInfoTask =
                    Task.map2 Measurement
                        (getFIClientRectById "from" from)
                        (getFIClientRectById "to" to)
            in
            ( Measuring (Lists from to)
            , flipDomInfoTask |> Task.attempt GotMeasurement
            )

        _ ->
            pure model


onGotMeasurement measurement model =
    case model of
        Measuring ls ->
            ( Starting ls measurement, Cmd.none )

        _ ->
            pure model


onShuffle : FlipList -> Return
onShuffle model =
    case model of
        Initial fl ->
            ( model
            , Random.List.shuffle fl
                |> Random.generate GotRandomShuffled
            )

        _ ->
            pure model


onHttpError : Http.Error -> FlipList -> Return
onHttpError err =
    let
        _ =
            Debug.log "HTTP Err" err
    in
    pure


onDomError : Dom.Error -> FlipList -> Return
onDomError err =
    let
        _ =
            Debug.log "Dom Err" err
    in
    pure


onGotFIList : List FlipItem -> FlipList -> Return
onGotFIList fiList _ =
    fiList
        |> List.take 10
        |> Initial
        |> pure


view : FlipList -> Html Msg
view model =
    div [ class "measure-wide center vs3" ]
        [ div [ class "pv1 b " ] [ text "FlipListDemo" ]
        , div [ class "flex hs3" ]
            [ button [ onClick OnShuffle ] [ text "Shuffle" ]
            ]
        , div [ class "relative" ] (viewList model)
        ]


viewList model =
    case model of
        Initial fl ->
            [ K.node "div"
                [ class "vs1" ]
                (List.map (viewItem Dict.empty "") fl)
            ]

        Measuring ls ->
            viewBothLists ls Dict.empty Dict.empty

        Starting ls measurement ->
            viewBothLists ls measurement.to measurement.from

        Animating ls measurement ->
            viewBothLists ls measurement.to measurement.to


viewBothLists ls mTo mFrom =
    [ K.node "div"
        [ class "o-0 absolute vs1 w-100" ]
        (List.map (viewItem mTo "to-") ls.to)
    , K.node "div"
        [ class "absolute vs1 w-100" ]
        (List.map (viewItem mFrom "from-") ls.from)
    ]


viewItem : FIClientRectById -> String -> FlipItem -> ( String, Html msg )
viewItem rectById idPrefix fi =
    let
        domId =
            idPrefix ++ FlipItem.strId fi

        strId =
            FlipItem.strId fi

        flipStyles =
            rectById
                |> Dict.get fi.id
                |> Maybe.Extra.unwrap []
                    (\cr ->
                        [ position fixed
                        , left (px cr.x)
                        , top (px cr.y)
                        , width (px cr.width)
                        , height (px cr.height)
                        ]
                    )
    in
    ( strId
    , div
        [ class "bg-black-80 white ba br-pill lh-copy pv1"
        , class "ph3"
        , A.id domId
        , css
            (flipStyles
                ++ [ transition
                        [ Transitions.left 1000
                        , Transitions.top 1000
                        , Transitions.width 1000
                        , Transitions.height 1000
                        ]
                   ]
            )
        ]
        [ text <| strId ++ ": " ++ fi.title ]
    )
