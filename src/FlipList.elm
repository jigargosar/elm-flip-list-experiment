module FlipList exposing (FlipList, Msg, empty, init, subscriptions, update, view)

import BasicsExtra exposing (callWith)
import Browser.Dom as Dom exposing (Element)
import Browser.Events
import Css exposing (absolute, fixed, height, left, position, px, top, width)
import Css.Transitions as Transitions exposing (transition)
import Dict exposing (Dict)
import FlipItem exposing (FlipItem)
import Html.Styled exposing (Html, button, div, text)
import Html.Styled.Attributes as A exposing (class, css, style)
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


type AnimState
    = NotStarted
    | Start FlipDomInfo
    | Playing FlipDomInfo


type alias FlippingModel =
    { from : List FlipItem
    , to : List FlipItem
    , animState : AnimState
    }


type FlipList
    = Stable (List FlipItem)
    | Flipping FlippingModel


type alias HttpResult a =
    Result Http.Error a


type Msg
    = NoOp
    | GotFlipItems (HttpResult (List FlipItem))
    | OnShuffle
    | GotRandomShuffled (List FlipItem)
    | OnGotFlipDomInfo (Result Dom.Error FlipDomInfo)
    | OnPlay


empty : FlipList
empty =
    Stable []


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
            Stable _ ->
                Sub.none

            Flipping rec ->
                case rec.animState of
                    NotStarted ->
                        Sub.none

                    Start _ ->
                        Browser.Events.onAnimationFrame (\_ -> OnPlay)

                    Playing _ ->
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

        OnGotFlipDomInfo res ->
            res
                |> Result.Extra.unpack onDomError onGotFlipDomInfo
                |> callWith model

        OnPlay ->
            case model of
                Stable _ ->
                    pure model

                Flipping rec ->
                    case rec.animState of
                        NotStarted ->
                            pure model

                        Start fdi ->
                            pure ({ rec | animState = Playing fdi } |> Flipping)

                        Playing _ ->
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


type alias FlipDomInfo =
    { from : FIClientRectById
    , to : FIClientRectById
    }


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
        Stable fl ->
            let
                from =
                    fl

                to =
                    shuffled

                flipDomInfoTask =
                    Task.map2 FlipDomInfo
                        (getFIClientRectById "from" from)
                        (getFIClientRectById "to" to)
            in
            ( Flipping <| { from = from, to = to, animState = NotStarted }
            , flipDomInfoTask |> Task.attempt OnGotFlipDomInfo
            )

        Flipping _ ->
            pure model


onGotFlipDomInfo domInfo model =
    case model of
        Stable _ ->
            pure model

        Flipping rec ->
            case rec.animState of
                NotStarted ->
                    ( { rec | animState = Start domInfo } |> Flipping
                    , Cmd.none
                    )

                Start di ->
                    pure model

                Playing di ->
                    pure model


onShuffle : FlipList -> Return
onShuffle model =
    case model of
        Stable fl ->
            ( model
            , Random.List.shuffle fl
                |> Random.generate GotRandomShuffled
            )

        Flipping rec ->
            ( Stable rec.to
            , Random.List.shuffle rec.to
                |> Random.generate GotRandomShuffled
            )


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
        |> Stable
        |> pure


view : FlipList -> Html Msg
view model =
    case model of
        Stable fl ->
            div [ class "measure-wide center vs3" ]
                [ div [ class "pv1 b " ] [ text "FlipListDemo" ]
                , div [ class "flex hs3" ]
                    [ button [ onClick OnShuffle ] [ text "Shuffle" ]
                    ]
                , viewList "" fl
                ]

        Flipping rec ->
            div [ class "measure-wide center vs3" ]
                [ div [ class "pv1 b " ] [ text "FlipListDemo" ]
                , div [ class "flex hs3" ]
                    [ button [ onClick OnShuffle ] [ text "Shuffle" ]
                    ]
                , div [ class "relative" ]
                    [ K.node "div"
                        [ class "o-0 absolute vs1 w-100" ]
                        (List.map (viewItem NotStarted "to-") rec.to)
                    , K.node "div"
                        [ class "absolute vs1 w-100" ]
                        (List.map (viewItem rec.animState "from-") rec.from)
                    ]
                ]


viewList : String -> List FlipItem -> Html msg
viewList idPrefix fl =
    K.node "div" [ class "vs1" ] (List.map (viewItem NotStarted idPrefix) fl)


viewItem : AnimState -> String -> FlipItem -> ( String, Html msg )
viewItem animState idPrefix fi =
    let
        domId =
            idPrefix ++ FlipItem.strId fi

        strId =
            FlipItem.strId fi

        flipStyles =
            case animState of
                NotStarted ->
                    []

                Start fdi ->
                    fdi.from
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

                Playing fdi ->
                    fdi.to
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
