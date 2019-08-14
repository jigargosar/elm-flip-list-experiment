module FlipList exposing (FlipList, Msg, empty, init, subscriptions, update, view)

import BasicsExtra exposing (callWith)
import Css exposing (animationDuration, animationName, fixed, height, left, ms, position, px, top, width)
import Css.Animations as Animations exposing (keyframes)
import Css.Transitions as Transitions exposing (transition)
import Dict exposing (Dict)
import FlipItem exposing (FlipItem)
import Html.Styled exposing (Html, button, div, text)
import Html.Styled.Attributes as A exposing (class, css)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as K
import Http
import Maybe.Extra
import Ports
import Random
import Random.List
import Result exposing (Result)
import Result.Extra
import UpdateExtra exposing (pure)


type alias Lists =
    { from : List FlipItem, to : List FlipItem }


type alias Measurements =
    { from : FIRectById
    , to : FIRectById
    }


type alias FlipList =
    { nextReqNum : Int
    , state : State
    }


type State
    = Initial (List FlipItem)
    | Measuring Int Lists
    | Animating Lists Measurements


type alias HttpResult a =
    Result Http.Error a


type Msg
    = NoOp
    | GotFlipItems (HttpResult (List FlipItem))
    | OnShuffle
    | OnReset
    | GotRandomShuffled (List FlipItem)
    | GotClientBoundingRects Ports.ClientBoundingRectsResponse


empty : FlipList
empty =
    { nextReqNum = 0, state = Initial [] }


init : ( FlipList, Cmd Msg )
init =
    ( empty
    , FlipItem.fetch GotFlipItems
    )


type alias Return =
    ( FlipList, Cmd Msg )


subscriptions : FlipList -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.onGotClientBoundingRects GotClientBoundingRects
        ]


setState state model =
    { model | state = state }


incReq model =
    { model | nextReqNum = model.nextReqNum + 1 }


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
            ( model
            , getTo model
                |> Random.List.shuffle
                |> Random.generate GotRandomShuffled
            )

        OnReset ->
            let
                sortedList =
                    getTo model
                        |> List.sortBy .idx
            in
            changeList sortedList model

        GotRandomShuffled shuffled ->
            changeList shuffled model

        GotClientBoundingRects res ->
            let
                measurement =
                    { from = res.from |> Dict.fromList
                    , to = res.to |> Dict.fromList
                    }
            in
            case model.state of
                Measuring reqId ls ->
                    if reqId == res.id then
                        ( setState (Animating ls measurement) model, Cmd.none )

                    else
                        pure model

                _ ->
                    pure model


onGotFIList : List FlipItem -> FlipList -> Return
onGotFIList fiList model =
    let
        newList =
            fiList
                |> List.take 5
    in
    ( setState (Initial newList) model, Cmd.none )


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias FIRectById =
    Dict FlipItem.Id Rect


changeList : List FlipItem -> FlipList -> Return
changeList newList model =
    let
        from =
            getTo model

        to =
            newList

        reqId =
            model.nextReqNum

        req : Ports.ClientBoundingRectsRequest
        req =
            { id = reqId
            , from =
                from
                    |> List.map
                        (\fi -> ( fi.id, "from-" ++ fi.id ))
            , to =
                to
                    |> List.map
                        (\fi -> ( fi.id, "to-" ++ fi.id ))
            }
    in
    ( setState (Measuring reqId (Lists from to)) model
        |> incReq
    , Ports.getClientBoundingRects req
    )


getTo : FlipList -> List FlipItem
getTo model =
    case model.state of
        Initial fl ->
            fl

        Measuring _ ls ->
            ls.to

        Animating ls _ ->
            ls.to


onHttpError : Http.Error -> FlipList -> Return
onHttpError err =
    let
        _ =
            Debug.log "HTTP Err" err
    in
    pure


view : FlipList -> Html Msg
view model =
    div [ class "measure-wide center vs3" ]
        [ div [ class "pv1 b " ] [ text "FlipListDemo" ]
        , div [ class "flex hs3" ]
            [ button [ onClick OnShuffle ] [ text "Shuffle" ]
            , button [ onClick OnReset ] [ text "Reset" ]
            ]
        , div [ class "relative" ] (viewList model)
        ]


viewList : FlipList -> List (Html msg)
viewList model =
    case model.state of
        Initial fl ->
            [ K.node "div"
                [ class "vs1" ]
                (List.map (viewItem Dict.empty "") fl)
            ]

        Measuring _ ls ->
            viewBothLists ls (Measurements Dict.empty Dict.empty)

        Animating ls measurement ->
            --            viewBothLists ls (Measurements measurement.to measurement.to)
            [ K.node "div"
                [ class "o-0 absolute vs1 w-100" ]
                (List.map (viewItem Dict.empty "to-") ls.to)
            , K.node "div"
                [ class "absolute vs1 w-100" ]
                (List.map
                    (viewAnimatingItem measurement.from measurement.to "from-")
                    ls.from
                )
            ]


viewBothLists : Lists -> Measurements -> List (Html msg)
viewBothLists ls measurements =
    [ K.node "div"
        [ class "o-0 absolute vs1 w-100" ]
        (List.map (viewItem Dict.empty "to-") ls.to)
    , K.node "div"
        [ class "absolute vs1 w-100" ]
        (List.map (viewItem measurements.from "from-") ls.from)
    ]


viewItem : FIRectById -> String -> FlipItem -> ( String, Html msg )
viewItem rectById idPrefix fi =
    let
        domId =
            idPrefix ++ fi.id

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
    ( fi.id
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
        [ text <| fi.id ++ ": " ++ fi.title ]
    )


viewAnimatingItem : FIRectById -> FIRectById -> String -> FlipItem -> ( String, Html msg )
viewAnimatingItem fromDict toDict idPrefix fi =
    let
        domId =
            idPrefix ++ fi.id

        --        flipStyles =
        --            rectById
        --                |> Dict.get fi.id
        --                |> Maybe.Extra.unwrap []
        --                    (\cr ->
        --                        [ position fixed
        --                        , left (px cr.x)
        --                        , top (px cr.y)
        --                        , width (px cr.width)
        --                        , height (px cr.height)
        --                        ]
        --                    )
        pxF float =
            String.fromFloat float ++ "px"

        animFloatProp name float =
            Animations.property name (pxF float)

        anim =
            Maybe.map2
                (\from to ->
                    keyframes
                        [ ( 0
                          , [ animFloatProp "top" from.y
                            , animFloatProp "left" from.x
                            , animFloatProp "width" from.width
                            , animFloatProp "height" from.height
                            ]
                          )
                        , ( 100
                          , [ animFloatProp "top" to.y
                            , animFloatProp "left" to.x
                            , animFloatProp "width" to.width
                            , animFloatProp "height" to.height
                            ]
                          )
                        ]
                )
                (Dict.get fi.id fromDict)
                (Dict.get fi.id toDict)
                |> Maybe.withDefault (keyframes [])
    in
    ( fi.id
    , div
        [ class "bg-black-80 white ba br-pill lh-copy pv1"
        , class "ph3"
        , A.id domId
        , class "fixed"
        , css
            [ animationName anim
            , animationDuration (ms 1000)
            , Css.property "animation-fill-mode" "forwards"
            ]
        ]
        [ text <| fi.id ++ ": " ++ fi.title ]
    )
