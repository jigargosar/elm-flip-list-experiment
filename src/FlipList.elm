module FlipList exposing (FlipList, Msg, empty, init, subscriptions, update, view)

import BasicsExtra exposing (callWith, eq_)
import Css exposing (animationDuration, animationName, ms, num, px, translateX, translateY, vh, zero)
import Css.Animations as Animations exposing (Keyframes, keyframes)
import Dict exposing (Dict)
import Dict.Extra
import FlipItem exposing (FlipItem)
import Html.Styled exposing (Html, button, div, text)
import Html.Styled.Attributes as A exposing (class, css)
import Html.Styled.Events exposing (on, onClick)
import Html.Styled.Keyed as K
import Http
import Json.Decode as JD
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
    , masterList : List FlipItem
    }


type AnimState
    = AnimEnd Int


type alias AnimatingModel =
    { animState : AnimState
    , lists : Lists
    , measurements : Measurements
    }


type State
    = Initial (List FlipItem)
    | Measuring Int Lists
    | Animating AnimatingModel


type alias HttpResult a =
    Result Http.Error a


type Msg
    = NoOp
    | GotFlipItems (HttpResult (List FlipItem))
    | OnHardReset
    | OnSoftReset
    | OnShuffle
    | OnSort
    | OnRemove
    | OnClicked FlipItem.Id
    | GotRandomShuffled (List FlipItem)
    | GotClientBoundingRects Ports.ClientBoundingRectsResponse
    | OnAnimationEnd


empty : FlipList
empty =
    { nextReqNum = 0
    , state = Initial []
    , masterList = []
    }


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


resetState : FlipList -> FlipList
resetState model =
    let
        newList =
            model.masterList
                |> List.take 5
    in
    if newList |> List.isEmpty then
        model

    else
        setState (Initial newList) model


setMasterList masterList model =
    { model | masterList = masterList }


incReq model =
    { model | nextReqNum = model.nextReqNum + 1 }


update : Msg -> FlipList -> Return
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnHardReset ->
            resetState model |> pure

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

        OnSoftReset ->
            let
                newList =
                    model.masterList
                        |> List.take 5
            in
            changeList newList model

        OnSort ->
            let
                sortedList =
                    getTo model
                        |> List.sortBy .idx
            in
            changeList sortedList model

        OnRemove ->
            getTo model
                |> List.tail
                |> Maybe.Extra.unwrap (pure model) (\newList -> changeList newList model)

        OnClicked fiId ->
            let
                newList =
                    getTo model
                        |> List.filter (.id >> eq_ fiId >> not)
            in
            changeList newList model

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
                        ( setState
                            (Animating <|
                                AnimatingModel (AnimEnd 0) ls measurement
                            )
                            model
                        , Cmd.none
                        )

                    else
                        pure model

                _ ->
                    pure model

        OnAnimationEnd ->
            case model.state of
                Animating am ->
                    case am.animState of
                        AnimEnd ct ->
                            let
                                newCt =
                                    ct + 1
                            in
                            if newCt == List.length am.lists.to then
                                setState (Initial am.lists.to) model
                                    |> pure

                            else
                                model
                                    |> setState
                                        (Animating
                                            { am
                                                | animState = AnimEnd newCt
                                            }
                                        )
                                    |> pure

                _ ->
                    pure model


onGotFIList : List FlipItem -> FlipList -> Return
onGotFIList fiList model =
    ( model
        |> setMasterList fiList
        |> resetState
    , Cmd.none
    )


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
    if from == to then
        pure model

    else
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

        Animating am ->
            am.lists.to


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
            , button [ onClick OnSort ] [ text "Sort" ]
            , button [ onClick OnRemove ] [ text "Remove" ]
            , button [ onClick OnSoftReset ] [ text "Soft Reset" ]
            , button [ onClick OnHardReset ] [ text "Hard Reset" ]
            ]
        , viewList model
        ]


viewList : FlipList -> Html Msg
viewList model =
    div [ class "relative" ] <|
        case model.state of
            Initial fl ->
                [ K.node "div"
                    [ class "o-0 absolute vs1 w-100" ]
                    (List.map (viewItem "to-") fl)
                , K.node "div"
                    [ class "absolute vs1 w-100" ]
                    (List.map (viewItem "from-") fl)
                ]

            Measuring _ ls ->
                [ K.node "div"
                    [ class "o-0 absolute vs1 w-100" ]
                    (List.map (viewItem "to-") ls.to)
                , K.node "div"
                    [ class "absolute vs1 w-100" ]
                    (List.map (viewItem "from-") ls.from)
                ]

            Animating am ->
                let
                    fromById =
                        am.lists.from
                            |> Dict.Extra.fromListBy .id

                    toById =
                        am.lists.to
                            |> Dict.Extra.fromListBy .id

                    newFrom =
                        Dict.union toById fromById
                            |> Dict.values
                in
                [ K.node "div"
                    [ class "o-0 absolute vs1 w-100" ]
                    (List.map (viewItem "to-") am.lists.to)
                , K.node "div"
                    [ class "absolute vs1 w-100"
                    , on "animationend" (JD.succeed OnAnimationEnd)
                    ]
                    (List.map
                        (viewAnimatingItem am.measurements "from-")
                        newFrom
                    )
                ]


viewItem : String -> FlipItem -> ( String, Html Msg )
viewItem idPrefix fi =
    let
        domId =
            idPrefix ++ fi.id
    in
    ( fi.id
    , div
        [ class "relative bg-black-80 white ba br-pill lh-copy pv1"
        , class "ph3"
        , A.id domId
        , onClick <| OnClicked fi.id
        ]
        [ text <| fi.id ++ ": " ++ fi.title ]
    )


viewAnimatingItem : Measurements -> String -> FlipItem -> ( String, Html msg )
viewAnimatingItem measurement idPrefix fi =
    let
        domId =
            idPrefix ++ fi.id
    in
    ( fi.id
    , div
        [ class "bg-black-80 white ba br-pill lh-copy pv1"
        , class "ph3"
        , A.id domId
        , class "fixed"
        , css
            [ animationName <| animHelp measurement fi
            , animationDuration (ms 1000)
            , Css.property "animation-fill-mode" "forwards"
            ]
        ]
        [ text <| fi.id ++ ": " ++ fi.title ]
    )


animHelp : Measurements -> FlipItem -> Keyframes {}
animHelp measurement fi =
    case
        ( Dict.get fi.id measurement.from
        , Dict.get fi.id measurement.to
        )
    of
        ( Just from, Just to ) ->
            keyframes
                [ ( 0
                  , boxAnimProps from
                  )
                , ( 100
                  , boxAnimProps to
                  )
                ]

        ( Just from, Nothing ) ->
            let
                commonProps =
                    boxAnimProps from
            in
            keyframes
                [ ( 0
                  , Animations.opacity (num 1)
                        :: Animations.transform [ translateY zero ]
                        :: commonProps
                  )
                , ( 100
                  , Animations.opacity (num 0)
                        :: Animations.transform [ translateY (vh 50) ]
                        :: commonProps
                  )
                ]

        ( Nothing, Just to ) ->
            let
                commonProps =
                    boxAnimProps to
            in
            keyframes
                [ ( 0
                  , Animations.opacity (num 0)
                        :: Animations.transform [ translateX (px -to.width) ]
                        :: commonProps
                  )
                , ( 100
                  , Animations.opacity (num 1)
                        :: Animations.transform [ translateX zero ]
                        :: commonProps
                  )
                ]

        _ ->
            keyframes []


pxF float =
    String.fromFloat float ++ "px"


animFloatProp name float =
    Animations.property name (pxF float)


boxAnimProps : Rect -> List Animations.Property
boxAnimProps rect =
    [ animFloatProp "top" rect.y
    , animFloatProp "left" rect.x
    , animFloatProp "width" rect.width
    , animFloatProp "height" rect.height
    ]
