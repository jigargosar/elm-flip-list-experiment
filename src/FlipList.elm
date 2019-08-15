module FlipList exposing (FlipList, Msg(..), empty, init, subscriptions, update, view)

import Css exposing (animationDuration, animationName, ms, num, px, translateX, translateY, vh, zero)
import Css.Animations as Animations exposing (Keyframes, keyframes)
import Dict exposing (Dict)
import Dict.Extra
import FlipItem exposing (FlipItem)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (on)
import Html.Styled.Keyed as K
import Http
import Json.Decode as JD
import Ports
import Result exposing (Result)
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


type AnimState
    = AnimEnd Int


type alias AnimatingModel =
    { animState : AnimState
    , lists : Lists
    , measurements : Measurements
    }


computeNewFromList : AnimatingModel -> List FlipItem
computeNewFromList am =
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
    newFrom


type State
    = Initial (List FlipItem)
    | Measuring Int Lists
    | Animating AnimatingModel


type alias HttpResult a =
    Result Http.Error a


type Msg
    = NoOp
    | ChangeList (List FlipItem)
    | GotClientBoundingRects Ports.ClientBoundingRectsResponse
    | OnAnimationEnd


empty : FlipList
empty =
    { nextReqNum = 0
    , state = Initial []
    }


init : List FlipItem -> FlipList
init fl =
    { empty | state = Initial fl }


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

        ChangeList newList ->
            changeList newList model

        GotClientBoundingRects res ->
            let
                measurements =
                    { from = res.from |> Dict.fromList
                    , to = res.to |> Dict.fromList
                    }
            in
            case model.state of
                Measuring reqId ls ->
                    if reqId == res.id then
                        ( setState
                            (Animating <|
                                AnimatingModel (AnimEnd 0) ls measurements
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


type alias Rect =
    Ports.BoundingRect


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


view : FlipList -> Html Msg
view model =
    viewList model


viewList : FlipList -> Html Msg
viewList model =
    let
        twoDivs to from =
            [ K.node "div"
                [ class "o-0 absolute vs1 w-100" ]
                (List.map (viewItem "to-") to)
            , K.node "div"
                [ class "absolute vs1 w-100" ]
                (List.map (viewItem "from-") from)
            ]

        animatingTwoDivs to from measurements =
            [ K.node "div"
                [ class "o-0 absolute vs1 w-100" ]
                (List.map (viewItem "to-") to)
            , K.node "div"
                [ class "absolute vs1 w-100"
                , on "animationend" (JD.succeed OnAnimationEnd)
                ]
                (List.map
                    (viewAnimatingItem measurements "from-")
                    from
                )
            ]
    in
    div [ class "relative" ] <|
        case model.state of
            Initial fl ->
                twoDivs fl fl

            Measuring _ ls ->
                twoDivs ls.to ls.from

            Animating am ->
                let
                    newFrom =
                        computeNewFromList am
                in
                animatingTwoDivs am.lists.to newFrom am.measurements


viewItem : String -> FlipItem -> ( String, Html msg )
viewItem idPrefix fi =
    let
        domId =
            idPrefix ++ fi.id
    in
    FlipItem.viewKeyed domId fi


viewAnimatingItem : Measurements -> String -> FlipItem -> ( String, Html msg )
viewAnimatingItem measurements idPrefix fi =
    let
        domId =
            idPrefix ++ fi.id

        animKFs =
            modToAnimationKeyframes <| fiToModification measurements fi

        animCss =
            Css.batch
                [ animationName <| animKFs
                , animationDuration (ms 1000)
                , Css.property "animation-fill-mode" "forwards"
                ]
    in
    FlipItem.viewAnimatingKeyed domId animCss fi


type Modification
    = Added Rect
    | Moved Rect Rect
    | Removed Rect
    | Unchanged


fiToModification measurements fi =
    case
        ( Dict.get fi.id measurements.from
        , Dict.get fi.id measurements.to
        )
    of
        ( Just from, Just to ) ->
            Moved from to

        ( Just from, Nothing ) ->
            Removed from

        ( Nothing, Just to ) ->
            Added to

        _ ->
            Unchanged


modToAnimationKeyframes : Modification -> Keyframes {}
modToAnimationKeyframes mod =
    case mod of
        Moved from to ->
            keyframes
                [ ( 0
                  , offsetBoxAnimProps from
                  )
                , ( 100
                  , offsetBoxAnimProps to
                  )
                ]

        Removed from ->
            let
                commonProps =
                    offsetBoxAnimProps from
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

        Added to ->
            let
                commonProps =
                    offsetBoxAnimProps to
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


offsetBoxAnimProps : Rect -> List Animations.Property
offsetBoxAnimProps rect =
    [ animFloatProp "top" rect.offsetTop
    , animFloatProp "left" rect.offsetLeft
    , animFloatProp "width" rect.width
    , animFloatProp "height" rect.height
    ]
