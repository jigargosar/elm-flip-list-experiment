module FlipList exposing (FlipList, Modification(..), Msg(..), ViewConfig, empty, init, update, view)

import Css exposing (animationDuration, animationName, ms)
import Css.Animations exposing (Keyframes)
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


setState state model =
    { model | state = state }


incReq model =
    { model | nextReqNum = model.nextReqNum + 1 }


type alias UpdateConfig msg =
    { getClientBoundingRects : Ports.ClientBoundingRectsRequest -> Cmd msg
    }


update : UpdateConfig msg -> Msg -> FlipList -> Return
update config message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        ChangeList newList ->
            changeList config newList model

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


changeList : UpdateConfig msg -> List FlipItem -> FlipList -> Return
changeList config newList model =
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
        , config.getClientBoundingRects req
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


type alias ViewConfig msg =
    { viewKeyed : String -> FlipItem -> ( String, Html msg )
    , viewAnimatingKeyed : String -> Css.Style -> FlipItem -> ( String, Html msg )
    , toMsg : Msg -> msg
    , modToAnimationKeyframes : Modification -> Keyframes {}
    }


view : ViewConfig msg -> FlipList -> Html msg
view config model =
    let
        twoDivs to from =
            [ K.node "div"
                [ class "o-0 absolute vs1 w-100" ]
                (List.map (viewItem config "to-") to)
            , K.node "div"
                [ class "absolute vs1 w-100" ]
                (List.map (viewItem config "from-") from)
            ]

        animatingTwoDivs to from measurements =
            [ K.node "div"
                [ class "o-0 absolute vs1 w-100" ]
                (List.map (viewItem config "to-") to)
            , K.node "div"
                [ class "absolute vs1 w-100"
                , on "animationend" (JD.succeed (config.toMsg OnAnimationEnd))
                ]
                (List.map
                    (viewAnimatingItem config measurements "from-")
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


viewItem : ViewConfig msg -> String -> FlipItem -> ( String, Html msg )
viewItem config idPrefix fi =
    let
        domId =
            idPrefix ++ fi.id
    in
    config.viewKeyed domId fi


viewAnimatingItem : ViewConfig msg -> Measurements -> String -> FlipItem -> ( String, Html msg )
viewAnimatingItem config measurements idPrefix fi =
    let
        domId =
            idPrefix ++ fi.id

        animKFs =
            config.modToAnimationKeyframes <| fiToModification measurements fi

        animCss =
            Css.batch
                [ animationName <| animKFs
                , animationDuration (ms 1000)
                , Css.property "animation-fill-mode" "forwards"
                ]
    in
    config.viewAnimatingKeyed domId animCss fi


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
