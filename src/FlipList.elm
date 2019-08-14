module FlipList exposing (FlipList, Msg, empty, init, subscriptions, update, view)

import BasicsExtra exposing (callWith)
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


type FlipList
    = Initial (List FlipItem)
    | Measuring Lists
    | Starting Lists Measurements
    | Animating Lists Measurements


type alias HttpResult a =
    Result Http.Error a


type Msg
    = NoOp
    | GotFlipItems (HttpResult (List FlipItem))
    | OnShuffle
    | OnReset
    | GotRandomShuffled (List FlipItem)
      --    | GotMeasurement (Result Dom.Error Measurements)
    | GotClientBoundingRects Ports.ClientBoundingRectsResponse
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
        , Ports.onGotClientBoundingRects GotClientBoundingRects
        ]


update : Msg -> FlipList -> Return
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        GotFlipItems res ->
            let
                onGotFIList fiList _ =
                    let
                        newList =
                            fiList
                                |> List.take 50
                    in
                    ( Initial newList, Cmd.none )
            in
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
                        |> List.sortBy .id
            in
            changeList sortedList model

        GotRandomShuffled shuffled ->
            changeList shuffled model

        --        GotMeasurement res ->
        --            let
        --                _ =
        --                    Debug.log "Got Measurements" ""
        --            in
        --            res
        --                |> Result.Extra.unpack onDomError onGotMeasurement
        --                |> callWith model
        GotClientBoundingRects res ->
            let
                _ =
                    Debug.log "res" res

                measurement =
                    { from = res.from |> Dict.fromList
                    , to =
                        res.to |> Dict.fromList
                    }
            in
            case model of
                Measuring ls ->
                    ( Starting ls measurement, Cmd.none )

                _ ->
                    pure model

        OnPlay ->
            case model of
                Starting lists measurement ->
                    pure (Animating lists measurement)

                _ ->
                    pure model


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias FIRectById =
    Dict FlipItem.Id Rect



--getFIClientRect : String -> FlipItem -> Task Dom.Error ( FlipItem.Id, Rect )
--getFIClientRect idPrefix fi =
--    let
--        domId =
--            idPrefix ++ "-" ++ FlipItem.strId fi
--    in
--    Dom.getElement domId
--        |> Task.map (\el -> ( fi.id, el.element ))
--getFIRectById : String -> List FlipItem -> Task Dom.Error FIRectById
--getFIRectById idPrefix fiList =
--    fiList
--        |> List.map (getFIClientRect idPrefix)
--        |> Task.sequence
--        |> Task.map Dict.fromList
--
--getFIRectByIdViaPort : String -> List FlipItem -> Cmd msg
--getFIRectByIdViaPort idPrefix fiList =
--    let
--        fiStrIdList : List String
--        fiStrIdList =
--            fiList
--                |> List.map FlipItem.strId
--    in
--    Ports.getClientBoundingRects ( idPrefix, fiStrIdList )


changeList : List FlipItem -> FlipList -> ( FlipList, Cmd Msg )
changeList newList model =
    let
        from =
            getTo model

        to =
            newList

        --        flipDomInfoTask =
        --            Task.map2 Measurements
        --                (getFIRectById "from" from)
        --                (getFIRectById "to" to)
        req : Ports.ClientBoundingRectsRequest
        req =
            { from =
                from
                    |> List.map
                        (FlipItem.strId
                            >> (\idStr -> ( idStr, "from-" ++ idStr ))
                        )
            , to =
                to
                    |> List.map
                        (FlipItem.strId
                            >> (\idStr -> ( idStr, "to-" ++ idStr ))
                        )
            }

        _ =
            Debug.log "Getting Measurements" ""
    in
    ( Measuring (Lists from to)
    , Cmd.batch
        [ {- flipDomInfoTask |> Task.attempt GotMeasurement

             ,
          -}
          Ports.getClientBoundingRects req
        ]
    )



--onGotMeasurement : Measurements -> FlipList -> Return
--onGotMeasurement measurement model =
--    case model of
--        Measuring ls ->
--            ( Starting ls measurement, Cmd.none )
--
--        _ ->
--            pure model


getTo : FlipList -> List FlipItem
getTo model =
    case model of
        Initial fl ->
            fl

        Measuring ls ->
            ls.to

        Starting ls _ ->
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



--onDomError : Dom.Error -> FlipList -> Return
--onDomError err =
--    let
--        _ =
--            Debug.log "Dom Err" err
--    in
--    pure


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
    case model of
        Initial fl ->
            [ K.node "div"
                [ class "vs1" ]
                (List.map (viewItem Dict.empty "") fl)
            ]

        Measuring ls ->
            viewBothLists ls (Measurements Dict.empty Dict.empty)

        Starting ls measurement ->
            viewBothLists ls measurement

        Animating ls measurement ->
            viewBothLists ls (Measurements measurement.to measurement.to)


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
