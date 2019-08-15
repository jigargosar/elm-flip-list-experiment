module FlipListDemo exposing (Model, Msg, empty, init, subscriptions, update, view)

import BasicsExtra exposing (callWith, eq_)
import Dict exposing (Dict)
import FlipItem exposing (FlipItem)
import FlipList exposing (FlipList)
import Html.Styled as H exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (class)
import Html.Styled.Events exposing (onClick)
import Http
import Maybe.Extra
import Ports
import Random
import Random.List
import Result exposing (Result)
import Result.Extra
import UpdateExtra exposing (pure)


type alias Model =
    { masterList : List FlipItem
    , currentList : List FlipItem
    , flipList : FlipList
    }


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
    | OnFlipListMsg FlipList.Msg


empty : Model
empty =
    { flipList = FlipList.empty
    , currentList = []
    , masterList = []
    }


init : ( Model, Cmd Msg )
init =
    ( empty
    , FlipItem.fetch GotFlipItems
    )


type alias Return =
    ( Model, Cmd Msg )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ FlipList.subscriptions model.flipList |> Sub.map OnFlipListMsg
        ]


maxItemCount =
    30


setFlipList flipList model =
    { model | flipList = flipList }


setCurrentList currentList model =
    { model | currentList = currentList }


resetState : Model -> Model
resetState model =
    let
        newList =
            model.masterList
                |> List.take maxItemCount
    in
    if newList |> List.isEmpty then
        model

    else
        setFlipList (FlipList.fromList newList) model
            |> setCurrentList newList


setMasterList masterList model =
    { model | masterList = masterList }


getCurrentList : Model -> List FlipItem
getCurrentList model =
    model.currentList


update : Msg -> Model -> Return
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        OnHardReset ->
            model |> pure

        GotFlipItems res ->
            res
                |> Result.Extra.unpack onHttpError onGotFIList
                |> callWith model

        OnShuffle ->
            ( model
            , getCurrentList model
                |> Random.List.shuffle
                |> Random.generate GotRandomShuffled
            )

        OnSoftReset ->
            let
                newList =
                    model.masterList
                        |> List.take maxItemCount
            in
            changeList newList model

        OnSort ->
            let
                sortedList =
                    getCurrentList model
                        |> List.sortBy .idx
            in
            changeList sortedList model

        OnRemove ->
            getCurrentList model
                |> List.tail
                |> Maybe.Extra.unwrap (pure model) (\newList -> changeList newList model)

        OnClicked fiId ->
            let
                newList =
                    getCurrentList model
                        |> List.filter (.id >> eq_ fiId >> not)
            in
            changeList newList model

        GotRandomShuffled shuffled ->
            changeList shuffled model

        OnFlipListMsg msg ->
            pure model


onGotFIList : List FlipItem -> Model -> Return
onGotFIList fiList model =
    ( model
        |> setMasterList fiList
        |> resetState
    , Cmd.none
    )


type alias Rect =
    Ports.BoundingRect


type alias FIRectById =
    Dict String Rect


changeList : List FlipItem -> Model -> Return
changeList newList model =
    --    let
    --        from =
    --            getTo model
    --
    --        to =
    --            newList
    --
    --        reqId =
    --            model.nextReqNum
    --
    --        req : Ports.ClientBoundingRectsRequest
    --        req =
    --            { id = reqId
    --            , from =
    --                from
    --                    |> List.map
    --                        (\fi -> ( fi.id, "from-" ++ fi.id ))
    --            , to =
    --                to
    --                    |> List.map
    --                        (\fi -> ( fi.id, "to-" ++ fi.id ))
    --            }
    --    in
    --    if from == to then
    --        pure model
    --
    --    else
    --        ( setState (Measuring reqId (Lists from to)) model
    --            |> incReq
    --        , Ports.getClientBoundingRects req
    --        )
    pure model


onHttpError : Http.Error -> Model -> Return
onHttpError err =
    let
        _ =
            Debug.log "HTTP Err" err
    in
    pure


view : Model -> Html Msg
view model =
    div [ class "measure-wide center vs3" ]
        [ div [ class "pv1 b" ] [ text "FlipListDemo" ]
        , div [ class "z-2 fixed flex hs3" ]
            [ button [ onClick OnShuffle ] [ text "Shuffle" ]
            , button [ onClick OnSort ] [ text "Sort" ]
            , button [ onClick OnRemove ] [ text "Remove" ]
            , button [ onClick OnSoftReset ] [ text "Soft Reset" ]
            , button [ onClick OnHardReset ] [ text "Hard Reset" ]
            ]
        , div [ class "pv3" ] []
        , FlipList.view model.flipList
            |> H.map OnFlipListMsg
        ]



--viewList : Model -> Html Msg
--viewList model =
--    let
--        twoDivs to from =
--            [ K.node "div"
--                [ class "o-0 absolute vs1 w-100" ]
--                (List.map (viewItem "to-") to)
--            , K.node "div"
--                [ class "absolute vs1 w-100" ]
--                (List.map (viewItem "from-") from)
--            ]
--
--        animatingTwoDivs to from measurements =
--            [ K.node "div"
--                [ class "o-0 absolute vs1 w-100" ]
--                (List.map (viewItem "to-") to)
--            , K.node "div"
--                [ class "absolute vs1 w-100"
--                , on "animationend" (JD.succeed OnAnimationEnd)
--                ]
--                (List.map
--                    (viewAnimatingItem measurements "from-")
--                    from
--                )
--            ]
--    in
--    div [ class "relative" ] <|
--        case model.state of
--            Initial fl ->
--                twoDivs fl fl
--
--            Measuring _ ls ->
--                twoDivs ls.to ls.from
--
--            Animating am ->
--                let
--                    newFrom =
--                        computeNewFromList am
--                in
--                animatingTwoDivs am.lists.to newFrom am.measurements
--
--
--viewItem : String -> FlipItem -> ( String, Html msg )
--viewItem idPrefix fi =
--    let
--        domId =
--            idPrefix ++ fi.id
--    in
--    FlipItem.viewKeyed domId fi
--
--
--viewAnimatingItem : Measurements -> String -> FlipItem -> ( String, Html msg )
--viewAnimatingItem measurements idPrefix fi =
--    let
--        domId =
--            idPrefix ++ fi.id
--
--        animKFs =
--            modToAnimationKeyframes <| fiToModification measurements fi
--
--        animCss =
--            Css.batch
--                [ animationName <| animKFs
--                , animationDuration (ms 1000)
--                , Css.property "animation-fill-mode" "forwards"
--                ]
--    in
--    FlipItem.viewAnimatingKeyed domId animCss fi
--
--
--type Modification
--    = Added Rect
--    | Moved Rect Rect
--    | Removed Rect
--    | Unchanged
--
--
--fiToModification measurements fi =
--    case
--        ( Dict.get fi.id measurements.from
--        , Dict.get fi.id measurements.to
--        )
--    of
--        ( Just from, Just to ) ->
--            Moved from to
--
--        ( Just from, Nothing ) ->
--            Removed from
--
--        ( Nothing, Just to ) ->
--            Added to
--
--        _ ->
--            Unchanged
--
--
--modToAnimationKeyframes : Modification -> Keyframes {}
--modToAnimationKeyframes mod =
--    case mod of
--        Moved from to ->
--            keyframes
--                [ ( 0
--                  , offsetBoxAnimProps from
--                  )
--                , ( 100
--                  , offsetBoxAnimProps to
--                  )
--                ]
--
--        Removed from ->
--            let
--                commonProps =
--                    offsetBoxAnimProps from
--            in
--            keyframes
--                [ ( 0
--                  , Animations.opacity (num 1)
--                        :: Animations.transform [ translateY zero ]
--                        :: commonProps
--                  )
--                , ( 100
--                  , Animations.opacity (num 0)
--                        :: Animations.transform [ translateY (vh 50) ]
--                        :: commonProps
--                  )
--                ]
--
--        Added to ->
--            let
--                commonProps =
--                    offsetBoxAnimProps to
--            in
--            keyframes
--                [ ( 0
--                  , Animations.opacity (num 0)
--                        :: Animations.transform [ translateX (px -to.width) ]
--                        :: commonProps
--                  )
--                , ( 100
--                  , Animations.opacity (num 1)
--                        :: Animations.transform [ translateX zero ]
--                        :: commonProps
--                  )
--                ]
--
--        _ ->
--            keyframes []
--
--
--pxF float =
--    String.fromFloat float ++ "px"
--
--
--animFloatProp name float =
--    Animations.property name (pxF float)
--
--
--offsetBoxAnimProps : Rect -> List Animations.Property
--offsetBoxAnimProps rect =
--    [ animFloatProp "top" rect.offsetTop
--    , animFloatProp "left" rect.offsetLeft
--    , animFloatProp "width" rect.width
--    , animFloatProp "height" rect.height
--    ]
