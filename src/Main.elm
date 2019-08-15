module Main exposing (main)

import BasicsExtra exposing (callWith)
import Browser
import Browser.Navigation as Nav
import Errors exposing (Errors)
import FlipListDemo
import FontAwesome.Attributes
import FontAwesome.Icon as FAIcon
import FontAwesome.Styles
import HasErrors
import Html.Styled as H exposing (Html, div)
import Html.Styled.Attributes exposing (class, href)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Lazy exposing (lazy)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Ports exposing (FirestoreQueryResponse)
import Result.Extra
import Return
import Route exposing (Route)
import UpdateExtra exposing (andThen, pure)
import Url exposing (Url)



-- MODEL


type alias Model =
    { flipDemo : FlipListDemo.Model
    , errors : Errors
    , key : Nav.Key
    , route : Route
    }


type alias Cache =
    {}


type alias Flags =
    { cache : Cache
    }


cacheDecoder : Decoder Cache
cacheDecoder =
    JD.succeed Cache


cacheEncoder : Cache -> Value
cacheEncoder _ =
    JE.object
        []


setModelFromCache : Cache -> Model -> Model
setModelFromCache _ model =
    model


cacheFromModel : Model -> Cache
cacheFromModel _ =
    {}


flagsDecoder : Decoder Flags
flagsDecoder =
    JD.succeed Flags
        |> JDP.required "cache" cacheDecoder



-- INIT


type alias Return =
    Return.Return Msg Model


init : Value -> Url -> Nav.Key -> Return
init encodedFlags url key =
    let
        route =
            Route.fromUrl url

        model : Model
        model =
            { flipDemo = FlipListDemo.empty
            , errors = Errors.fromStrings [ "Testing Error View" ]
            , key = key
            , route = route
            }
    in
    model
        |> pure
        |> andThen (updateFromEncodedFlags encodedFlags)
        |> andThen
            (\m ->
                let
                    ( flipDemo, cmd ) =
                        FlipListDemo.init
                in
                ( { m | flipDemo = flipDemo }, Cmd.map OnFlipDemoMsg cmd )
            )



-- MSG


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | OnFlipDemoMsg FlipListDemo.Msg



-- SUB


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ FlipListDemo.subscriptions model.flipDemo |> Sub.map OnFlipDemoMsg
        ]



-- UPDATE


update : Msg -> Model -> Return
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    if Route.fromUrl url == model.route then
                        ( model, Nav.replaceUrl model.key (Url.toString url) )

                    else
                        ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    Route.fromUrl url
            in
            ( { model | route = route }, {- queryTodoListForRouteCmd route -} Cmd.none )

        OnFlipDemoMsg msg ->
            updateFlipList msg model


updateFlipList message model =
    let
        ( flipDemo, cmd ) =
            FlipListDemo.update message model.flipDemo
    in
    ( { model | flipDemo = flipDemo }
    , Cmd.map OnFlipDemoMsg cmd
    )


cacheEffect : Model -> Cmd msg
cacheEffect model =
    Ports.setCache (cacheEncoder (cacheFromModel model))


updateFromEncodedFlags : Value -> Model -> Return
updateFromEncodedFlags encodedFlags model =
    JD.decodeValue flagsDecoder encodedFlags
        |> Result.Extra.unpack onDecodeError updateFromFlags
        |> callWith model


updateFromFlags : Flags -> Model -> Return
updateFromFlags flags model =
    model
        |> setModelFromCache flags.cache
        |> pure


onDecodeError : JD.Error -> Model -> Return
onDecodeError error model =
    HasErrors.prependDecodeError error model
        |> pure



-- VIEW


view : Model -> Browser.Document Msg
view model =
    viewRoute model.route model
        |> toUnStyledDocument
        |> prependFontAwesomeCss


prependFontAwesomeCss : Browser.Document Msg -> Browser.Document Msg
prependFontAwesomeCss doc =
    { doc | body = FontAwesome.Styles.css :: doc.body }


type alias StyledDocument msg =
    { title : String, body : List (Html msg) }


toUnStyledDocument : StyledDocument msg -> Browser.Document msg
toUnStyledDocument { title, body } =
    { title = title, body = body |> List.map H.toUnstyled }


viewRoute : Route -> Model -> StyledDocument Msg
viewRoute route model =
    case route of
        Route.NotFound _ ->
            viewRoute Route.FlipDemo model

        Route.FlipDemo ->
            viewFlipDemo model.flipDemo


viewFlipDemo : FlipListDemo.Model -> StyledDocument Msg
viewFlipDemo flipDemo =
    { title = "FlipList Demo"
    , body =
        [ lazy FlipListDemo.view flipDemo
            |> H.map OnFlipDemoMsg
        ]
    }


faBtn : msg -> FAIcon.Icon -> Html msg
faBtn clickHandler icon =
    div
        [ class "gray hover-dark-gray pointer"
        , onClick clickHandler
        ]
        [ icon
            |> FAIcon.viewStyled [ FontAwesome.Attributes.lg ]
            |> H.fromUnstyled
        ]



-- MAIN


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
