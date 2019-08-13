module Edit exposing (Edit(..), decoder, encoder, initial)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JD
import Json.Encode as JE exposing (Value)
import Set exposing (Set)
import Todo exposing (Todo, TodoId)


type Edit
    = None
    | InlineTodo Todo
    | Bulk (Set TodoId)


initial : Edit
initial =
    None


encodeType type_ otherFields =
    ( "type", JE.string type_ )
        :: otherFields
        |> JE.object


encoder : Edit -> Value
encoder edit =
    case edit of
        None ->
            encodeType "None" []

        Bulk idSet ->
            encodeType "Bulk" [ ( "idSet", JE.set JE.string idSet ) ]

        InlineTodo todo ->
            encodeType "InlineTodo" [ ( "todo", Todo.encoder todo ) ]


bulkDecoder : Decoder Edit
bulkDecoder =
    JD.field "idSet" (JD.list JD.string)
        |> JD.map (Set.fromList >> Bulk)


inlineTodoDecoder : Decoder Edit
inlineTodoDecoder =
    JD.field "todo" Todo.decoder
        |> JD.map InlineTodo


decoderFromType : String -> Decoder Edit
decoderFromType type_ =
    case type_ of
        "None" ->
            None |> JD.succeed

        "Bulk" ->
            bulkDecoder

        "InlineTodo" ->
            inlineTodoDecoder

        _ ->
            JD.fail ("Unable to decode Edit type: " ++ type_)


decoder : Decoder Edit
decoder =
    JD.oneOf
        [ bulkDecoder
        , JD.field "type" JD.string |> JD.andThen decoderFromType
        ]
