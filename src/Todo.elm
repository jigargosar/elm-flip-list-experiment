module Todo exposing
    ( CompareBy(..)
    , DueAt(..)
    , Filter(..)
    , Msg(..)
    , Todo
    , TodoDict
    , TodoList
    , compareDueDate
    , concatCompareBy
    , decoder
    , dueAtDecoder
    , dueAtEncoder
    , dueAtToMillis
    , dueDate
    , dueDateEq
    , dueMilli
    , encoder
    , filter
    , filterSingle
    , filterSort
    , listDecoder
    , listEncoder
    , matchesFilter
    , modify
    , modifyPatch
    , newForProject
    , newToday
    , sortWith
    )

import Calendar
import Compare exposing (Comparator)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Millis exposing (Millis)
import ProjectId exposing (ProjectId)
import Time
import TodoId exposing (TodoId)


type DueAt
    = DueAt Millis
    | NoDue


dueAtDecoder : Decoder DueAt
dueAtDecoder =
    let
        tagDecoder tag =
            case tag of
                "DueAt" ->
                    JD.field "millis" JD.int
                        |> JD.map DueAt

                "NoDue" ->
                    JD.succeed NoDue

                _ ->
                    JD.fail ("Invalid dueAt tag: " ++ tag)
    in
    JD.oneOf
        [ JD.int |> JD.map DueAt
        , JD.null NoDue
        , JD.field "tag" JD.string |> JD.andThen tagDecoder
        ]


dueAtEncoder : DueAt -> Value
dueAtEncoder due =
    case due of
        DueAt mi ->
            JE.object
                [ ( "tag", JE.string "DueAt" )
                , ( "millis", JE.int mi )
                ]

        NoDue ->
            JE.object
                [ ( "tag", JE.string "NoDue" )
                ]


type alias Todo =
    { id : TodoId
    , title : String
    , sortIdx : Int
    , projectId : ProjectId
    , projectIdModifiedAt : Millis
    , isDone : Bool
    , dueAt : DueAt
    , createdAt : Millis
    , modifiedAt : Millis
    }


decoder : Decoder Todo
decoder =
    JD.succeed Todo
        |> JDP.required "id" JD.string
        |> JDP.required "title" JD.string
        |> JDP.required "sortIdx" JD.int
        |> JDP.optional "projectId" ProjectId.decoder ProjectId.default
        |> JDP.optional "projectIdModifiedAt" JD.int 0
        |> JDP.required "isDone" JD.bool
        |> JDP.optional "dueAt" dueAtDecoder NoDue
        |> JDP.required "createdAt" JD.int
        |> JDP.required "modifiedAt" JD.int


encoder : Todo -> Value
encoder { id, title, sortIdx, projectId, projectIdModifiedAt, isDone, dueAt, createdAt, modifiedAt } =
    JE.object
        [ ( "id", JE.string id )
        , ( "title", JE.string title )
        , ( "sortIdx", JE.int sortIdx )
        , ( "projectId", ProjectId.encoder projectId )
        , ( "projectIdModifiedAt", JE.int projectIdModifiedAt )
        , ( "isDone", JE.bool isDone )
        , ( "dueAt", dueAtEncoder dueAt )
        , ( "createdAt", JE.int createdAt )
        , ( "modifiedAt", JE.int modifiedAt )
        ]


type Msg
    = SetCompleted Bool
    | SetProjectId ProjectId
    | SetTitle String
    | SetSortIdx Int
    | SetDueAt DueAt


newForProject : Millis -> ProjectId -> Value
newForProject now pid =
    { id = ""
    , title = ""
    , sortIdx = 0
    , projectId = pid
    , projectIdModifiedAt = now
    , isDone = False
    , dueAt = NoDue
    , createdAt = now
    , modifiedAt = now
    }
        |> encoder


newToday : Millis -> Millis -> Value
newToday now dueAt =
    { id = ""
    , title = ""
    , sortIdx = 0
    , projectId = ProjectId.default
    , projectIdModifiedAt = now
    , isDone = False
    , dueAt = DueAt dueAt
    , createdAt = now
    , modifiedAt = now
    }
        |> encoder


modifyPatch : Msg -> Millis -> List ( String, Value )
modifyPatch msg now =
    ( "modifiedAt", JE.int now )
        :: (case msg of
                SetCompleted bool ->
                    [ ( "isDone", JE.bool bool ) ]

                SetProjectId projectId ->
                    [ ( "projectId", ProjectId.encoder projectId )
                    , ( "projectIdModifiedAt", JE.int now )
                    ]

                SetTitle title ->
                    [ ( "title", JE.string title ) ]

                SetSortIdx sortIdx ->
                    [ ( "sortIdx", JE.int sortIdx ) ]

                SetDueAt dueAt ->
                    [ ( "dueAt", dueAt |> dueAtEncoder ) ]
           )


update : Msg -> Todo -> Todo
update msg model =
    case msg of
        SetCompleted bool ->
            { model | isDone = bool }

        SetProjectId projectId ->
            { model | projectId = projectId }

        SetTitle title ->
            { model | title = title }

        SetSortIdx sortIdx ->
            { model | sortIdx = sortIdx }

        SetDueAt dueAt ->
            { model | dueAt = dueAt }


modifyWithNow : Millis -> Msg -> Todo -> Maybe Todo
modifyWithNow now msg model =
    let
        newModel =
            update msg model
    in
    if newModel == model then
        Nothing

    else
        newModel |> setModifiedAt now |> Just


modify : Msg -> Millis -> Todo -> Maybe Todo
modify msg now model =
    modifyWithNow now msg model


dueDate : Todo -> Maybe Calendar.Date
dueDate model =
    case model.dueAt of
        NoDue ->
            Nothing

        DueAt mi ->
            mi
                |> Time.millisToPosix
                |> Calendar.fromPosix
                |> Just


dueAtToMillis : DueAt -> Maybe Millis
dueAtToMillis dueAt =
    case dueAt of
        NoDue ->
            Nothing

        DueAt mi ->
            mi |> Just


dueMilli : Todo -> Maybe Millis
dueMilli model =
    dueAtToMillis model.dueAt


dueDateEq : Calendar.Date -> Todo -> Bool
dueDateEq dt model =
    case model.dueAt of
        NoDue ->
            False

        DueAt mi ->
            mi
                |> Time.millisToPosix
                |> Calendar.fromPosix
                |> (==) dt


compareDueDate : Calendar.Date -> Todo -> Maybe Order
compareDueDate dt model =
    case model.dueAt of
        NoDue ->
            Nothing

        DueAt mi ->
            mi
                |> Time.millisToPosix
                |> Calendar.fromPosix
                |> Calendar.compare dt
                |> Just


setModifiedAt now todo =
    { todo | modifiedAt = now }


type Filter
    = Pending
    | Completed
    | BelongsToProject ProjectId
    | NotInProject ProjectId
    | AndFilter Filter Filter


matchesFilter : Filter -> Todo -> Bool
matchesFilter filter_ todo =
    case filter_ of
        Pending ->
            not todo.isDone

        Completed ->
            todo.isDone

        BelongsToProject pid ->
            todo.projectId == pid

        NotInProject pid ->
            todo.projectId /= pid

        AndFilter a b ->
            matchesFilter a todo && matchesFilter b todo


type alias TodoList =
    List Todo


type alias TodoDict =
    Dict TodoId Todo


listDecoder : Decoder TodoList
listDecoder =
    JD.list decoder


listEncoder : TodoList -> Value
listEncoder =
    JE.list encoder


filter : Filter -> TodoList -> TodoList
filter filter_ =
    List.filter (matchesFilter filter_)


filterSingle : Filter -> Todo -> Maybe Todo
filterSingle filter_ =
    List.singleton >> filter filter_ >> List.head


type CompareBy
    = ByIdx
    | ByRecentlyModified
    | ByRecentlyCreated
    | ByRecentlyModifiedProjectId


toComparator : CompareBy -> Comparator Todo
toComparator compareBy =
    case compareBy of
        ByIdx ->
            Compare.by .sortIdx

        ByRecentlyModified ->
            Compare.by .modifiedAt |> Compare.reverse

        ByRecentlyCreated ->
            Compare.by .createdAt |> Compare.reverse

        ByRecentlyModifiedProjectId ->
            Compare.by .projectIdModifiedAt |> Compare.reverse


concatCompareBy : List CompareBy -> Comparator Todo
concatCompareBy comps =
    Compare.concat (List.map toComparator comps)


sortWith : List CompareBy -> TodoList -> TodoList
sortWith comps =
    List.sortWith (concatCompareBy comps)


filterSort : Filter -> List CompareBy -> TodoList -> TodoList
filterSort fil comps =
    filter fil >> sortWith comps
