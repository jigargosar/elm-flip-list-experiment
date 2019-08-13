module TodoCollection exposing
    ( Msg(..)
    , Return
    , TodoCollection
    , Update
    , completedForProjectList
    , completedList
    , decoder
    , encoder
    , initial
    , pendingList
    , pendingWithId
    , pendingWithProjectId
    , update
    , updateFromServerResponse
    )

import Dict exposing (Dict)
import Dict.Extra
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import List.Extra
import Millis exposing (Millis)
import ProjectId exposing (ProjectId)
import Todo exposing (Todo, TodoDict, TodoList)
import TodoId exposing (TodoId)



-- MODEL


type alias TodoCollection =
    { dict : TodoDict }


initial : TodoCollection
initial =
    { dict = Dict.empty }


fromDict : TodoDict -> TodoCollection
fromDict dict =
    { dict = dict }


dictDecoder : Decoder TodoDict
dictDecoder =
    JD.oneOf
        [ JD.dict Todo.decoder
        , Todo.listDecoder |> JD.map (Dict.Extra.fromListBy .id)
        ]


modelDecoder : Decoder TodoCollection
modelDecoder =
    JD.succeed TodoCollection
        |> JDP.required "dict" (JD.dict Todo.decoder)


decoder : Decoder TodoCollection
decoder =
    JD.oneOf
        [ modelDecoder
        , dictDecoder |> JD.map fromDict
        ]


encoder : TodoCollection -> Value
encoder { dict } =
    JE.object
        [ ( "dict", JE.dict identity Todo.encoder dict )
        ]



-- QUERY


filterSort : Todo.Filter -> List Todo.CompareBy -> TodoCollection -> TodoList
filterSort f s model =
    model |> .dict |> Dict.values |> Todo.filterSort f s


pendingList : TodoCollection -> TodoList
pendingList =
    filterSort Todo.Pending [ Todo.ByIdx ]


completedList : TodoCollection -> TodoList
completedList =
    filterSort Todo.Completed [ Todo.ByRecentlyModified ]


pendingWithProjectId pid model =
    pendingList model
        |> Todo.filter (Todo.AndFilter Todo.Pending (Todo.BelongsToProject pid))


completedForProjectList pid model =
    completedList model
        |> Todo.filter (Todo.AndFilter Todo.Completed (Todo.BelongsToProject pid))


get : TodoId -> TodoCollection -> Maybe Todo
get todoId =
    .dict >> Dict.get todoId


pendingWithId : TodoId -> TodoCollection -> Maybe Todo
pendingWithId todoId =
    get todoId
        >> Maybe.andThen (Todo.filterSingle Todo.Pending)



-- UPDATE


type alias Update =
    ( List TodoId, List Msg )


type Msg
    = MarkComplete
    | MarkPending
    | SetTitle String
    | MoveToProject ProjectId


type alias Return =
    TodoCollection


updateFromServerResponse : TodoList -> TodoCollection -> TodoCollection
updateFromServerResponse todoList model =
    todoList |> List.foldl insert model


update : Update -> Millis -> TodoCollection -> Return
update ( idList, msgList ) now model =
    idList
        |> List.foldl (updateWithMsgList now msgList) model


updateWithMsgList : Millis -> List Msg -> TodoId -> Return -> Return
updateWithMsgList now msgList todoId return =
    msgList
        |> List.foldl
            (\msg acc ->
                updateWithMsg now todoId msg acc
                    |> Maybe.withDefault acc
            )
            return


updateWithMsg : Millis -> TodoId -> Msg -> Return -> Maybe Return
updateWithMsg now todoId message =
    let
        moveToBottom =
            modifyTodo now todoId computeMoveToBottomTodoMsg
    in
    case message of
        MarkComplete ->
            modifyTodo now todoId (\_ -> Todo.SetCompleted True)

        MarkPending ->
            modifyTodo now todoId (\_ -> Todo.SetCompleted False)
                >> Maybe.andThen moveToBottom

        MoveToProject pid ->
            modifyTodo now todoId (\_ -> Todo.SetProjectId pid)
                >> Maybe.andThen moveToBottom

        SetTitle title ->
            modifyTodo now todoId (\_ -> Todo.SetTitle title)


computeMoveToBottomTodoMsg ( todo, model ) =
    let
        bottomIdx =
            todo.projectId
                |> (\pid -> pendingWithProjectId pid model)
                |> List.filter (.id >> (/=) todo.id)
                |> (List.Extra.last
                        >> Maybe.map (.sortIdx >> (+) 1)
                        >> Maybe.withDefault 0
                   )
    in
    Todo.SetSortIdx bottomIdx


modifyTodo :
    Millis
    -> TodoId
    -> (( Todo, TodoCollection ) -> Todo.Msg)
    -> Return
    -> Maybe Return
modifyTodo now todoId computeTodoMsg model =
    get todoId model
        |> Maybe.andThen
            (\todo ->
                let
                    todoMsg =
                        computeTodoMsg ( todo, model )
                in
                Todo.modify todoMsg now todo
                    |> Maybe.map
                        (\newTodo ->
                            insert newTodo model
                        )
            )


insert : Todo -> TodoCollection -> TodoCollection
insert todo model =
    { model
        | dict = Dict.insert todo.id todo model.dict
    }
