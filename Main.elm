module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (filter, map, length)


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }



-- model


type alias Model =
    { nextId : Int
    , todoName : String
    , todos : List Todo
    }


type alias Todo =
    { id : Int
    , title : String
    , completed : Bool
    }


initModel : Model
initModel =
    Model 0 "" []


-- update


type Msg
    = AddTodo
    | InputTodoName String
    | ToggleTodo Int
    | DeleteTodo Int

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTodo ->
            let
                todo =
                    Todo model.nextId model.todoName False
            in
                { model
                    | todos = todo :: model.todos
                    , todoName = ""
                    , nextId = model.nextId + 1
                }

        InputTodoName name ->
            { model | todoName = name }

        ToggleTodo todoId ->
            let
                todos =
                    toggleTodo model.todos todoId
            in
                { model | todos = todos }

        DeleteTodo todoId ->
            let 
                todos =
                    deleteTodo model.todos todoId
            in
                { model | todos = todos }


toggleTodo : List Todo -> Int -> List Todo
toggleTodo todos todoId =
    map
        (\todo ->
            if todo.id == todoId then
                { todo | completed = not todo.completed }
            else
                todo
        )
        todos

deleteTodo : List Todo -> Int -> List Todo
deleteTodo todos todoId =
    filter (\todo -> todo.id /= todoId) todos

-- view


view : Model -> Html Msg
view model =
    div []
        [ renderAddTodoForm model
        , renderTodos model
        , div [] [ text (toString model) ]
        ]


renderTodos : Model -> Html Msg
renderTodos model =
    let
        output =
            map renderTodo model.todos
    in
        div [] output


renderAddTodoForm : Model -> Html Msg
renderAddTodoForm model =
    Html.form
        [ onSubmit AddTodo
        ]
        [ input
            [ type' "text"
            , placeholder "Add Todo"
            , onInput InputTodoName
            , value model.todoName
            ]
            []
        , button
            [ type' "Submit"
            ]
            [ text "Add" ]
        ]


renderTodo : Todo -> Html Msg
renderTodo todo =
    let
        styles =
            getStyles todo.completed
    in
        div [ style styles ]
            [ input
                [ type' "checkbox"
                , onClick (ToggleTodo todo.id)
                ]
                []
            , text todo.title
            , button [ onClick (DeleteTodo todo.id) ] [ text "Delete" ]
            ]


getStyles : Bool -> List ( String, String )
getStyles completed =
    if completed then
        [ ( "text-decoration", "line-through" ) ]
    else
        []
