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
    , filter : VisibilityFilter
    }


type alias Todo =
    { id : Int
    , title : String
    , completed : Bool
    }


type VisibilityFilter
    = All
    | Active
    | Complete


initModel : Model
initModel =
    let
        todos =
            [ Todo 0 "Learn Elm" False
            , Todo 1 "Create Elm Todo app" True
            ]

        nextId =
            List.length todos
    in
        Model nextId "" todos All



-- update


type Msg
    = AddTodo
    | InputTodoName String
    | ToggleTodo Int
    | DeleteTodo Int
    | Filter VisibilityFilter


update : Msg -> Model -> Model
update msg model =
    case msg of
        Filter filter ->
            { model | filter = filter }

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
        , renderFilter model.filter
        , div [] [ text (toString model) ]
        ]


renderTodos : Model -> Html Msg
renderTodos model =
    let
        filteredTodos =
            filterTodos model.filter model.todos

        output =
            map renderTodo filteredTodos
    in
        div [] output


filterTodos : VisibilityFilter -> List Todo -> List Todo
filterTodos filter todos =
    List.filter
        (\todo ->
            case filter of
                All ->
                    True

                Complete ->
                    todo.completed

                Active ->
                    not todo.completed
        )
        todos


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
                , checked todo.completed
                ]
                []
            , text todo.title
            , button [ onClick (DeleteTodo todo.id) ] [ text "Delete" ]
            ]


renderFilter : VisibilityFilter -> Html Msg
renderFilter visiblityFilter =
    div []
        [ input
            [ type' "radio"
            , name "filter"
            , value "All"
            , onClick (Filter All)
            , checked (visiblityFilter == All)
            ]
            []
        , label [] [ text "All" ]
        , input
            [ type' "radio"
            , name "filter"
            , value "Active"
            , onClick (Filter Active)
            , checked (visiblityFilter == Active)
            ]
            []
        , label [] [ text "Active" ]
        , input
            [ type' "radio"
            , name "filter"
            , value "Complete"
            , onClick (Filter Complete)
            , checked (visiblityFilter == Complete)
            ]
            []
        , label [] [ text "Complete" ]
        ]


getStyles : Bool -> List ( String, String )
getStyles completed =
    if completed then
        [ ( "text-decoration", "line-through" ) ]
    else
        []
