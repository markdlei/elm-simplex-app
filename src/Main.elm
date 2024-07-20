module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Array exposing (Array, initialize, get, set, push, slice, toList, indexedMap)
import String exposing (toInt, fromInt, dropRight)



-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { numRows: Int
  , numCols: Int
  , data: Data
  , outputText: String
  }

type alias Data =
  { objective: String
  , matrix: Array (Array String)
  , constraint: Array String
  }


init : Model
init =
  Model
    3
    3
    (Data 
      ""
      (initialize 3 (always (initialize 3 (always ""))))
      (initialize 3 (always ""))
    )
    ""



-- UPDATE


type Msg
  = IncrementRow
  | DecrementRow
  | IncrementCol
  | DecrementCol
  | Reset
  | ObjectiveInput String
  | MatrixInput Int Int String
  | ConstraintInput Int String
  | Finish


update : Msg -> Model -> Model
update msg model =
  case msg of
    IncrementRow ->
      let
        oldData = model.data
        newData =
          { oldData
          | matrix = push (initialize model.numCols (always "")) model.data.matrix
          , constraint = push "" model.data.constraint
          }
      in
        { model
        | numRows = model.numRows + 1
        , data = newData
        }

    DecrementRow ->
      if model.numRows /= 1 then
        let
          oldData = model.data
          newData = 
            { oldData
            | matrix = pop model.data.matrix
            , constraint = pop model.data.constraint
            }
        in
          { model
          | numRows = model.numRows - 1
          , data = newData
          }
      else
        model

    IncrementCol ->
      let
        oldData = model.data
        newData = { oldData | matrix = Array.map (push "") model.data.matrix }
      in
        { model
        | numCols = model.numCols + 1
        , data = newData
        }

    DecrementCol ->
      if model.numCols /= 1 then
        let
          oldData = model.data
          newData = { oldData | matrix = Array.map pop model.data.matrix }
        in
          { model
          | numCols = model.numCols - 1
          , data = newData
          }
      else
        model

    Reset ->
      init

    ObjectiveInput input ->
      let
        oldData = model.data
        newData = { oldData | objective = input }
      in
        { model
        | data = newData
        }

    MatrixInput rowIdx colIdx input ->
      let
        oldData = model.data
        newData = { oldData | matrix = setMatrixCell model.data.matrix rowIdx colIdx (forceInt input) }
      in
        { model
        | data = newData
        }

    ConstraintInput rowIdx input ->
      let
        oldData = model.data
        newData = { oldData | constraint = set rowIdx (forceInt input) model.data.constraint }
      in
        { model
        | data = newData
        }

    Finish ->
      { model | outputText = Debug.toString (Debug.log "Finish" model.data) }


-- Update Helpers
setMatrixCell : Array (Array String) -> Int -> Int -> String -> Array (Array String)
setMatrixCell matrix rowIdx colIdx val =
  set rowIdx (set colIdx val (getMatrixRow matrix rowIdx)) matrix

getMatrixRow : Array (Array String) -> Int -> Array String
getMatrixRow matrix rowIdx = 
  case get rowIdx matrix of
    Just arr ->
      arr
    Nothing ->
      Debug.todo "Handle offset getter"

pop : Array a -> Array a
pop arr =
  slice 0 -1 arr

forceInt : String -> String
forceInt str =
  if str == "-" then
    str
  else
    case toInt str of
      Just num ->
        fromInt num
      Nothing ->
        dropRight 1 str



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Simplex Solver" ]
    , dl []
      [ dt [] [ text "Objective Function" ]
      , dd [] 
        [ span [] [ text "Maximize:" ]
        , input [ value model.data.objective, onInput ObjectiveInput ] [] 
        ]
      , dt [] [ text "Constraint Matrix" ]
      , dd [] 
        [ button [ onClick DecrementRow ] [ text "- Row" ]
        , button [ onClick IncrementRow ] [ text "+ Row" ]
        , button [ onClick DecrementCol ] [ text "- Col" ]
        , button [ onClick IncrementCol ] [ text "+ Col" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , inputMatrix model
        , button [ onClick Finish ] [ text "Finish" ]
        , span [] [ text model.outputText ]
        ]
      ]
    ]


-- View Helpers
inputMatrix : Model -> Html Msg
inputMatrix model =
  div [] (toList (indexedMap (inputRow model.data.constraint) model.data.matrix))

inputRow : Array String -> Int -> Array String -> Html Msg
inputRow constraintArray rowIdx rowArray =
  div [ style "display" "flex"] 
    [ div [] (toList (indexedMap (viewInput rowIdx) rowArray))
    , div []
      [ span [] [ text "=" ]
      , input [ value (getString rowIdx constraintArray), onInput (ConstraintInput rowIdx) ] [] 
      ]
    ]

viewInput : Int -> Int -> String -> Html Msg
viewInput rowIdx colIdx str =
  input [ value str, onInput (MatrixInput rowIdx colIdx) ] []

getString : Int -> Array String -> String
getString rowIdx arr =
  case get rowIdx arr of
    Just str ->
      str
    Nothing ->
      ""

