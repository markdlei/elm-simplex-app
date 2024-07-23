module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Array exposing (Array, initialize, get, set, push, slice, toList, indexedMap, fromList)
import String exposing (toInt, fromInt, dropRight)
import Maybe exposing (withDefault)
import List exposing (concat)
import List exposing (take)



-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { numRows: Int
  , numCols: Int
  , inputData: InputData
  , outputText: String
  , outputData: Data
  }

type alias InputData =
  { objective: Array String
  , matrix: Array (Array String)
  , constraint: Array String
  }

type alias Data =
  { objective: Array Int
  , matrix: Array (Array Int)
  , constraint: Array Int
  }


init : Model
init =
  Model
    3
    3
    (InputData 
      (initialize 3 (always ""))
      (initialize 3 (always (initialize 3 (always ""))))
      (initialize 3 (always ""))
    )
    ""
    (Data (fromList []) (fromList []) (fromList []))



-- UPDATE


type Msg
  = IncrementRow
  | DecrementRow
  | IncrementCol
  | DecrementCol
  | Reset
  | ObjectiveInput Int String
  | MatrixInput Int Int String
  | ConstraintInput Int String
  | Finish


update : Msg -> Model -> Model
update msg model =
  case msg of
    IncrementRow ->
      let
        oldData = model.inputData
        newData =
          { oldData
          | matrix = push (initialize model.numCols (always "")) model.inputData.matrix
          , constraint = push "" model.inputData.constraint
          }
      in
        { model
        | numRows = model.numRows + 1
        , inputData = newData
        }

    DecrementRow ->
      if model.numRows /= 1 then
        let
          oldData = model.inputData
          newData = 
            { oldData
            | matrix = pop model.inputData.matrix
            , constraint = pop model.inputData.constraint
            }
        in
          { model
          | numRows = model.numRows - 1
          , inputData = newData
          }
      else
        model

    IncrementCol ->
      let
        oldData = model.inputData
        newData = { oldData | matrix = Array.map (push "") model.inputData.matrix }
      in
        { model
        | numCols = model.numCols + 1
        , inputData = newData
        }

    DecrementCol ->
      if model.numCols /= 1 then
        let
          oldData = model.inputData
          newData = { oldData | matrix = Array.map pop model.inputData.matrix }
        in
          { model
          | numCols = model.numCols - 1
          , inputData = newData
          }
      else
        model

    Reset ->
      init

    ObjectiveInput colIdx input ->
      let
        oldData = model.inputData
        newData = { oldData | objective = set colIdx (forceIntInput input) model.inputData.objective }
      in
        { model
        | inputData = newData
        }

    MatrixInput rowIdx colIdx input ->
      let
        oldData = model.inputData
        newData = { oldData | matrix = setMatrixCell model.inputData.matrix rowIdx colIdx (forceIntInput input) }
      in
        { model
        | inputData = newData
        }

    ConstraintInput rowIdx input ->
      let
        oldData = model.inputData
        newData = { oldData | constraint = set rowIdx (forceIntInput input) model.inputData.constraint }
      in
        { model
        | inputData = newData
        }

    Finish ->
      { model | outputData = inputToNumData model.inputData, outputText = Debug.toString (Debug.log "Finish" model.outputData) }


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

forceIntInput : String -> String
forceIntInput str =
  if str == "-" then
    str
  else
    case (toInt str) of
      Just _ ->
        str
      Nothing ->
        dropRight 1 str

inputToNumData : InputData -> Data
inputToNumData input =
  { objective = Array.map anyStringToInt input.objective
  , matrix = Array.map (Array.map anyStringToInt) input.matrix
  , constraint = Array.map anyStringToInt input.constraint
  }

parseObjective : String -> Maybe Int
parseObjective objective =
  toInt objective

anyStringToInt : String -> Int
anyStringToInt str =
  withDefault 0 (toInt str)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Simplex Solver" ]
    , dl []
      [ dt [] [ text "Objective Function" ]
      , dd [] 
        [ span [] [ text "Maximize:" ]
        , objectiveVector model
        ]
      , dt [] [ text "Constraint Matrix" ]
      , dd [] 
        [ button [ onClick DecrementRow ] [ text "- Row" ]
        , button [ onClick IncrementRow ] [ text "+ Row" ]
        , button [ onClick DecrementCol ] [ text "- Col" ]
        , button [ onClick IncrementCol ] [ text "+ Col" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , constraintMatrix model
        , button [ onClick Finish ] [ text "Finish" ]
        , span [] [ text model.outputText ]
        ]
      ]
    ]


-- View Helpers
objectiveVector : Model -> Html Msg
objectiveVector model =
  div [] (take (model.numCols * 2 - 1) (concat (toList (indexedMap objectiveInput model.inputData.objective))))

constraintMatrix : Model -> Html Msg
constraintMatrix model =
  div [] (toList (indexedMap (inputRow model.inputData.constraint) model.inputData.matrix))

inputRow : Array String -> Int -> Array String -> Html Msg
inputRow constraintArray rowIdx rowArray =
  div [ style "display" "flex"] 
    [ div [] (toList (indexedMap (matrixInput rowIdx) rowArray))
    , div []
      [ span [] [ text "=" ]
      , input [ value (getString rowIdx constraintArray), onInput (ConstraintInput rowIdx) ] [] 
      ]
    ]

matrixInput : Int -> Int -> String -> Html Msg
matrixInput rowIdx colIdx str =
  input [ value str, onInput (MatrixInput rowIdx colIdx) ] []

objectiveInput : Int -> String -> List (Html Msg)
objectiveInput colIdx str =
  [ input [ value str, onInput (ObjectiveInput colIdx) ] []
  , span [] [ text "+" ]
  ]

getString : Int -> Array String -> String
getString rowIdx arr =
  withDefault "" (get rowIdx arr)

