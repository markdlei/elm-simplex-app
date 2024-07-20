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
  , dataMatrix: Array (Array String)
  , outputText: String
  }


init : Model
init =
  Model 3 3 (initialize 3 (always (initialize 3 (always "")))) ""



-- UPDATE


type Msg
  = IncrementRow
  | DecrementRow
  | IncrementCol
  | DecrementCol
  | Reset
  | CellInput Int Int String
  | Finish


update : Msg -> Model -> Model
update msg model =
  case msg of
    IncrementRow ->
      { model
      | numRows = model.numRows + 1
      , dataMatrix = push (initialize model.numCols (always "")) model.dataMatrix
      }

    DecrementRow ->
      if model.numRows /= 1 then
        { model
        | numRows = model.numRows - 1
        , dataMatrix = pop model.dataMatrix
        }
      else
        model

    IncrementCol ->
      { model
      | numCols = model.numCols + 1
      , dataMatrix = Array.map (push "") model.dataMatrix
      }

    DecrementCol ->
      if model.numCols /= 1 then
        { model
        | numCols = model.numCols - 1
        , dataMatrix = Array.map pop model.dataMatrix
        }
      else
        model

    Reset ->
      init

    CellInput rowIdx colIdx input ->
      { model
      | dataMatrix = setMatrixCell model.dataMatrix rowIdx colIdx (forceInt input)
      }

    Finish ->
      { model | outputText = Debug.toString (Debug.log "Finish" model.dataMatrix) }


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
        , input [] [] 
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
  div [] (toList (indexedMap inputRow model.dataMatrix))

inputRow : Int -> Array String -> Html Msg
inputRow rowIdx rowArray =
  div [ style "display" "flex"] 
    [ div [] (toList (indexedMap (viewInput rowIdx) rowArray))
    , div []
      [ span [] [ text "=" ]
      , input [] [] 
      ]
    ]

viewInput : Int -> Int -> String -> Html Msg
viewInput rowIdx colIdx str =
  input [ value str, onInput (CellInput rowIdx colIdx) ] []

