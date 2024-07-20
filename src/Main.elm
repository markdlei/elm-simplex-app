module Main exposing (..)

import Browser
import Html exposing (Html, div, button, input, span, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)

import Array exposing (Array, initialize, get, set, push, slice, toList, map, indexedMap)
import Array exposing (toIndexedList)
import String exposing (toInt, fromInt)
import Html exposing (strong)
import String exposing (dropRight)



-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { numRows: Int
  , numCols: Int
  , dataMatrix: Array (Array String)
  , randText: String
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
      , dataMatrix = map  (push "") model.dataMatrix
      }

    DecrementCol ->
      if model.numCols /= 1 then
        { model
        | numCols = model.numCols - 1
        , dataMatrix = map pop model.dataMatrix
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
      { model | randText = Debug.toString (Debug.log "Finish" model.dataMatrix) }

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
    [ button [ onClick DecrementRow ] [ text "- Row" ]
    , button [ onClick IncrementRow ] [ text "+ Row" ]
    , button [ onClick DecrementCol ] [ text "- Col" ]
    , button [ onClick IncrementCol ] [ text "+ Col" ]
    , button [ onClick Reset ] [ text "Reset" ]
    , inputMatrix model
    , button [ onClick Finish ] [ text "Finish" ]
    , span [] [ text model.randText ]
    ]

inputMatrix : Model -> Html Msg
inputMatrix model =
  div [] (toList (indexedMap inputRow model.dataMatrix))

inputRow : Int -> Array String -> Html Msg
inputRow rowIdx rowArray =
  div [] (toList (indexedMap (viewInput rowIdx) rowArray))

viewInput : Int -> Int -> String -> Html Msg
viewInput rowIdx colIdx str =
  input [ value str, onInput (CellInput rowIdx colIdx) ] []

