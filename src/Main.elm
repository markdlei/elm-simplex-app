module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, value, selected)
import Html.Events exposing (onClick, onInput)
import Array exposing (Array, initialize, get, set, push, slice, toList, indexedMap, repeat, length, filter, foldl, append)
import String exposing (toInt, fromInt, dropRight, contains)
import Maybe exposing (withDefault)
import List exposing (concat, take)
import Tuple exposing (pair, first, second, mapSecond)



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
  -- , outputData: Data
  }

type alias InputData =
  { isMaximizationProb: Bool
  , objective: Array String
  , matrix: Array (Array String)
  , constraint: Array String
  , constraintType: Array String
  , domains: Array String
  }

type alias Data =
  { isMaximizationProb: Bool
  , objective: Array Int
  , matrix: Array (Array Int)
  , constraint: Array Int
  , constraintType: Array String
  , domains: Array String
  }


init : Model
init =
  Model
    3
    3
    (InputData 
      True
      (repeat 3 "")
      (repeat 3 (repeat 3 ""))
      (repeat 3 "")
      (repeat 3 "=")
      (repeat 3 ">= 0")
    )
    ""



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
  | ChangeProblemType String
  | ChangeConstraintType Int String
  | ChangeXDomain Int String
  | Finish


update : Msg -> Model -> Model
update msg model =
  case msg of
    IncrementRow ->
      let
        oldData = model.inputData
        newData =
          { oldData
          | matrix = push (repeat model.numCols "") model.inputData.matrix
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

    ChangeProblemType maxOrMin ->
      let
        oldData = model.inputData
        newData = { oldData | isMaximizationProb = contains "max" maxOrMin }
      in
        { model
        | inputData = newData
        }

    ChangeConstraintType idx constraint ->
      let
        oldData = model.inputData
        newData = { oldData | constraintType = set idx constraint model.inputData.constraintType }
      in
        { model
        | inputData = newData
        }

    ChangeXDomain idx str ->
      let
        oldData = model.inputData
        newData = { oldData | domains = set idx str model.inputData.domains }
      in
        { model
        | inputData = newData
        }

    Finish ->
      { model | outputText = Debug.toString (toSEF (inputToNumData model.inputData)) }
      -- { model | outputText = formatOutput solve findCanon toSEF inputToNumData model.inputData }


-- Update Helpers
setMatrixCell : Array (Array String) -> Int -> Int -> String -> Array (Array String)
setMatrixCell matrix rowIdx colIdx val =
  set rowIdx (set colIdx val (getMatrixRow matrix rowIdx)) matrix

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
  {  isMaximizationProb = input.isMaximizationProb
  , objective = Array.map anyStringToInt input.objective
  , matrix = Array.map (Array.map anyStringToInt) input.matrix
  , constraint = Array.map anyStringToInt input.constraint
  , constraintType = input.constraintType
  , domains = input.domains
  }

toSEF : Data -> Data
toSEF data =
  makeConstraintsEq (makeDomainsPositive (makeMaximization data))

makeMaximization : Data -> Data
makeMaximization data =
  if data.isMaximizationProb then
    data
  else
    { data
    | isMaximizationProb = True
    , objective = Array.map ((*) -1) data.objective
    }

makeDomainsPositive : Data -> Data
makeDomainsPositive data =
  let
    domainToNum : String -> Int
    domainToNum domain = 
      if domain == "<= 0" then
        -1
      else
        1
    addIdxIfFree : Int -> String -> Int
    addIdxIfFree idx domain =
      if domain == "free" then
        idx
      else
        -1
    addAntiFreeCols : Array Int -> Array Int -> Array Int
    addAntiFreeCols frees shortArray =
      foldl duplicateAndNegate shortArray frees
    duplicateAndNegate : Int -> Array Int -> Array Int
    duplicateAndNegate idx shortArray =
      push ((getInt idx shortArray) * -1) shortArray
      
    freeArray = filter ((/=) -1) (indexedMap addIdxIfFree data.domains)
    multArray = Array.map domainToNum data.domains
    mapMult idx val = (getInt idx multArray) * val
  in
    { data
    | objective = addAntiFreeCols freeArray (indexedMap mapMult data.objective)
    , matrix = Array.map (addAntiFreeCols freeArray) (Array.map (indexedMap mapMult) data.matrix)
    , domains = repeat ((length data.domains) + (length freeArray)) ">= 0"
    }

makeConstraintsEq : Data -> Data
makeConstraintsEq data =
  let
    secondIsNotEq tup = (second tup) /= "="
    typeToVal conType = 
      if conType == ">=" then
        -1
      else
        1
    constraintTypeLoc = (filter secondIsNotEq (indexedMap pair data.constraintType))
    newMatrixColEntry = Array.map (mapSecond typeToVal) constraintTypeLoc
  in
  { data
  | objective = append data.objective (repeat (length constraintTypeLoc) 0)
  , matrix = indexedMap (addToMatrix newMatrixColEntry) data.matrix
  , constraintType = repeat (length data.constraint) "="
  , domains = append data.domains (repeat (length constraintTypeLoc) ">= 0")
  }

addToMatrix : Array (Int, Int) -> Int -> Array Int -> Array Int
addToMatrix newMatrixColEntry rowIdx matrixRow =
  let
    idxMatch tup =
      if (first tup) == rowIdx then
        second tup
      else
        0
  in
    append matrixRow (Array.map idxMatch newMatrixColEntry)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Simplex Solver" ]
    , dl []
      [ dt [] [ text "Objective Function" ]
      , dd [] 
        [ select [ onInput ChangeProblemType ]
          [ option [ value "max", selected True ] [ text "Maximize" ]
          , option [ value "min" ] [ text "Minimize" ]
          ]
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
        , selectXDomains model
        , button [ onClick Finish ] [ text "Finish" ]
        , span [] [ text model.outputText ]
        ]
      ]
    ]


-- View Helpers
objectiveVector : Model -> Html Msg
objectiveVector model =
  div [] (take (model.numCols * 3 - 1) (concat (toList (indexedMap objectiveInput model.inputData.objective))))

constraintMatrix : Model -> Html Msg
constraintMatrix model =
  div [] (toList (indexedMap (inputRow model.numCols model.inputData.constraint) model.inputData.matrix))

inputRow : Int -> Array String -> Int -> Array String -> Html Msg
inputRow numCols constraintArray rowIdx rowArray =
  div [ style "display" "flex" ] 
    [ div [] (take (numCols * 3 - 1) (concat (toList (indexedMap (matrixInput rowIdx) rowArray))))
    , div []
      [ select [ onInput (ChangeConstraintType rowIdx) ]
        [ option [ value "=", selected True ] [ text "=" ]
        , option [ value ">=" ] [ text ">=" ]
        , option [ value "<=" ] [ text "<=" ]
        ]
      , input [ value (getString rowIdx constraintArray), onInput (ConstraintInput rowIdx) ] [] 
      ]
    ]

matrixInput : Int -> Int -> String -> List (Html Msg)
matrixInput rowIdx colIdx str =
  [ input [ value str, onInput (MatrixInput rowIdx colIdx) ] []
  , span [] [ text ("x" ++ (fromInt colIdx)) ]
  , span [] [ text "+" ]
  ]

objectiveInput : Int -> String -> List (Html Msg)
objectiveInput colIdx str =
  [ input [ value str, onInput (ObjectiveInput colIdx) ] []
  , span [] [ text ("x" ++ (fromInt colIdx)) ]
  , span [] [ text "+" ]
  ]

selectXDomains : Model -> Html Msg
selectXDomains model =
  div [ style "display" "flex" ] (toList (initialize model.numCols xDomainOptions))

xDomainOptions : Int -> Html Msg
xDomainOptions idx =
  div []
  [ span [] [ text ("x" ++ (fromInt idx)) ]
  , select [ onInput (ChangeXDomain idx) ]
      [ option [ value ">= 0", selected True ] [ text ">= 0"]
      , option [ value "<= 0" ] [ text "<= 0"]
      , option [ value "free" ] [ text "free"]
      ]
  ]



-- Helpers 


getMatrixRow : Array (Array String) -> Int -> Array String
getMatrixRow matrix rowIdx = 
  case get rowIdx matrix of
    Just arr ->
      arr
    Nothing ->
      Debug.todo "Handle offset getter"

getString : Int -> Array String -> String
getString rowIdx arr =
  withDefault "" (get rowIdx arr)

getInt : Int -> Array Int -> Int
getInt rowIdx arr =
  withDefault 0 (get rowIdx arr)

anyStringToInt : String -> Int
anyStringToInt str =
  withDefault 0 (toInt str)

pop : Array a -> Array a
pop arr =
  slice 0 -1 arr

