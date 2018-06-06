port module ExactBinomial exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Utilities.Border as Border
import Bootstrap.Utilities.Flex as Flex
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup 
import Json.Encode as Encode
import VegaLite exposing (..)
import KaTeX exposing (render, renderToString, renderWithOptions, defaultOptions)





main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Utility 

meanBinom : Int -> Float -> Float
meanBinom n p = (toFloat n)*p

sdBinom : Int -> Float -> Float
sdBinom n p = (meanBinom n p)*(1 - p)^0.5

binomLogCoef : Int -> Int -> Float
binomLogCoef n k =
    let
        nF = toFloat n
        kF = toFloat k
        ks = List.map toFloat (List.range 0 (k-1))
        terms = List.map (\i -> logBase 10 (nF - i) - logBase 10 (kF - i)) ks
    in
        List.sum terms


binomLogCoefRange : Int -> Int -> Int -> List(Float)
binomLogCoefRange n start stop =
    let
        coefStart = binomLogCoef n start
        logDiff = \k -> logBase 10 ( (toFloat n) - k + 1) - logBase 10 k
        nums = List.map toFloat (List.range (start + 1) n)
        logDiffs = List.map logDiff nums 
    in
        (List.scanl (+) coefStart logDiffs)

probRange: Int -> Float -> Int -> Int -> List(Float)
probRange n p start stop =
    let
        xs = List.map toFloat (List.range start stop)
        binomceof = binomLogCoefRange n start stop
        term = \logCoef x -> logCoef + x*(logBase 10 p) + ((toFloat n) - x)*(logBase 10 (1 - p)) 
        logProbs = List.map2 term binomceof xs
    in
        List.map (\logp -> 10^logp) logProbs

binomLogCoefs : Int -> List (Float)
binomLogCoefs n = 
  let
    logDiff = \k -> logBase 10 ( (toFloat n) - k + 1) - logBase 10 k
    nums = List.map toFloat (List.range 1 n)
    logDiffs = List.map logDiff nums 
  in
    (List.scanl (+) 0 logDiffs)

newXs : Int -> List (Int)
newXs n =
    List.range 0 n

type alias Ps = List (Float)

newPs: Int -> Float -> Ps
newPs n p =
    let
        xs = List.map toFloat (newXs n)
        binomceof = binomLogCoefs n
        term = \logCoef x -> logCoef + x*(logBase 10 p) + ((toFloat n) - x)*(logBase 10 (1 - p)) 
        logProbs = List.map2 term binomceof xs
    in
        List.map (\logp -> 10^logp) logProbs

roundFloat : Int -> Float -> Float
roundFloat digits n =
   let
     div = toFloat 10^(toFloat digits)
     shifted = n*div
   in
     round shifted |> toFloat |> \n -> n/div

addAndAppend : Float -> List(Float) -> List(Float)
addAndAppend n acc =
    case List.head acc of
        Just subTotal ->
            (n + subTotal) :: acc
        Nothing ->
            [n]

cumultSuml l = List.reverse (List.foldl addAndAppend [] l)
cumultSumr l = List.foldr addAndAppend [] l

-- MODEL

type alias BinomModel = {xs : List(Int)
                        , ps : List(Float)
                        , psDict : Dict.Dict Int Float
                        , lowerPsDict : Dict.Dict Int Float
                        , upperPsDict : Dict.Dict Int Float
                        , twoPsDict : Dict.Dict Int Float
                        , n : Int
                        , p : Float
                        , mean : Float
                        , minX : Int
                        , maxX : Int
                        }



binomDataRange : Int -> Float -> Int -> Int -> BinomModel
binomDataRange n p start stop = 
    let
        xs = List.range start stop
        ps = probRange n p start stop
        psDict = Dict.fromList (List.map2 (,) xs ps)
        mean = meanBinom n p
        lowerPs = (cumultSuml ps)
        upperPs = (cumultSumr ps)
        lowerDict = Dict.fromList (List.map2 (,) xs lowerPs)
        upperDict = Dict.fromList (List.map2 (,) xs upperPs)
        (lLim, uLim) = List.unzip (List.map (twoTailLimits mean) xs)
        getTailProb = \dict x -> 
            (case Dict.get x dict of 
                Just p -> p 
                Nothing -> 0)
        leftTail = List.map (getTailProb lowerDict) lLim
        rightTail = List.map (getTailProb upperDict) uLim
        tailProbs = List.map2 (+) leftTail rightTail
        twoDict = Dict.fromList (List.map2 (,) xs tailProbs)
    in
        BinomModel xs 
                   ps 
                   psDict
                   lowerDict 
                   upperDict 
                   twoDict
                   n 
                   p 
                   mean 
                   start 
                   stop

binomDataTrimmed : Int -> Float -> BinomModel
binomDataTrimmed n p =
    let
        mean = meanBinom n p
        sd = sdBinom n p
        minX = Basics.max 0 (round (mean - 6*sd))
        maxX = Basics.min n (round (mean + 6*sd))
    in
        binomDataRange n p minX maxX


filterCol : List(Bool) -> List(number) -> List(number)
filterCol valToKeep vals =
    let
        pairs = List.map2 (,) valToKeep vals
        pairsToKeep = List.filter (\tup -> Tuple.first tup) pairs
    in 
        List.map Tuple.second pairsToKeep


type Tail = Left | Right | Two | None

filterXs : (Int -> Bool) -> BinomModel -> BinomModel
filterXs pred binom =
    let
        toKeep = List.map pred binom.xs
        newXs = (filterCol toKeep binom.xs)
        minX = 
            case List.minimum newXs of
                Just x -> x
                Nothing -> 0
        maxX = 
            case List.maximum newXs of
                Just x -> x
                Nothing -> binom.n
    in
        binomDataRange binom.n binom.p minX maxX

probX : Int -> BinomModel -> Float
probX x binom = 
    case (Dict.get x binom.psDict) of
        Just p -> p
        Nothing -> 0

lowerTail : Int -> BinomModel -> Float
lowerTail x binom =
    case (Dict.get x binom.lowerPsDict) of
        Just p -> p
        Nothing -> 
            if (x < binom.minX) then 0 else 1

upperTail : Int -> BinomModel -> Float
upperTail x binom =
    case (Dict.get x binom.upperPsDict) of
        Just p -> p
        Nothing -> 
            if (x < binom.minX) then 1 else 0

twoTail : BinomModel -> Int -> Float
twoTail binom x = 
    case (Dict.get x binom.twoPsDict) of
        Just p -> Basics.min 1 p
        Nothing -> 0

twoTailLimits : Float -> Int -> (Int, Int)
twoTailLimits mean value =
    let
        valueF = toFloat value
        diff = if (valueF <= mean) then mean - valueF else valueF - mean
        maybeLower = floor (mean - diff)
        maybeUpper = ceiling (mean + diff)
        lower = if (valueF <= mean) then value else maybeLower
        upper = if (valueF <= mean) then maybeUpper else value
    in
        (lower, upper)


type alias Model =
  { n : Result String Int
  , p : Result String Float
  , x : Maybe Int
  , xMsg: Result String Int
  , tail : Tail
  , binom : Result String BinomModel
  , vegaSpec : Result String Spec
  }


model : Model
model =
    let 
        n = (String.toInt "10")
        p = (String.toFloat "0.5")
        x = Nothing
        xMsg = Err "Empty"
        tail = None
        binom = Result.map2 binomDataTrimmed n p
        vegaSpec = Result.map (spec tail x) binom
    in
        Model n p x xMsg tail binom vegaSpec

makeCmd : Model -> Cmd Msg
makeCmd model =
    case model.vegaSpec of
        Ok spec -> elmToJS spec

        Err _ -> Cmd.none

init : (Model, Cmd Msg)
init = (model, makeCmd model)

removeTails : BinomModel -> BinomModel
removeTails binom =
    let
        mean = (toFloat binom.n)*binom.p
        var = (toFloat binom.n)*binom.p*(1.0 - binom.p)
        sd = var^0.5
        minX = Basics.max 0 (mean - 4*sd)
        maxX = Basics.min (mean + 4*sd) (toFloat binom.n)
        pred = \x -> 
            let
                xF = toFloat x
            in
                (xF >= minX) && (xF <= maxX)
    in
        filterXs pred binom

-- old
spec : Tail -> Maybe Int -> BinomModel -> Spec
spec tail limit fullBinom = 
    let
        mean = (toFloat fullBinom.n)*fullBinom.p
        expr = 
            case (tail, limit) of
                (_, Nothing) -> "false"
                (None, _) -> "false"
                (Left, Just limit) ->  "datum.X <= " ++ (toString limit)
                (Right, Just limit) -> "datum.X >= " ++ (toString limit)
                (Two, Just limit)->  
                    let
                        (lower, upper) = twoTailLimits mean limit 
                    in
                        if (mean == (toFloat limit)) then "true" 
                        else"datum.X <= " ++ (toString lower) ++ " || " ++
                             "datum.X >= " ++ (toString upper)

        binom = removeTails fullBinom
        d = dataFromColumns []
            << dataColumn "X" (nums (List.map toFloat binom.xs))
            << dataColumn "P(X)" (nums binom.ps)

        trans =
            transform
                << filter (fiExpr expr)
        
        encPMF = 
            encoding
                << position X [ pName "X", pMType Quantitative]
                << position Y [ pName "P(X)", pAggregate Sum, pMType Quantitative ]
                << tooltips [ [ tName "X", tMType Quantitative]
                            , [ tName "P(X)", tMType Quantitative, tFormat ".3f"]
                            ]
        
        selectedEnc =
            encoding
                << position X [ pName "X", pMType Quantitative]
                << position Y [ pName "P(X)", pAggregate Sum, pMType Quantitative ]
                << tooltips [ [ tName "X", tMType Quantitative]
                            , [ tName "P(X)", tMType Quantitative, tFormat ".3f"]
                            ]
                << color [ mStr "red" ]


    in
        toVegaLite
            [ VegaLite.width 600
            , VegaLite.height 400
            ,
            d []
            , layer [ asSpec [ bar [], encPMF []]
                    , asSpec  [ bar [], selectedEnc [], trans []]
                    ]
            ]

-- UPDATE

type Msg
  = ChangeN String 
  | ChangeP String 
  | ChangeTail Tail
  | ChangeSearch String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeTail tail ->
        let
            new_model = model
                        |> updateTail tail
                        |> updateSpec
        in
            (new_model, makeCmd new_model)

    ChangeN txt ->
        let
            new_model = model 
                        |> updateN txt
                        |> updateX ""
                        |> updateBinom
                        |> updateSpec
        in
            (new_model, makeCmd new_model)

    ChangeP txt ->
        let
            new_model = model
                        |> updateP txt
                        |> updateX ""
                        |> updateBinom
                        |> updateSpec
        in
            (new_model, makeCmd new_model)

    ChangeSearch txt ->
        let
            new_model = model
                        |> updateX txt
                        |> updateSpec
        in
            (new_model, makeCmd new_model)

updateTail : Tail -> Model -> Model
updateTail tail model =
        {model | tail = tail}

updateSpec : Model -> Model
updateSpec model = 
    {model | vegaSpec = Result.map (spec model.tail model.x) model.binom}

updateBinom : Model -> Model
updateBinom model = 
            {model | binom = Result.map2 binomDataTrimmed model.n model.p}

updateVal : (String -> Result String a) -> (a -> Bool) -> String -> String -> Result String a
updateVal convert validRange errorMsg txt =  
                    case convert txt of 
                        Ok val -> 
                            if (validRange val) then
                                Ok val
                            else Err errorMsg
            
                        Err _ ->
                            Err errorMsg

updateN : String -> Model -> Model
updateN txt model = 
    let
        maxN = 20000
    in
        {model | n = (updateVal String.toInt 
                             (\n -> n > 1 && n <= maxN)  
                             ("n needs to be a whole number with 1 < n <= " ++ (toString maxN))
                             txt)}

updateX : String -> Model -> Model
updateX txt model =
    case model.n of
        Ok n ->
            if (String.isEmpty txt) then 
                {model | x = Nothing,
                         xMsg = Err "Empty"}
            else case String.toInt txt of
                    Ok x -> 
                        if (0 <= x && x<= n) then 
                            {model | x = Just x,
                                     xMsg = Ok x}
                        else {model | x = Nothing,
                                     xMsg = Err "x must satisfy 0 <= x <= n"}
                    Err _ ->
                        {model | x = Nothing,
                                 xMsg = Err "X needs to be in integer"}
        _ -> model

updateInner : String -> String -> Result String Float
updateInner name txt =  (updateVal String.toFloat 
                               (\p -> p >= 0 && p <= 1)
                               (name ++" needs to be between 0 and 1 ")
                               txt)
updateP : String -> Model -> Model
updateP txt model = {model | p = updateInner "p" txt}


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    div [] [ Grid.container []
                [ Grid.row []
                    [ Grid.col [ Col.md4, Col.sm4 ] [sidebar model]
                    , Grid.col [ Col.md8, Col.sm8] [div [id "vis"][]]
                    ]
                ]
            ]


sidebar model =
   div [] [ h3 [] [Html.text "Exact Binomial Probability"]
          , (inputGroup "Sample Size" "10" ChangeN "n = ")
          , outputVal model.n
          , inputGroup "probability" "0.5" ChangeP "p = "
          , outputVal model.p
          , br [] [] 
          , ButtonGroup.radioButtonGroup []
                  [ ButtonGroup.radioButton
                          (model.tail == Left)
                          [ Button.primary, Button.onClick <| ChangeTail Left ]
                          [ Html.text "Left-tail" ]
                  , ButtonGroup.radioButton
                          (model.tail == Right)
                          [ Button.primary, Button.onClick <| ChangeTail Right ]
                          [ Html.text "Right-tail" ]
                  , ButtonGroup.radioButton
                          (model.tail == Two)
                          [ Button.primary, Button.onClick <| ChangeTail Two ]
                          [ Html.text "Two-tail" ]
                  ]
          , inputX model
          , outputX model.xMsg
          , br [] [] 
          , br [] [] 
          , h4 [] [Html.text "Probability"]
          , displayXProb model
          ]


inputGroup id default onchange intext =
    InputGroup.config (InputGroup.number [ Input.id id
                                         , Input.small
                                         , Input.defaultValue default
                                         , Input.onInput onchange
                                         ])
   |> InputGroup.predecessors [ InputGroup.span [] [ Html.text intext] ]
   |> InputGroup.view

inputX model =
    case model.x of
        Just x -> inputGroup "x" (toString x) ChangeSearch "x = "
        Nothing -> inputGroup "x" "" ChangeSearch "x = "

probXString : Tail -> Maybe Int -> BinomModel -> String
probXString  tail x binom =
    case x of
        Just val ->
            let
                xStr = toString val
                prob =  roundFloat 3 (case tail of
                                          Left -> lowerTail val binom
                                          Right -> upperTail val binom
                                          Two -> twoTail binom val
                                          None -> 0)
                probStr = toString prob
                (left, right) = twoTailLimits binom.mean val
                (leftStr, rightStr) = (toString left, toString right)
            in
                case tail of
                    Left ->  "P(X \\le " ++ xStr ++ ") = " ++ probStr
                    Right -> "P(X \\ge " ++ xStr ++ ") = " ++ probStr
                    Two ->   "P(X \\le " ++ leftStr ++ "\\text{ or }X \\ge" ++ rightStr ++ ") = " ++ probStr
                    None -> ""
        Nothing -> ""

displayXProb model =
    let
        maybeProbStr = Result.map (probXString model.tail model.x) model.binom
    in
        case maybeProbStr of
            Ok probStr -> 
                div [] [ render probStr ]
            _ ->
                div [] [Html.text ""]


outputVal : Result String a -> Html msg
outputVal resultVal = 
    case resultVal of 
        Ok val -> Html.text ""
            
        Err msg ->
            span [class "error"] [Html.text msg]

outputX resultVal =
    case resultVal of 
        Ok val -> Html.text ""
            
        Err "Empty" ->
            span [class "error"] [Html.text  "Enter a value for the observed number of success, x."]

        Err msg ->
            span [class "error"] [Html.text msg]

display : String -> a -> Html msg
display name value =
  div [] [ Html.text (name ++ " ==> " ++ toString value) ]


-- VegaStuff

port elmToJS : Spec -> Cmd msg
