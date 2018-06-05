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

type alias N = Int

type alias Xs = List (Int)

newXs : Int -> Xs
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
                        , lowerPs : List(Float)
                        , lowerPsDict : Dict.Dict Int Float
                        , upperPs : List(Float)
                        , upperPsDict : Dict.Dict Int Float
                        , n : Int
                        , p : Float
                        , mean : Float
                        , minX : Int
                        , maxX : Int
                        }

binomData : Int -> Float -> BinomModel
binomData n p = 
    let
        xs = List.range 0 n
        ps = newPs n p
        psDict = Dict.fromList (List.map2 (,) xs ps)
        lowerPs = cumultSuml ps
        upperPs = cumultSumr ps
        lowerDict = Dict.fromList (List.map2 (,) xs lowerPs)
        upperDict = Dict.fromList (List.map2 (,) xs upperPs)
        mean = meanBinom n p
    in
        BinomModel xs 
                   ps 
                   psDict
                   lowerPs 
                   lowerDict 
                   upperPs 
                   upperDict 
                   n 
                   p 
                   mean 
                   0 
                   n


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
    in
        BinomModel xs 
                   ps 
                   psDict
                   lowerPs 
                   lowerDict 
                   upperPs 
                   upperDict 
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

maxProb : BinomModel -> Float
maxProb binom = 
    case List.maximum binom.ps of
        Just num -> num

        Nothing -> 1


filterCol : List(Bool) -> List(number) -> List(number)
filterCol valToKeep vals =
    let
        pairs = List.map2 (,) valToKeep vals
        pairsToKeep = List.filter (\tup -> Tuple.first tup) pairs
    in 
        List.map Tuple.second pairsToKeep


filterProbs : Float -> BinomModel -> BinomModel
filterProbs limit binom =
    let
        toKeep = List.map (\p -> p > limit) binom.ps
        newXs = (filterCol toKeep binom.xs)
        mean = meanBinom binom.n binom.p
        minX = 
            case List.minimum newXs of
                Just x -> x
                Nothing -> 0
        maxX = 
            case List.maximum newXs of
                Just x -> x
                Nothing -> binom.n
        ps = filterCol toKeep binom.ps
        psDict = Dict.fromList (List.map2 (,) newXs ps)
        lowerPs = filterCol toKeep binom.lowerPs
        upperPs = filterCol toKeep binom.upperPs
        lowerDict = Dict.fromList (List.map2 (,) newXs lowerPs)
        upperDict = Dict.fromList (List.map2 (,) newXs upperPs)

    in
        BinomModel newXs
                   ps
                   psDict
                   lowerPs
                   lowerDict
                   upperPs
                   upperDict
                   binom.n
                   binom.p
                   mean
                   minX
                   maxX

type Tail = Left | Right | Two

filterXs : (Int -> Bool) -> BinomModel -> BinomModel
filterXs pred binom =
    let
        toKeep = List.map pred binom.xs
        newXs = (filterCol toKeep binom.xs)
        mean = meanBinom binom.n binom.p
        minX = 
            case List.minimum newXs of
                Just x -> x
                Nothing -> 0
        maxX = 
            case List.maximum newXs of
                Just x -> x
                Nothing -> binom.n
        ps = filterCol toKeep binom.ps
        psDict = Dict.fromList (List.map2 (,) newXs ps)
        lowerPs = filterCol toKeep binom.lowerPs
        upperPs = filterCol toKeep binom.upperPs
        lowerDict = Dict.fromList (List.map2 (,) newXs lowerPs)
        upperDict = Dict.fromList (List.map2 (,) newXs upperPs)
    in
        BinomModel newXs
                   ps
                   psDict
                   lowerPs
                   lowerDict
                   upperPs
                   upperDict
                   binom.n
                   binom.p
                   mean
                   minX
                   maxX

twoTailProb : BinomModel -> Int -> Float
twoTailProb binom x =
    let
        mean = (toFloat binom.n)*binom.p
        diff = (toFloat x) - mean
        lower = if (diff <= 0) then x else (floor (mean - diff))
        upper = if (diff <= 0) then (ceiling (mean + diff)) else x
        lowerArea = pBinomOneTail .lowerPs lower binom
        upperArea = pBinomOneTail .upperPs upper binom
    in
        if (diff == 0) then 1 else (lowerArea + upperArea)

twoTailProbs : BinomModel -> List(Float)
twoTailProbs binom = 
    List.map (twoTailProb binom) binom.xs

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

twoTailLimits : Int -> Float -> (Int, Int)
twoTailLimits value mean =
    let
        valueF = toFloat value
        diff = if (valueF <= mean) then mean - valueF else valueF - mean
        maybeLower = floor (mean - diff)
        maybeUpper = ceiling (mean + diff)
        lower = if (valueF <= mean) then value else maybeLower
        upper = if (valueF <= mean) then maybeUpper else value
    in
        (lower, upper)

twoTail : BinomModel -> Int -> Float
twoTail binom value =
    let
        valueF = toFloat value
        nF = toFloat binom.n
        mean = nF*binom.p
        (lower, upper) = twoTailLimits value mean
    in 
        if ((mean - valueF) < 0.01) then 1 else (lowerTail lower binom) + (upperTail upper binom)

pBinomOneTail selector x binom =
    let
        probX : (Int, Float) -> Float -> Float
        probX tup acc = if Tuple.first tup == x then Tuple.second tup else acc
        tailProbs = selector binom
        pairs = List.map2 (,) binom.xs tailProbs
    in
        List.foldl probX 0 pairs


pBinom : Tail -> Int -> BinomModel -> Float
pBinom tail x binom =
    case tail of
        Left ->
            pBinomOneTail .lowerPs x binom
        Right ->
            pBinomOneTail .upperPs x binom
        Two ->
            pBinomOneTail twoTailProbs x binom

qBinom : Tail -> BinomModel -> Float -> Int
qBinom  tail binom area =
    let
        xs = 
            case tail of
                Right -> List.reverse (.xs binom)
                _ -> .xs binom
        tailProbs =
            case tail of
                Left -> .lowerPs binom

                Right -> List.reverse (.upperPs binom)

                Two -> List.map (twoTail binom) binom.xs

        tupHelp =  \tup acc-> if Tuple.second tup <=  area then Tuple.first tup else acc

        pairs = List.map2 (,) xs tailProbs

    in
        List.foldl tupHelp 0 pairs

binomToStringList : BinomModel -> String
binomToStringList bData = 
   (let 
       header = (List.map Encode.string ["X", "P(X)", "P(X or lower)", "P(X or above)"])
       xs = (List.map Encode.int bData.xs)
       ps = (List.map Encode.float bData.ps)
       lowerPs = (List.map Encode.float bData.lowerPs)
       upperPs = (List.map Encode.float bData.upperPs)
       rows = (header :: (List.map4 (\x p lp up -> [x, p, lp, up]) xs ps lowerPs upperPs))
   in
       Encode.encode 0 (Encode.list (List.map Encode.list rows))
       )



type alias Model =
  { n : Result String Int
  , p : Result String Float
  , x : Result String Int
  , mean : Float
  , tail : Tail
  , lower : Result String Int
  , lowerActual : Int
  , upper : Result String Int
  , upperActual : Int
  , value : Result String Int
  , valueActual : Int
  , twoLeft : Int
  , twoRight : Int
  , binom : Result String BinomModel
  , probActual : Float
  , vegaSpec : Result String Spec
  }


model : Model
model =
    let 
        n = (String.toInt "10")
        p = (String.toFloat "0.5")
        x = (String.toInt "")
        mean = 5
        tail = Left
        lower = (String.toInt "2")
        lowerActual = 2
        upper = (String.toInt "8")
        value = (String.toInt "2")
        twoLeft = 2
        twoRight = 8
        binom = Result.map2 binomDataTrimmed n p
        probActual = lowerTail lowerActual (binomData 10 0.5)
        vegaSpec = Result.map (spec tail lowerActual) binom
    in
        Model n p x mean tail lower lowerActual upper 8 value 2 twoLeft twoRight binom probActual vegaSpec

makeCmd : Model -> Cmd Msg
makeCmd model =
    case model.vegaSpec of
        Ok spec -> elmToJS spec

        Err _ -> Cmd.none

init : (Model, Cmd Msg)
init = (model, makeCmd model)


encBar minX maxX =
            encoding
                << position X [ pName "X", pMType Quantitative, pScale [ scDomain (doNums [minX, maxX])]]
                << position Y [ pName "P(X)", pAggregate Sum, pMType Quantitative ]
                << tooltips [ [ tName "X", tMType Quantitative]
                            , [ tName "P(X)", tMType Quantitative, tFormat ".3f"]
                            ]

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

spec : Tail -> Int -> BinomModel -> Spec
spec tail limit fullBinom = 
    let
        mean = (toFloat fullBinom.n)*fullBinom.p
        (lower, upper) = twoTailLimits limit mean
        expr = 
            case tail of
                Left ->  "datum.X <= " ++ (toString limit)
                Right -> "datum.X >= " ++ (toString limit)
                Two ->  
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
  | ChangeLimit String 
  | ChangeTail Tail
  | ChangeSearch String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeTail tail ->
        let
            new_model = model
                        |> updateTail tail
                        |> updateLimit
                        |> updateActual
                        |> updateTwoTailLimits
                        |> updateProb
                        |> updateSpec
        in
            (new_model, makeCmd new_model)

    ChangeN txt ->
        let
            new_model = model 
                        |> updateN txt
                        |> updateX ""
                        |> updateMean
                        |> updateBinom
                        |> updateLimit
                        |> updateActual
                        |> updateTwoTailLimits
                        |> updateProb
                        |> updateSpec
        in
            (new_model, makeCmd new_model)

    ChangeP txt ->
        let
            new_model = model
                        |> updateP txt
                        |> updateX ""
                        |> updateMean
                        |> updateBinom
                        |> updateLimit
                        |> updateActual
                        |> updateTwoTailLimits
                        |> updateProb
                        |> updateSpec
        in
            (new_model, makeCmd new_model)

    ChangeLimit txt ->
        let
            new_model = model 
                        |> updateNewLimit txt model.n 
                        |> updateActual
                        |> updateTwoTailLimits
                        |> updateProb
                        |> updateSpec
        in
            (new_model, makeCmd new_model)

    ChangeSearch txt ->
        let
            new_model = model
                        |> updateX txt
        in
            (new_model, Cmd.none)

updateTail : Tail -> Model -> Model
updateTail tail model =
        {model | tail = tail}



maybeUpdateActual : Result String Int -> Int
maybeUpdateActual value =
        case value of
            Ok val -> val
            Err _ -> 0

updateActual : Model -> Model
updateActual model =
            {model | lowerActual = maybeUpdateActual model.lower,
                     upperActual = maybeUpdateActual model.upper,
                     valueActual = maybeUpdateActual model.value}


updateSpec : Model -> Model
updateSpec model =
    let
        limit = 
            case model.tail of
                Left -> model.lowerActual
                Right -> model.upperActual
                Two -> model.valueActual
    in
        {model | vegaSpec = Result.map (spec model.tail limit) model.binom}

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

updateCurrentTail : Model -> Result String Int -> Model
updateCurrentTail model val =
    case model.tail of
        Left ->
            {model | lower = val} 
        Right ->
            {model | upper = val} 
        Two ->
            {model | value = val} 

updateLimit : Model -> Model
updateLimit model =
    let
        tailProb =
            case model.tail of
                Two -> "0.025"
                _ -> "0.05"
        updateHelp = \tail -> Result.map2 (qBinom tail) model.binom (String.toFloat tailProb)
    in
        {model | lower = updateHelp Left,
                 upper = updateHelp Right,
                 value = updateHelp Two}


updateNewLimit : String -> Result String Int -> Model -> Model
updateNewLimit txt n model = 
        case n of
            Ok nVal -> 
                let
                    val = (updateVal String.toInt 
                                             (\k -> (k >= 0) && (k <= nVal))
                                             ((toString model.tail) ++ "- tail bound needs to be a whole number between 0 and n")
                                             txt)
                in
                    updateCurrentTail model val 

            Err _ -> model

updateTwoTailLimits : Model -> Model
updateTwoTailLimits model =
    let
        (lower, upper) = twoTailLimits model.valueActual model.mean
    in
        {model | twoLeft = lower, twoRight = upper}

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
    case String.toInt txt of
        Ok x -> {model | x = String.toInt txt}
        Err _ ->
            {model | x = Err "X needs to be in interger"}

updateInner : String -> String -> Result String Float
updateInner name txt =  (updateVal String.toFloat 
                               (\p -> p >= 0 && p <= 1)
                               (name ++" needs to be between 0 and 1 ")
                               txt)
updateP : String -> Model -> Model
updateP txt model = {model | p = updateInner "p" txt}

updateMean : Model -> Model
updateMean model = 
    case (model.n, model.p) of
        (Ok n, Ok p) -> {model | mean = (toFloat n)*p}
        _ -> model

updateProb : Model -> Model
updateProb model =
    let
        prob = 
            case model.tail of
                Left ->  Result.map (lowerTail model.lowerActual)  model.binom
                Right -> Result.map (upperTail model.upperActual)  model.binom
                Two ->   Result.map (\binom -> twoTail binom model.valueActual) model.binom
    in
        case prob of
            Ok p -> {model | probActual = p}
            Err _ -> model
        

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
               div [] [ (inputGroup "Sample Size" "10" ChangeN "n = ")
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
                                          , inputTail model
                                          , outputLimit model
                                          , br [] [] 
                                          , h4 [] [Html.text "Probability"]
                                          , probElement model
                                          , br [] [] 
                                          , h4 [] [Html.text "Search"]
                                          , inputGroup "x" "" ChangeSearch "x = "
                                          , displayXProbs model
                                          ]

displayCoef n = display ("Binom " ++ (toString n))  
                        (List.map round
                          (List.map (\n -> 10^n)
                            (List.map
                              (binomLogCoef n) 
                              (List.range 0 n))))

displayRange n start stop = 
    let
        lbl = "Range from " ++ (toString start) ++ 
              " to " ++ (toString stop) ++
              " with n = " ++ (toString n)
    in 
        display lbl 
            (List.map (round << (\n -> 10.0^n))
                (binomLogCoefRange n start stop))

displayProbs n p start stop = 
    let
        lbl = "Range from " ++ (toString start) ++ 
              " to " ++ (toString stop) ++
              " with n = " ++ (toString n)
    in 
        display lbl (probRange n p start stop)

inputTail model =
    let 
        label = (toString model.tail) ++ "-tail"
        txt =
            case model.tail of
                Left ->
                    label ++ " upper bound"
                Right ->
                    label ++ " lower bound"
                Two ->
                    "Value"
        default = 
            case model.tail of
                Left ->  toString model.lowerActual
                Right -> toString model.upperActual
                Two ->   toString model.valueActual
    in
        inputGroup label default ChangeLimit txt


outputLimit model =
    case model.tail of
        Left -> outputVal model.lower
        Right -> outputVal model.upper
        Two -> outputVal model.value

inputGroup id default onchange intext =
    InputGroup.config (InputGroup.number [ Input.id id
                                         , Input.small
                                         , Input.defaultValue default
                                         , Input.onInput onchange
                                         ])
   |> InputGroup.predecessors [ InputGroup.span [] [ Html.text intext] ]
   |> InputGroup.view

probString model = 
    let
        lower = toString model.lowerActual
        upper = toString model.upperActual
        left = toString model.twoLeft
        right = toString model.twoRight
        prob = toString (roundFloat 3 model.probActual)
    in
        case model.tail of
            Left ->  "P(X \\le " ++ lower ++ ") = " ++ prob
            Right -> "P(X \\ge " ++ upper ++ ") = " ++ prob
            Two ->   "P(X \\le " ++ left ++ "\\text{ or }X \\ge" ++ right ++ ") = " ++ prob


probElement model =
    div []
            [ render (probString model)
            ]

baseProbString : String -> Int -> Float -> String
baseProbString comp val prob =
  "P(X " ++ comp ++ " " ++ (toString val) ++ ") = " ++ (toString prob)

probXStrings : Int -> BinomModel -> (String, String, String) 
probXStrings x binom =
    let
        prob = probX x binom
        lower = lowerTail x binom
        upper = upperTail x binom
    in
        (baseProbString "=" x prob
        , baseProbString "\\le" x lower
        , baseProbString "\\ge" x upper
        ) 

displayXProbs model =
    let
        maybeProbStr = Result.map2 probXStrings model.x model.binom
    in
        case maybeProbStr of
            Ok (eqStr, lowerStr, upperStr) -> 
                div []
                        [ render eqStr
                        , render lowerStr
                        , render upperStr
                        ]
            _ ->
                div [class "error"] 
                    [Html.text "Enter a value for x."]

outputLeftLimit : Result String Int -> Html Msg
outputLeftLimit  resultLimit =
    case resultLimit of 
        Ok val -> inputGroup "LeftTailLimit" (toString val) ChangeLimit "Left-tail upper bound"
            
        Err msg ->
            div [] [ inputGroup "LeftTailLimit" "" ChangeLimit "Left-tail upper bound"
                   , span [class "error"] [Html.text msg]]

outputVal : Result String a -> Html msg
outputVal resultVal = 
    case resultVal of 
        Ok val -> Html.text ""
            
        Err msg ->
            span [class "error"] [Html.text msg]


outputBinom : Model -> Html msg
outputBinom model =
    case model.binom of
        Ok binom -> 
            div [] [ display ("Binom Model") (binomToStringList binom)]

        Err msg ->
            span [class "error"] [Html.text msg]

display : String -> a -> Html msg
display name value =
  div [] [ Html.text (name ++ " ==> " ++ toString value) ]


-- VegaStuff

port elmToJS : Spec -> Cmd msg
