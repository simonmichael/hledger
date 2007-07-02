module Amount
where
import Utils
import Types

{- 
a simple amount is a currency, quantity pair:

  $1 
  £-50
  EUR 3.44 
  GOOG 500
  1.5h
  90m
  0 

a mixed amount is one or more simple amounts:

  $50, EUR 3, AAPL 500
  16h, $13.55, oranges 6

arithmetic:

  $1 - $5 = $-4
  $1 + EUR 0.76 = $2
  EUR0.76 + $1 = EUR 1.52
  EUR0.76 - $1 = 0
  ($5, 2h) + $1 = ($6, 2h)
  ($50, EUR 3, AAPL 500) + ($13.55, oranges 6) = $67.51, AAPL 500, oranges 6
  ($50, EUR 3) * $-1 = $-53.96
  ($50, AAPL 500) * $-1 = error
   
-}

tests = runTestTT $ test [
         show (dollars 1)   ~?= "$1.00"
        ,
         show (hours 1)     ~?= "1h"      -- currently h1.00
        ,
         parseAmount "$1"   ~?= dollars 1 -- currently 0
        ]

-- currency

data Currency = Currency {
      symbol :: String,
      rate :: Double -- relative to the dollar
    } deriving (Eq,Show)

currencies = 
    [
     Currency "$"   1        
    ,Currency "EUR" 0.760383 
    ,Currency "£"   0.512527 
    ,Currency "h"   60         -- hours
    ,Currency "m"   1          -- minutes
    ]

getcurrency :: String -> Currency
getcurrency s = head $ [(Currency symbol rate) | (Currency symbol rate) <- currencies, symbol==s]

-- convenience
dollars = Amount $ getcurrency "$"
euro    = Amount $ getcurrency "EUR"
pounds  = Amount $ getcurrency "£"
hours   = Amount $ getcurrency "h"
minutes = Amount $ getcurrency "m"

conversionRate :: Currency -> Currency -> Double
conversionRate oldc newc = (rate newc) / (rate oldc)


-- amount    

data Amount = Amount {
                      currency :: Currency,
                      quantity :: Double
                     } deriving (Eq)

instance Show Amount where show = showAmountRoundedOrZero

nullamt = dollars 0

parseAmount :: String -> Amount
parseAmount s = nullamt

showAmountRoundedOrZero :: Amount -> String
showAmountRoundedOrZero (Amount cur qty) =
    let rounded = printf "%.2f" qty in
    case rounded of
      "0.00"    -> "0"
      "-0.00"   -> "0"
      otherwise -> (symbol cur) ++ rounded

instance Num Amount where
    abs (Amount c q) = Amount c (abs q)
    signum (Amount c q) = Amount c (signum q)
    fromInteger i = Amount (getcurrency "$") (fromInteger i)
    (+) = amountAdd
    (-) = amountSub
    (*) = amountMul
Amount ac aq `amountAdd` b = Amount ac (aq + (quantity $ toCurrency ac b))
Amount ac aq `amountSub` b = Amount ac (aq - (quantity $ toCurrency ac b))
Amount ac aq `amountMul` b = Amount ac (aq * (quantity $ toCurrency ac b))

toCurrency :: Currency -> Amount -> Amount
toCurrency newc (Amount oldc q) =
    Amount newc (q * (conversionRate oldc newc))


-- mixed amounts

--data MixedAmount = MixedAmount [Amount] deriving (Eq,Ord)

