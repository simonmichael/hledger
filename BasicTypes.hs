module BasicTypes
where
import Utils


type Date = String
type DateTime = String

-- amounts
{- a simple amount is a currency, quantity pair:
   0 
   $1 
   £-50
   EUR 3.44 
   HRS 1.5
   DAYS 3
   GOOG 500

   a mixed amount is one or more simple amounts:
   $50, EUR 3, AAPL 500
   HRS 16, $13.55, oranges 6

   arithmetic:
   $1 - $5 = $-4
   $1 + EUR 0.76 = $2
   EUR0.76 + $1 = EUR 1.52
   EUR0.76 - $1 = 0
   ($5, HRS 2) + $1 = ($6, HRS 2)
   ($50, EUR 3, AAPL 500) + ($13.55, oranges 6) = $67.51, AAPL 500, oranges 6
   ($50, EUR 3) * $-1 = $-53.96
   ($50, AAPL 500) * $-1 = error
   
-}

type Currency = String

data Amount = Amount {
                      currency :: Currency,
                      quantity :: Double
                     } deriving (Eq,Ord)

instance Show Amount where show = showAmountRoundedOrZero

nullamt = Amount "" 0

showAmountRoundedOrZero :: Amount -> String
showAmountRoundedOrZero (Amount cur qty) =
    let rounded = printf "%.2f" qty in
    case rounded of
      "0.00"    -> "0"
      "-0.00"   -> "0"
      otherwise -> cur ++ rounded

instance Num Amount where
    abs (Amount c q) = Amount c (abs q)
    signum (Amount c q) = Amount c (signum q)
    fromInteger i = Amount "$" (fromInteger i)
    (+) = amountAdd
    (-) = amountSub
    (*) = amountMul
Amount ac aq `amountAdd` b = Amount ac (aq + (quantity $ toCurrency ac b))
Amount ac aq `amountSub` b = Amount ac (aq - (quantity $ toCurrency ac b))
Amount ac aq `amountMul` b = Amount ac (aq * (quantity $ toCurrency ac b))

toCurrency :: Currency -> Amount -> Amount
toCurrency newc (Amount oldc q) =
    Amount newc (q * (conversionRate oldc newc))

conversionRate :: Currency -> Currency -> Double
conversionRate oldc newc = (rate newc) / (rate oldc)

rate :: Currency -> Double
rate "$"   = 1.0
rate "EUR" = 0.760383
rate "£"   = 0.512527
rate _     = 1


data MixedAmount = MixedAmount [Amount] deriving (Eq,Ord)

