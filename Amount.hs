{-|
A simple amount is a currency, quantity pair:

@
  $1 
  £-50
  EUR 3.44 
  GOOG 500
  1.5h
  90m
  0 
@

A mixed amount is one or more simple amounts:

@
  $50, EUR 3, AAPL 500
  16h, $13.55, oranges 6
@

Currencies may be convertible or not (eg, currencies representing
non-money commodities). A mixed amount containing only convertible
currencies can be converted to a simple amount. Arithmetic examples:

@
  $1 - $5 = $-4
  $1 + EUR 0.76 = $2
  EUR0.76 + $1 = EUR 1.52
  EUR0.76 - $1 = 0
  ($5, 2h) + $1 = ($6, 2h)
  ($50, EUR 3, AAPL 500) + ($13.55, oranges 6) = $67.51, AAPL 500, oranges 6
  ($50, EUR 3) * $-1 = $-53.96
  ($50, AAPL 500) * $-1 = error
@   
-}

module Amount
where
import Ledger.Utils
import Ledger.Types
import Currency

tests = runTestTT $ test [
         show (dollars 1)   ~?= "$1.00"
        ,show (hours 1)     ~?= "1h"      -- currently h1.00
        ,parseAmount "$1"   ~?= dollars 1 -- currently 0
        ]

nullamt = dollars 0

parseAmount :: String -> Amount
parseAmount s = nullamt

instance Show Amount where show = showAmountRounded

showAmountRounded :: Amount -> String
showAmountRounded (Amount c q p) =
    (symbol c) ++ ({-punctuatethousands $ -}printf ("%."++show p++"f") q)

showAmountRoundedOrZero :: Amount -> String
showAmountRoundedOrZero a@(Amount c _ _) =
    let s = showAmountRounded a
        noncurrency = drop (length $ symbol c)
        nonnulls = filter (flip notElem "-+,.0")
        iszero = (nonnulls $ noncurrency s) == ""
    in if iszero then "0" else s

punctuatethousands :: String -> String
punctuatethousands s =
    sign ++ (punctuate int) ++ frac
    where 
      (sign,num) = break isDigit s
      (int,frac) = break (=='.') num
      punctuate = reverse . concat . intersperse "," . triples . reverse
      triples "" = []
      triples s = [take 3 s] ++ (triples $ drop 3 s)

instance Num Amount where
    abs (Amount c q p) = Amount c (abs q) p
    signum (Amount c q p) = Amount c (signum q) p
    fromInteger i = Amount (getcurrency "$") (fromInteger i) amtintprecision
    (+) = amountop (+)
    (-) = amountop (-)
    (*) = amountop (*)

-- problem: when an integer is converted to an amount it must pick a
-- precision, which we specify here (should be infinite ?). This can
-- affect amount arithmetic, in particular the sum of a list of amounts.
-- So, we may need to adjust the precision after summing amounts.
amtintprecision = 2

-- | apply op to two amounts, adopting a's currency and lowest precision
amountop :: (Double -> Double -> Double) -> Amount -> Amount -> Amount
amountop op (Amount ac aq ap) b@(Amount _ _ bp) = 
    Amount ac (aq `op` (quantity $ toCurrency ac b)) (min ap bp)

toCurrency :: Currency -> Amount -> Amount
toCurrency newc (Amount oldc q p) =
    Amount newc (q * (conversionRate oldc newc)) p


