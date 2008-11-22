{-|
An 'Amount' is some quantity of money, shares, or anything else.

A simple amount is a commodity, quantity pair (where commodity can be anything):

@
  $1 
  £-50
  EUR 3.44 
  GOOG 500
  1.5h
  90apples
  0 
@

A mixed amount (not yet implemented) is one or more simple amounts:

@
  $50, EUR 3, AAPL 500
  16h, $13.55, oranges 6
@

Commodities may be convertible or not. A mixed amount containing only
convertible commodities can be converted to a simple amount. Arithmetic
examples:

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

module Ledger.Amount
where
import qualified Data.Map as Map
import Ledger.Utils
import Ledger.Types
import Ledger.Commodity


instance Show Amount where show = showAmount
instance Show MixedAmount where show = showMixedAmount

instance Num Amount where
    abs (Amount c q p) = Amount c (abs q) p
    signum (Amount c q p) = Amount c (signum q) p
    fromInteger i = Amount (comm "") (fromInteger i) Nothing
    (+) = amountop (+)
    (-) = amountop (-)
    (*) = amountop (*)

instance Num MixedAmount where
    fromInteger i = Mixed [Amount (comm "") (fromInteger i) Nothing]
    negate (Mixed as) = Mixed $ map negate as
    (+) (Mixed as) (Mixed bs) = normaliseMixedAmount $ Mixed $ filter (not . isZeroAmount) $ as ++ bs
    (*)    = error "programming error, mixed amounts do not support multiplication"
    abs    = error "programming error, mixed amounts do not support abs"
    signum = error "programming error, mixed amounts do not support signum"

-- | Apply a binary arithmetic operator to two amounts - converting to the
-- second one's commodity, adopting the lowest precision, and discarding
-- any price information. (Using the second commodity is best since sum
-- and other folds start with a no-commodity amount.)
amountop :: (Double -> Double -> Double) -> Amount -> Amount -> Amount
amountop op a@(Amount ac aq ap) b@(Amount bc bq bp) = 
    Amount bc ((quantity $ convertAmountTo bc a) `op` bq) Nothing

-- | Convert an amount to the specified commodity using the appropriate
-- exchange rate (which is currently always 1).
convertAmountTo :: Commodity -> Amount -> Amount
convertAmountTo c2 (Amount c1 q p) = Amount c2 (q * conversionRate c1 c2) Nothing

-- | Get the string representation of an amount, based on its commodity's
-- display settings.
showAmount :: Amount -> String
showAmount (Amount (Commodity {symbol=sym,side=side,spaced=spaced,comma=comma,precision=p}) q pri)
    | sym=="AUTO" = "" -- can display one of these in an error message
    | side==L = printf "%s%s%s%s" sym space quantity price
    | side==R = printf "%s%s%s%s" quantity space sym price
    where 
      space = if spaced then " " else ""
      quantity = commad $ printf ("%."++show p++"f") q
      commad = if comma then punctuatethousands else id
      price = case pri of (Just pamt) -> " @ " ++ showMixedAmount pamt
                          Nothing -> ""

-- | Add thousands-separating commas to a decimal number string
punctuatethousands :: String -> String
punctuatethousands s =
    sign ++ (addcommas int) ++ frac
    where 
      (sign,num) = break isDigit s
      (int,frac) = break (=='.') num
      addcommas = reverse . concat . intersperse "," . triples . reverse
      triples [] = []
      triples l  = [take 3 l] ++ (triples $ drop 3 l)

-- | Does this amount appear to be zero when displayed with its given precision ?
isZeroAmount :: Amount -> Bool
isZeroAmount a = nonzerodigits == ""
    where nonzerodigits = filter (`elem` "123456789") $ showAmount a

-- | Access a mixed amount's components.
amounts :: MixedAmount -> [Amount]
amounts (Mixed as) = as

-- | Does this mixed amount appear to be zero - empty, or
-- containing only simple amounts which appear to be zero ?
isZeroMixedAmount :: MixedAmount -> Bool
isZeroMixedAmount = all isZeroAmount . amounts . normaliseMixedAmount

-- | Get the string representation of a mixed amount, showing each of
-- its component amounts.
showMixedAmount :: MixedAmount -> String
showMixedAmount m = concat $ intersperse ", " $ map show as
    where (Mixed as) = normaliseMixedAmount m

-- | Get the string representation of a mixed amount, and if it
-- appears to be all zero just show a bare 0, ledger-style.
showMixedAmountOrZero :: MixedAmount -> String
showMixedAmountOrZero a
    | isZeroMixedAmount a = "0"
    | otherwise = showMixedAmount a

-- | Simplify a mixed amount by combining any of its component amounts
-- which have the same commodity.
normaliseMixedAmount :: MixedAmount -> MixedAmount
normaliseMixedAmount (Mixed as) = Mixed $ map sum $ grouped
    where 
      grouped = [filter (hassymbol s) as | s <- symbols]
      symbols = sort $ nub $ map (symbol . commodity) as
      hassymbol s a = s == (symbol $ commodity a)


-- | The empty simple amount.
nullamt :: Amount
nullamt = Amount unknown 0 Nothing

-- | The empty mixed amount.
nullmixedamt :: MixedAmount
nullmixedamt = Mixed []

-- | A temporary value for parsed transactions which had no amount specified.
missingamt :: MixedAmount
missingamt = Mixed [Amount (Commodity {symbol="AUTO",side=L,spaced=False,comma=False,precision=0}) 0 Nothing]

