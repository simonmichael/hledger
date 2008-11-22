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

instance Ord Amount where
    compare (Amount ac aq ap) (Amount bc bq bp) = compare (ac,aq,ap) (bc,bq,bp)

instance Num MixedAmount where
    fromInteger i = Mixed [Amount (comm "") (fromInteger i) Nothing]
    negate (Mixed as) = Mixed $ map negateAmountPreservingPrice as
    (+) (Mixed as) (Mixed bs) = normaliseMixedAmount $ Mixed $ filter (not . isZeroAmount) $ as ++ bs
    (*)    = error "programming error, mixed amounts do not support multiplication"
    abs    = error "programming error, mixed amounts do not support abs"
    signum = error "programming error, mixed amounts do not support signum"

instance Ord MixedAmount where
    compare (Mixed as) (Mixed bs) = compare as bs

negateAmountPreservingPrice a = (-a){price=price a}

-- | Apply a binary arithmetic operator to two amounts - converting to the
-- second one's commodity, adopting the lowest precision, and discarding
-- any price information. (Using the second commodity is best since sum
-- and other folds start with a no-commodity amount.)
amountop :: (Double -> Double -> Double) -> Amount -> Amount -> Amount
amountop op a@(Amount ac aq ap) b@(Amount bc bq bp) = 
    Amount bc ((quantity $ convertAmountTo bc a) `op` bq) Nothing

-- | Convert an amount to the commodity of its saved price, if any.
costOfAmount :: Amount -> Amount
costOfAmount a@(Amount _ _ Nothing) = a
costOfAmount a@(Amount _ q (Just price))
    | isZeroMixedAmount price = nullamt
    | otherwise = Amount pc (pq*q) Nothing
    where (Amount pc pq _) = head $ amounts price

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
showMixedAmount m = concat $ intersperse "\n" $ map showfixedwidth as
    where 
      (Mixed as) = normaliseMixedAmount m
      width = maximum $ map (length . show) $ as
      showfixedwidth = printf (printf "%%%ds" width) . show

-- | Get the string representation of a mixed amount, and if it
-- appears to be all zero just show a bare 0, ledger-style.
showMixedAmountOrZero :: MixedAmount -> String
showMixedAmountOrZero a
    | isZeroMixedAmount a = "0"
    | otherwise = showMixedAmount a

-- | Simplify a mixed amount by combining any component amounts which have
-- the same commodity and the same price.
normaliseMixedAmount :: MixedAmount -> MixedAmount
normaliseMixedAmount (Mixed as) = Mixed as'
    where 
      as' = map sumAmountsPreservingPrice $ group $ sort as
      sort = sortBy cmpsymbolandprice
      cmpsymbolandprice a1 a2 = compare (sym a1,price a1) (sym a2,price a2)
      group = groupBy samesymbolandprice 
      samesymbolandprice a1 a2 = (sym a1 == sym a2) && (price a1 == price a2)
      sym = symbol . commodity

sumAmountsPreservingPrice [] = nullamt
sumAmountsPreservingPrice as = (sum as){price=price $ head as}

-- | Convert a mixed amount's component amounts to the commodity of their
-- saved price, if any.
costOfMixedAmount :: MixedAmount -> MixedAmount
costOfMixedAmount (Mixed as) = Mixed $ map costOfAmount as

-- | The empty simple amount.
nullamt :: Amount
nullamt = Amount unknown 0 Nothing

-- | The empty mixed amount.
nullmixedamt :: MixedAmount
nullmixedamt = Mixed []

-- | A temporary value for parsed transactions which had no amount specified.
missingamt :: MixedAmount
missingamt = Mixed [Amount (Commodity {symbol="AUTO",side=L,spaced=False,comma=False,precision=0}) 0 Nothing]

