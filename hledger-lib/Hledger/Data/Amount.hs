{-# LANGUAGE StandaloneDeriving #-}
{-|
An 'Amount' is some quantity of money, shares, or anything else.

A simple amount is a 'Commodity', quantity pair:

@
  $1 
  £-50
  EUR 3.44 
  GOOG 500
  1.5h
  90 apples
  0 
@

An amount may also have a per-unit price, or conversion rate, in terms
of some other commodity. If present, this is displayed after \@:

@
  EUR 3 \@ $1.35
@

A 'MixedAmount' is zero or more simple amounts.  Mixed amounts are
usually normalised so that there is no more than one amount in each
commodity, and no zero amounts (or, there is just a single zero amount
and no others.):

@
  $50 + EUR 3
  16h + $13.55 + AAPL 500 + 6 oranges
  0
@

We can do limited arithmetic with simple or mixed amounts: either
price-preserving arithmetic with similarly-priced amounts, or
price-discarding arithmetic which ignores and discards prices.

-}

module Hledger.Data.Amount
where
import Hledger.Data.Utils
import Hledger.Data.Types
import Hledger.Data.Commodity


instance Show Amount where show = showAmount
instance Show MixedAmount where show = showMixedAmount
deriving instance Show HistoricalPrice

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
    (+) (Mixed as) (Mixed bs) = normaliseMixedAmount $ Mixed $ as ++ bs
    (*)    = error' "programming error, mixed amounts do not support multiplication"
    abs    = error' "programming error, mixed amounts do not support abs"
    signum = error' "programming error, mixed amounts do not support signum"

instance Ord MixedAmount where
    compare (Mixed as) (Mixed bs) = compare as bs

negateAmountPreservingPrice a = (-a){price=price a}

-- | Apply a binary arithmetic operator to two amounts, converting to the
-- second one's commodity (and display precision), discarding any price
-- information. (Using the second commodity is best since sum and other
-- folds start with a no-commodity amount.)
amountop :: (Double -> Double -> Double) -> Amount -> Amount -> Amount
amountop op a@(Amount _ _ _) (Amount bc bq _) = 
    Amount bc (quantity (convertAmountTo bc a) `op` bq) Nothing

-- | Convert an amount to the specified commodity using the appropriate
-- exchange rate (which is currently always 1).
convertAmountTo :: Commodity -> Amount -> Amount
convertAmountTo c2 (Amount c1 q _) = Amount c2 (q * conversionRate c1 c2) Nothing

-- | Convert mixed amount to the specified commodity
convertMixedAmountTo :: Commodity -> MixedAmount -> Amount
convertMixedAmountTo c2 (Mixed ams) = Amount c2 total Nothing
    where
    total = sum . map (quantity . convertAmountTo c2) $ ams

-- | Convert an amount to the commodity of its saved price, if any.
costOfAmount :: Amount -> Amount
costOfAmount a@(Amount _ _ Nothing) = a
costOfAmount (Amount _ q (Just price))
    | isZeroMixedAmount price = nullamt
    | otherwise = Amount pc (pq*q) Nothing
    where (Amount pc pq _) = head $ amounts price

-- | Get the string representation of an amount, based on its commodity's
-- display settings.
showAmount :: Amount -> String
showAmount (Amount (Commodity {symbol="AUTO"}) _ _) = "" -- can appear in an error message
showAmount a@(Amount (Commodity {symbol=sym,side=side,spaced=spaced}) _ pri) =
    case side of
      L -> printf "%s%s%s%s" sym' space quantity price
      R -> printf "%s%s%s%s" quantity space sym' price
    where
      sym' = quoteCommoditySymbolIfNeeded sym
      space = if spaced then " " else ""
      quantity = showAmount' a
      price = case pri of (Just pamt) -> " @ " ++ showMixedAmount pamt
                          Nothing -> ""

-- XXX refactor
-- | Get the unambiguous string representation of an amount, for debugging.
showAmountDebug :: Amount -> String
showAmountDebug (Amount c q pri) = printf "Amount {commodity = %s, quantity = %s, price = %s}"
                                   (show c) (show q) (maybe "" showMixedAmountDebug pri)

-- | Get the string representation of an amount, without any \@ price.
showAmountWithoutPrice :: Amount -> String
showAmountWithoutPrice a = showAmount a{price=Nothing}

-- | Get the string representation (of the number part of) of an amount
showAmount' :: Amount -> String
showAmount' (Amount (Commodity {comma=comma,precision=p}) q _) = quantity
  where
    quantity = commad $ printf ("%."++show p++"f") q
    commad = if comma then punctuatethousands else id

-- | Add thousands-separating commas to a decimal number string
punctuatethousands :: String -> String
punctuatethousands s =
    sign ++ addcommas int ++ frac
    where 
      (sign,num) = break isDigit s
      (int,frac) = break (=='.') num
      addcommas = reverse . concat . intersperse "," . triples . reverse
      triples [] = []
      triples l  = take 3 l : triples (drop 3 l)

-- | Does this amount appear to be zero when displayed with its given precision ?
isZeroAmount :: Amount -> Bool
isZeroAmount = null . filter (`elem` "123456789") . showAmountWithoutPrice

-- | Is this amount "really" zero, regardless of the display precision ?
-- Since we are using floating point, for now just test to some high precision.
isReallyZeroAmount :: Amount -> Bool
isReallyZeroAmount = null . filter (`elem` "123456789") . printf ("%."++show zeroprecision++"f") . quantity
    where zeroprecision = 8

-- | Is this amount negative ? The price is ignored.
isNegativeAmount :: Amount -> Bool
isNegativeAmount Amount{quantity=q} = q < 0

-- | Access a mixed amount's components.
amounts :: MixedAmount -> [Amount]
amounts (Mixed as) = as

-- | Does this mixed amount appear to be zero - empty, or
-- containing only simple amounts which appear to be zero ?
isZeroMixedAmount :: MixedAmount -> Bool
isZeroMixedAmount = all isZeroAmount . amounts . normaliseMixedAmount

-- | Is this mixed amount "really" zero ? See isReallyZeroAmount.
isReallyZeroMixedAmount :: MixedAmount -> Bool
isReallyZeroMixedAmount = all isReallyZeroAmount . amounts . normaliseMixedAmount

-- | Is this mixed amount negative, if it can be normalised to a single commodity ?
isNegativeMixedAmount :: MixedAmount -> Maybe Bool
isNegativeMixedAmount m = case as of [a] -> Just $ isNegativeAmount a
                                     _   -> Nothing
    where
      as = amounts $ normaliseMixedAmount m

-- | Is this mixed amount "really" zero, after converting to cost
-- commodities where possible ?
isReallyZeroMixedAmountCost :: MixedAmount -> Bool
isReallyZeroMixedAmountCost = isReallyZeroMixedAmount . costOfMixedAmount

-- | MixedAmount derives Eq in Types.hs, but that doesn't know that we
-- want $0 = EUR0 = 0. Yet we don't want to drag all this code in there.
-- When zero equality is important, use this, for now; should be used
-- everywhere.
mixedAmountEquals :: MixedAmount -> MixedAmount -> Bool
mixedAmountEquals a b = amounts a' == amounts b' || (isZeroMixedAmount a' && isZeroMixedAmount b')
    where a' = normaliseMixedAmount a
          b' = normaliseMixedAmount b

-- | Get the string representation of a mixed amount, showing each of
-- its component amounts. NB a mixed amount can have an empty amounts
-- list in which case it shows as \"\".
showMixedAmount :: MixedAmount -> String
showMixedAmount m = vConcatRightAligned $ map show $ amounts $ normaliseMixedAmount m

-- | Get an unambiguous string representation of a mixed amount for debugging.
showMixedAmountDebug :: MixedAmount -> String
showMixedAmountDebug m = printf "Mixed [%s]" as
    where as = intercalate "\n       " $ map showAmountDebug $ amounts $ normaliseMixedAmount m

-- | Get the string representation of a mixed amount, but without
-- any \@ prices.
showMixedAmountWithoutPrice :: MixedAmount -> String
showMixedAmountWithoutPrice m = concat $ intersperse "\n" $ map showfixedwidth as
    where
      (Mixed as) = normaliseMixedAmountIgnoringPrice m
      width = maximum $ map (length . show) as
      showfixedwidth = printf (printf "%%%ds" width) . showAmountWithoutPrice

-- | Get the string representation of a mixed amount, and if it
-- appears to be all zero just show a bare 0, ledger-style.
showMixedAmountOrZero :: MixedAmount -> String
showMixedAmountOrZero a | a == missingamt = ""
                        | isZeroMixedAmount a = "0"
                        | otherwise = showMixedAmount a

-- | Get the string representation of a mixed amount, or a bare 0,
-- without any \@ prices.
showMixedAmountOrZeroWithoutPrice :: MixedAmount -> String
showMixedAmountOrZeroWithoutPrice a
    | isZeroMixedAmount a = "0"
    | otherwise = showMixedAmountWithoutPrice a

-- | Simplify a mixed amount by combining any component amounts which have
-- the same commodity and the same price. Also removes zero amounts,
-- or adds a single zero amount if there are no amounts at all.
normaliseMixedAmount :: MixedAmount -> MixedAmount
normaliseMixedAmount (Mixed as) = Mixed as''
    where 
      as'' = map sumSamePricedAmountsPreservingPrice $ group $ sort as'
      sort = sortBy cmpsymbolandprice
      cmpsymbolandprice a1 a2 = compare (sym a1,price a1) (sym a2,price a2)
      group = groupBy samesymbolandprice 
      samesymbolandprice a1 a2 = (sym a1 == sym a2) && (price a1 == price a2)
      sym = symbol . commodity
      as' | null nonzeros = [head $ zeros ++ [nullamt]]
          | otherwise = nonzeros
      (zeros,nonzeros) = partition isReallyZeroAmount as

-- various sum variants..

sumAmountsDiscardingPrice [] = nullamt
sumAmountsDiscardingPrice as = (sum as){price=Nothing}

sumSamePricedAmountsPreservingPrice [] = nullamt
sumSamePricedAmountsPreservingPrice as = (sum as){price=price $ head as}

-- | Simplify a mixed amount by combining any component amounts which have
-- the same commodity, ignoring and discarding their unit prices if any.
-- Also removes zero amounts, or adds a single zero amount if there are no
-- amounts at all.
normaliseMixedAmountIgnoringPrice :: MixedAmount -> MixedAmount
normaliseMixedAmountIgnoringPrice (Mixed as) = Mixed as''
    where
      as'' = map sumAmountsDiscardingPrice $ group $ sort as'
      group = groupBy samesymbol where samesymbol a1 a2 = sym a1 == sym a2
      sort = sortBy (comparing sym)
      sym = symbol . commodity
      as' | null nonzeros = [head $ zeros ++ [nullamt]]
          | otherwise = nonzeros
          where (zeros,nonzeros) = partition isZeroAmount as

sumMixedAmountsPreservingHighestPrecision :: [MixedAmount] -> MixedAmount
sumMixedAmountsPreservingHighestPrecision ms = foldl' (+~) 0 ms
    where (+~) (Mixed as) (Mixed bs) = normaliseMixedAmountPreservingHighestPrecision $ Mixed $ as ++ bs

normaliseMixedAmountPreservingHighestPrecision :: MixedAmount -> MixedAmount
normaliseMixedAmountPreservingHighestPrecision (Mixed as) = Mixed as''
    where
      as'' = map sumSamePricedAmountsPreservingPriceAndHighestPrecision $ group $ sort as'
      sort = sortBy cmpsymbolandprice
      cmpsymbolandprice a1 a2 = compare (sym a1,price a1) (sym a2,price a2)
      group = groupBy samesymbolandprice
      samesymbolandprice a1 a2 = (sym a1 == sym a2) && (price a1 == price a2)
      sym = symbol . commodity
      as' | null nonzeros = [head $ zeros ++ [nullamt]]
          | otherwise = nonzeros
      (zeros,nonzeros) = partition isReallyZeroAmount as

sumSamePricedAmountsPreservingPriceAndHighestPrecision [] = nullamt
sumSamePricedAmountsPreservingPriceAndHighestPrecision as = (sumAmountsPreservingHighestPrecision as){price=price $ head as}

sumAmountsPreservingHighestPrecision :: [Amount] -> Amount
sumAmountsPreservingHighestPrecision as = foldl' (+~) 0 as
    where (+~) = amountopPreservingHighestPrecision (+)

amountopPreservingHighestPrecision :: (Double -> Double -> Double) -> Amount -> Amount -> Amount
amountopPreservingHighestPrecision op a@(Amount ac@Commodity{precision=ap} _ _) (Amount bc@Commodity{precision=bp} bq _) = 
    Amount c q Nothing
    where
      q = quantity (convertAmountTo bc a) `op` bq
      c = if ap > bp then ac else bc
--

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
missingamt = Mixed [Amount Commodity {symbol="AUTO",side=L,spaced=False,comma=False,precision=0} 0 Nothing]


tests_Amount = TestList [

   "showMixedAmount" ~: do
     showMixedAmount (Mixed [Amount dollar 0 Nothing]) `is` "$0.00"
     showMixedAmount (Mixed []) `is` "0"
     showMixedAmount missingamt `is` ""

  ,"showMixedAmountOrZero" ~: do
     showMixedAmountOrZero (Mixed [Amount dollar 0 Nothing]) `is` "0"
     showMixedAmountOrZero (Mixed []) `is` "0"
     showMixedAmountOrZero missingamt `is` ""

  ,"amount arithmetic" ~: do
    let a1 = dollars 1.23
    let a2 = Amount (comm "$") (-1.23) Nothing
    let a3 = Amount (comm "$") (-1.23) Nothing
    (a1 + a2) `is` Amount (comm "$") 0 Nothing
    (a1 + a3) `is` Amount (comm "$") 0 Nothing
    (a2 + a3) `is` Amount (comm "$") (-2.46) Nothing
    (a3 + a3) `is` Amount (comm "$") (-2.46) Nothing
    sum [a2,a3] `is` Amount (comm "$") (-2.46) Nothing
    sum [a3,a3] `is` Amount (comm "$") (-2.46) Nothing
    sum [a1,a2,a3,-a3] `is` Amount (comm "$") 0 Nothing
    let dollar0 = dollar{precision=0}
    (sum [Amount dollar 1.25 Nothing, Amount dollar0 (-1) Nothing, Amount dollar (-0.25) Nothing])
      `is` (Amount dollar 0 Nothing)

  ,"mixed amount arithmetic" ~: do
    let dollar0 = dollar{precision=0}
    (sum $ map (Mixed . (\a -> [a]))
             [Amount dollar 1.25 Nothing,
              Amount dollar0 (-1) Nothing,
              Amount dollar (-0.25) Nothing])
      `is` Mixed [Amount dollar 0 Nothing]


  ]
