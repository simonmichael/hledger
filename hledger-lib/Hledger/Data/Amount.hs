{-# LANGUAGE StandaloneDeriving #-}
{-|
A simple "Amount" is some quantity of money, shares, or anything else.
It has a (possibly null) "Commodity" and a numeric quantity:

@
  $1 
  £-50
  EUR 3.44 
  GOOG 500
  1.5h
  90 apples
  0 
@

It may also have an assigned unit price, which is another (unpriced)
simple amount in a different commodity. If present, this is rendered like so:

@
  EUR 3 \@ $1.35
@

A "MixedAmount" is zero or more simple amounts, so can represent multiple
commodities; this is the type most often used:

@
  0
  $50 + EUR 3
  16h + $13.55 + AAPL 500 + 6 oranges
@

When a mixed amount has been \"normalised\", it has no more than one amount
in each commodity and no zero amounts; or it has just a single zero amount
and no others.

We can do two kinds of limited arithmetic with simple or mixed amounts:
price-preserving (for amounts with the same prices) or price-ignoring
(ignores and discards any prices).

-}

-- XXX due for review/rewrite

module Hledger.Data.Amount (
  -- * Amount
  nullamt,
  amountWithCommodity,
  canonicaliseAmountCommodity,
  setAmountPrecision,
  -- ** arithmetic
  costOfAmount,
  divideAmount,
  -- ** rendering
  showAmount,
  showAmountDebug,
  showAmountWithoutPrice,
  maxprecision,
  maxprecisionwithpoint,
  -- * MixedAmount
  nullmixedamt,
  missingamt,
  amounts,
  normaliseMixedAmount,
  canonicaliseMixedAmountCommodity,
  mixedAmountWithCommodity,
  setMixedAmountPrecision,
  -- ** arithmetic
  costOfMixedAmount,
  divideMixedAmount,
  isNegativeMixedAmount,
  isZeroMixedAmount,
  isReallyZeroMixedAmountCost,
  -- ** rendering
  showMixedAmount,
  showMixedAmountDebug,
  showMixedAmountOrZero,
  showMixedAmountOrZeroWithoutPrice,
  showMixedAmountWithoutPrice,
  showMixedAmountWithPrecision,
  -- * misc.
  tests_Hledger_Data_Amount
) where

import Data.Char (isDigit)
import Data.List
import Data.Map (findWithDefault)
import Test.HUnit
import Text.Printf
import qualified Data.Map as Map

import Hledger.Data.Types
import Hledger.Data.Commodity
import Hledger.Utils


deriving instance Show HistoricalPrice

-------------------------------------------------------------------------------
-- Amount

instance Show Amount where show = showAmount

instance Num Amount where
    abs (Amount c q p) = Amount c (abs q) p
    signum (Amount c q p) = Amount c (signum q) p
    fromInteger i = Amount (comm "") (fromInteger i) Nothing
    negate a@Amount{quantity=q} = a{quantity=(-q)}
    (+) = similarAmountsOp (+)
    (-) = similarAmountsOp (-)
    (*) = similarAmountsOp (*)

-- | The empty simple amount.
nullamt :: Amount
nullamt = Amount unknown 0 Nothing

-- | Apply a binary arithmetic operator to two amounts, ignoring and
-- discarding any assigned prices, and converting the first to the
-- commodity of the second in a simplistic way (1-1 exchange rate).
-- The highest precision of either amount is preserved in the result.
similarAmountsOp :: (Double -> Double -> Double) -> Amount -> Amount -> Amount
similarAmountsOp op a@(Amount Commodity{precision=ap} _ _) (Amount bc@Commodity{precision=bp} bq _) =
    Amount bc{precision=max ap bp} (quantity (amountWithCommodity bc a) `op` bq) Nothing

-- | Convert an amount to the specified commodity, ignoring and discarding
-- any assigned prices and assuming an exchange rate of 1.
amountWithCommodity :: Commodity -> Amount -> Amount
amountWithCommodity c (Amount _ q _) = Amount c q Nothing

-- | Convert an amount to the commodity of its assigned price, if any.  Notes:
-- - price amounts must be MixedAmounts with exactly one component Amount (or there will be a runtime error)
-- - price amounts should be positive, though this is not currently enforced
costOfAmount :: Amount -> Amount
costOfAmount a@(Amount _ q price) =
    case price of
      Nothing -> a
      Just (UnitPrice  (Mixed [Amount pc pq Nothing])) -> Amount pc (pq*q) Nothing
      Just (TotalPrice (Mixed [Amount pc pq Nothing])) -> Amount pc (pq*signum q) Nothing
      _ -> error' "costOfAmount: Malformed price encountered, programmer error"

-- | Divide an amount's quantity by a constant.
divideAmount :: Amount -> Double -> Amount
divideAmount a@Amount{quantity=q} d = a{quantity=q/d}

-- | Is this amount negative ? The price is ignored.
isNegativeAmount :: Amount -> Bool
isNegativeAmount Amount{quantity=q} = q < 0

-- | Does this amount appear to be zero when displayed with its given precision ?
isZeroAmount :: Amount -> Bool
isZeroAmount = null . filter (`elem` "123456789") . showAmountWithoutPriceOrCommodity

-- | Is this amount "really" zero, regardless of the display precision ?
-- Since we are using floating point, for now just test to some high precision.
isReallyZeroAmount :: Amount -> Bool
isReallyZeroAmount = null . filter (`elem` "123456789") . printf ("%."++show zeroprecision++"f") . quantity
    where zeroprecision = 8

-- | Get the string representation of an amount, based on its commodity's
-- display settings except using the specified precision.
showAmountWithPrecision :: Int -> Amount -> String
showAmountWithPrecision p = showAmount . setAmountPrecision p

-- | Set the display precision in the amount's commodity.
setAmountPrecision :: Int -> Amount -> Amount
setAmountPrecision p a@Amount{commodity=c} = a{commodity=c{precision=p}}

-- | Get the unambiguous string representation of an amount, for debugging.
showAmountDebug :: Amount -> String
showAmountDebug (Amount c q pri) = printf "Amount {commodity = %s, quantity = %s, price = %s}"
                                   (show c) (show q) (maybe "" showPriceDebug pri)

-- | Get the string representation of an amount, without any \@ price.
showAmountWithoutPrice :: Amount -> String
showAmountWithoutPrice a = showAmount a{price=Nothing}

-- | Get the string representation of an amount, without any price or commodity symbol.
showAmountWithoutPriceOrCommodity :: Amount -> String
showAmountWithoutPriceOrCommodity a@Amount{commodity=c} = showAmount a{commodity=c{symbol=""}, price=Nothing}

showPrice :: Price -> String
showPrice (UnitPrice pa)  = " @ "  ++ showMixedAmount pa
showPrice (TotalPrice pa) = " @@ " ++ showMixedAmount pa

showPriceDebug :: Price -> String
showPriceDebug (UnitPrice pa)  = " @ "  ++ showMixedAmountDebug pa
showPriceDebug (TotalPrice pa) = " @@ " ++ showMixedAmountDebug pa

-- | Get the string representation of an amount, based on its commodity's
-- display settings. Amounts whose string representation would mean zero
-- are rendered as just "0".
showAmount :: Amount -> String
showAmount (Amount (Commodity {symbol="AUTO"}) _ _) = "" -- can appear in an error message
showAmount a@(Amount (Commodity {symbol=sym,side=side,spaced=spaced}) _ pri) =
    case side of
      L -> printf "%s%s%s%s" sym' space quantity' price
      R -> printf "%s%s%s%s" quantity' space sym' price
    where
      quantity = showamountquantity a
      displayingzero = null $ filter (`elem` "123456789") $ quantity
      (quantity',sym') | displayingzero = ("0","")
                       | otherwise      = (quantity,quoteCommoditySymbolIfNeeded sym)
      space = if (not (null sym') && spaced) then " " else ""
      price = maybe "" showPrice pri

-- | Get the string representation of the number part of of an amount,
-- using the display settings from its commodity.
showamountquantity :: Amount -> String
showamountquantity (Amount (Commodity {decimalpoint=d,precision=p,separator=s,separatorpositions=spos}) q _) =
    punctuatenumber d s spos $ qstr
    where
    -- isint n = fromIntegral (round n) == n
    qstr -- p == maxprecision && isint q = printf "%d" (round q::Integer)
         | p == maxprecisionwithpoint    = printf "%f" q
         | p == maxprecision             = chopdotzero $ printf "%f" q
         | otherwise                    = printf ("%."++show p++"f") q

-- | Replace a number string's decimal point with the specified character,
-- and add the specified digit group separators.
punctuatenumber :: Char -> Char -> [Int] -> String -> String
punctuatenumber dec sep grps str = sign ++ reverse (addseps sep (extend grps) (reverse int)) ++ frac''
    where
      (sign,num) = break isDigit str
      (int,frac) = break (=='.') num
      frac' = dropWhile (=='.') frac
      frac'' | null frac' = ""
             | otherwise  = dec:frac'
      extend [] = []
      extend gs = init gs ++ repeat (last gs)
      addseps _ [] str = str
      addseps sep (g:gs) str
          | length str <= g = str
          | otherwise = let (s,rest) = splitAt g str
                        in s ++ [sep] ++ addseps sep gs rest

chopdotzero str = reverse $ case reverse str of
                              '0':'.':s -> s
                              s         -> s

-- | For rendering: a special precision value which means show all available digits.
maxprecision :: Int
maxprecision = 999998

-- | For rendering: a special precision value which forces display of a decimal point.
maxprecisionwithpoint :: Int
maxprecisionwithpoint = 999999

-- | Replace an amount's commodity with the canonicalised version from
-- the provided commodity map.
canonicaliseAmountCommodity :: Maybe (Map.Map String Commodity) -> Amount -> Amount
canonicaliseAmountCommodity Nothing                      = id
canonicaliseAmountCommodity (Just canonicalcommoditymap) = fixamount
    where
      -- like journalCanonicaliseAmounts
      fixamount a@Amount{commodity=c} = a{commodity=fixcommodity c}
      fixcommodity c@Commodity{symbol=s} = findWithDefault c s canonicalcommoditymap

-------------------------------------------------------------------------------
-- MixedAmount

instance Show MixedAmount where show = showMixedAmount

instance Num MixedAmount where
    fromInteger i = Mixed [Amount (comm "") (fromInteger i) Nothing]
    negate (Mixed as) = Mixed $ map negate as
    (+) (Mixed as) (Mixed bs) = normaliseMixedAmount $ Mixed $ as ++ bs
    (*)    = error' "programming error, mixed amounts do not support multiplication"
    abs    = error' "programming error, mixed amounts do not support abs"
    signum = error' "programming error, mixed amounts do not support signum"

-- | The empty mixed amount.
nullmixedamt :: MixedAmount
nullmixedamt = Mixed []

-- | A temporary value for parsed transactions which had no amount specified.
missingamt :: MixedAmount
missingamt = Mixed [Amount unknown{symbol="AUTO"} 0 Nothing]

-- | Simplify a mixed amount by removing redundancy in its component amounts,
-- as follows:
--
-- 1. combine amounts which have the same commodity, discarding all but the first's price.
--
-- 2. remove zero amounts
--
-- 3. if there are no amounts at all, add a single zero amount
normaliseMixedAmount :: MixedAmount -> MixedAmount
normaliseMixedAmount (Mixed as) = Mixed as''
    where 
      as'' = if null nonzeros then [nullamt] else nonzeros
      (_,nonzeros) = partition (\a -> isReallyZeroAmount a && Mixed [a] /= missingamt) as'
      as' = map sumAmountsDiscardingAllButFirstPrice $ group $ sort as
      sort = sortBy (\a1 a2 -> compare (sym a1) (sym a2))
      group = groupBy (\a1 a2 -> sym a1 == sym a2)
      sym = symbol . commodity

sumAmountsDiscardingAllButFirstPrice [] = nullamt
sumAmountsDiscardingAllButFirstPrice as = (sum as){price=price $ head as}

-- | Get a mixed amount's component amounts.
amounts :: MixedAmount -> [Amount]
amounts (Mixed as) = as

-- | Convert a mixed amount's component amounts to the commodity of their
-- assigned price, if any.
costOfMixedAmount :: MixedAmount -> MixedAmount
costOfMixedAmount (Mixed as) = Mixed $ map costOfAmount as

-- | Divide a mixed amount's quantities by a constant.
divideMixedAmount :: MixedAmount -> Double -> MixedAmount
divideMixedAmount (Mixed as) d = Mixed $ map (flip divideAmount d) as

-- | Is this mixed amount negative, if it can be normalised to a single commodity ?
isNegativeMixedAmount :: MixedAmount -> Maybe Bool
isNegativeMixedAmount m = case as of [a] -> Just $ isNegativeAmount a
                                     _   -> Nothing
    where as = amounts $ normaliseMixedAmount m

-- | Does this mixed amount appear to be zero when displayed with its given precision ?
isZeroMixedAmount :: MixedAmount -> Bool
isZeroMixedAmount = all isZeroAmount . amounts . normaliseMixedAmount

-- | Is this mixed amount "really" zero ? See isReallyZeroAmount.
isReallyZeroMixedAmount :: MixedAmount -> Bool
isReallyZeroMixedAmount = all isReallyZeroAmount . amounts . normaliseMixedAmount

-- | Is this mixed amount "really" zero, after converting to cost
-- commodities where possible ?
isReallyZeroMixedAmountCost :: MixedAmount -> Bool
isReallyZeroMixedAmountCost = isReallyZeroMixedAmount . costOfMixedAmount

-- -- | Convert a mixed amount to the specified commodity, assuming an exchange rate of 1.
mixedAmountWithCommodity :: Commodity -> MixedAmount -> Amount
mixedAmountWithCommodity c (Mixed as) = Amount c total Nothing
    where
      total = sum $ map (quantity . amountWithCommodity c) as

-- -- | MixedAmount derived Eq instance in Types.hs doesn't know that we
-- -- want $0 = EUR0 = 0. Yet we don't want to drag all this code over there.
-- -- For now, use this when cross-commodity zero equality is important.
-- mixedAmountEquals :: MixedAmount -> MixedAmount -> Bool
-- mixedAmountEquals a b = amounts a' == amounts b' || (isZeroMixedAmount a' && isZeroMixedAmount b')
--     where a' = normaliseMixedAmount a
--           b' = normaliseMixedAmount b

-- | Get the string representation of a mixed amount, showing each of
-- its component amounts. NB a mixed amount can have an empty amounts
-- list in which case it shows as \"\".
showMixedAmount :: MixedAmount -> String
showMixedAmount m = vConcatRightAligned $ map show $ amounts $ normaliseMixedAmount m

-- | Set the display precision in the amount's commodities.
setMixedAmountPrecision :: Int -> MixedAmount -> MixedAmount
setMixedAmountPrecision p (Mixed as) = Mixed $ map (setAmountPrecision p) as

-- | Get the string representation of a mixed amount, showing each of its
-- component amounts with the specified precision, ignoring their
-- commoditys' display precision settings. NB a mixed amount can have an
-- empty amounts list in which case it shows as \"\".
showMixedAmountWithPrecision :: Int -> MixedAmount -> String
showMixedAmountWithPrecision p m =
    vConcatRightAligned $ map (showAmountWithPrecision p) $ amounts $ normaliseMixedAmount m

-- | Get an unambiguous string representation of a mixed amount for debugging.
showMixedAmountDebug :: MixedAmount -> String
showMixedAmountDebug m = printf "Mixed [%s]" as
    where as = intercalate "\n       " $ map showAmountDebug $ amounts $ normaliseMixedAmount m

-- | Get the string representation of a mixed amount, but without
-- any \@ prices.
showMixedAmountWithoutPrice :: MixedAmount -> String
showMixedAmountWithoutPrice m = concat $ intersperse "\n" $ map showfixedwidth as
    where
      (Mixed as) = normaliseMixedAmount $ stripPrices m
      stripPrices (Mixed as) = Mixed $ map stripprice as where stripprice a = a{price=Nothing}
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

-- | Replace a mixed amount's commodity with the canonicalised version from
-- the provided commodity map.
canonicaliseMixedAmountCommodity :: Maybe (Map.Map String Commodity) -> MixedAmount -> MixedAmount
canonicaliseMixedAmountCommodity canonicalcommoditymap (Mixed as) = Mixed $ map (canonicaliseAmountCommodity canonicalcommoditymap) as

-------------------------------------------------------------------------------
-- misc

tests_Hledger_Data_Amount = TestList [

  -- Amount

   "costOfAmount" ~: do
    costOfAmount (euros 1) `is` euros 1
    costOfAmount (euros 2){price=Just $ UnitPrice $ Mixed [dollars 2]} `is` dollars 4
    costOfAmount (euros 1){price=Just $ TotalPrice $ Mixed [dollars 2]} `is` dollars 2
    costOfAmount (euros (-1)){price=Just $ TotalPrice $ Mixed [dollars 2]} `is` dollars (-2)

  ,"isZeroAmount" ~: do
    assertBool "" $ isZeroAmount $ Amount unknown 0 Nothing
    assertBool "" $ isZeroAmount $ dollars 0

  ,"negating amounts" ~: do
    let a = dollars 1
    negate a `is` a{quantity=(-1)}
    let b = (dollars 1){price=Just $ UnitPrice $ Mixed [euros 2]}
    negate b `is` b{quantity=(-1)}

  ,"adding amounts" ~: do
    let a1 = dollars 1.23
    let a2 = dollars (-1.23)
    let a3 = dollars (-1.23)
    (a1 + a2) `is` Amount (comm "$") 0 Nothing
    (a1 + a3) `is` Amount (comm "$") 0 Nothing
    (a2 + a3) `is` Amount (comm "$") (-2.46) Nothing
    (a3 + a3) `is` Amount (comm "$") (-2.46) Nothing
    sum [a1,a2,a3,-a3] `is` Amount (comm "$") 0 Nothing
    -- highest precision is preserved
    let ap1 = (dollars 1){commodity=dollar{precision=1}}
        ap3 = (dollars 1){commodity=dollar{precision=3}}
    (sum [ap1,ap3]) `is` ap3{quantity=2}
    (sum [ap3,ap1]) `is` ap3{quantity=2}
    -- adding different commodities assumes conversion rate 1
    assertBool "" $ isZeroAmount (a1 - euros 1.23)

  ,"showAmount" ~: do
    showAmount (dollars 0 + pounds 0) `is` "0"

  -- MixedAmount

  ,"normaliseMixedAmount" ~: do
    normaliseMixedAmount (Mixed []) `is` Mixed [nullamt]
    assertBool "" $ isZeroMixedAmount $ normaliseMixedAmount (Mixed [Amount {commodity=dollar, quantity=10,    price=Nothing}
                                                                    ,Amount {commodity=dollar, quantity=10,    price=Just (TotalPrice (Mixed [Amount {commodity=euro, quantity=7, price=Nothing}]))}
                                                                    ,Amount {commodity=dollar, quantity=(-10), price=Nothing}
                                                                    ,Amount {commodity=dollar, quantity=(-10), price=Just (TotalPrice (Mixed [Amount {commodity=euro, quantity=7, price=Nothing}]))}
                                                                    ])

  ,"adding mixed amounts" ~: do
    let dollar0 = dollar{precision=0}
    (sum $ map (Mixed . (\a -> [a]))
             [Amount dollar 1.25 Nothing,
              Amount dollar0 (-1) Nothing,
              Amount dollar (-0.25) Nothing])
      `is` Mixed [Amount unknown 0 Nothing]

  ,"showMixedAmount" ~: do
    showMixedAmount (Mixed [dollars 1]) `is` "$1.00"
    showMixedAmount (Mixed [(dollars 1){price=Just $ UnitPrice $ Mixed [euros 2]}]) `is` "$1.00 @ €2.00"
    showMixedAmount (Mixed [dollars 0]) `is` "0"
    showMixedAmount (Mixed []) `is` "0"
    showMixedAmount missingamt `is` ""

  ,"showMixedAmountOrZero" ~: do
    showMixedAmountOrZero (Mixed [Amount dollar 0 Nothing]) `is` "0"
    showMixedAmountOrZero (Mixed []) `is` "0"
    showMixedAmountOrZero missingamt `is` ""

  ,"showMixedAmountWithoutPrice" ~: do
    let a = (dollars 1){price=Just $ UnitPrice $ Mixed [euros 2]}
    showMixedAmountWithoutPrice (Mixed [a]) `is` "$1.00"
    showMixedAmountWithoutPrice (Mixed [a, (-a)]) `is` "0"

  ]
