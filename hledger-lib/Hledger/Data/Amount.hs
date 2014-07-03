{-# LANGUAGE StandaloneDeriving, RecordWildCards  #-}
{-|
A simple 'Amount' is some quantity of money, shares, or anything else.
It has a (possibly null) 'Commodity' and a numeric quantity:

@
  $1 
  £-50
  EUR 3.44 
  GOOG 500
  1.5h
  90 apples
  0 
@

It may also have an assigned 'Price', representing this amount's per-unit
or total cost in a different commodity. If present, this is rendered like
so:

@
  EUR 2 \@ $1.50  (unit price)
  EUR 2 \@\@ $3   (total price)
@

A 'MixedAmount' is zero or more simple amounts, so can represent multiple
commodities; this is the type most often used:

@
  0
  $50 + EUR 3
  16h + $13.55 + AAPL 500 + 6 oranges
@

When a mixed amount has been \"normalised\", it has no more than one amount
in each commodity and no zero amounts; or it has just a single zero amount
and no others.

Limited arithmetic with simple and mixed amounts is supported, best used
with similar amounts since it mostly ignores assigned prices and commodity
exchange rates.

-}

module Hledger.Data.Amount (
  -- * Amount
  amount,
  nullamt,
  missingamt,
  num,
  usd,
  eur,
  gbp,
  hrs,
  at,
  (@@),
  amountWithCommodity,
  -- ** arithmetic
  costOfAmount,
  divideAmount,
  sumAmounts,
  -- ** rendering
  amountstyle,
  showAmount,
  showAmountDebug,
  showAmountWithoutPrice,
  showMixedAmountOneLineWithoutPrice,
  maxprecision,
  maxprecisionwithpoint,
  setAmountPrecision,
  withPrecision,
  canonicaliseAmount,
  -- * MixedAmount
  nullmixedamt,
  missingmixedamt,
  mixed,
  amounts,
  filterMixedAmount,
  normaliseMixedAmountPreservingFirstPrice,
  normaliseMixedAmountPreservingPrices,
  -- ** arithmetic
  costOfMixedAmount,
  divideMixedAmount,
  isNegativeMixedAmount,
  isZeroMixedAmount,
  isReallyZeroMixedAmount,
  isReallyZeroMixedAmountCost,
  -- ** rendering
  showMixedAmount,
  showMixedAmountDebug,
  showMixedAmountWithoutPrice,
  showMixedAmountWithPrecision,
  setMixedAmountPrecision,
  canonicaliseMixedAmount,
  -- * misc.
  ltraceamount,
  tests_Hledger_Data_Amount
) where

import Data.Char (isDigit)
import Data.List
import Data.Map (findWithDefault)
import Data.Maybe
import Test.HUnit
import Text.Printf
import qualified Data.Map as M

import Hledger.Data.Types
import Hledger.Data.Commodity
import Hledger.Utils


deriving instance Show HistoricalPrice

amountstyle = AmountStyle L False 0 (Just '.') Nothing

-------------------------------------------------------------------------------
-- Amount

instance Show Amount where
  show _a@Amount{..}
    --  debugLevel < 2 = showAmountWithoutPrice a
    --  debugLevel < 3 = showAmount a
    | debugLevel < 6 =
       printf "Amount {acommodity=%s, aquantity=%s, ..}" (show acommodity) (show aquantity)
    | otherwise      = --showAmountDebug a
       printf "Amount {acommodity=%s, aquantity=%s, aprice=%s, astyle=%s}" (show acommodity) (show aquantity) (showPriceDebug aprice) (show astyle)

instance Num Amount where
    abs a@Amount{aquantity=q}    = a{aquantity=abs q}
    signum a@Amount{aquantity=q} = a{aquantity=signum q}
    fromInteger i                = nullamt{aquantity=fromInteger i}
    negate a@Amount{aquantity=q} = a{aquantity=(-q)}
    (+)                          = similarAmountsOp (+)
    (-)                          = similarAmountsOp (-)
    (*)                          = similarAmountsOp (*)

-- | The empty simple amount.
amount, nullamt :: Amount
amount = Amount{acommodity="", aquantity=0, aprice=NoPrice, astyle=amountstyle}
nullamt = amount

-- handy amount constructors for tests
num n = amount{acommodity="",  aquantity=n}
usd n = amount{acommodity="$", aquantity=n, astyle=amountstyle{asprecision=2}}
eur n = amount{acommodity="€", aquantity=n, astyle=amountstyle{asprecision=2}}
gbp n = amount{acommodity="£", aquantity=n, astyle=amountstyle{asprecision=2}}
hrs n = amount{acommodity="h", aquantity=n, astyle=amountstyle{asprecision=1, ascommodityside=R}}

-- | Apply a binary arithmetic operator to two amounts in the same
-- commodity.  Warning, as a kludge to support folds (eg sum) we assign
-- the second's commodity to the first so the same commodity requirement
-- is not checked. The highest precision of either amount is preserved in
-- the result. Any prices are currently ignored and discarded. The display
-- style is that of the first amount, with precision set to the highest of
-- either amount.
similarAmountsOp :: (Double -> Double -> Double) -> Amount -> Amount -> Amount
similarAmountsOp op Amount{acommodity=_,  aquantity=aq, astyle=AmountStyle{asprecision=ap}}
                    Amount{acommodity=bc, aquantity=bq, astyle=bs@AmountStyle{asprecision=bp}} =
   -- trace ("a:"++showAmount a) $ trace ("b:"++showAmount b++"\n") $ tracewith (("=:"++).showAmount)
   amount{acommodity=bc, aquantity=aq `op` bq, astyle=bs{asprecision=max ap bp}}
  --  ac==bc    = amount{acommodity=ac, aquantity=aq `op` bq, astyle=as{asprecision=max ap bp}}
  --  otherwise = error "tried to do simple arithmetic with amounts in different commodities"

-- | Convert an amount to the specified commodity, ignoring and discarding
-- any assigned prices and assuming an exchange rate of 1.
amountWithCommodity :: Commodity -> Amount -> Amount
amountWithCommodity c a = a{acommodity=c, aprice=NoPrice}

-- | A more complete amount adding operation.
sumAmounts :: [Amount] -> MixedAmount
sumAmounts = normaliseMixedAmountPreservingPrices . Mixed

-- | Set an amount's unit price.
at :: Amount -> Amount -> Amount
amt `at` priceamt = amt{aprice=UnitPrice priceamt}

-- | Set an amount's total price.
(@@) :: Amount -> Amount -> Amount
amt @@ priceamt = amt{aprice=TotalPrice priceamt}

tests_sumAmounts = [
  "sumAmounts" ~: do
    -- when adding, we don't convert to the price commodity - just
    -- combine what amounts we can.
    -- amounts with same unit price
    sumAmounts [usd 1 `at` eur 1, usd 1 `at` eur 1] `is` Mixed [usd 2 `at` eur 1]
    -- amounts with different unit prices
    -- amounts with total prices
    sumAmounts  [usd 1 @@ eur 1, usd 1 @@ eur 1] `is` Mixed [usd 1 @@ eur 1, usd 1 @@ eur 1]
    -- amounts with no, unit, and/or total prices
 ]

-- | Convert an amount to the commodity of its assigned price, if any.  Notes:
--
-- - price amounts must be MixedAmounts with exactly one component Amount (or there will be a runtime error)
--
-- - price amounts should be positive, though this is not currently enforced
costOfAmount :: Amount -> Amount
costOfAmount a@Amount{aquantity=q, aprice=price} =
    case price of
      NoPrice -> a
      UnitPrice  p@Amount{aquantity=pq} -> p{aquantity=pq * q}
      TotalPrice p@Amount{aquantity=pq} -> p{aquantity=pq * signum q}

-- | Divide an amount's quantity by a constant.
divideAmount :: Amount -> Double -> Amount
divideAmount a@Amount{aquantity=q} d = a{aquantity=q/d}

-- | Is this amount negative ? The price is ignored.
isNegativeAmount :: Amount -> Bool
isNegativeAmount Amount{aquantity=q} = q < 0

digits = "123456789" :: String

-- | Does this amount appear to be zero when displayed with its given precision ?
isZeroAmount :: Amount -> Bool
isZeroAmount a --  a==missingamt = False
               | otherwise     = (null . filter (`elem` digits) . showAmountWithoutPriceOrCommodity) a

-- | Is this amount "really" zero, regardless of the display precision ?
-- Since we are using floating point, for now just test to some high precision.
isReallyZeroAmount :: Amount -> Bool
isReallyZeroAmount a --  a==missingamt = False
                     | otherwise     = (null . filter (`elem` digits) . printf ("%."++show zeroprecision++"f") . aquantity) a
    where zeroprecision = 8

-- | Get the string representation of an amount, based on its commodity's
-- display settings except using the specified precision.
showAmountWithPrecision :: Int -> Amount -> String
showAmountWithPrecision p = showAmount . setAmountPrecision p

-- | Set an amount's display precision.
setAmountPrecision :: Int -> Amount -> Amount
setAmountPrecision p a@Amount{astyle=s} = a{astyle=s{asprecision=p}}

-- | Set an amount's display precision, flipped.
withPrecision :: Amount -> Int -> Amount
withPrecision = flip setAmountPrecision

-- | Get a string representation of an amount for debugging,
-- appropriate to the current debug level. 9 shows maximum detail.
showAmountDebug :: Amount -> String
showAmountDebug Amount{acommodity="AUTO"} = "(missing)"
showAmountDebug Amount{..} = printf "Amount {acommodity=%s, aquantity=%s, aprice=%s, astyle=%s}" (show acommodity) (show aquantity) (showPriceDebug aprice) (show astyle)

-- | Get the string representation of an amount, without any \@ price.
showAmountWithoutPrice :: Amount -> String
showAmountWithoutPrice a = showAmount a{aprice=NoPrice}

-- | Get the string representation of an amount, without any price or commodity symbol.
showAmountWithoutPriceOrCommodity :: Amount -> String
showAmountWithoutPriceOrCommodity a = showAmount a{acommodity="", aprice=NoPrice}

showPrice :: Price -> String
showPrice NoPrice         = ""
showPrice (UnitPrice pa)  = " @ "  ++ showAmount pa
showPrice (TotalPrice pa) = " @@ " ++ showAmount pa

showPriceDebug :: Price -> String
showPriceDebug NoPrice         = ""
showPriceDebug (UnitPrice pa)  = " @ "  ++ showAmountDebug pa
showPriceDebug (TotalPrice pa) = " @@ " ++ showAmountDebug pa

-- | Get the string representation of an amount, based on its commodity's
-- display settings. String representations equivalent to zero are
-- converted to just \"0\".
showAmount :: Amount -> String
showAmount Amount{acommodity="AUTO"} = ""
showAmount a@(Amount{acommodity=c, aprice=p, astyle=AmountStyle{..}}) =
    case ascommodityside of
      L -> printf "%s%s%s%s" c' space quantity' price
      R -> printf "%s%s%s%s" quantity' space c' price
    where
      quantity = showamountquantity a
      displayingzero = null $ filter (`elem` digits) $ quantity
      (quantity',c') | displayingzero = ("0","")
                     | otherwise      = (quantity, quoteCommoditySymbolIfNeeded c)
      space = if (not (null c') && ascommodityspaced) then " " else "" :: String
      price = showPrice p

-- | Get the string representation of the number part of of an amount,
-- using the display settings from its commodity.
showamountquantity :: Amount -> String
showamountquantity Amount{aquantity=q, astyle=AmountStyle{asprecision=p, asdecimalpoint=mdec, asdigitgroups=mgrps}} =
    punctuatenumber (fromMaybe '.' mdec) mgrps $ qstr
    where
      -- isint n = fromIntegral (round n) == n
      qstr -- p == maxprecision && isint q = printf "%d" (round q::Integer)
        | p == maxprecisionwithpoint    = printf "%f" q
        | p == maxprecision             = chopdotzero $ printf "%f" q
        | otherwise                    = printf ("%."++show p++"f") q

-- | Replace a number string's decimal point with the specified character,
-- and add the specified digit group separators. The last digit group will
-- be repeated as needed.
punctuatenumber :: Char -> Maybe DigitGroupStyle -> String -> String
punctuatenumber dec mgrps s = sign ++ reverse (applyDigitGroupStyle mgrps (reverse int)) ++ frac''
    where
      (sign,num) = break isDigit s
      (int,frac) = break (=='.') num
      frac' = dropWhile (=='.') frac
      frac'' | null frac' = ""
             | otherwise  = dec:frac'

applyDigitGroupStyle :: Maybe DigitGroupStyle -> String -> String
applyDigitGroupStyle Nothing s = s
applyDigitGroupStyle (Just (DigitGroups c gs)) s = addseps (repeatLast gs) s
  where
    addseps [] s = s
    addseps (g:gs) s
      | length s <= g = s
      | otherwise     = let (part,rest) = splitAt g s
                        in part ++ [c] ++ addseps gs rest
    repeatLast [] = []
    repeatLast gs = init gs ++ repeat (last gs)

chopdotzero str = reverse $ case reverse str of
                              '0':'.':s -> s
                              s         -> s

-- | For rendering: a special precision value which means show all available digits.
maxprecision :: Int
maxprecision = 999998

-- | For rendering: a special precision value which forces display of a decimal point.
maxprecisionwithpoint :: Int
maxprecisionwithpoint = 999999

-- like journalCanonicaliseAmounts
-- | Canonicalise an amount's display style using the provided commodity style map.
canonicaliseAmount :: M.Map Commodity AmountStyle -> Amount -> Amount
canonicaliseAmount styles a@Amount{acommodity=c, astyle=s} = a{astyle=s'}
    where
      s' = findWithDefault s c styles

-------------------------------------------------------------------------------
-- MixedAmount

instance Show MixedAmount where
  show
    | debugLevel < 3 = intercalate "\\n" . lines . showMixedAmountWithoutPrice
    --  debugLevel < 6 = intercalate "\\n" . lines . showMixedAmount
    | otherwise      = showMixedAmountDebug

instance Num MixedAmount where
    fromInteger i = Mixed [fromInteger i]
    negate (Mixed as) = Mixed $ map negate as
    (+) (Mixed as) (Mixed bs) = normaliseMixedAmountPreservingPrices $ Mixed $ as ++ bs
    (*)    = error' "programming error, mixed amounts do not support multiplication"
    abs    = error' "programming error, mixed amounts do not support abs"
    signum = error' "programming error, mixed amounts do not support signum"

-- | The empty mixed amount.
nullmixedamt :: MixedAmount
nullmixedamt = Mixed []

-- | A temporary value for parsed transactions which had no amount specified.
missingamt :: Amount
missingamt = amount{acommodity="AUTO"}

missingmixedamt :: MixedAmount
missingmixedamt = Mixed [missingamt]

mixed :: Amount -> MixedAmount
mixed a = Mixed [a]
  
-- | Simplify a mixed amount's component amounts: we can combine amounts
-- with the same commodity and unit price. Also remove any zero or missing
-- amounts and replace an empty amount list with a single zero amount.
normaliseMixedAmountPreservingPrices :: MixedAmount -> MixedAmount
normaliseMixedAmountPreservingPrices (Mixed as) = Mixed as''
    where
      as'' = if null nonzeros then [nullamt] else nonzeros
      (_,nonzeros) = partition isReallyZeroAmount as'
      as' = map sumAmountsUsingFirstPrice $ group $ sort $ filter (/= missingamt) as
      sort = sortBy (\a1 a2 -> compare (acommodity a1, aprice a1) (acommodity a2, aprice a2))
      group = groupBy (\a1 a2 -> acommodity a1 == acommodity a2 && sameunitprice a1 a2)
        where
          sameunitprice a1 a2 =
            case (aprice a1, aprice a2) of
              (NoPrice, NoPrice) -> True
              (UnitPrice p1, UnitPrice p2) -> p1 == p2
              _ -> False

tests_normaliseMixedAmountPreservingPrices = [
  "normaliseMixedAmountPreservingPrices" ~: do
   assertEqual "discard missing amount" (Mixed [nullamt]) (normaliseMixedAmountPreservingPrices $ Mixed [usd 0, missingamt])
   assertEqual "combine unpriced same-commodity amounts" (Mixed [usd 2]) (normaliseMixedAmountPreservingPrices $ Mixed [usd 0, usd 2])
   assertEqual "don't combine total-priced amounts"
     (Mixed
      [usd 1 @@ eur 1
      ,usd (-2) @@ eur 1
      ])
     (normaliseMixedAmountPreservingPrices $ Mixed
      [usd 1 @@ eur 1
      ,usd (-2) @@ eur 1
      ])

 ]

-- | Simplify a mixed amount's component amounts: combine amounts with
-- the same commodity, using the first amount's price for subsequent
-- amounts in each commodity (ie, this function alters the amount and
-- is best used as a rendering helper.). Also remove any zero amounts
-- and replace an empty amount list with a single zero amount.
normaliseMixedAmountPreservingFirstPrice :: MixedAmount -> MixedAmount
normaliseMixedAmountPreservingFirstPrice (Mixed as) = Mixed as''
    where 
      as'' = if null nonzeros then [nullamt] else nonzeros
      (_,nonzeros) = partition (\a -> isReallyZeroAmount a && a /= missingamt) as'
      as' = map sumAmountsUsingFirstPrice $ group $ sort as
      sort = sortBy (\a1 a2 -> compare (acommodity a1) (acommodity a2))
      group = groupBy (\a1 a2 -> acommodity a1 == acommodity a2)

-- discardPrice :: Amount -> Amount
-- discardPrice a = a{price=Nothing}

-- discardPrices :: MixedAmount -> MixedAmount
-- discardPrices (Mixed as) = Mixed $ map discardPrice as

sumAmountsUsingFirstPrice [] = nullamt
sumAmountsUsingFirstPrice as = (sum as){aprice=aprice $ head as}

-- | Get a mixed amount's component amounts.
amounts :: MixedAmount -> [Amount]
amounts (Mixed as) = as

-- | Filter a mixed amount's component amounts by a predicate.
filterMixedAmount :: (Amount -> Bool) -> MixedAmount -> MixedAmount
filterMixedAmount p (Mixed as) = Mixed $ filter p as

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
    where as = amounts $ normaliseMixedAmountPreservingFirstPrice m

-- | Does this mixed amount appear to be zero when displayed with its given precision ?
isZeroMixedAmount :: MixedAmount -> Bool
isZeroMixedAmount = all isZeroAmount . amounts . normaliseMixedAmountPreservingFirstPrice

-- | Is this mixed amount "really" zero ? See isReallyZeroAmount.
isReallyZeroMixedAmount :: MixedAmount -> Bool
isReallyZeroMixedAmount = all isReallyZeroAmount . amounts . normaliseMixedAmountPreservingFirstPrice

-- | Is this mixed amount "really" zero, after converting to cost
-- commodities where possible ?
isReallyZeroMixedAmountCost :: MixedAmount -> Bool
isReallyZeroMixedAmountCost = isReallyZeroMixedAmount . costOfMixedAmount

-- -- | MixedAmount derived Eq instance in Types.hs doesn't know that we
-- -- want $0 = EUR0 = 0. Yet we don't want to drag all this code over there.
-- -- For now, use this when cross-commodity zero equality is important.
-- mixedAmountEquals :: MixedAmount -> MixedAmount -> Bool
-- mixedAmountEquals a b = amounts a' == amounts b' || (isZeroMixedAmount a' && isZeroMixedAmount b')
--     where a' = normaliseMixedAmountPreservingFirstPrice a
--           b' = normaliseMixedAmountPreservingFirstPrice b

-- | Get the string representation of a mixed amount, showing each of
-- its component amounts. NB a mixed amount can have an empty amounts
-- list in which case it shows as \"\".
showMixedAmount :: MixedAmount -> String
showMixedAmount m = vConcatRightAligned $ map showAmount $ amounts $  normaliseMixedAmountPreservingFirstPrice m

-- | Compact labelled trace of a mixed amount, for debugging.
ltraceamount :: String -> MixedAmount -> MixedAmount
ltraceamount s = traceWith (((s ++ ": ") ++).showMixedAmount)

-- | Set the display precision in the amount's commodities.
setMixedAmountPrecision :: Int -> MixedAmount -> MixedAmount
setMixedAmountPrecision p (Mixed as) = Mixed $ map (setAmountPrecision p) as

-- | Get the string representation of a mixed amount, showing each of its
-- component amounts with the specified precision, ignoring their
-- commoditys' display precision settings.
showMixedAmountWithPrecision :: Int -> MixedAmount -> String
showMixedAmountWithPrecision p m =
    vConcatRightAligned $ map (showAmountWithPrecision p) $ amounts $ normaliseMixedAmountPreservingFirstPrice m

-- | Get an unambiguous string representation of a mixed amount for debugging.
showMixedAmountDebug :: MixedAmount -> String
showMixedAmountDebug m | m == missingmixedamt = "(missing)"
                       | otherwise       = printf "Mixed [%s]" as
    where as = intercalate "\n       " $ map showAmountDebug $ amounts m -- normaliseMixedAmountPreservingFirstPrice m

-- | Get the string representation of a mixed amount, but without
-- any \@ prices.
showMixedAmountWithoutPrice :: MixedAmount -> String
showMixedAmountWithoutPrice m = concat $ intersperse "\n" $ map showfixedwidth as
    where
      (Mixed as) = normaliseMixedAmountPreservingFirstPrice $ stripPrices m
      stripPrices (Mixed as) = Mixed $ map stripprice as where stripprice a = a{aprice=NoPrice}
      width = maximum $ map (length . showAmount) as
      showfixedwidth = printf (printf "%%%ds" width) . showAmountWithoutPrice

-- | Get the one-line string representation of a mixed amount, but without
-- any \@ prices.
showMixedAmountOneLineWithoutPrice :: MixedAmount -> String
showMixedAmountOneLineWithoutPrice m = concat $ intersperse ", " $ map showAmountWithoutPrice as
    where
      (Mixed as) = normaliseMixedAmountPreservingFirstPrice $ stripPrices m
      stripPrices (Mixed as) = Mixed $ map stripprice as where stripprice a = a{aprice=NoPrice}

-- | Canonicalise a mixed amount's display styles using the provided commodity style map.
canonicaliseMixedAmount :: M.Map Commodity AmountStyle -> MixedAmount -> MixedAmount
canonicaliseMixedAmount styles (Mixed as) = Mixed $ map (canonicaliseAmount styles) as

-------------------------------------------------------------------------------
-- misc

tests_Hledger_Data_Amount = TestList $
     tests_normaliseMixedAmountPreservingPrices
  ++ tests_sumAmounts
  ++ [

  -- Amount

   "costOfAmount" ~: do
    costOfAmount (eur 1) `is` eur 1
    costOfAmount (eur 2){aprice=UnitPrice $ usd 2} `is` usd 4
    costOfAmount (eur 1){aprice=TotalPrice $ usd 2} `is` usd 2
    costOfAmount (eur (-1)){aprice=TotalPrice $ usd 2} `is` usd (-2)

  ,"isZeroAmount" ~: do
    assertBool "" $ isZeroAmount $ amount
    assertBool "" $ isZeroAmount $ usd 0

  ,"negating amounts" ~: do
    let a = usd 1
    negate a `is` a{aquantity=(-1)}
    let b = (usd 1){aprice=UnitPrice $ eur 2}
    negate b `is` b{aquantity=(-1)}

  ,"adding amounts without prices" ~: do
    let a1 = usd 1.23
    let a2 = usd (-1.23)
    let a3 = usd (-1.23)
    (a1 + a2) `is` usd 0
    (a1 + a3) `is` usd 0
    (a2 + a3) `is` usd (-2.46)
    (a3 + a3) `is` usd (-2.46)
    sum [a1,a2,a3,-a3] `is` usd 0
    -- highest precision is preserved
    let ap1 = usd 1 `withPrecision` 1
        ap3 = usd 1 `withPrecision` 3
    (asprecision $ astyle $ sum [ap1,ap3]) `is` 3
    (asprecision $ astyle $ sum [ap3,ap1]) `is` 3
    -- adding different commodities assumes conversion rate 1
    assertBool "" $ isZeroAmount (a1 - eur 1.23)

  ,"showAmount" ~: do
    showAmount (usd 0 + gbp 0) `is` "0"

  -- MixedAmount

  ,"normaliseMixedAmountPreservingFirstPrice" ~: do
    normaliseMixedAmountPreservingFirstPrice (Mixed []) `is` Mixed [nullamt]
    assertBool "" $ isZeroMixedAmount $ normaliseMixedAmountPreservingFirstPrice
      (Mixed [usd 10
             ,usd 10 @@ eur 7
             ,usd (-10)
             ,usd (-10) @@ eur 7
             ])

  ,"adding mixed amounts" ~: do
    (sum $ map (Mixed . (\a -> [a]))
             [usd 1.25
             ,usd (-1) `withPrecision` 0
             ,usd (-0.25)
             ])
      `is` Mixed [amount{aquantity=0}]
  
  ,"adding mixed amounts with total prices" ~: do
    (sum $ map (Mixed . (\a -> [a]))
     [usd 1 @@ eur 1
     ,usd (-2) @@ eur 1
     ])
      `is` (Mixed [usd 1 @@ eur 1
                  ,usd (-2) @@ eur 1
                  ])

  ,"showMixedAmount" ~: do
    showMixedAmount (Mixed [usd 1]) `is` "$1.00"
    showMixedAmount (Mixed [usd 1 `at` eur 2]) `is` "$1.00 @ €2.00"
    showMixedAmount (Mixed [usd 0]) `is` "0"
    showMixedAmount (Mixed []) `is` "0"
    showMixedAmount missingmixedamt `is` ""

  ,"showMixedAmountWithoutPrice" ~: do
    let a = usd 1 `at` eur 2
    showMixedAmountWithoutPrice (Mixed [a]) `is` "$1.00"
    showMixedAmountWithoutPrice (Mixed [a, (-a)]) `is` "0"

  ]
