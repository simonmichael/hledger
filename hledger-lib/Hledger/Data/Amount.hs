{-|
A simple 'Amount' is some quantity of money, shares, or anything else.
It has a (possibly null) 'CommoditySymbol' and a numeric quantity:

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

{-# LANGUAGE StandaloneDeriving, RecordWildCards, OverloadedStrings #-}

module Hledger.Data.Amount (
  -- * Amount
  amount,
  nullamt,
  missingamt,
  num,
  usd,
  eur,
  gbp,
  per,
  hrs,
  at,
  (@@),
  amountWithCommodity,
  -- ** arithmetic
  amountCost,
  amountIsZero,
  amountLooksZero,
  divideAmount,
  multiplyAmount,
  divideAmountAndPrice,
  multiplyAmountAndPrice,
  amountTotalPriceToUnitPrice,
  -- ** rendering
  amountstyle,
  styleAmount,
  styleAmountExceptPrecision,
  showAmount,
  cshowAmount,
  showAmountWithZeroCommodity,
  showAmountDebug,
  showAmountWithoutPrice,
  setAmountPrecision,
  withPrecision,
  setFullPrecision,
  setAmountInternalPrecision,
  withInternalPrecision,
  setAmountDecimalPoint,
  withDecimalPoint,
  canonicaliseAmount,
  -- * MixedAmount
  nullmixedamt,
  missingmixedamt,
  mixed,
  amounts,
  filterMixedAmount,
  filterMixedAmountByCommodity,
  mapMixedAmount,
  normaliseMixedAmountSquashPricesForDisplay,
  normaliseMixedAmount,
  unifyMixedAmount,
  mixedAmountStripPrices,
  -- ** arithmetic
  mixedAmountCost,
  divideMixedAmount,
  multiplyMixedAmount,
  divideMixedAmountAndPrice,
  multiplyMixedAmountAndPrice,
  averageMixedAmounts,
  isNegativeAmount,
  isNegativeMixedAmount,
  mixedAmountIsZero,
  mixedAmountLooksZero,
  mixedAmountTotalPriceToUnitPrice,
  -- ** rendering
  styleMixedAmount,
  showMixedAmount,
  showMixedAmountOneLine,
  showMixedAmountDebug,
  showMixedAmountWithoutPrice,
  showMixedAmountOneLineWithoutPrice,
  showMixedAmountElided,
  showMixedAmountWithZeroCommodity,
  showMixedAmountWithPrecision,
  showMixed,
  showMixedUnnormalised,
  showMixedOneLine,
  showMixedOneLineUnnormalised,
  setMixedAmountPrecision,
  canonicaliseMixedAmount,
  -- * misc.
  ltraceamount,
  tests_Amount
) where

import Control.Monad (foldM)
import Data.Char (isDigit)
import Data.Decimal (decimalPlaces, normalizeDecimal, roundTo)
import Data.Function (on)
import Data.List (genericSplitAt, groupBy, intercalate, mapAccumL,
                  partition, sortBy)
import qualified Data.Map as M
import Data.Map (findWithDefault)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word8)
import Safe (lastDef, lastMay)
import Text.Printf (printf)

import Hledger.Data.Types
import Hledger.Data.Commodity
import Hledger.Utils

deriving instance Show MarketPrice


-------------------------------------------------------------------------------
-- Amount styles

-- | Default amount style
amountstyle = AmountStyle L False (Precision 0) (Just '.') Nothing


-------------------------------------------------------------------------------
-- Amount

instance Num Amount where
    abs a@Amount{aquantity=q}    = a{aquantity=abs q}
    signum a@Amount{aquantity=q} = a{aquantity=signum q}
    fromInteger i                = nullamt{aquantity=fromInteger i}
    negate a@Amount{aquantity=q} = a{aquantity= -q}
    (+)                          = similarAmountsOp (+)
    (-)                          = similarAmountsOp (-)
    (*)                          = similarAmountsOp (*)

-- | The empty simple amount.
amount, nullamt :: Amount
amount = Amount{acommodity="", aquantity=0, aprice=Nothing, astyle=amountstyle, aismultiplier=False}
nullamt = amount

-- | A temporary value for parsed transactions which had no amount specified.
missingamt :: Amount
missingamt = amount{acommodity="AUTO"}

-- Handy amount constructors for tests.
-- usd/eur/gbp round their argument to a whole number of pennies/cents.
num n = amount{acommodity="",  aquantity=n}
hrs n = amount{acommodity="h", aquantity=n,           astyle=amountstyle{asprecision=Precision 2, ascommodityside=R}}
usd n = amount{acommodity="$", aquantity=roundTo 2 n, astyle=amountstyle{asprecision=Precision 2}}
eur n = amount{acommodity="€", aquantity=roundTo 2 n, astyle=amountstyle{asprecision=Precision 2}}
gbp n = amount{acommodity="£", aquantity=roundTo 2 n, astyle=amountstyle{asprecision=Precision 2}}
per n = amount{acommodity="%", aquantity=n,           astyle=amountstyle{asprecision=Precision 1, ascommodityside=R, ascommodityspaced=True}}
amt `at` priceamt = amt{aprice=Just $ UnitPrice priceamt}
amt @@ priceamt = amt{aprice=Just $ TotalPrice priceamt}

-- | Apply a binary arithmetic operator to two amounts, which should
-- be in the same commodity if non-zero (warning, this is not checked).
-- A zero result keeps the commodity of the second amount.
-- The result's display style is that of the second amount, with
-- precision set to the highest of either amount.
-- Prices are ignored and discarded.
-- Remember: the caller is responsible for ensuring both amounts have the same commodity.
similarAmountsOp :: (Quantity -> Quantity -> Quantity) -> Amount -> Amount -> Amount
similarAmountsOp op Amount{acommodity=_,  aquantity=q1, astyle=AmountStyle{asprecision=p1}}
                    Amount{acommodity=c2, aquantity=q2, astyle=s2@AmountStyle{asprecision=p2}} =
   -- trace ("a1:"++showAmountDebug a1) $ trace ("a2:"++showAmountDebug a2) $ traceWith (("= :"++).showAmountDebug)
   amount{acommodity=c2, aquantity=q1 `op` q2, astyle=s2{asprecision=max p1 p2}}
  --  c1==c2 || q1==0 || q2==0 =
  --  otherwise = error "tried to do simple arithmetic with amounts in different commodities"

-- | Convert an amount to the specified commodity, ignoring and discarding
-- any assigned prices and assuming an exchange rate of 1.
amountWithCommodity :: CommoditySymbol -> Amount -> Amount
amountWithCommodity c a = a{acommodity=c, aprice=Nothing}

-- | Convert a amount to its "cost" or "selling price" in another commodity,
-- using its attached transaction price if it has one.  Notes:
--
-- - price amounts must be MixedAmounts with exactly one component Amount
--   (or there will be a runtime error XXX)
--
-- - price amounts should be positive
--   (though this is currently not enforced)
amountCost :: Amount -> Amount
amountCost a@Amount{aquantity=q, aprice=mp} =
    case mp of
      Nothing                                  -> a
      Just (UnitPrice  p@Amount{aquantity=pq}) -> p{aquantity=pq * q}
      Just (TotalPrice p@Amount{aquantity=pq}) -> p{aquantity=pq * signum q}

-- | Replace an amount's TotalPrice, if it has one, with an equivalent UnitPrice.
-- Has no effect on amounts without one.
-- Also increases the unit price's display precision to show one extra decimal place,
-- to help keep transaction amounts balancing.
-- Does Decimal division, might be some rounding/irrational number issues.
amountTotalPriceToUnitPrice :: Amount -> Amount
amountTotalPriceToUnitPrice
    a@Amount{aquantity=q, aprice=Just (TotalPrice pa@Amount{aquantity=pq, astyle=ps})}
    = a{aprice = Just $ UnitPrice pa{aquantity=abs (pq/q), astyle=ps{asprecision=pp}}}
  where
    -- Increase the precision by 1, capping at the max bound.
    pp = case asprecision ps of
                NaturalPrecision -> NaturalPrecision
                Precision p      -> Precision $ if p == maxBound then maxBound else p + 1
amountTotalPriceToUnitPrice a = a

-- | Divide an amount's quantity by a constant.
divideAmount :: Quantity -> Amount -> Amount
divideAmount n a@Amount{aquantity=q} = a{aquantity=q/n}

-- | Multiply an amount's quantity by a constant.
multiplyAmount :: Quantity -> Amount -> Amount
multiplyAmount n a@Amount{aquantity=q} = a{aquantity=q*n}

-- | Divide an amount's quantity (and its total price, if it has one) by a constant.
-- The total price will be kept positive regardless of the multiplier's sign.
divideAmountAndPrice :: Quantity -> Amount -> Amount
divideAmountAndPrice n a@Amount{aquantity=q,aprice=p} = a{aquantity=q/n, aprice=f <$> p}
  where
    f (TotalPrice a) = TotalPrice $ abs $ n `divideAmount` a
    f p = p

-- | Multiply an amount's quantity (and its total price, if it has one) by a constant.
-- The total price will be kept positive regardless of the multiplier's sign.
multiplyAmountAndPrice :: Quantity -> Amount -> Amount
multiplyAmountAndPrice n a@Amount{aquantity=q,aprice=p} = a{aquantity=q*n, aprice=f <$> p}
  where
    f (TotalPrice a) = TotalPrice $ abs $ n `multiplyAmount` a
    f p = p

-- | Is this amount negative ? The price is ignored.
isNegativeAmount :: Amount -> Bool
isNegativeAmount Amount{aquantity=q} = q < 0

-- | Round an Amount's Quantity to its specified display precision. If that is
-- NaturalPrecision, this does nothing.
amountRoundedQuantity :: Amount -> Quantity
amountRoundedQuantity Amount{aquantity=q, astyle=AmountStyle{asprecision=p}} = case p of
    NaturalPrecision -> q
    Precision p'     -> roundTo p' q

-- | Does mixed amount appear to be zero when rendered with its
-- display precision ?
amountLooksZero :: Amount -> Bool
amountLooksZero = (0==) . amountRoundedQuantity

-- | Is this amount exactly zero, ignoring its display precision ?
amountIsZero :: Amount -> Bool
amountIsZero Amount{aquantity=q} = q == 0

-- | Set an amount's display precision, flipped.
withPrecision :: Amount -> AmountPrecision -> Amount
withPrecision = flip setAmountPrecision

-- | Set an amount's display precision.
setAmountPrecision :: AmountPrecision -> Amount -> Amount
setAmountPrecision p a@Amount{astyle=s} = a{astyle=s{asprecision=p}}

-- | Increase an amount's display precision, if needed, to enough decimal places
-- to show it exactly (showing all significant decimal digits, excluding trailing
-- zeros).
setFullPrecision :: Amount -> Amount
setFullPrecision a = setAmountPrecision p a
  where
    p                = max displayprecision naturalprecision
    displayprecision = asprecision $ astyle a
    naturalprecision = Precision . decimalPlaces . normalizeDecimal $ aquantity a

-- | Set an amount's internal precision, ie rounds the Decimal representing
-- the amount's quantity to some number of decimal places.
-- Rounding is done with Data.Decimal's default roundTo function:
-- "If the value ends in 5 then it is rounded to the nearest even value (Banker's Rounding)".
-- Does not change the amount's display precision.
-- Intended only for internal use, eg when comparing amounts in tests.
setAmountInternalPrecision :: Word8 -> Amount -> Amount
setAmountInternalPrecision p a@Amount{ aquantity=q, astyle=s } = a{
   astyle=s{asprecision=Precision p}
  ,aquantity=roundTo p q
  }

-- | Set an amount's internal precision, flipped.
-- Intended only for internal use, eg when comparing amounts in tests.
withInternalPrecision :: Amount -> Word8 -> Amount
withInternalPrecision = flip setAmountInternalPrecision

-- | Set (or clear) an amount's display decimal point.
setAmountDecimalPoint :: Maybe Char -> Amount -> Amount
setAmountDecimalPoint mc a@Amount{ astyle=s } = a{ astyle=s{asdecimalpoint=mc} }

-- | Set (or clear) an amount's display decimal point, flipped.
withDecimalPoint :: Amount -> Maybe Char -> Amount
withDecimalPoint = flip setAmountDecimalPoint

showAmountPrice :: Maybe AmountPrice -> String
showAmountPrice Nothing                = ""
showAmountPrice (Just (UnitPrice pa))  = " @ "  ++ showAmount pa
showAmountPrice (Just (TotalPrice pa)) = " @@ " ++ showAmount pa

showAmountPriceDebug :: Maybe AmountPrice -> String
showAmountPriceDebug Nothing                = ""
showAmountPriceDebug (Just (UnitPrice pa))  = " @ "  ++ showAmountDebug pa
showAmountPriceDebug (Just (TotalPrice pa)) = " @@ " ++ showAmountDebug pa

-- | Given a map of standard commodity display styles, apply the
-- appropriate one to this amount. If there's no standard style for
-- this amount's commodity, return the amount unchanged.
styleAmount :: M.Map CommoditySymbol AmountStyle -> Amount -> Amount
styleAmount styles a =
  case M.lookup (acommodity a) styles of
    Just s  -> a{astyle=s}
    Nothing -> a

-- | Like styleAmount, but keep the number of decimal places unchanged.
styleAmountExceptPrecision :: M.Map CommoditySymbol AmountStyle -> Amount -> Amount
styleAmountExceptPrecision styles a@Amount{astyle=AmountStyle{asprecision=origp}} =
  case M.lookup (acommodity a) styles of
    Just s  -> a{astyle=s{asprecision=origp}}
    Nothing -> a

-- | Get the string representation of an amount, based on its
-- commodity's display settings. String representations equivalent to
-- zero are converted to just \"0\". The special "missing" amount is
-- displayed as the empty string.
showAmount :: Amount -> String
showAmount = showAmountHelper False

-- | Colour version. For a negative amount, adds ANSI codes to change the colour,
-- currently to hard-coded red.
cshowAmount :: Amount -> String
cshowAmount a = (if isNegativeAmount a then color Dull Red else id) $
                showAmountHelper False a

-- | Get the string representation of an amount, without any \@ price.
showAmountWithoutPrice :: Amount -> String
showAmountWithoutPrice a = showAmount a{aprice=Nothing}

-- | Get the string representation of an amount, based on its commodity's
-- display settings except using the specified precision.
showAmountWithPrecision :: AmountPrecision -> Amount -> String
showAmountWithPrecision p = showAmount . setAmountPrecision p

showAmountHelper :: Bool -> Amount -> String
showAmountHelper _ Amount{acommodity="AUTO"} = ""
showAmountHelper showzerocommodity a@Amount{acommodity=c, aprice=mp, astyle=AmountStyle{..}} =
    case ascommodityside of
      L -> printf "%s%s%s%s" (T.unpack c') space quantity' price
      R -> printf "%s%s%s%s" quantity' space (T.unpack c') price
    where
      quantity = showamountquantity a
      (quantity',c') | amountLooksZero a && not showzerocommodity = ("0","")
                     | otherwise = (quantity, quoteCommoditySymbolIfNeeded c)
      space = if not (T.null c') && ascommodityspaced then " " else "" :: String
      price = showAmountPrice mp

-- | Like showAmount, but show a zero amount's commodity if it has one.
showAmountWithZeroCommodity :: Amount -> String
showAmountWithZeroCommodity = showAmountHelper True

-- | Get a string representation of an amount for debugging,
-- appropriate to the current debug level. 9 shows maximum detail.
showAmountDebug :: Amount -> String
showAmountDebug Amount{acommodity="AUTO"} = "(missing)"
showAmountDebug Amount{..} = printf "Amount {acommodity=%s, aquantity=%s, aprice=%s, astyle=%s}" (show acommodity) (show aquantity) (showAmountPriceDebug aprice) (show astyle)

-- | Get the string representation of the number part of of an amount,
-- using the display settings from its commodity.
showamountquantity :: Amount -> String
showamountquantity amt@Amount{astyle=AmountStyle{asdecimalpoint=mdec, asdigitgroups=mgrps}} =
    punctuatenumber (fromMaybe '.' mdec) mgrps . show $ amountRoundedQuantity amt

-- | Replace a number string's decimal mark with the specified
-- character, and add the specified digit group marks. The last digit
-- group will be repeated as needed.
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
      | toInteger (length s) <= toInteger g = s
      | otherwise     = let (part,rest) = genericSplitAt g s
                        in part ++ c : addseps gs rest
    repeatLast [] = []
    repeatLast gs = init gs ++ repeat (last gs)

-- like journalCanonicaliseAmounts
-- | Canonicalise an amount's display style using the provided commodity style map.
canonicaliseAmount :: M.Map CommoditySymbol AmountStyle -> Amount -> Amount
canonicaliseAmount styles a@Amount{acommodity=c, astyle=s} = a{astyle=s'}
    where
      s' = findWithDefault s c styles

-------------------------------------------------------------------------------
-- MixedAmount

instance Num MixedAmount where
    fromInteger i = Mixed [fromInteger i]
    negate (Mixed as) = Mixed $ map negate as
    (+) (Mixed as) (Mixed bs) = normaliseMixedAmount $ Mixed $ as ++ bs
    (*)    = error' "error, mixed amounts do not support multiplication" -- PARTIAL:
    abs    = error' "error, mixed amounts do not support abs"
    signum = error' "error, mixed amounts do not support signum"

-- | The empty mixed amount.
nullmixedamt :: MixedAmount
nullmixedamt = Mixed []

-- | A temporary value for parsed transactions which had no amount specified.
missingmixedamt :: MixedAmount
missingmixedamt = Mixed [missingamt]

-- | Convert amounts in various commodities into a normalised MixedAmount.
mixed :: [Amount] -> MixedAmount
mixed = normaliseMixedAmount . Mixed

-- | Simplify a mixed amount's component amounts:
--
-- * amounts in the same commodity are combined unless they have different prices or total prices
--
-- * multiple zero amounts, all with the same non-null commodity, are replaced by just the last of them, preserving the commodity and amount style (all but the last zero amount are discarded)
--
-- * multiple zero amounts with multiple commodities, or no commodities, are replaced by one commodity-less zero amount
--
-- * an empty amount list is replaced by one commodity-less zero amount
--
-- * the special "missing" mixed amount remains unchanged
--
normaliseMixedAmount :: MixedAmount -> MixedAmount
normaliseMixedAmount = normaliseHelper False

normaliseHelper :: Bool -> MixedAmount -> MixedAmount
normaliseHelper squashprices (Mixed as)
  | missingamt `elem` as = missingmixedamt -- missingamt should always be alone, but detect it even if not
  | null nonzeros        = Mixed [newzero]
  | otherwise            = Mixed nonzeros
  where
    newzero = lastDef nullamt $ filter (not . T.null . acommodity) zeros
    (zeros, nonzeros) = partition amountIsZero $
                        map sumSimilarAmountsUsingFirstPrice $
                        groupBy groupfn $
                        sortBy sortfn
                        as
    sortfn  | squashprices = compare `on` acommodity
            | otherwise    = compare `on` \a -> (acommodity a, aprice a)
    groupfn | squashprices = (==) `on` acommodity
            | otherwise    = \a1 a2 -> acommodity a1 == acommodity a2 && combinableprices a1 a2

    combinableprices Amount{aprice=Nothing} Amount{aprice=Nothing} = True
    combinableprices Amount{aprice=Just (UnitPrice p1)} Amount{aprice=Just (UnitPrice p2)} = p1 == p2
    combinableprices _ _ = False

-- | Like normaliseMixedAmount, but combine each commodity's amounts
-- into just one by throwing away all prices except the first. This is
-- only used as a rendering helper, and could show a misleading price.
normaliseMixedAmountSquashPricesForDisplay :: MixedAmount -> MixedAmount
normaliseMixedAmountSquashPricesForDisplay = normaliseHelper True

-- | Unify a MixedAmount to a single commodity value if possible.
-- Like normaliseMixedAmount, this consolidates amounts of the same commodity
-- and discards zero amounts; but this one insists on simplifying to
-- a single commodity, and will return Nothing if this is not possible.
unifyMixedAmount :: MixedAmount -> Maybe Amount
unifyMixedAmount = foldM combine 0 . amounts
  where
    combine amount result
      | amountIsZero amount                    = Just result
      | amountIsZero result                    = Just amount
      | acommodity amount == acommodity result = Just $ amount + result
      | otherwise                              = Nothing

-- | Sum same-commodity amounts in a lossy way, applying the first
-- price to the result and discarding any other prices. Only used as a
-- rendering helper.
sumSimilarAmountsUsingFirstPrice :: [Amount] -> Amount
sumSimilarAmountsUsingFirstPrice [] = nullamt
sumSimilarAmountsUsingFirstPrice as = (sumStrict as){aprice=aprice $ head as}

-- -- | Sum same-commodity amounts. If there were different prices, set
-- -- the price to a special marker indicating "various". Only used as a
-- -- rendering helper.
-- sumSimilarAmountsNotingPriceDifference :: [Amount] -> Amount
-- sumSimilarAmountsNotingPriceDifference [] = nullamt
-- sumSimilarAmountsNotingPriceDifference as = undefined

-- | Get a mixed amount's component amounts.
amounts :: MixedAmount -> [Amount]
amounts (Mixed as) = as

-- | Filter a mixed amount's component amounts by a predicate.
filterMixedAmount :: (Amount -> Bool) -> MixedAmount -> MixedAmount
filterMixedAmount p (Mixed as) = Mixed $ filter p as

-- | Return an unnormalised MixedAmount containing exactly one Amount
-- with the specified commodity and the quantity of that commodity
-- found in the original. NB if Amount's quantity is zero it will be
-- discarded next time the MixedAmount gets normalised.
filterMixedAmountByCommodity :: CommoditySymbol -> MixedAmount -> MixedAmount
filterMixedAmountByCommodity c (Mixed as) = Mixed as'
  where
    as' = case filter ((==c) . acommodity) as of
            []   -> [nullamt{acommodity=c}]
            as'' -> [sum as'']

-- | Apply a transform to a mixed amount's component 'Amount's.
mapMixedAmount :: (Amount -> Amount) -> MixedAmount -> MixedAmount
mapMixedAmount f (Mixed as) = Mixed $ map f as

-- | Convert all component amounts to cost/selling price where
-- possible (see amountCost).
mixedAmountCost :: MixedAmount -> MixedAmount
mixedAmountCost (Mixed as) = Mixed $ map amountCost as

-- | Divide a mixed amount's quantities by a constant.
divideMixedAmount :: Quantity -> MixedAmount -> MixedAmount
divideMixedAmount n = mapMixedAmount (divideAmount n)

-- | Multiply a mixed amount's quantities by a constant.
multiplyMixedAmount :: Quantity -> MixedAmount -> MixedAmount
multiplyMixedAmount n = mapMixedAmount (multiplyAmount n)

-- | Divide a mixed amount's quantities (and total prices, if any) by a constant.
-- The total prices will be kept positive regardless of the multiplier's sign.
divideMixedAmountAndPrice :: Quantity -> MixedAmount -> MixedAmount
divideMixedAmountAndPrice n = mapMixedAmount (divideAmountAndPrice n)

-- | Multiply a mixed amount's quantities (and total prices, if any) by a constant.
-- The total prices will be kept positive regardless of the multiplier's sign.
multiplyMixedAmountAndPrice :: Quantity -> MixedAmount -> MixedAmount
multiplyMixedAmountAndPrice n = mapMixedAmount (multiplyAmountAndPrice n)

-- | Calculate the average of some mixed amounts.
averageMixedAmounts :: [MixedAmount] -> MixedAmount
averageMixedAmounts [] = 0
averageMixedAmounts as = fromIntegral (length as) `divideMixedAmount` sum as

-- | Is this mixed amount negative, if we can tell that unambiguously?
-- Ie when normalised, are all individual commodity amounts negative ?
isNegativeMixedAmount :: MixedAmount -> Maybe Bool
isNegativeMixedAmount m =
  case amounts $ normaliseMixedAmountSquashPricesForDisplay m of
    []  -> Just False
    [a] -> Just $ isNegativeAmount a
    as | all isNegativeAmount as -> Just True
    as | not (any isNegativeAmount as) -> Just False
    _ -> Nothing  -- multiple amounts with different signs

-- | Does this mixed amount appear to be zero when rendered with its
-- display precision ?
mixedAmountLooksZero :: MixedAmount -> Bool
mixedAmountLooksZero = all amountLooksZero . amounts . normaliseMixedAmountSquashPricesForDisplay

-- | Is this mixed amount exactly zero, ignoring display precisions ?
mixedAmountIsZero :: MixedAmount -> Bool
mixedAmountIsZero = all amountIsZero . amounts . normaliseMixedAmountSquashPricesForDisplay

-- -- | MixedAmount derived Eq instance in Types.hs doesn't know that we
-- -- want $0 = EUR0 = 0. Yet we don't want to drag all this code over there.
-- -- For now, use this when cross-commodity zero equality is important.
-- mixedAmountEquals :: MixedAmount -> MixedAmount -> Bool
-- mixedAmountEquals a b = amounts a' == amounts b' || (mixedAmountLooksZero a' && mixedAmountLooksZero b')
--     where a' = normaliseMixedAmountSquashPricesForDisplay a
--           b' = normaliseMixedAmountSquashPricesForDisplay b

-- | Given a map of standard commodity display styles, apply the
-- appropriate one to each individual amount.
styleMixedAmount :: M.Map CommoditySymbol AmountStyle -> MixedAmount -> MixedAmount
styleMixedAmount styles (Mixed as) = Mixed $ map (styleAmount styles) as

-- | Get the string representation of a mixed amount, after
-- normalising it to one amount per commodity. Assumes amounts have
-- no or similar prices, otherwise this can show misleading prices.
showMixedAmount :: MixedAmount -> String
showMixedAmount = fst . showMixed showAmount Nothing Nothing False

-- | Get the one-line string representation of a mixed amount.
showMixedAmountOneLine :: MixedAmount -> String
showMixedAmountOneLine = fst . showMixedOneLine showAmountWithoutPrice Nothing Nothing False

-- | Like showMixedAmount, but zero amounts are shown with their
-- commodity if they have one.
showMixedAmountWithZeroCommodity :: MixedAmount -> String
showMixedAmountWithZeroCommodity = fst . showMixed showAmountWithZeroCommodity Nothing Nothing False

-- | Get the string representation of a mixed amount, showing each of its
-- component amounts with the specified precision, ignoring their
-- commoditys' display precision settings.
showMixedAmountWithPrecision :: AmountPrecision -> MixedAmount -> String
showMixedAmountWithPrecision p = fst . showMixed (showAmountWithPrecision p) Nothing Nothing False

-- | Get the string representation of a mixed amount, without showing any transaction prices.
-- With a True argument, adds ANSI codes to show negative amounts in red.
showMixedAmountWithoutPrice :: Bool -> MixedAmount -> String
showMixedAmountWithoutPrice c = fst . showMixed showAmountWithoutPrice Nothing Nothing c

-- | Get the one-line string representation of a mixed amount, but without
-- any \@ prices.
-- With a True argument, adds ANSI codes to show negative amounts in red.
showMixedAmountOneLineWithoutPrice :: Bool -> MixedAmount -> String
showMixedAmountOneLineWithoutPrice c = fst . showMixedOneLine showAmountWithoutPrice Nothing Nothing c

-- | Like showMixedAmountOneLineWithoutPrice, but show at most the given width,
-- with an elision indicator if there are more.
-- With a True argument, adds ANSI codes to show negative amounts in red.
showMixedAmountElided :: Int -> Bool -> MixedAmount -> String
showMixedAmountElided w c = fst . showMixedOneLine showAmountWithoutPrice Nothing (Just w) c

-- | Get an unambiguous string representation of a mixed amount for debugging.
showMixedAmountDebug :: MixedAmount -> String
showMixedAmountDebug m | m == missingmixedamt = "(missing)"
                       | otherwise       = printf "Mixed [%s]" as
    where as = intercalate "\n       " $ map showAmountDebug $ amounts m

-- | General function to display a MixedAmount, one Amount on each line.
-- It takes a function to display each Amount, an optional minimum width
-- to pad to, an optional maximum width to display, and a Bool to determine
-- whether to colourise negative numbers. Amounts longer than the maximum
-- width (if given) will be elided. The function also returns the actual
-- width of the output string.
showMixed :: (Amount -> String) -> Maybe Int -> Maybe Int -> Bool -> MixedAmount -> (String, Int)
showMixed showamt mmin mmax c =
    showMixedUnnormalised showamt mmin mmax c . normaliseMixedAmountSquashPricesForDisplay

-- | Like showMixed, but does not normalise the MixedAmount before displaying.
showMixedUnnormalised :: (Amount -> String) -> Maybe Int -> Maybe Int -> Bool -> MixedAmount -> (String, Int)
showMixedUnnormalised showamt mmin mmax c (Mixed as) =
    (intercalate "\n" $ map finalise elided, width)
  where
    width    = maximum $ fromMaybe 0 mmin : map adLength elided
    astrs    = amtDisplayList sepwidth showamt as
    sepwidth = 0  -- "\n" has width 0

    finalise = adString . pad . if c then colourise else id
    pad amt = amt{ adString = applyN (width - adLength amt) (' ':) $ adString amt
                 , adLength = width
                 }

    elided = maybe id elideTo mmax astrs
    elideTo m xs = maybeAppend elisionStr short
      where
        elisionStr = elisionDisplay (Just m) sepwidth (length long) $ lastDef nullAmountDisplay short
        (short, long) = partition ((m>=) . adLength) xs

-- | General function to display a MixedAmount on a single line. It
-- takes a function to display each Amount, an optional minimum width to
-- pad to, an optional maximum width to display, and a Bool to determine
-- whether to colourise negative numbers. It will display as many Amounts
-- as it can in the maximum width (if given), and further Amounts will be
-- elided. The function also returns the actual width of the output string.
showMixedOneLine :: (Amount -> String) -> Maybe Int -> Maybe Int -> Bool -> MixedAmount -> (String, Int)
showMixedOneLine showamt mmin mmax c =
    showMixedOneLineUnnormalised showamt mmin mmax c . normaliseMixedAmountSquashPricesForDisplay

-- | Like showMixedOneLine, but does not normalise the MixedAmount before
-- displaying.
showMixedOneLineUnnormalised :: (Amount -> String) -> Maybe Int -> Maybe Int -> Bool -> MixedAmount -> (String, Int)
showMixedOneLineUnnormalised showamt mmin mmax c (Mixed as) =
    (pad . intercalate ", " $ map finalise elided, max width $ fromMaybe 0 mmin)
  where
    width    = maybe 0 adTotal $ lastMay elided
    astrs    = amtDisplayList sepwidth showamt as
    sepwidth = 2  -- ", " has width 2
    n        = length as

    finalise = adString . if c then colourise else id
    pad = applyN (fromMaybe 0 mmin - width) (' ':)

    elided = maybe id elideTo mmax astrs
    elideTo m = addElide . takeFitting m . withElided
    -- Add the last elision string to the end of the display list
    addElide [] = []
    addElide xs = maybeAppend (snd $ last xs) $ map fst xs
    -- Return the elements of the display list which fit within the maximum width
    -- (including their elision strings)
    takeFitting m = filter (\(_,e) -> maybe True ((m>=) . adTotal) e)
                  . takeWhile (\(amt,_) -> adTotal amt <= m)
    -- Add the elision strings (if any) to each amount
    withElided = zipWith (\num amt -> (amt, elisionDisplay Nothing sepwidth num amt)) [n-1,n-2..0]

data AmountDisplay = AmountDisplay
  { adAmount :: !Amount  -- ^ Amount displayed
  , adString :: !String  -- ^ String representation of the Amount
  , adLength :: !Int     -- ^ Length of the string representation
  , adTotal  :: !Int     -- ^ Cumulative length of MixedAmount this Amount is part of,
                         --   including separators
  } deriving (Show)

nullAmountDisplay :: AmountDisplay
nullAmountDisplay = AmountDisplay nullamt "" 0 0

amtDisplayList :: Int -> (Amount -> String) -> [Amount] -> [AmountDisplay]
amtDisplayList sep showamt = snd . mapAccumL display (-sep)
  where
    display tot amt = (tot', AmountDisplay amt str width tot')
      where
        str  = showamt amt
        width  = strWidth str
        tot' = tot + width + sep

-- The string "m more", added to the previous running total
elisionDisplay :: Maybe Int -> Int -> Int -> AmountDisplay -> Maybe AmountDisplay
elisionDisplay mmax sep n lastAmt
  | n > 0     = Just $ AmountDisplay 0 str len (adTotal lastAmt + len)
  | otherwise = Nothing
  where
    fullString = show n ++ " more.."
    -- sep from the separator, 7 from " more..", 1 + floor (logBase 10 n) from number
    fullLength = sep + 8 + floor (logBase 10 $ fromIntegral n)

    str | Just m <- mmax, fullLength > m = take (m - 2) fullString ++ ".."
        | otherwise                      = fullString
    len = case mmax of Nothing -> fullLength
                       Just m  -> max 2 $ min m fullLength

maybeAppend :: Maybe a -> [a] -> [a]
maybeAppend Nothing  = id
maybeAppend (Just a) = (++[a])

colourise :: AmountDisplay -> AmountDisplay
colourise amt = amt{adString=markColour $ adString amt}
  where markColour = if isNegativeAmount (adAmount amt) then color Dull Red else id

-- | Compact labelled trace of a mixed amount, for debugging.
ltraceamount :: String -> MixedAmount -> MixedAmount
ltraceamount s = traceWith (((s ++ ": ") ++).showMixedAmount)

-- | Set the display precision in the amount's commodities.
setMixedAmountPrecision :: AmountPrecision -> MixedAmount -> MixedAmount
setMixedAmountPrecision p (Mixed as) = Mixed $ map (setAmountPrecision p) as

mixedAmountStripPrices :: MixedAmount -> MixedAmount
mixedAmountStripPrices (Mixed as) = Mixed $ map (\a -> a{aprice=Nothing}) as

-- | Canonicalise a mixed amount's display styles using the provided commodity style map.
canonicaliseMixedAmount :: M.Map CommoditySymbol AmountStyle -> MixedAmount -> MixedAmount
canonicaliseMixedAmount styles (Mixed as) = Mixed $ map (canonicaliseAmount styles) as

-- | Replace each component amount's TotalPrice, if it has one, with an equivalent UnitPrice.
-- Has no effect on amounts without one.
-- Does Decimal division, might be some rounding/irrational number issues.
mixedAmountTotalPriceToUnitPrice :: MixedAmount -> MixedAmount
mixedAmountTotalPriceToUnitPrice (Mixed as) = Mixed $ map amountTotalPriceToUnitPrice as


-------------------------------------------------------------------------------
-- tests

tests_Amount = tests "Amount" [
   tests "Amount" [

     test "amountCost" $ do
       amountCost (eur 1) @?= eur 1
       amountCost (eur 2){aprice=Just $ UnitPrice $ usd 2} @?= usd 4
       amountCost (eur 1){aprice=Just $ TotalPrice $ usd 2} @?= usd 2
       amountCost (eur (-1)){aprice=Just $ TotalPrice $ usd 2} @?= usd (-2)

    ,test "amountLooksZero" $ do
       assertBool "" $ amountLooksZero amount
       assertBool "" $ amountLooksZero $ usd 0

    ,test "negating amounts" $ do
       negate (usd 1) @?= (usd 1){aquantity= -1}
       let b = (usd 1){aprice=Just $ UnitPrice $ eur 2} in negate b @?= b{aquantity= -1}

    ,test "adding amounts without prices" $ do
       (usd 1.23 + usd (-1.23)) @?= usd 0
       (usd 1.23 + usd (-1.23)) @?= usd 0
       (usd (-1.23) + usd (-1.23)) @?= usd (-2.46)
       sum [usd 1.23,usd (-1.23),usd (-1.23),-(usd (-1.23))] @?= usd 0
       -- highest precision is preserved
       asprecision (astyle $ sum [usd 1 `withPrecision` Precision 1, usd 1 `withPrecision` Precision 3]) @?= Precision 3
       asprecision (astyle $ sum [usd 1 `withPrecision` Precision 3, usd 1 `withPrecision` Precision 1]) @?= Precision 3
       -- adding different commodities assumes conversion rate 1
       assertBool "" $ amountLooksZero (usd 1.23 - eur 1.23)

    ,test "showAmount" $ do
      showAmount (usd 0 + gbp 0) @?= "0"

  ]

  ,tests "MixedAmount" [

     test "adding mixed amounts to zero, the commodity and amount style are preserved" $
      sum (map (Mixed . (:[]))
               [usd 1.25
               ,usd (-1) `withPrecision` Precision 3
               ,usd (-0.25)
               ])
        @?= Mixed [usd 0 `withPrecision` Precision 3]

    ,test "adding mixed amounts with total prices" $ do
      sum (map (Mixed . (:[]))
       [usd 1 @@ eur 1
       ,usd (-2) @@ eur 1
       ])
        @?= Mixed [usd 1 @@ eur 1
                   ,usd (-2) @@ eur 1
                   ]

    ,test "showMixedAmount" $ do
       showMixedAmount (Mixed [usd 1]) @?= "$1.00"
       showMixedAmount (Mixed [usd 1 `at` eur 2]) @?= "$1.00 @ €2.00"
       showMixedAmount (Mixed [usd 0]) @?= "0"
       showMixedAmount (Mixed []) @?= "0"
       showMixedAmount missingmixedamt @?= ""

    ,test "showMixedAmountWithoutPrice" $ do
      let a = usd 1 `at` eur 2
      showMixedAmountWithoutPrice False (Mixed [a]) @?= "$1.00"
      showMixedAmountWithoutPrice False (Mixed [a, -a]) @?= "0"

    ,tests "normaliseMixedAmount" [
       test "a missing amount overrides any other amounts" $
        normaliseMixedAmount (Mixed [usd 1, missingamt]) @?= missingmixedamt
      ,test "unpriced same-commodity amounts are combined" $
        normaliseMixedAmount (Mixed [usd 0, usd 2]) @?= Mixed [usd 2]
      ,test "amounts with same unit price are combined" $
        normaliseMixedAmount (Mixed [usd 1 `at` eur 1, usd 1 `at` eur 1]) @?= Mixed [usd 2 `at` eur 1]
      ,test "amounts with different unit prices are not combined" $
        normaliseMixedAmount (Mixed [usd 1 `at` eur 1, usd 1 `at` eur 2]) @?= Mixed [usd 1 `at` eur 1, usd 1 `at` eur 2]
      ,test "amounts with total prices are not combined" $
        normaliseMixedAmount (Mixed  [usd 1 @@ eur 1, usd 1 @@ eur 1]) @?= Mixed [usd 1 @@ eur 1, usd 1 @@ eur 1]
    ]

    ,test "normaliseMixedAmountSquashPricesForDisplay" $ do
       normaliseMixedAmountSquashPricesForDisplay (Mixed []) @?= Mixed [nullamt]
       assertBool "" $ mixedAmountLooksZero $ normaliseMixedAmountSquashPricesForDisplay
        (Mixed [usd 10
               ,usd 10 @@ eur 7
               ,usd (-10)
               ,usd (-10) @@ eur 7
               ])

  ]

 ]
