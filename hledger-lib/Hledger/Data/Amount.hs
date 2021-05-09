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

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

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
  amountTotalPriceToUnitPrice,
  -- ** rendering
  AmountDisplayOpts(..),
  noColour,
  noPrice,
  oneLine,
  amountstyle,
  styleAmount,
  styleAmountExceptPrecision,
  amountUnstyled,
  showAmountB,
  showAmount,
  cshowAmount,
  showAmountWithZeroCommodity,
  showAmountDebug,
  showAmountWithoutPrice,
  amountSetPrecision,
  withPrecision,
  amountSetFullPrecision,
  setAmountInternalPrecision,
  withInternalPrecision,
  setAmountDecimalPoint,
  withDecimalPoint,
  amountStripPrices,
  canonicaliseAmount,
  -- * MixedAmount
  nullmixedamt,
  missingmixedamt,
  mixed,
  mixedAmount,
  maAddAmount,
  maAddAmounts,
  amounts,
  amountsRaw,
  filterMixedAmount,
  filterMixedAmountByCommodity,
  mapMixedAmount,
  normaliseMixedAmountSquashPricesForDisplay,
  normaliseMixedAmount,
  unifyMixedAmount,
  mixedAmountStripPrices,
  -- ** arithmetic
  mixedAmountCost,
  maNegate,
  maPlus,
  maMinus,
  maSum,
  divideMixedAmount,
  multiplyMixedAmount,
  averageMixedAmounts,
  isNegativeAmount,
  isNegativeMixedAmount,
  mixedAmountIsZero,
  maIsZero,
  maIsNonZero,
  mixedAmountLooksZero,
  mixedAmountTotalPriceToUnitPrice,
  -- ** rendering
  styleMixedAmount,
  mixedAmountUnstyled,
  showMixedAmount,
  showMixedAmountOneLine,
  showMixedAmountDebug,
  showMixedAmountWithoutPrice,
  showMixedAmountOneLineWithoutPrice,
  showMixedAmountElided,
  showMixedAmountWithZeroCommodity,
  showMixedAmountB,
  showMixedAmountLinesB,
  wbToText,
  wbUnpack,
  mixedAmountSetPrecision,
  mixedAmountSetFullPrecision,
  canonicaliseMixedAmount,
  -- * misc.
  ltraceamount,
  tests_Amount
) where

import Control.Monad (foldM)
import Data.Decimal (DecimalRaw(..), decimalPlaces, normalizeDecimal, roundTo)
import Data.Default (Default(..))
import Data.Foldable (toList)
import Data.List (find, foldl', intercalate, intersperse, mapAccumL, partition)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Semigroup (Semigroup(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import Data.Word (Word8)
import Safe (headDef, lastDef, lastMay)
import Text.Printf (printf)

import Hledger.Data.Types
import Hledger.Data.Commodity
import Hledger.Utils

deriving instance Show MarketPrice


-- | Options for the display of Amount and MixedAmount.
data AmountDisplayOpts = AmountDisplayOpts
  { displayPrice         :: Bool       -- ^ Whether to display the Price of an Amount.
  , displayZeroCommodity :: Bool       -- ^ If the Amount rounds to 0, whether to display its commodity string.
  , displayColour        :: Bool       -- ^ Whether to colourise negative Amounts.
  , displayOneLine       :: Bool       -- ^ Whether to display on one line.
  , displayMinWidth      :: Maybe Int  -- ^ Minimum width to pad to
  , displayMaxWidth      :: Maybe Int  -- ^ Maximum width to clip to
  } deriving (Show)

-- | Display Amount and MixedAmount with no colour.
instance Default AmountDisplayOpts where def = noColour

-- | Display Amount and MixedAmount with no colour.
noColour :: AmountDisplayOpts
noColour = AmountDisplayOpts { displayPrice         = True
                             , displayColour        = False
                             , displayZeroCommodity = False
                             , displayOneLine       = False
                             , displayMinWidth      = Nothing
                             , displayMaxWidth      = Nothing
                             }

-- | Display Amount and MixedAmount with no prices.
noPrice :: AmountDisplayOpts
noPrice = def{displayPrice=False}

-- | Display Amount and MixedAmount on one line with no prices.
oneLine :: AmountDisplayOpts
oneLine = def{displayOneLine=True, displayPrice=False}

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
    negate a                     = transformAmount negate a
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
similarAmountsOp op !Amount{acommodity=_,  aquantity=q1, astyle=AmountStyle{asprecision=p1}}
                    !Amount{acommodity=c2, aquantity=q2, astyle=s2@AmountStyle{asprecision=p2}} =
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
-- - price amounts should be positive in the Journal
--   (though this is currently not enforced)
amountCost :: Amount -> Amount
amountCost a@Amount{aquantity=q, aprice=mp} =
    case mp of
      Nothing                                  -> a
      Just (UnitPrice  p@Amount{aquantity=pq}) -> p{aquantity=pq * q}
      Just (TotalPrice p@Amount{aquantity=pq}) -> p{aquantity=pq}

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

-- | Apply a function to an amount's quantity (and its total price, if it has one).
transformAmount :: (Quantity -> Quantity) -> Amount -> Amount
transformAmount f a@Amount{aquantity=q,aprice=p} = a{aquantity=f q, aprice=f' <$> p}
  where
    f' (TotalPrice a@Amount{aquantity=pq}) = TotalPrice a{aquantity = f pq}
    f' p = p

-- | Divide an amount's quantity (and its total price, if it has one) by a constant.
divideAmount :: Quantity -> Amount -> Amount
divideAmount n = transformAmount (/n)

-- | Multiply an amount's quantity (and its total price, if it has one) by a constant.
multiplyAmount :: Quantity -> Amount -> Amount
multiplyAmount n = transformAmount (*n)

-- | Is this amount negative ? The price is ignored.
isNegativeAmount :: Amount -> Bool
isNegativeAmount Amount{aquantity=q} = q < 0

-- | Round an Amount's Quantity to its specified display precision. If that is
-- NaturalPrecision, this does nothing.
amountRoundedQuantity :: Amount -> Quantity
amountRoundedQuantity Amount{aquantity=q, astyle=AmountStyle{asprecision=p}} = case p of
    NaturalPrecision -> q
    Precision p'     -> roundTo p' q

-- | Apply a test to both an Amount and its total price, if it has one.
testAmountAndTotalPrice :: (Amount -> Bool) -> Amount -> Bool
testAmountAndTotalPrice f amt = case aprice amt of
    Just (TotalPrice price) -> f amt && f price
    _                       -> f amt

-- | Do this Amount and (and its total price, if it has one) appear to be zero when rendered with its
-- display precision ?
amountLooksZero :: Amount -> Bool
amountLooksZero = testAmountAndTotalPrice looksZero
  where
    looksZero Amount{aquantity=Decimal e q, astyle=AmountStyle{asprecision=p}} = case p of
        Precision d      -> if e > d then abs q <= 5*10^(e-d-1) else q == 0
        NaturalPrecision -> q == 0

-- | Is this Amount (and its total price, if it has one) exactly zero, ignoring its display precision ?
amountIsZero :: Amount -> Bool
amountIsZero = testAmountAndTotalPrice (\Amount{aquantity=Decimal _ q} -> q == 0)

-- | Set an amount's display precision, flipped.
withPrecision :: Amount -> AmountPrecision -> Amount
withPrecision = flip amountSetPrecision

-- | Set an amount's display precision.
amountSetPrecision :: AmountPrecision -> Amount -> Amount
amountSetPrecision p a@Amount{astyle=s} = a{astyle=s{asprecision=p}}

-- | Increase an amount's display precision, if needed, to enough decimal places
-- to show it exactly (showing all significant decimal digits, excluding trailing
-- zeros).
amountSetFullPrecision :: Amount -> Amount
amountSetFullPrecision a = amountSetPrecision p a
  where
    p                = max displayprecision naturalprecision
    displayprecision = asprecision $ astyle a
    naturalprecision = Precision . decimalPlaces . normalizeDecimal $ aquantity a

-- | Set an amount's internal precision, ie rounds the Decimal representing
-- the amount's quantity to some number of decimal places.
-- Rounding is done with Data.Decimal's default roundTo function:
-- "If the value ends in 5 then it is rounded to the nearest even value (Banker's Rounding)".
-- Does not change the amount's display precision.
-- Intended mainly for internal use, eg when comparing amounts in tests.
setAmountInternalPrecision :: Word8 -> Amount -> Amount
setAmountInternalPrecision p a@Amount{ aquantity=q, astyle=s } = a{
   astyle=s{asprecision=Precision p}
  ,aquantity=roundTo p q
  }

-- | Set an amount's internal precision, flipped.
-- Intended mainly for internal use, eg when comparing amounts in tests.
withInternalPrecision :: Amount -> Word8 -> Amount
withInternalPrecision = flip setAmountInternalPrecision

-- | Set (or clear) an amount's display decimal point.
setAmountDecimalPoint :: Maybe Char -> Amount -> Amount
setAmountDecimalPoint mc a@Amount{ astyle=s } = a{ astyle=s{asdecimalpoint=mc} }

-- | Set (or clear) an amount's display decimal point, flipped.
withDecimalPoint :: Amount -> Maybe Char -> Amount
withDecimalPoint = flip setAmountDecimalPoint

-- | Strip all prices from an Amount
amountStripPrices :: Amount -> Amount
amountStripPrices a = a{aprice=Nothing}

showAmountPrice :: Amount -> WideBuilder
showAmountPrice amt = case aprice amt of
    Nothing              -> mempty
    Just (UnitPrice  pa) -> WideBuilder (TB.fromString " @ ")  3 <> showAmountB noColour pa
    Just (TotalPrice pa) -> WideBuilder (TB.fromString " @@ ") 4 <> showAmountB noColour (sign pa)
  where sign = if aquantity amt < 0 then negate else id

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

-- | Reset this amount's display style to the default.
amountUnstyled :: Amount -> Amount
amountUnstyled a = a{astyle=amountstyle}

-- | Get the string representation of an amount, based on its
-- commodity's display settings. String representations equivalent to
-- zero are converted to just \"0\". The special "missing" amount is
-- displayed as the empty string.
--
-- > showAmount = wbUnpack . showAmountB noColour
showAmount :: Amount -> String
showAmount = wbUnpack . showAmountB noColour

-- | General function to generate a WideBuilder for an Amount, according the
-- supplied AmountDisplayOpts. The special "missing" amount is displayed as
-- the empty string. This is the main function to use for showing
-- Amounts, constructing a builder; it can then be converted to a Text with
-- wbToText, or to a String with wbUnpack.
showAmountB :: AmountDisplayOpts -> Amount -> WideBuilder
showAmountB _ Amount{acommodity="AUTO"} = mempty
showAmountB opts a@Amount{astyle=style} =
    color $ case ascommodityside style of
      L -> c' <> space <> quantity' <> price
      R -> quantity' <> space <> c' <> price
  where
    quantity = showamountquantity a
    (quantity',c) | amountLooksZero a && not (displayZeroCommodity opts) = (WideBuilder (TB.singleton '0') 1,"")
                  | otherwise = (quantity, quoteCommoditySymbolIfNeeded $ acommodity a)
    space = if not (T.null c) && ascommodityspaced style then WideBuilder (TB.singleton ' ') 1 else mempty
    c' = WideBuilder (TB.fromText c) (textWidth c)
    price = if displayPrice opts then showAmountPrice a else mempty
    color = if displayColour opts && isNegativeAmount a then colorB Dull Red else id

-- | Colour version. For a negative amount, adds ANSI codes to change the colour,
-- currently to hard-coded red.
--
-- > cshowAmount = wbUnpack . showAmountB def{displayColour=True}
cshowAmount :: Amount -> String
cshowAmount = wbUnpack . showAmountB def{displayColour=True}

-- | Get the string representation of an amount, without any \@ price.
--
-- > showAmountWithoutPrice = wbUnpack . showAmountB noPrice
showAmountWithoutPrice :: Amount -> String
showAmountWithoutPrice = wbUnpack . showAmountB noPrice

-- | Like showAmount, but show a zero amount's commodity if it has one.
--
-- > showAmountWithZeroCommodity = wbUnpack . showAmountB noColour{displayZeryCommodity=True}
showAmountWithZeroCommodity :: Amount -> String
showAmountWithZeroCommodity = wbUnpack . showAmountB noColour{displayZeroCommodity=True}

-- | Get a string representation of an amount for debugging,
-- appropriate to the current debug level. 9 shows maximum detail.
showAmountDebug :: Amount -> String
showAmountDebug Amount{acommodity="AUTO"} = "(missing)"
showAmountDebug Amount{..} = printf "Amount {acommodity=%s, aquantity=%s, aprice=%s, astyle=%s}" (show acommodity) (show aquantity) (showAmountPriceDebug aprice) (show astyle)

-- | Get a Text Builder for the string representation of the number part of of an amount,
-- using the display settings from its commodity. Also returns the width of the
-- number.
showamountquantity :: Amount -> WideBuilder
showamountquantity amt@Amount{astyle=AmountStyle{asdecimalpoint=mdec, asdigitgroups=mgrps}} =
    signB <> intB <> fracB
  where
    Decimal e n = amountRoundedQuantity amt

    strN = T.pack . show $ abs n
    len = T.length strN
    intLen = max 1 $ len - fromIntegral e
    dec = fromMaybe '.' mdec
    padded = T.replicate (fromIntegral e + 1 - len) "0" <> strN
    (intPart, fracPart) = T.splitAt intLen padded

    intB = applyDigitGroupStyle mgrps intLen $ if e == 0 then strN else intPart
    signB = if n < 0 then WideBuilder (TB.singleton '-') 1 else mempty
    fracB = if e > 0 then WideBuilder (TB.singleton dec <> TB.fromText fracPart) (fromIntegral e + 1) else mempty

-- | Split a string representation into chunks according to DigitGroupStyle,
-- returning a Text builder and the number of separators used.
applyDigitGroupStyle :: Maybe DigitGroupStyle -> Int -> T.Text -> WideBuilder
applyDigitGroupStyle Nothing                       l s = WideBuilder (TB.fromText s) l
applyDigitGroupStyle (Just (DigitGroups _ []))     l s = WideBuilder (TB.fromText s) l
applyDigitGroupStyle (Just (DigitGroups c (g:gs))) l s = addseps (g:|gs) (toInteger l) s
  where
    addseps (g:|gs) l s
        | l' > 0    = addseps gs' l' rest <> WideBuilder (TB.singleton c <> TB.fromText part) (fromIntegral g + 1)
        | otherwise = WideBuilder (TB.fromText s) (fromInteger l)
      where
        (rest, part) = T.splitAt (fromInteger l') s
        gs' = fromMaybe (g:|[]) $ nonEmpty gs
        l' = l - toInteger g

-- like journalCanonicaliseAmounts
-- | Canonicalise an amount's display style using the provided commodity style map.
canonicaliseAmount :: M.Map CommoditySymbol AmountStyle -> Amount -> Amount
canonicaliseAmount styles a@Amount{acommodity=c, astyle=s} = a{astyle=s'}
  where s' = M.findWithDefault s c styles

-------------------------------------------------------------------------------
-- MixedAmount

instance Semigroup MixedAmount where
  (<>) = maPlus
  sconcat = maSum
  stimes n = multiplyMixedAmount (fromIntegral n)

instance Monoid MixedAmount where
  mempty = nullmixedamt
  mconcat = maSum

instance Num MixedAmount where
    fromInteger = mixedAmount . fromInteger
    negate = maNegate
    (+)    = maPlus
    (*)    = error' "error, mixed amounts do not support multiplication" -- PARTIAL:
    abs    = error' "error, mixed amounts do not support abs"
    signum = error' "error, mixed amounts do not support signum"

-- | Calculate the key used to store an Amount within a MixedAmount.
amountKey :: Amount -> MixedAmountKey
amountKey amt@Amount{acommodity=c} = case aprice amt of
    Nothing             -> MixedAmountKeyNoPrice    c
    Just (TotalPrice p) -> MixedAmountKeyTotalPrice c (acommodity p)
    Just (UnitPrice  p) -> MixedAmountKeyUnitPrice  c (acommodity p) (aquantity p)

-- | The empty mixed amount.
nullmixedamt :: MixedAmount
nullmixedamt = Mixed mempty

-- | A temporary value for parsed transactions which had no amount specified.
missingmixedamt :: MixedAmount
missingmixedamt = mixedAmount missingamt

-- | Convert amounts in various commodities into a mixed amount.
mixed :: Foldable t => t Amount -> MixedAmount
mixed = maAddAmounts nullmixedamt

-- | Create a MixedAmount from a single Amount.
mixedAmount :: Amount -> MixedAmount
mixedAmount a = Mixed $ M.singleton (amountKey a) a

-- | Add an Amount to a MixedAmount, normalising the result.
maAddAmount :: MixedAmount -> Amount -> MixedAmount
maAddAmount (Mixed ma) a = Mixed $ M.insertWith sumSimilarAmountsUsingFirstPrice (amountKey a) a ma

-- | Add a collection of Amounts to a MixedAmount, normalising the result.
maAddAmounts :: Foldable t => MixedAmount -> t Amount -> MixedAmount
maAddAmounts = foldl' maAddAmount

-- | Negate mixed amount's quantities (and total prices, if any).
maNegate :: MixedAmount -> MixedAmount
maNegate = transformMixedAmount negate

-- | Sum two MixedAmount.
maPlus :: MixedAmount -> MixedAmount -> MixedAmount
maPlus (Mixed as) (Mixed bs) = Mixed $ M.unionWith sumSimilarAmountsUsingFirstPrice as bs

-- | Subtract a MixedAmount from another.
maMinus :: MixedAmount -> MixedAmount -> MixedAmount
maMinus a = maPlus a . maNegate

-- | Sum a collection of MixedAmounts.
maSum :: Foldable t => t MixedAmount -> MixedAmount
maSum = foldl' maPlus nullmixedamt

-- | Divide a mixed amount's quantities (and total prices, if any) by a constant.
divideMixedAmount :: Quantity -> MixedAmount -> MixedAmount
divideMixedAmount n = transformMixedAmount (/n)

-- | Multiply a mixed amount's quantities (and total prices, if any) by a constant.
multiplyMixedAmount :: Quantity -> MixedAmount -> MixedAmount
multiplyMixedAmount n = transformMixedAmount (*n)

-- | Apply a function to a mixed amount's quantities (and its total prices, if it has any).
transformMixedAmount :: (Quantity -> Quantity) -> MixedAmount -> MixedAmount
transformMixedAmount f = mapMixedAmountUnsafe (transformAmount f)

-- | Calculate the average of some mixed amounts.
averageMixedAmounts :: [MixedAmount] -> MixedAmount
averageMixedAmounts as = fromIntegral (length as) `divideMixedAmount` maSum as

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

-- | Does this mixed amount appear to be zero when rendered with its display precision?
-- i.e. does it have zero quantity with no price, zero quantity with a total price (which is also zero),
-- and zero quantity for each unit price?
mixedAmountLooksZero :: MixedAmount -> Bool
mixedAmountLooksZero = all amountLooksZero . amounts . normaliseMixedAmount

-- | Is this mixed amount exactly zero, ignoring its display precision?
-- i.e. does it have zero quantity with no price, zero quantity with a total price (which is also zero),
-- and zero quantity for each unit price?
mixedAmountIsZero :: MixedAmount -> Bool
mixedAmountIsZero = all amountIsZero . amounts . normaliseMixedAmount

-- | Is this mixed amount exactly zero, ignoring its display precision?
--
-- A convenient alias for mixedAmountIsZero.
maIsZero :: MixedAmount -> Bool
maIsZero = mixedAmountIsZero

-- | Is this mixed amount non-zero, ignoring its display precision?
--
-- A convenient alias for not . mixedAmountIsZero.
maIsNonZero :: MixedAmount -> Bool
maIsNonZero = not . mixedAmountIsZero

-- | Get a mixed amount's component amounts.
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
amounts :: MixedAmount -> [Amount]
amounts (Mixed ma)
  | missingkey `M.member` ma = [missingamt]  -- missingamt should always be alone, but detect it even if not
  | M.null nonzeros          = [newzero]
  | otherwise                = toList nonzeros
  where
    newzero = fromMaybe nullamt $ find (not . T.null . acommodity) zeros
    (zeros, nonzeros) = M.partition amountIsZero ma
    missingkey = amountKey missingamt

-- | Get a mixed amount's component amounts without normalising zero and missing
-- amounts. This is used for JSON serialisation, so the order is important. In
-- particular, we want the Amounts given in the order of the MixedAmountKeys,
-- i.e. lexicographically first by commodity, then by price commodity, then by
-- unit price from most negative to most positive.
amountsRaw :: MixedAmount -> [Amount]
amountsRaw (Mixed ma) = toList ma

normaliseMixedAmount :: MixedAmount -> MixedAmount
normaliseMixedAmount = id  -- XXX Remove

-- | Strip prices from a MixedAmount.
normaliseMixedAmountSquashPricesForDisplay :: MixedAmount -> MixedAmount
normaliseMixedAmountSquashPricesForDisplay = mixedAmountStripPrices  -- XXX Remove

-- | Unify a MixedAmount to a single commodity value if possible.
-- This consolidates amounts of the same commodity and discards zero
-- amounts; but this one insists on simplifying to a single commodity,
-- and will return Nothing if this is not possible.
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
sumSimilarAmountsUsingFirstPrice :: Amount -> Amount -> Amount
sumSimilarAmountsUsingFirstPrice a b = (a + b){aprice=p}
  where
    p = case (aprice a, aprice b) of
        (Just (TotalPrice ap), Just (TotalPrice bp))
          -> Just . TotalPrice $ ap{aquantity = aquantity ap + aquantity bp }
        _ -> aprice a

-- -- | Sum same-commodity amounts. If there were different prices, set
-- -- the price to a special marker indicating "various". Only used as a
-- -- rendering helper.
-- sumSimilarAmountsNotingPriceDifference :: [Amount] -> Amount
-- sumSimilarAmountsNotingPriceDifference [] = nullamt
-- sumSimilarAmountsNotingPriceDifference as = undefined

-- | Filter a mixed amount's component amounts by a predicate.
filterMixedAmount :: (Amount -> Bool) -> MixedAmount -> MixedAmount
filterMixedAmount p (Mixed ma) = Mixed $ M.filter p ma

-- | Return an unnormalised MixedAmount containing exactly one Amount
-- with the specified commodity and the quantity of that commodity
-- found in the original. NB if Amount's quantity is zero it will be
-- discarded next time the MixedAmount gets normalised.
filterMixedAmountByCommodity :: CommoditySymbol -> MixedAmount -> MixedAmount
filterMixedAmountByCommodity c (Mixed ma)
  | M.null ma' = mixedAmount nullamt{acommodity=c}
  | otherwise  = Mixed ma'
  where ma' = M.filter ((c==) . acommodity) ma

-- | Apply a transform to a mixed amount's component 'Amount's.
mapMixedAmount :: (Amount -> Amount) -> MixedAmount -> MixedAmount
mapMixedAmount f (Mixed ma) = mixed . map f $ toList ma

-- | Apply a transform to a mixed amount's component 'Amount's, which does not
-- affect the key of the amount (i.e. doesn't change the commodity, price
-- commodity, or unit price amount). This condition is not checked.
mapMixedAmountUnsafe :: (Amount -> Amount) -> MixedAmount -> MixedAmount
mapMixedAmountUnsafe f (Mixed ma) = Mixed $ M.map f ma  -- Use M.map instead of fmap to maintain strictness

-- | Convert all component amounts to cost/selling price where
-- possible (see amountCost).
mixedAmountCost :: MixedAmount -> MixedAmount
mixedAmountCost = mapMixedAmount amountCost

-- -- | MixedAmount derived Eq instance in Types.hs doesn't know that we
-- -- want $0 = EUR0 = 0. Yet we don't want to drag all this code over there.
-- -- For now, use this when cross-commodity zero equality is important.
-- mixedAmountEquals :: MixedAmount -> MixedAmount -> Bool
-- mixedAmountEquals a b = amounts a' == amounts b' || (mixedAmountLooksZero a' && mixedAmountLooksZero b')
--     where a' = mixedAmountStripPrices a
--           b' = mixedAmountStripPrices b

-- | Given a map of standard commodity display styles, apply the
-- appropriate one to each individual amount.
styleMixedAmount :: M.Map CommoditySymbol AmountStyle -> MixedAmount -> MixedAmount
styleMixedAmount styles = mapMixedAmountUnsafe (styleAmount styles)

-- | Reset each individual amount's display style to the default.
mixedAmountUnstyled :: MixedAmount -> MixedAmount
mixedAmountUnstyled = mapMixedAmountUnsafe amountUnstyled

-- | Get the string representation of a mixed amount, after
-- normalising it to one amount per commodity. Assumes amounts have
-- no or similar prices, otherwise this can show misleading prices.
--
-- > showMixedAmount = wbUnpack . showMixedAmountB noColour
showMixedAmount :: MixedAmount -> String
showMixedAmount = wbUnpack . showMixedAmountB noColour

-- | Get the one-line string representation of a mixed amount.
--
-- > showMixedAmountOneLine = wbUnpack . showMixedAmountB oneLine
showMixedAmountOneLine :: MixedAmount -> String
showMixedAmountOneLine = wbUnpack . showMixedAmountB oneLine

-- | Like showMixedAmount, but zero amounts are shown with their
-- commodity if they have one.
--
-- > showMixedAmountWithZeroCommodity = wbUnpack . showMixedAmountB noColour{displayZeroCommodity=True}
showMixedAmountWithZeroCommodity :: MixedAmount -> String
showMixedAmountWithZeroCommodity = wbUnpack . showMixedAmountB noColour{displayZeroCommodity=True}

-- | Get the string representation of a mixed amount, without showing any transaction prices.
-- With a True argument, adds ANSI codes to show negative amounts in red.
--
-- > showMixedAmountWithoutPrice c = wbUnpack . showMixedAmountB noPrice{displayColour=c}
showMixedAmountWithoutPrice :: Bool -> MixedAmount -> String
showMixedAmountWithoutPrice c = wbUnpack . showMixedAmountB noPrice{displayColour=c}

-- | Get the one-line string representation of a mixed amount, but without
-- any \@ prices.
-- With a True argument, adds ANSI codes to show negative amounts in red.
--
-- > showMixedAmountOneLineWithoutPrice c = wbUnpack . showMixedAmountB oneLine{displayColour=c}
showMixedAmountOneLineWithoutPrice :: Bool -> MixedAmount -> String
showMixedAmountOneLineWithoutPrice c = wbUnpack . showMixedAmountB oneLine{displayColour=c}

-- | Like showMixedAmountOneLineWithoutPrice, but show at most the given width,
-- with an elision indicator if there are more.
-- With a True argument, adds ANSI codes to show negative amounts in red.
--
-- > showMixedAmountElided w c = wbUnpack . showMixedAmountB oneLine{displayColour=c, displayMaxWidth=Just w}
showMixedAmountElided :: Int -> Bool -> MixedAmount -> String
showMixedAmountElided w c = wbUnpack . showMixedAmountB oneLine{displayColour=c, displayMaxWidth=Just w}

-- | Get an unambiguous string representation of a mixed amount for debugging.
showMixedAmountDebug :: MixedAmount -> String
showMixedAmountDebug m | m == missingmixedamt = "(missing)"
                       | otherwise       = printf "Mixed [%s]" as
    where as = intercalate "\n       " $ map showAmountDebug $ amounts m

-- | General function to generate a WideBuilder for a MixedAmount, according to the
-- supplied AmountDisplayOpts. This is the main function to use for showing
-- MixedAmounts, constructing a builder; it can then be converted to a Text with
-- wbToText, or to a String with wbUnpack.
--
-- If a maximum width is given then:
-- - If displayed on one line, it will display as many Amounts as can
--   fit in the given width, and further Amounts will be elided. There
--   will always be at least one amount displayed, even if this will
--   exceed the requested maximum width.
-- - If displayed on multiple lines, any Amounts longer than the
--   maximum width will be elided.
showMixedAmountB :: AmountDisplayOpts -> MixedAmount -> WideBuilder
showMixedAmountB opts ma
    | displayOneLine opts = showMixedAmountOneLineB opts ma
    | otherwise           = WideBuilder (wbBuilder . mconcat $ intersperse sep lines) width
  where
    lines = showMixedAmountLinesB opts ma
    width = headDef 0 $ map wbWidth lines
    sep = WideBuilder (TB.singleton '\n') 0

-- | Helper for showMixedAmountB to show a list of Amounts on multiple lines. This returns
-- the list of WideBuilders: one for each Amount, and padded/elided to the appropriate
-- width. This does not honour displayOneLine: all amounts will be displayed as if
-- displayOneLine were False.
showMixedAmountLinesB :: AmountDisplayOpts -> MixedAmount -> [WideBuilder]
showMixedAmountLinesB opts@AmountDisplayOpts{displayMaxWidth=mmax,displayMinWidth=mmin} ma =
    map (adBuilder . pad) elided
  where
    astrs = amtDisplayList (wbWidth sep) (showAmountB opts) . amounts $
              if displayPrice opts then ma else mixedAmountStripPrices ma
    sep   = WideBuilder (TB.singleton '\n') 0
    width = maximum $ fromMaybe 0 mmin : map (wbWidth . adBuilder) elided

    pad amt = amt{ adBuilder = WideBuilder (TB.fromText $ T.replicate w " ") w <> adBuilder amt }
      where w = width - wbWidth (adBuilder amt)

    elided = maybe id elideTo mmax astrs
    elideTo m xs = maybeAppend elisionStr short
      where
        elisionStr = elisionDisplay (Just m) (wbWidth sep) (length long) $ lastDef nullAmountDisplay short
        (short, long) = partition ((m>=) . wbWidth . adBuilder) xs

-- | Helper for showMixedAmountB to deal with single line displays. This does not
-- honour displayOneLine: all amounts will be displayed as if displayOneLine
-- were True.
showMixedAmountOneLineB :: AmountDisplayOpts -> MixedAmount -> WideBuilder
showMixedAmountOneLineB opts@AmountDisplayOpts{displayMaxWidth=mmax,displayMinWidth=mmin} ma =
    WideBuilder (wbBuilder . pad . mconcat . intersperse sep $ map adBuilder elided)
    . max width $ fromMaybe 0 mmin
  where
    width  = maybe 0 adTotal $ lastMay elided
    astrs  = amtDisplayList (wbWidth sep) (showAmountB opts) . amounts $
               if displayPrice opts then ma else mixedAmountStripPrices ma
    sep    = WideBuilder (TB.fromString ", ") 2
    n      = length astrs

    pad = (WideBuilder (TB.fromText $ T.replicate w " ") w <>)
      where w = fromMaybe 0 mmin - width

    elided = maybe id elideTo mmax astrs
    elideTo m = addElide . takeFitting m . withElided
    -- Add the last elision string to the end of the display list
    addElide [] = []
    addElide xs = maybeAppend (snd $ last xs) $ map fst xs
    -- Return the elements of the display list which fit within the maximum width
    -- (including their elision strings). Always display at least one amount,
    -- regardless of width.
    takeFitting _ []     = []
    takeFitting m (x:xs) = x : dropWhileRev (\(a,e) -> m < adTotal (fromMaybe a e)) xs
    dropWhileRev p = foldr (\x xs -> if null xs && p x then [] else x:xs) []

    -- Add the elision strings (if any) to each amount
    withElided = zipWith (\num amt -> (amt, elisionDisplay Nothing (wbWidth sep) num amt)) [n-1,n-2..0]

data AmountDisplay = AmountDisplay
  { adBuilder :: !WideBuilder  -- ^ String representation of the Amount
  , adTotal   :: !Int            -- ^ Cumulative length of MixedAmount this Amount is part of,
                                --   including separators
  } deriving (Show)

nullAmountDisplay :: AmountDisplay
nullAmountDisplay = AmountDisplay mempty 0

amtDisplayList :: Int -> (Amount -> WideBuilder) -> [Amount] -> [AmountDisplay]
amtDisplayList sep showamt = snd . mapAccumL display (-sep)
  where
    display tot amt = (tot', AmountDisplay str tot')
      where
        str  = showamt amt
        tot' = tot + (wbWidth str) + sep

-- The string "m more", added to the previous running total
elisionDisplay :: Maybe Int -> Int -> Int -> AmountDisplay -> Maybe AmountDisplay
elisionDisplay mmax sep n lastAmt
  | n > 0     = Just $ AmountDisplay (WideBuilder (TB.fromText str) len) (adTotal lastAmt + len)
  | otherwise = Nothing
  where
    fullString = T.pack $ show n ++ " more.."
    -- sep from the separator, 7 from " more..", 1 + floor (logBase 10 n) from number
    fullLength = sep + 8 + floor (logBase 10 $ fromIntegral n)

    str | Just m <- mmax, fullLength > m = T.take (m - 2) fullString <> ".."
        | otherwise                      = fullString
    len = case mmax of Nothing -> fullLength
                       Just m  -> max 2 $ min m fullLength

maybeAppend :: Maybe a -> [a] -> [a]
maybeAppend Nothing  = id
maybeAppend (Just a) = (++[a])

-- | Compact labelled trace of a mixed amount, for debugging.
ltraceamount :: String -> MixedAmount -> MixedAmount
ltraceamount s = traceWith (((s ++ ": ") ++).showMixedAmount)

-- | Set the display precision in the amount's commodities.
mixedAmountSetPrecision :: AmountPrecision -> MixedAmount -> MixedAmount
mixedAmountSetPrecision p = mapMixedAmountUnsafe (amountSetPrecision p)

-- | In each component amount, increase the display precision sufficiently
-- to render it exactly (showing all significant decimal digits).
mixedAmountSetFullPrecision :: MixedAmount -> MixedAmount
mixedAmountSetFullPrecision = mapMixedAmountUnsafe amountSetFullPrecision

-- | Remove all prices from a MixedAmount.
mixedAmountStripPrices :: MixedAmount -> MixedAmount
mixedAmountStripPrices (Mixed ma) =
    foldl' (\m a -> maAddAmount m a{aprice=Nothing}) (Mixed noPrices) withPrices
  where (noPrices, withPrices) = M.partition (isNothing . aprice) ma

-- | Canonicalise a mixed amount's display styles using the provided commodity style map.
canonicaliseMixedAmount :: M.Map CommoditySymbol AmountStyle -> MixedAmount -> MixedAmount
canonicaliseMixedAmount styles = mapMixedAmountUnsafe (canonicaliseAmount styles)

-- | Replace each component amount's TotalPrice, if it has one, with an equivalent UnitPrice.
-- Has no effect on amounts without one.
-- Does Decimal division, might be some rounding/irrational number issues.
mixedAmountTotalPriceToUnitPrice :: MixedAmount -> MixedAmount
mixedAmountTotalPriceToUnitPrice = mapMixedAmount amountTotalPriceToUnitPrice


-------------------------------------------------------------------------------
-- tests

tests_Amount = tests "Amount" [
   tests "Amount" [

     test "amountCost" $ do
       amountCost (eur 1) @?= eur 1
       amountCost (eur 2){aprice=Just $ UnitPrice $ usd 2} @?= usd 4
       amountCost (eur 1){aprice=Just $ TotalPrice $ usd 2} @?= usd 2
       amountCost (eur (-1)){aprice=Just $ TotalPrice $ usd (-2)} @?= usd (-2)

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
      maSum (map mixedAmount
        [usd 1.25
        ,usd (-1) `withPrecision` Precision 3
        ,usd (-0.25)
        ])
        @?= mixedAmount (usd 0 `withPrecision` Precision 3)

    ,test "adding mixed amounts with total prices" $ do
      maSum (map mixedAmount
        [usd 1 @@ eur 1
        ,usd (-2) @@ eur 1
        ])
        @?= mixedAmount (usd (-1) @@ eur 2)

    ,test "showMixedAmount" $ do
       showMixedAmount (mixedAmount (usd 1)) @?= "$1.00"
       showMixedAmount (mixedAmount (usd 1 `at` eur 2)) @?= "$1.00 @ €2.00"
       showMixedAmount (mixedAmount (usd 0)) @?= "0"
       showMixedAmount nullmixedamt @?= "0"
       showMixedAmount missingmixedamt @?= ""

    ,test "showMixedAmountWithoutPrice" $ do
      let a = usd 1 `at` eur 2
      showMixedAmountWithoutPrice False (mixedAmount (a)) @?= "$1.00"
      showMixedAmountWithoutPrice False (mixed [a, -a]) @?= "0"

    ,tests "amounts" [
       test "a missing amount overrides any other amounts" $
        amounts (mixed [usd 1, missingamt]) @?= [missingamt]
      ,test "unpriced same-commodity amounts are combined" $
        amounts (mixed [usd 0, usd 2]) @?= [usd 2]
      ,test "amounts with same unit price are combined" $
        amounts (mixed [usd 1 `at` eur 1, usd 1 `at` eur 1]) @?= [usd 2 `at` eur 1]
      ,test "amounts with different unit prices are not combined" $
        amounts (mixed [usd 1 `at` eur 1, usd 1 `at` eur 2]) @?= [usd 1 `at` eur 1, usd 1 `at` eur 2]
      ,test "amounts with total prices are combined" $
        amounts (mixed [usd 1 @@ eur 1, usd 1 @@ eur 1]) @?= [usd 2 @@ eur 2]
    ]

    ,test "mixedAmountStripPrices" $ do
       amounts (mixedAmountStripPrices nullmixedamt) @?= [nullamt]
       assertBool "" $ mixedAmountLooksZero $ mixedAmountStripPrices
        (mixed [usd 10
               ,usd 10 @@ eur 7
               ,usd (-10)
               ,usd (-10) @@ eur (-7)
               ])

  ]

 ]
