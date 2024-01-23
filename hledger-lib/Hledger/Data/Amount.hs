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

It may also have an 'AmountCost', representing this amount's per-unit
or total cost in a different commodity. If present, this is rendered like
so:

@
  EUR 2 \@ $1.50  (unit cost)
  EUR 2 \@\@ $3   (total cost)
@

A 'MixedAmount' is zero or more simple amounts, so can represent multiple
commodities; this is the type most often used:

@
  0
  $50 + EUR 3
  16h + $13.55 + AAPL 500 + 6 oranges
@

A mixed amount is always \"normalised\", it has no more than one amount
in each commodity and cost. When calling 'amounts' it will have no zero
amounts, or just a single zero amount and no other amounts.

Limited arithmetic with simple and mixed amounts is supported, best used
with similar amounts since it mostly ignores costss and commodity exchange rates.

-}

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.Data.Amount (
  -- * Commodity
  showCommoditySymbol,
  isNonsimpleCommodityChar,
  quoteCommoditySymbolIfNeeded,

  -- * Amount
  -- ** arithmetic
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
  amountCost,
  amountIsZero,
  amountLooksZero,
  divideAmount,
  multiplyAmount,
  invertAmount,
  -- ** styles
  amountstyle,
  canonicaliseAmount,
  styleAmount,
  amountSetStyles,
  amountStyleSetRounding,
  amountStylesSetRounding,
  amountUnstyled,
  -- ** rendering
  AmountDisplayOpts(..),
  noColour,
  noCost,
  oneLine,
  csvDisplay,
  showAmountB,
  showAmount,
  showAmountCostB,
  cshowAmount,
  showAmountWithZeroCommodity,
  showAmountDebug,
  showAmountWithoutPrice,
  amountSetPrecision,
  amountSetPrecisionMin,
  amountSetPrecisionMax,
  withPrecision,
  amountSetFullPrecision,
  amountSetFullPrecisionOr,
  amountInternalPrecision,
  amountDisplayPrecision,
  defaultMaxPrecision,
  setAmountInternalPrecision,
  withInternalPrecision,
  setAmountDecimalPoint,
  withDecimalPoint,
  amountStripCost,

  -- * MixedAmount
  nullmixedamt,
  missingmixedamt,
  isMissingMixedAmount,
  mixed,
  mixedAmount,
  maAddAmount,
  maAddAmounts,
  amounts,
  amountsRaw,
  amountsPreservingZeros,
  maCommodities,
  filterMixedAmount,
  filterMixedAmountByCommodity,
  mapMixedAmount,
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
  -- ** styles
  canonicaliseMixedAmount,
  styleMixedAmount,
  mixedAmountSetStyles,
  mixedAmountUnstyled,
  -- ** rendering
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
  mixedAmountSetPrecisionMin,
  mixedAmountSetPrecisionMax,

  -- * misc.
  tests_Amount
) where

import Prelude hiding (Applicative(..))
import Control.Applicative (Applicative(..))
import Control.Monad (foldM)
import Data.Char (isDigit)
import Data.Decimal (DecimalRaw(..), decimalPlaces, normalizeDecimal, roundTo)
import Data.Default (Default(..))
import Data.Foldable (toList)
import Data.List (find, foldl', intercalate, intersperse, mapAccumL, partition)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, isNothing)
import Data.Semigroup (Semigroup(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import Data.Word (Word8)
import Safe (headDef, lastDef, lastMay)
import System.Console.ANSI (Color(..),ColorIntensity(..))

import Test.Tasty (testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

import Hledger.Data.Types
import Hledger.Utils (colorB, numDigitsInt, numDigitsInteger)
import Hledger.Utils.Text (textQuoteIfNeeded)
import Text.WideString (WideBuilder(..), wbFromText, wbToText, wbUnpack)
import Data.Functor ((<&>))
-- import Data.Function ((&))
-- import Hledger.Utils.Debug (dbg0)


-- A 'Commodity' is a symbol representing a currency or some other kind of
-- thing we are tracking, and some display preferences that tell how to
-- display 'Amount's of the commodity - is the symbol on the left or right,
-- are thousands separated by comma, significant decimal places and so on.

-- | Show space-containing commodity symbols quoted, as they are in a journal.
showCommoditySymbol :: T.Text -> T.Text
showCommoditySymbol = textQuoteIfNeeded

-- characters that may not be used in a non-quoted commodity symbol
isNonsimpleCommodityChar :: Char -> Bool
isNonsimpleCommodityChar = liftA2 (||) isDigit isOther
  where
    otherChars = "-+.@*;\t\n \"{}=" :: T.Text
    isOther c = T.any (==c) otherChars

quoteCommoditySymbolIfNeeded :: T.Text -> T.Text
quoteCommoditySymbolIfNeeded s
  | T.any isNonsimpleCommodityChar s = "\"" <> s <> "\""
  | otherwise = s


-- | Options for the display of Amount and MixedAmount.
-- (ee also Types.AmountStyle.
data AmountDisplayOpts = AmountDisplayOpts
  { displayCommodity        :: Bool       -- ^ Whether to display commodity symbols.
  , displayZeroCommodity    :: Bool       -- ^ Whether to display commodity symbols for zero Amounts.
  , displayCommodityOrder   :: Maybe [CommoditySymbol]
                                          -- ^ For a MixedAmount, an optional order in which to display the commodities.
                                          --   Also, causes 0s to be generated for any commodities which are not present
                                          --   (important for tabular reports).
  , displayDigitGroups      :: Bool       -- ^ Whether to display digit group marks (eg thousands separators)
  , displayForceDecimalMark :: Bool       -- ^ Whether to add a trailing decimal mark when there are no decimal digits 
                                          --   and there are digit group marks, to disambiguate
  , displayOneLine          :: Bool       -- ^ Whether to display on one line.
  , displayMinWidth         :: Maybe Int  -- ^ Minimum width to pad to
  , displayMaxWidth         :: Maybe Int  -- ^ Maximum width to clip to
  , displayCost             :: Bool       -- ^ Whether to display Amounts' costs.
  , displayColour           :: Bool       -- ^ Whether to ansi-colourise negative Amounts.
  } deriving (Show)

-- | By default, display Amount and MixedAmount using @noColour@ amount display options.
instance Default AmountDisplayOpts where def = noColour

-- | Display amounts without colour, and with various other defaults.
noColour :: AmountDisplayOpts
noColour = AmountDisplayOpts {
    displayCommodity        = True
  , displayZeroCommodity    = False
  , displayCommodityOrder   = Nothing
  , displayDigitGroups      = True
  , displayForceDecimalMark   = False
  , displayOneLine          = False
  , displayMinWidth         = Just 0
  , displayMaxWidth         = Nothing
  , displayCost             = True
  , displayColour           = False
  }

-- | Display Amount and MixedAmount with no costs.
noCost :: AmountDisplayOpts
noCost = def{displayCost=False}

-- | Display Amount and MixedAmount on one line with no costs.
oneLine :: AmountDisplayOpts
oneLine = def{displayOneLine=True, displayCost=False}

-- | Display Amount and MixedAmount in a form suitable for CSV output.
csvDisplay :: AmountDisplayOpts
csvDisplay = oneLine{displayDigitGroups=False}

-------------------------------------------------------------------------------
-- Amount arithmetic

instance Num Amount where
    abs a@Amount{aquantity=q}    = a{aquantity=abs q}
    signum a@Amount{aquantity=q} = a{aquantity=signum q}
    fromInteger i                = nullamt{aquantity=fromInteger i}
    negate                       = transformAmount negate
    (+)                          = similarAmountsOp (+)
    (-)                          = similarAmountsOp (-)
    (*)                          = similarAmountsOp (*)

-- | The empty simple amount - a zero with no commodity symbol or cost
-- and the default amount display style.
nullamt :: Amount
nullamt = Amount{acommodity="", aquantity=0, acost=Nothing, astyle=amountstyle}

-- | A special amount used as a marker, meaning
-- "no explicit amount provided here, infer it when needed".
-- It is nullamt with commodity symbol "AUTO".
missingamt :: Amount
missingamt = nullamt{acommodity="AUTO"}

-- Handy amount constructors for tests.
-- usd/eur/gbp round their argument to a whole number of pennies/cents.
-- XXX these are a bit clashy
num n = nullamt{acommodity="",  aquantity=n}
hrs n = nullamt{acommodity="h", aquantity=n,           astyle=amountstyle{asprecision=Precision 2, ascommodityside=R}}
usd n = nullamt{acommodity="$", aquantity=roundTo 2 n, astyle=amountstyle{asprecision=Precision 2}}
eur n = nullamt{acommodity="€", aquantity=roundTo 2 n, astyle=amountstyle{asprecision=Precision 2}}
gbp n = nullamt{acommodity="£", aquantity=roundTo 2 n, astyle=amountstyle{asprecision=Precision 2}}
per n = nullamt{acommodity="%", aquantity=n,           astyle=amountstyle{asprecision=Precision 1, ascommodityside=R, ascommodityspaced=True}}
amt `at` costamt = amt{acost=Just $ UnitCost costamt}
amt @@ costamt = amt{acost=Just $ TotalCost costamt}

-- | Apply a binary arithmetic operator to two amounts, which should
-- be in the same commodity if non-zero (warning, this is not checked).
-- A zero result keeps the commodity of the second amount.
-- The result's display style is that of the second amount, with
-- precision set to the highest of either amount.
-- Costs are ignored and discarded.
-- Remember: the caller is responsible for ensuring both amounts have the same commodity.
similarAmountsOp :: (Quantity -> Quantity -> Quantity) -> Amount -> Amount -> Amount
similarAmountsOp op Amount{acommodity=_,  aquantity=q1, astyle=AmountStyle{asprecision=p1}}
                    Amount{acommodity=c2, aquantity=q2, astyle=s2@AmountStyle{asprecision=p2}} =
   -- trace ("a1:"++showAmountDebug a1) $ trace ("a2:"++showAmountDebug a2) $ traceWith (("= :"++).showAmountDebug)
   nullamt{acommodity=c2, aquantity=q1 `op` q2, astyle=s2{asprecision=max p1 p2}}
  --  c1==c2 || q1==0 || q2==0 =
  --  otherwise = error "tried to do simple arithmetic with amounts in different commodities"

-- | Convert an amount to the specified commodity, ignoring and discarding
-- any costs and assuming an exchange rate of 1.
amountWithCommodity :: CommoditySymbol -> Amount -> Amount
amountWithCommodity c a = a{acommodity=c, acost=Nothing}

-- | Convert a amount to its "cost" or "selling price" in another commodity,
-- using its attached cost if it has one.  Notes:
--
-- - cost amounts must be MixedAmounts with exactly one component Amount
--   (or there will be a runtime error XXX)
--
-- - cost amounts should be positive in the Journal
--   (though this is currently not enforced)
amountCost :: Amount -> Amount
amountCost a@Amount{aquantity=q, acost=mp} =
    case mp of
      Nothing                                  -> a
      Just (UnitCost  p@Amount{aquantity=pq}) -> p{aquantity=pq * q}
      Just (TotalCost p@Amount{aquantity=pq}) -> p{aquantity=pq}

-- | Strip all costs from an Amount
amountStripCost :: Amount -> Amount
amountStripCost a = a{acost=Nothing}

-- | Apply a function to an amount's quantity (and its total cost, if it has one).
transformAmount :: (Quantity -> Quantity) -> Amount -> Amount
transformAmount f a@Amount{aquantity=q,acost=p} = a{aquantity=f q, acost=f' <$> p}
  where
    f' (TotalCost a1@Amount{aquantity=pq}) = TotalCost a1{aquantity = f pq}
    f' p' = p'

-- | Divide an amount's quantity (and total cost, if any) by some number.
divideAmount :: Quantity -> Amount -> Amount
divideAmount n = transformAmount (/n)

-- | Multiply an amount's quantity (and its total cost, if it has one) by a constant.
multiplyAmount :: Quantity -> Amount -> Amount
multiplyAmount n = transformAmount (*n)

-- | Invert an amount (replace its quantity q with 1/q).
-- (Its cost if any is not changed, currently.)
invertAmount :: Amount -> Amount
invertAmount a@Amount{aquantity=q} = a{aquantity=1/q}

-- | Is this amount negative ? The cost is ignored.
isNegativeAmount :: Amount -> Bool
isNegativeAmount Amount{aquantity=q} = q < 0

-- | Round an Amount's Quantity (internally) to match its display precision. 
-- If that is unset or NaturalPrecision, this does nothing.
amountRoundedQuantity :: Amount -> Quantity
amountRoundedQuantity Amount{aquantity=q, astyle=AmountStyle{asprecision=mp}} = case mp of
    NaturalPrecision -> q
    Precision p      -> roundTo p q

-- | Apply a test to both an Amount and its total cost, if it has one.
testAmountAndTotalCost :: (Amount -> Bool) -> Amount -> Bool
testAmountAndTotalCost f amt = case acost amt of
    Just (TotalCost cost) -> f amt && f cost
    _                       -> f amt

-- | Do this Amount and (and its total cost, if it has one) appear to be zero
-- when rendered with its display precision ?
-- The display precision should usually have a specific value here;
-- if unset, it will be treated like NaturalPrecision.
amountLooksZero :: Amount -> Bool
amountLooksZero = testAmountAndTotalCost looksZero
  where
    looksZero Amount{aquantity=Decimal e q, astyle=AmountStyle{asprecision=p}} = case p of
        Precision d      -> if e > d then abs q <= 5*10^(e-d-1) else q == 0
        NaturalPrecision -> q == 0

-- | Is this Amount (and its total cost, if it has one) exactly zero, ignoring its display precision ?
amountIsZero :: Amount -> Bool
amountIsZero = testAmountAndTotalCost (\Amount{aquantity=Decimal _ q} -> q == 0)

-- | Does this amount's internal Decimal representation have the
-- maximum number of digits, suggesting that it probably is
-- representing an infinite decimal ?
amountHasMaxDigits :: Amount -> Bool
amountHasMaxDigits = (>= 255) . numDigitsInteger . decimalMantissa . aquantity
-- XXX this seems not always right. Eg:
-- ghci> let n = 100 / (3.0 :: Decimal)
-- decimalPlaces n
-- 255
-- numDigitsInteger $ decimalMantissa n
-- 257


-- | Set an amount's display precision, flipped.
withPrecision :: Amount -> AmountPrecision -> Amount
withPrecision = flip amountSetPrecision

-- | Set an amount's display precision.
amountSetPrecision :: AmountPrecision -> Amount -> Amount
amountSetPrecision p a@Amount{astyle=s} = a{astyle=s{asprecision=p}}

-- | Ensure an amount's display precision is at least the given minimum precision.
-- Always sets an explicit Precision.
amountSetPrecisionMin :: Word8 -> Amount -> Amount
amountSetPrecisionMin minp a = amountSetPrecision p a
  where p = Precision $ max minp (amountDisplayPrecision a)

-- | Ensure an amount's display precision is at most the given maximum precision.
-- Always sets an explicit Precision.
amountSetPrecisionMax :: Word8 -> Amount -> Amount
amountSetPrecisionMax maxp a = amountSetPrecision p a
  where p = Precision $ min maxp (amountDisplayPrecision a)

-- | Increase an amount's display precision, if needed, to enough decimal places
-- to show it exactly (showing all significant decimal digits, without trailing zeros).
-- If the amount's display precision is unset, it will be treated as precision 0.
amountSetFullPrecision :: Amount -> Amount
amountSetFullPrecision a = amountSetPrecision p a
  where
    p                = max displayprecision naturalprecision
    displayprecision = asprecision $ astyle a
    naturalprecision = Precision $ amountInternalPrecision a
-- XXX Is that last sentence correct ?
-- max (Precision n) NaturalPrecision is NaturalPrecision.
-- Would this work instead ?
-- amountSetFullPrecision a = amountSetPrecision (Precision p) a
--   where p = max (amountDisplayPrecision a) (amountInternalPrecision a)


-- | We often want to display "infinite decimal" amounts rounded to some readable
-- number of digits, while still displaying amounts with a large "non infinite" number
-- of decimal digits (eg, 100 or 200 digits) in full.
-- This helper is like amountSetFullPrecision, but with some refinements:
-- 1. If the internal precision is the maximum (255), indicating an infinite decimal, 
-- the display precision is set to a smaller hard-coded default (8).
-- 2. A maximum display precision can be specified, setting a hard upper limit.
-- This function always sets an explicit display precision (ie, Precision n).
amountSetFullPrecisionOr :: Maybe Word8 -> Amount -> Amount
amountSetFullPrecisionOr mmaxp a = amountSetPrecision (Precision p2) a
  where
    p1 = if -- dbg0 "maxdigits" $
            amountHasMaxDigits a then defaultMaxPrecision else max disp intp
      -- & dbg0 "p1"
      where
        intp = amountInternalPrecision a
        disp = amountDisplayPrecision a
    p2 = maybe p1 (min p1) mmaxp
      -- & dbg0 "p2"

-- | The fallback display precision used when showing amounts
-- representing an infinite decimal.
defaultMaxPrecision :: Word8
defaultMaxPrecision = 8

-- | How many internal decimal digits are stored for this amount ?
amountInternalPrecision :: Amount -> Word8
amountInternalPrecision = decimalPlaces . normalizeDecimal . aquantity

-- | How many decimal digits will be displayed for this amount ?
amountDisplayPrecision :: Amount -> Word8
amountDisplayPrecision a =
  case asprecision $ astyle a of
    Precision n      -> n
    NaturalPrecision -> amountInternalPrecision a

-- | Set an amount's internal decimal precision as well as its display precision.
-- This rounds or pads its Decimal quantity to the specified number of decimal places.
-- Rounding is done with Data.Decimal's default roundTo function:
-- "If the value ends in 5 then it is rounded to the nearest even value (Banker's Rounding)".
setAmountInternalPrecision :: Word8 -> Amount -> Amount
setAmountInternalPrecision p a@Amount{ aquantity=q, astyle=s } = a{
   aquantity=roundTo p q
  ,astyle=s{asprecision=Precision p}
  }

-- | setAmountInternalPrecision with arguments flipped.
withInternalPrecision :: Amount -> Word8 -> Amount
withInternalPrecision = flip setAmountInternalPrecision

-- Amount display styles

-- v1
{-# DEPRECATED canonicaliseAmount "please use styleAmounts instead" #-}
canonicaliseAmount :: M.Map CommoditySymbol AmountStyle -> Amount -> Amount
canonicaliseAmount = styleAmounts

-- v2
{-# DEPRECATED styleAmount "please use styleAmounts instead" #-}
styleAmount :: M.Map CommoditySymbol AmountStyle -> Amount -> Amount
styleAmount = styleAmounts

-- v3
{-# DEPRECATED amountSetStyles "please use styleAmounts instead" #-}
amountSetStyles :: M.Map CommoditySymbol AmountStyle -> Amount -> Amount
amountSetStyles = styleAmounts

-- v4
instance HasAmounts Amount where
  -- | Given some commodity display styles, find and apply the appropriate one to this amount,
  -- and its cost amount if any (and stop; we assume costs don't have costs).
  -- Display precision will be applied (or not) as specified by the style's rounding strategy,
  -- except that costs' precision is never changed (costs are often recorded inexactly,
  -- so we don't want to imply greater precision than they were recorded with).
  -- If no style is found for an amount, it is left unchanged.
  styleAmounts styles a@Amount{aquantity=qty, acommodity=comm, astyle=oldstyle, acost=mcost0} =
    a{astyle=newstyle, acost=mcost1}
    where
      newstyle = mknewstyle False qty oldstyle comm 

      mcost1 = case mcost0 of
        Nothing -> Nothing
        Just (UnitCost  ca@Amount{aquantity=cq, astyle=cs, acommodity=ccomm}) -> Just $ UnitCost  ca{astyle=mknewstyle True cq cs ccomm}
        Just (TotalCost ca@Amount{aquantity=cq, astyle=cs, acommodity=ccomm}) -> Just $ TotalCost ca{astyle=mknewstyle True cq cs ccomm}

      mknewstyle :: Bool -> Quantity -> AmountStyle -> CommoditySymbol -> AmountStyle
      mknewstyle iscost oldq olds com =
        case M.lookup com styles of
          Just s  -> 
            -- dbg0 "new      style" $ 
            amountStyleApplyWithRounding iscost oldq 
              (
                -- dbg0 "applying style"
                s)
              (
                -- dbg0 "old      style"
                olds)
          Nothing -> olds

-- AmountStyle helpers

-- | Replace one AmountStyle with another, but don't just replace the display precision;
-- update that in one of several ways as selected by the new style's "rounding strategy":
--
-- NoRounding - keep the precision unchanged
--
-- SoftRounding -
--
--  if either precision is NaturalPrecision, use NaturalPrecision;
--
--  if the new precision is greater than the old, use the new (adds decimal zeros);
--
--  if the new precision is less than the old, use as close to the new as we can get
--    without dropping (more) non-zero digits (drops decimal zeros).
--
--  for a cost amount, keep the precision unchanged
--
-- HardRounding -
--
--  for a posting amount, use the new precision (may truncate significant digits);
--
--  for a cost amount, keep the precision unchanged
--
-- AllRounding -
--
--  for both posting and cost amounts, do hard rounding.
--
-- Arguments:
--
--  whether this style is for a posting amount or a cost amount,
--
--  the amount's decimal quantity (for inspecting its internal representation), 
--
--  the new style, 
--
--  the old style.
--
amountStyleApplyWithRounding :: Bool -> Quantity -> AmountStyle -> AmountStyle -> AmountStyle
amountStyleApplyWithRounding iscost q news@AmountStyle{asprecision=newp, asrounding=newr} AmountStyle{asprecision=oldp} =
  case newr of
    NoRounding   -> news{asprecision=oldp}
    SoftRounding -> news{asprecision=if iscost then oldp else newp'}
      where
        newp' = case (newp, oldp) of
          (Precision new, Precision old) ->
            if new >= old
            then Precision new
            else Precision $ max (min old internal) new
              where internal = decimalPlaces $ normalizeDecimal q
          _ -> NaturalPrecision
    HardRounding -> news{asprecision=if iscost then oldp else newp}
    AllRounding  -> news

-- | Set this amount style's rounding strategy when being applied to amounts.
amountStyleSetRounding :: Rounding -> AmountStyle -> AmountStyle
amountStyleSetRounding r as = as{asrounding=r}

amountStylesSetRounding :: Rounding -> M.Map CommoditySymbol AmountStyle -> M.Map CommoditySymbol AmountStyle
amountStylesSetRounding r = M.map (amountStyleSetRounding r) 

-- | Default amount style
amountstyle = AmountStyle L False Nothing (Just '.') (Precision 0) NoRounding

-- | Reset this amount's display style to the default.
amountUnstyled :: Amount -> Amount
amountUnstyled a = a{astyle=amountstyle}

-- | Set (or clear) an amount's display decimal point.
setAmountDecimalPoint :: Maybe Char -> Amount -> Amount
setAmountDecimalPoint mc a@Amount{ astyle=s } = a{ astyle=s{asdecimalmark=mc} }

-- | Set (or clear) an amount's display decimal point, flipped.
withDecimalPoint :: Amount -> Maybe Char -> Amount
withDecimalPoint = flip setAmountDecimalPoint

-- Amount rendering

showAmountCostB :: Amount -> WideBuilder
showAmountCostB amt = case acost amt of
    Nothing              -> mempty
    Just (UnitCost  pa) -> WideBuilder (TB.fromString " @ ")  3 <> showAmountB noColour{displayZeroCommodity=True} pa
    Just (TotalCost pa) -> WideBuilder (TB.fromString " @@ ") 4 <> showAmountB noColour{displayZeroCommodity=True} (sign pa)
  where sign = if aquantity amt < 0 then negate else id

showAmountCostDebug :: Maybe AmountCost -> String
showAmountCostDebug Nothing                = ""
showAmountCostDebug (Just (UnitCost pa))  = " @ "  ++ showAmountDebug pa
showAmountCostDebug (Just (TotalCost pa)) = " @@ " ++ showAmountDebug pa

-- | Get the string representation of an amount, based on its
-- commodity's display settings. String representations equivalent to
-- zero are converted to just \"0\". The special "missing" amount is
-- displayed as the empty string.
--
-- > showAmount = wbUnpack . showAmountB noColour
showAmount :: Amount -> String
showAmount = wbUnpack . showAmountB noColour

-- | General function to generate a WideBuilder for an Amount, according the
-- supplied AmountDisplayOpts. This is the main function to use for showing
-- Amounts, constructing a builder; it can then be converted to a Text with
-- wbToText, or to a String with wbUnpack.
-- Some special cases:
--
-- * The special "missing" amount is displayed as the empty string. 
--
-- * If an amount is showing digit group separators but no decimal places,
--   we force showing a decimal mark (with nothing after it) to make
--   it easier to parse correctly.
--
showAmountB :: AmountDisplayOpts -> Amount -> WideBuilder
showAmountB _ Amount{acommodity="AUTO"} = mempty
showAmountB
  AmountDisplayOpts{displayCommodity, displayZeroCommodity, displayDigitGroups
                   ,displayForceDecimalMark, displayCost, displayColour}
  a@Amount{astyle=style} =
    color $ case ascommodityside style of
      L -> (if displayCommodity then wbFromText comm <> space else mempty) <> quantity' <> cost
      R -> quantity' <> (if displayCommodity then space <> wbFromText comm else mempty) <> cost
  where
    color = if displayColour && isNegativeAmount a then colorB Dull Red else id
    quantity = showAmountQuantity displayForceDecimalMark $
      if displayDigitGroups then a else a{astyle=(astyle a){asdigitgroups=Nothing}}
    (quantity', comm)
      | amountLooksZero a && not displayZeroCommodity = (WideBuilder (TB.singleton '0') 1, "")
      | otherwise = (quantity, quoteCommoditySymbolIfNeeded $ acommodity a)
    space = if not (T.null comm) && ascommodityspaced style then WideBuilder (TB.singleton ' ') 1 else mempty
    cost = if displayCost then showAmountCostB a else mempty

-- | Colour version. For a negative amount, adds ANSI codes to change the colour,
-- currently to hard-coded red.
--
-- > cshowAmount = wbUnpack . showAmountB def{displayColour=True}
cshowAmount :: Amount -> String
cshowAmount = wbUnpack . showAmountB def{displayColour=True}

-- | Get the string representation of an amount, without any \@ cost.
--
-- > showAmountWithoutPrice = wbUnpack . showAmountB noCost
showAmountWithoutPrice :: Amount -> String
showAmountWithoutPrice = wbUnpack . showAmountB noCost

-- | Like showAmount, but show a zero amount's commodity if it has one.
--
-- > showAmountWithZeroCommodity = wbUnpack . showAmountB noColour{displayZeryCommodity=True}
showAmountWithZeroCommodity :: Amount -> String
showAmountWithZeroCommodity = wbUnpack . showAmountB noColour{displayZeroCommodity=True}

-- | Get a string representation of an amount for debugging,
-- appropriate to the current debug level. 9 shows maximum detail.
showAmountDebug :: Amount -> String
showAmountDebug Amount{acommodity="AUTO"} = "(missing)"
showAmountDebug Amount{..} =
      "Amount {acommodity=" ++ show acommodity ++ ", aquantity=" ++ show aquantity
   ++ ", acost=" ++ showAmountCostDebug acost ++ ", astyle=" ++ show astyle ++ "}"

-- | Get a Text Builder for the string representation of the number part of of an amount,
-- using the display settings from its commodity. Also returns the width of the number.
-- With a true first argument, if there are no decimal digits but there are digit group separators,
-- it shows the amount with a trailing decimal mark to help disambiguate it for parsing.
showAmountQuantity :: Bool -> Amount -> WideBuilder
showAmountQuantity disambiguate amt@Amount{astyle=AmountStyle{asdecimalmark=mdec, asdigitgroups=mgrps}} =
    signB <> intB <> fracB
  where
    Decimal decplaces mantissa = amountRoundedQuantity amt
    numtxt = T.pack . show $ abs mantissa
    numlen = T.length numtxt
    intLen = max 1 $ numlen - fromIntegral decplaces
    dec = fromMaybe '.' mdec
    numtxtwithzero = T.replicate (fromIntegral decplaces + 1 - numlen) "0" <> numtxt
    (intPart, fracPart) = T.splitAt intLen numtxtwithzero
    intB = applyDigitGroupStyle mgrps intLen $ if decplaces == 0 then numtxt else intPart
    signB = if mantissa < 0 then WideBuilder (TB.singleton '-') 1 else mempty
    fracB = if decplaces > 0 || (isshowingdigitgroupseparator && disambiguate)
      then WideBuilder (TB.singleton dec <> TB.fromText fracPart) (1 + fromIntegral decplaces)
      else mempty
      where
        isshowingdigitgroupseparator = case mgrps of
          Just (DigitGroups _ (rightmostgrplen:_)) -> intLen > fromIntegral rightmostgrplen
          _ -> False

-- | Given an integer as text, and its length, apply the given DigitGroupStyle,
-- inserting digit group separators between digit groups where appropriate.
-- Returns a Text builder and the number of digit group separators used.
applyDigitGroupStyle :: Maybe DigitGroupStyle -> Int -> T.Text -> WideBuilder
applyDigitGroupStyle Nothing                       l s = WideBuilder (TB.fromText s) l
applyDigitGroupStyle (Just (DigitGroups _ []))     l s = WideBuilder (TB.fromText s) l
applyDigitGroupStyle (Just (DigitGroups c (g0:gs0))) l0 s0 = addseps (g0:|gs0) (toInteger l0) s0
  where
    addseps (g1:|gs1) l1 s1
        | l2 > 0    = addseps gs2 l2 rest <> WideBuilder (TB.singleton c <> TB.fromText part) (fromIntegral g1 + 1)
        | otherwise = WideBuilder (TB.fromText s1) (fromInteger l1)
      where
        (rest, part) = T.splitAt (fromInteger l2) s1
        gs2 = fromMaybe (g1:|[]) $ nonEmpty gs1
        l2 = l1 - toInteger g1

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
    (*)    = error "error, mixed amounts do not support multiplication" -- PARTIAL:
    abs    = error "error, mixed amounts do not support abs"
    signum = error "error, mixed amounts do not support signum"

-- | Calculate the key used to store an Amount within a MixedAmount.
amountKey :: Amount -> MixedAmountKey
amountKey amt@Amount{acommodity=c} = case acost amt of
    Nothing             -> MixedAmountKeyNoCost    c
    Just (TotalCost p) -> MixedAmountKeyTotalCost c (acommodity p)
    Just (UnitCost  p) -> MixedAmountKeyUnitCost  c (acommodity p) (aquantity p)

-- | The empty mixed amount.
nullmixedamt :: MixedAmount
nullmixedamt = Mixed mempty

-- | A special mixed amount used as a marker, meaning
-- "no explicit amount provided here, infer it when needed".
missingmixedamt :: MixedAmount
missingmixedamt = mixedAmount missingamt

-- | Does this MixedAmount include the "missing amount" marker ?
-- Note: currently does not test for equality with missingmixedamt,
-- instead it looks for missingamt among the Amounts.
-- missingamt should always be alone, but detect it even if not.
isMissingMixedAmount :: MixedAmount -> Bool
isMissingMixedAmount (Mixed ma) = amountKey missingamt `M.member` ma

-- | Convert amounts in various commodities into a mixed amount.
mixed :: Foldable t => t Amount -> MixedAmount
mixed = maAddAmounts nullmixedamt

-- | Create a MixedAmount from a single Amount.
mixedAmount :: Amount -> MixedAmount
mixedAmount a = Mixed $ M.singleton (amountKey a) a

-- | Add an Amount to a MixedAmount, normalising the result.
-- Amounts with different costs are kept separate.
maAddAmount :: MixedAmount -> Amount -> MixedAmount
maAddAmount (Mixed ma) a = Mixed $ M.insertWith sumSimilarAmountsUsingFirstPrice (amountKey a) a ma

-- | Add a collection of Amounts to a MixedAmount, normalising the result.
-- Amounts with different costs are kept separate.
maAddAmounts :: Foldable t => MixedAmount -> t Amount -> MixedAmount
maAddAmounts = foldl' maAddAmount

-- | Negate mixed amount's quantities (and total costs, if any).
maNegate :: MixedAmount -> MixedAmount
maNegate = transformMixedAmount negate

-- | Sum two MixedAmount, keeping the cost of the first if any.
-- Amounts with different costs are kept separate (since 2021).
maPlus :: MixedAmount -> MixedAmount -> MixedAmount
maPlus (Mixed as) (Mixed bs) = Mixed $ M.unionWith sumSimilarAmountsUsingFirstPrice as bs

-- | Subtract a MixedAmount from another.
-- Amounts with different costs are kept separate.
maMinus :: MixedAmount -> MixedAmount -> MixedAmount
maMinus a = maPlus a . maNegate

-- | Sum a collection of MixedAmounts.
-- Amounts with different costs are kept separate.
maSum :: Foldable t => t MixedAmount -> MixedAmount
maSum = foldl' maPlus nullmixedamt

-- | Divide a mixed amount's quantities (and total costs, if any) by a constant.
divideMixedAmount :: Quantity -> MixedAmount -> MixedAmount
divideMixedAmount n = transformMixedAmount (/n)

-- | Multiply a mixed amount's quantities (and total costs, if any) by a constant.
multiplyMixedAmount :: Quantity -> MixedAmount -> MixedAmount
multiplyMixedAmount n = transformMixedAmount (*n)

-- | Apply a function to a mixed amount's quantities (and its total costs, if it has any).
transformMixedAmount :: (Quantity -> Quantity) -> MixedAmount -> MixedAmount
transformMixedAmount f = mapMixedAmountUnsafe (transformAmount f)

-- | Calculate the average of some mixed amounts.
averageMixedAmounts :: [MixedAmount] -> MixedAmount
averageMixedAmounts as = fromIntegral (length as) `divideMixedAmount` maSum as

-- | Is this mixed amount negative, if we can tell that unambiguously?
-- Ie when normalised, are all individual commodity amounts negative ?
isNegativeMixedAmount :: MixedAmount -> Maybe Bool
isNegativeMixedAmount m =
  case amounts $ mixedAmountStripPrices m of
    []  -> Just False
    [a] -> Just $ isNegativeAmount a
    as | all isNegativeAmount as -> Just True
    as | not (any isNegativeAmount as) -> Just False
    _ -> Nothing  -- multiple amounts with different signs

-- | Does this mixed amount appear to be zero when rendered with its display precision?
-- See amountLooksZero.
mixedAmountLooksZero :: MixedAmount -> Bool
mixedAmountLooksZero (Mixed ma) = all amountLooksZero ma

-- | Is this mixed amount exactly zero, ignoring its display precision?
-- See amountIsZero.
mixedAmountIsZero :: MixedAmount -> Bool
mixedAmountIsZero (Mixed ma) = all amountIsZero ma

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

-- | Get a mixed amount's component amounts, with some cleanups.
-- The following descriptions are old and possibly wrong:
--
-- * amounts in the same commodity are combined unless they have different costs or total costs
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
  | isMissingMixedAmount (Mixed ma) = [missingamt]
  | M.null nonzeros                 = [newzero]
  | otherwise                       = toList nonzeros
  where
    newzero = fromMaybe nullamt $ find (not . T.null . acommodity) zeros
    (zeros, nonzeros) = M.partition amountIsZero ma

-- | Get a mixed amount's component amounts, with some cleanups.
-- This is a new version of @amounts@, with updated descriptions
-- and optimised for @print@ to show commodityful zeros.
--
-- * If it contains the "missing amount" marker, only that is returned
--   (discarding any additional amounts).
--
-- * Or if it contains any non-zero amounts, only those are returned
--   (discarding any zeroes).
--
-- * Or if it contains any zero amounts (possibly more than one,
--   possibly in different commodities), all of those are returned.
--
-- * Otherwise the null amount is returned.
--
amountsPreservingZeros :: MixedAmount -> [Amount]
amountsPreservingZeros (Mixed ma)
  | isMissingMixedAmount (Mixed ma) = [missingamt]
  | not $ M.null nonzeros           = toList nonzeros
  | not $ M.null zeros              = toList zeros
  | otherwise                       = [nullamt]
  where
    (zeros, nonzeros) = M.partition amountIsZero ma

-- | Get a mixed amount's component amounts without normalising zero and missing
-- amounts. This is used for JSON serialisation, so the order is important. In
-- particular, we want the Amounts given in the order of the MixedAmountKeys,
-- i.e. lexicographically first by commodity, then by cost commodity, then by
-- unit cost from most negative to most positive.
amountsRaw :: MixedAmount -> [Amount]
amountsRaw (Mixed ma) = toList ma

-- | Get this mixed amount's commodities as a set.
-- Returns an empty set if there are no amounts.
maCommodities :: MixedAmount -> S.Set CommoditySymbol
maCommodities = S.fromList . fmap acommodity . amounts'
  where amounts' ma@(Mixed m) = if M.null m then [] else amounts ma

-- | Unify a MixedAmount to a single commodity value if possible.
-- This consolidates amounts of the same commodity and discards zero
-- amounts; but this one insists on simplifying to a single commodity,
-- and will return Nothing if this is not possible.
unifyMixedAmount :: MixedAmount -> Maybe Amount
unifyMixedAmount = foldM combine 0 . amounts
  where
    combine amt result
      | amountIsZero amt                    = Just result
      | amountIsZero result                 = Just amt
      | acommodity amt == acommodity result = Just $ amt + result
      | otherwise                           = Nothing

-- | Sum same-commodity amounts in a lossy way, applying the first
-- cost to the result and discarding any other costs. Only used as a
-- rendering helper.
sumSimilarAmountsUsingFirstPrice :: Amount -> Amount -> Amount
sumSimilarAmountsUsingFirstPrice a b = (a + b){acost=p}
  where
    p = case (acost a, acost b) of
        (Just (TotalCost ap), Just (TotalCost bp))
          -> Just . TotalCost $ ap{aquantity = aquantity ap + aquantity bp }
        _ -> acost a

-- -- | Sum same-commodity amounts. If there were different costs, set
-- -- the cost to a special marker indicating "various". Only used as a
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
-- affect the key of the amount (i.e. doesn't change the commodity, cost
-- commodity, or unit cost amount). This condition is not checked.
mapMixedAmountUnsafe :: (Amount -> Amount) -> MixedAmount -> MixedAmount
mapMixedAmountUnsafe f (Mixed ma) = Mixed $ M.map f ma  -- Use M.map instead of fmap to maintain strictness

-- | Convert all component amounts to cost/selling price where
-- possible (see amountCost).
mixedAmountCost :: MixedAmount -> MixedAmount
mixedAmountCost (Mixed ma) =
    foldl' (\m a -> maAddAmount m (amountCost a)) (Mixed noCosts) withCosts
  where (noCosts, withCosts) = M.partition (isNothing . acost) ma

-- -- | MixedAmount derived Eq instance in Types.hs doesn't know that we
-- -- want $0 = EUR0 = 0. Yet we don't want to drag all this code over there.
-- -- For now, use this when cross-commodity zero equality is important.
-- mixedAmountEquals :: MixedAmount -> MixedAmount -> Bool
-- mixedAmountEquals a b = amounts a' == amounts b' || (mixedAmountLooksZero a' && mixedAmountLooksZero b')
--     where a' = mixedAmountStripPrices a
--           b' = mixedAmountStripPrices b

-- Mixed amount styles

-- v1
{-# DEPRECATED canonicaliseMixedAmount "please use mixedAmountSetStyle False (or styleAmounts) instead" #-}
canonicaliseMixedAmount :: M.Map CommoditySymbol AmountStyle -> MixedAmount -> MixedAmount
canonicaliseMixedAmount = styleAmounts

-- v2
{-# DEPRECATED styleMixedAmount "please use styleAmounts instead" #-}
-- | Given a map of standard commodity display styles, find and apply
-- the appropriate style to each individual amount.
styleMixedAmount :: M.Map CommoditySymbol AmountStyle -> MixedAmount -> MixedAmount
styleMixedAmount = styleAmounts

-- v3
{-# DEPRECATED mixedAmountSetStyles "please use styleAmounts instead" #-}
mixedAmountSetStyles :: M.Map CommoditySymbol AmountStyle -> MixedAmount -> MixedAmount
mixedAmountSetStyles = styleAmounts

-- v4
instance HasAmounts MixedAmount where
  styleAmounts styles = mapMixedAmountUnsafe (styleAmounts styles)

instance HasAmounts Account where
  styleAmounts styles acct@Account{aebalance,aibalance} =
    acct{aebalance=styleAmounts styles aebalance, aibalance=styleAmounts styles aibalance}

-- | Reset each individual amount's display style to the default.
mixedAmountUnstyled :: MixedAmount -> MixedAmount
mixedAmountUnstyled = mapMixedAmountUnsafe amountUnstyled

-- Mixed amount rendering

-- | Get the string representation of a mixed amount, after
-- normalising it to one amount per commodity. Assumes amounts have
-- no or similar costs, otherwise this can show misleading costs.
--
-- > showMixedAmount = wbUnpack . showMixedAmountB noColour
showMixedAmount :: MixedAmount -> String
showMixedAmount = wbUnpack . showMixedAmountB noColour

-- | Get the one-line string representation of a mixed amount (also showing any costs).
--
-- > showMixedAmountOneLine = wbUnpack . showMixedAmountB oneLine
showMixedAmountOneLine :: MixedAmount -> String
showMixedAmountOneLine = wbUnpack . showMixedAmountB oneLine{displayCost=True}

-- | Like showMixedAmount, but zero amounts are shown with their
-- commodity if they have one.
--
-- > showMixedAmountWithZeroCommodity = wbUnpack . showMixedAmountB noColour{displayZeroCommodity=True}
showMixedAmountWithZeroCommodity :: MixedAmount -> String
showMixedAmountWithZeroCommodity = wbUnpack . showMixedAmountB noColour{displayZeroCommodity=True}

-- | Get the string representation of a mixed amount, without showing any costs.
-- With a True argument, adds ANSI codes to show negative amounts in red.
--
-- > showMixedAmountWithoutPrice c = wbUnpack . showMixedAmountB noCost{displayColour=c}
showMixedAmountWithoutPrice :: Bool -> MixedAmount -> String
showMixedAmountWithoutPrice c = wbUnpack . showMixedAmountB noCost{displayColour=c}

-- | Get the one-line string representation of a mixed amount, but without
-- any \@ costs.
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
                       | otherwise       = "Mixed [" ++ as ++ "]"
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
    | otherwise           = WideBuilder (wbBuilder . mconcat $ intersperse sep ls) width
  where
    ls = showMixedAmountLinesB opts ma
    width = headDef 0 $ map wbWidth ls
    sep = WideBuilder (TB.singleton '\n') 0

-- | Helper for showMixedAmountB (and postingAsLines, ...) to show a list of Amounts on multiple lines.
-- This returns the list of WideBuilders: one for each Amount, and padded/elided to the appropriate width.
-- This does not honour displayOneLine; all amounts will be displayed as if displayOneLine were False.
showMixedAmountLinesB :: AmountDisplayOpts -> MixedAmount -> [WideBuilder]
showMixedAmountLinesB opts@AmountDisplayOpts{displayMaxWidth=mmax,displayMinWidth=mmin} ma =
    map (adBuilder . pad) elided
  where
    astrs = amtDisplayList (wbWidth sep) (showAmountB opts) . orderedAmounts opts $
              if displayCost opts then ma else mixedAmountStripPrices ma
    sep   = WideBuilder (TB.singleton '\n') 0
    width = maximum $ map (wbWidth . adBuilder) elided

    pad amt
      | Just mw <- mmin =
          let w = (max width mw) - wbWidth (adBuilder amt)
           in amt{ adBuilder = WideBuilder (TB.fromText $ T.replicate w " ") w <> adBuilder amt }
      | otherwise = amt

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
    astrs  = amtDisplayList (wbWidth sep) (showAmountB opts) . orderedAmounts opts $
               if displayCost opts then ma else mixedAmountStripPrices ma
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
    withElided = zipWith (\n2 amt -> (amt, elisionDisplay Nothing (wbWidth sep) n2 amt)) [n-1,n-2..0]

-- Get a mixed amount's component amounts with a bit of cleanup,
-- optionally preserving multiple zeros in different commodities,
-- optionally sorting them according to a commodity display order.
orderedAmounts :: AmountDisplayOpts -> MixedAmount -> [Amount]
orderedAmounts AmountDisplayOpts{displayZeroCommodity=preservezeros, displayCommodityOrder=mcommodityorder} =
  if preservezeros then amountsPreservingZeros else amounts
  <&> maybe id (mapM findfirst) mcommodityorder  -- maybe sort them (somehow..)
  where
    -- Find the first amount with the given commodity, otherwise a null amount in that commodity.
    findfirst :: CommoditySymbol -> [Amount] -> Amount
    findfirst c = fromMaybe nullamtc . find ((c==) . acommodity)
      where
        nullamtc = amountWithCommodity c nullamt

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
    -- sep from the separator, 7 from " more..", numDigits n from number
    fullLength = sep + 7 + numDigitsInt n

    str | Just m <- mmax, fullLength > m = T.take (m - 2) fullString <> ".."
        | otherwise                      = fullString
    len = case mmax of Nothing -> fullLength
                       Just m  -> max 2 $ min m fullLength

maybeAppend :: Maybe a -> [a] -> [a]
maybeAppend Nothing  = id
maybeAppend (Just a) = (++[a])

-- | Set the display precision in the amount's commodities.
mixedAmountSetPrecision :: AmountPrecision -> MixedAmount -> MixedAmount
mixedAmountSetPrecision p = mapMixedAmountUnsafe (amountSetPrecision p)

-- | In each component amount, increase the display precision sufficiently
-- to render it exactly (showing all significant decimal digits).
mixedAmountSetFullPrecision :: MixedAmount -> MixedAmount
mixedAmountSetFullPrecision = mapMixedAmountUnsafe amountSetFullPrecision

-- | In each component amount, ensure the display precision is at least the given value.
-- Makes all amounts have an explicit Precision.
mixedAmountSetPrecisionMin :: Word8 -> MixedAmount -> MixedAmount
mixedAmountSetPrecisionMin p = mapMixedAmountUnsafe (amountSetPrecisionMin p)

-- | In each component amount, ensure the display precision is at most the given value.
-- Makes all amounts have an explicit Precision.
mixedAmountSetPrecisionMax :: Word8 -> MixedAmount -> MixedAmount
mixedAmountSetPrecisionMax p = mapMixedAmountUnsafe (amountSetPrecisionMax p)

-- | Remove all costs from a MixedAmount.
mixedAmountStripPrices :: MixedAmount -> MixedAmount
mixedAmountStripPrices (Mixed ma) =
    foldl' (\m a -> maAddAmount m a{acost=Nothing}) (Mixed noPrices) withPrices
  where (noPrices, withPrices) = M.partition (isNothing . acost) ma


-------------------------------------------------------------------------------
-- tests

tests_Amount = testGroup "Amount" [
   testGroup "Amount" [

     testCase "amountCost" $ do
       amountCost (eur 1) @?= eur 1
       amountCost (eur 2){acost=Just $ UnitCost $ usd 2} @?= usd 4
       amountCost (eur 1){acost=Just $ TotalCost $ usd 2} @?= usd 2
       amountCost (eur (-1)){acost=Just $ TotalCost $ usd (-2)} @?= usd (-2)

    ,testCase "amountLooksZero" $ do
       assertBool "" $ amountLooksZero nullamt
       assertBool "" $ amountLooksZero $ usd 0

    ,testCase "negating amounts" $ do
       negate (usd 1) @?= (usd 1){aquantity= -1}
       let b = (usd 1){acost=Just $ UnitCost $ eur 2} in negate b @?= b{aquantity= -1}

    ,testCase "adding amounts without costs" $ do
       (usd 1.23 + usd (-1.23)) @?= usd 0
       (usd 1.23 + usd (-1.23)) @?= usd 0
       (usd (-1.23) + usd (-1.23)) @?= usd (-2.46)
       sum [usd 1.23,usd (-1.23),usd (-1.23),-(usd (-1.23))] @?= usd 0
       -- highest precision is preserved
       asprecision (astyle $ sum [usd 1 `withPrecision` Precision 1, usd 1 `withPrecision` Precision 3]) @?= Precision 3
       asprecision (astyle $ sum [usd 1 `withPrecision` Precision 3, usd 1 `withPrecision` Precision 1]) @?= Precision 3
       -- adding different commodities assumes conversion rate 1
       assertBool "" $ amountLooksZero (usd 1.23 - eur 1.23)

    ,testCase "showAmount" $ do
      showAmount (usd 0 + gbp 0) @?= "0"

  ]

  ,testGroup "MixedAmount" [

     testCase "comparing mixed amounts compares based on quantities" $ do
       let usdpos = mixed [usd 1]
           usdneg = mixed [usd (-1)]
           eurneg = mixed [eur (-12)]
       compare usdneg usdpos @?= LT
       compare eurneg usdpos @?= LT

     ,testCase "adding mixed amounts to zero, the commodity and amount style are preserved" $
      maSum (map mixedAmount
        [usd 1.25
        ,usd (-1) `withPrecision` Precision 3
        ,usd (-0.25)
        ])
        @?= mixedAmount (usd 0 `withPrecision` Precision 3)

    ,testCase "adding mixed amounts with total costs" $ do
      maSum (map mixedAmount
        [usd 1 @@ eur 1
        ,usd (-2) @@ eur 1
        ])
        @?= mixedAmount (usd (-1) @@ eur 2)

    ,testCase "showMixedAmount" $ do
       showMixedAmount (mixedAmount (usd 1)) @?= "$1.00"
       showMixedAmount (mixedAmount (usd 1 `at` eur 2)) @?= "$1.00 @ €2.00"
       showMixedAmount (mixedAmount (usd 0)) @?= "0"
       showMixedAmount nullmixedamt @?= "0"
       showMixedAmount missingmixedamt @?= ""

    ,testCase "showMixedAmountWithoutPrice" $ do
      let a = usd 1 `at` eur 2
      showMixedAmountWithoutPrice False (mixedAmount (a)) @?= "$1.00"
      showMixedAmountWithoutPrice False (mixed [a, -a]) @?= "0"

    ,testGroup "amounts" [
       testCase "a missing amount overrides any other amounts" $
        amounts (mixed [usd 1, missingamt]) @?= [missingamt]
      ,testCase "costless same-commodity amounts are combined" $
        amounts (mixed [usd 0, usd 2]) @?= [usd 2]
      ,testCase "amounts with same unit cost are combined" $
        amounts (mixed [usd 1 `at` eur 1, usd 1 `at` eur 1]) @?= [usd 2 `at` eur 1]
      ,testCase "amounts with different unit costs are not combined" $
        amounts (mixed [usd 1 `at` eur 1, usd 1 `at` eur 2]) @?= [usd 1 `at` eur 1, usd 1 `at` eur 2]
      ,testCase "amounts with total costs are combined" $
        amounts (mixed [usd 1 @@ eur 1, usd 1 @@ eur 1]) @?= [usd 2 @@ eur 2]
    ]

    ,testCase "mixedAmountStripPrices" $ do
       amounts (mixedAmountStripPrices nullmixedamt) @?= [nullamt]
       assertBool "" $ mixedAmountLooksZero $ mixedAmountStripPrices
        (mixed [usd 10
               ,usd 10 @@ eur 7
               ,usd (-10)
               ,usd (-10) @@ eur (-7)
               ])

  ]

 ]
