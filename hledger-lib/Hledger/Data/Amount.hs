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

-- XXX due for review/rewrite

module Hledger.Data.Amount (
                            amounts,
                            canonicaliseAmount,
                            canonicaliseMixedAmount,
                            convertMixedAmountToSimilarCommodity,
                            costOfAmount,
                            costOfMixedAmount,
                            divideAmount,
                            divideMixedAmount,
                            isNegativeMixedAmount,
                            isReallyZeroMixedAmountCost,
                            isZeroMixedAmount,
                            maxprecision,
                            missingamt,
                            normaliseMixedAmount,
                            nullamt,
                            nullmixedamt,
                            punctuatethousands,
                            setAmountPrecision,
                            setMixedAmountPrecision,
                            showAmountDebug,
                            showMixedAmount,
                            showMixedAmountDebug,
                            showMixedAmountOrZero,
                            showMixedAmountOrZeroWithoutPrice,
                            showMixedAmountWithoutPrice,
                            showMixedAmountWithPrecision,
                            sumMixedAmountsPreservingHighestPrecision,
                            tests_Hledger_Data_Amount
                            -- Hledger.Data.Amount.tests_Hledger_Data_Amount
                           )
where
import qualified Data.Map as Map
import Data.Map (findWithDefault)

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
    (+) = similarAmountsOp (+)
    (-) = similarAmountsOp (-)
    (*) = similarAmountsOp (*)

instance Num MixedAmount where
    fromInteger i = Mixed [Amount (comm "") (fromInteger i) Nothing]
    negate (Mixed as) = Mixed $ map negateAmountPreservingPrice as
        where negateAmountPreservingPrice a = (-a){price=price a}
    (+) (Mixed as) (Mixed bs) = normaliseMixedAmount $ Mixed $ as ++ bs
    (*)    = error' "programming error, mixed amounts do not support multiplication"
    abs    = error' "programming error, mixed amounts do not support abs"
    signum = error' "programming error, mixed amounts do not support signum"

-- | Apply a binary arithmetic operator to two amounts, after converting
-- the first to the commodity (and display precision) of the second in a
-- simplistic way. This should be used only for two amounts in the same
-- commodity, since the conversion rate is assumed to be 1.
-- NB preserving the second commodity is preferred since sum and other
-- folds start with the no-commodity zero amount.
similarAmountsOp :: (Double -> Double -> Double) -> Amount -> Amount -> Amount
similarAmountsOp op a (Amount bc bq _) =
    Amount bc (quantity (convertAmountToSimilarCommodity bc a) `op` bq) Nothing

-- | Convert an amount to the specified commodity, assuming an exchange rate of 1.
convertAmountToSimilarCommodity :: Commodity -> Amount -> Amount
convertAmountToSimilarCommodity c (Amount _ q _) = Amount c q Nothing

-- | Convert a mixed amount to the specified commodity, assuming an exchange rate of 1.
convertMixedAmountToSimilarCommodity :: Commodity -> MixedAmount -> Amount
convertMixedAmountToSimilarCommodity c (Mixed as) = Amount c total Nothing
    where
      total = sum $ map (quantity . convertAmountToSimilarCommodity c) as

-- | Convert an amount to the commodity of its saved price, if any.  Notes:
-- - price amounts must be MixedAmounts with exactly one component Amount (or there will be a runtime error)
-- - price amounts should be positive, though this is not currently enforced
costOfAmount :: Amount -> Amount
costOfAmount a@(Amount _ q price) =
    case price of
      Nothing -> a
      Just (UnitPrice  (Mixed [Amount pc pq Nothing])) -> Amount pc (pq*q) Nothing
      Just (TotalPrice (Mixed [Amount pc pq Nothing])) -> Amount pc (pq*signum q) Nothing
      _ -> error' "costOfAmount: Malformed price encountered, programmer error"

-- | Get the string representation of an amount, based on its commodity's
-- display settings except using the specified precision.
showAmountWithPrecision :: Int -> Amount -> String
showAmountWithPrecision p = showAmount . setAmountPrecision p

setAmountPrecision p a@Amount{commodity=c} = a{commodity=c{precision=p}}

-- XXX refactor
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
-- display settings. Amounts which look like zero are rendered without sign or commodity.
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
    qstr -- | p == maxprecision && isint q = printf "%d" (round q::Integer)
         | p == maxprecision            = printf "%f" q
         | otherwise                    = printf ("%."++show p++"f") q
    -- isint n = fromIntegral (round n) == n

-- | A special precision value meaning show all available digits.
maxprecision = 999999

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
isZeroAmount = null . filter (`elem` "123456789") . showAmountWithoutPriceOrCommodity

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

-- | Does this mixed amount appear to be zero when displayed with its given precision ?
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

-- -- | MixedAmount derives Eq in Types.hs, but that doesn't know that we
-- -- want $0 = EUR0 = 0. Yet we don't want to drag all this code in there.
-- -- When zero equality is important, use this, for now; should be used
-- -- everywhere.
-- mixedAmountEquals :: MixedAmount -> MixedAmount -> Bool
-- mixedAmountEquals a b = amounts a' == amounts b' || (isZeroMixedAmount a' && isZeroMixedAmount b')
--     where a' = normaliseMixedAmount a
--           b' = normaliseMixedAmount b

-- | Get the string representation of a mixed amount, showing each of
-- its component amounts. NB a mixed amount can have an empty amounts
-- list in which case it shows as \"\".
showMixedAmount :: MixedAmount -> String
showMixedAmount m = vConcatRightAligned $ map show $ amounts $ normaliseMixedAmount m

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

-- | Simplify a mixed amount by removing redundancy in its component amounts, as follows:
-- 1. sum amounts which have the same commodity (ignoring their price)
-- 2. remove zero amounts
-- 3. if there are no amounts at all, add a single zero amount
normaliseMixedAmount :: MixedAmount -> MixedAmount
normaliseMixedAmount (Mixed as) = Mixed as''
    where 
      as'' = if null nonzeros then [nullamt] else nonzeros
      (_,nonzeros) = partition (\a -> isReallyZeroAmount a && Mixed [a] /= missingamt) as'
      as' = map sumSamePricedAmountsPreservingPrice $ group $ sort as
      sort = sortBy (\a1 a2 -> compare (sym a1) (sym a2))
      group = groupBy (\a1 a2 -> sym a1 == sym a2)
      sym = symbol . commodity

-- | Set a mixed amount's commodity to the canonicalised commodity from
-- the provided commodity map.
canonicaliseMixedAmount :: Maybe (Map.Map String Commodity) -> MixedAmount -> MixedAmount
canonicaliseMixedAmount canonicalcommoditymap (Mixed as) = Mixed $ map (canonicaliseAmount canonicalcommoditymap) as

-- | Set an amount's commodity to the canonicalised commodity from
-- the provided commodity map.
canonicaliseAmount :: Maybe (Map.Map String Commodity) -> Amount -> Amount
canonicaliseAmount Nothing                      = id
canonicaliseAmount (Just canonicalcommoditymap) = fixamount
    where
      -- like journalCanonicaliseAmounts
      fixamount a@Amount{commodity=c} = a{commodity=fixcommodity c}
      fixcommodity c@Commodity{symbol=s} = findWithDefault c s canonicalcommoditymap

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
      q = quantity (convertAmountToSimilarCommodity bc a) `op` bq
      c = if ap > bp then ac else bc
--

-- | Convert a mixed amount's component amounts to the commodity of their
-- saved price, if any.
costOfMixedAmount :: MixedAmount -> MixedAmount
costOfMixedAmount (Mixed as) = Mixed $ map costOfAmount as

-- | Divide a mixed amount's quantities by some constant.
divideMixedAmount :: MixedAmount -> Double -> MixedAmount
divideMixedAmount (Mixed as) d = Mixed $ map (flip divideAmount d) as

-- | Divide an amount's quantity by some constant.
divideAmount :: Amount -> Double -> Amount
divideAmount a@Amount{quantity=q} d = a{quantity=q/d}

-- | The empty simple amount.
nullamt :: Amount
nullamt = Amount unknown 0 Nothing

-- | The empty mixed amount.
nullmixedamt :: MixedAmount
nullmixedamt = Mixed []

-- | A temporary value for parsed transactions which had no amount specified.
missingamt :: MixedAmount
missingamt = Mixed [Amount unknown{symbol="AUTO"} 0 Nothing]


tests_Hledger_Data_Amount = TestList [

   "showAmount" ~: do
     showAmount (dollars 0 + pounds 0) `is` "0"

  ,"showMixedAmount" ~: do
     showMixedAmount (Mixed [Amount dollar 0 Nothing]) `is` "0"
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
    -- arithmetic with different commodities currently assumes conversion rate 1:
    let a4 = euros (-1.23)
    assertBool "" $ isZeroAmount (a1 + a4)

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
      `is` Mixed [Amount unknown 0 Nothing]

  ,"normaliseMixedAmount" ~: do
     normaliseMixedAmount (Mixed []) `is` Mixed [nullamt]
     assertBool "" $ isZeroMixedAmount $ normaliseMixedAmount (Mixed [Amount {commodity=dollar, quantity=10,    price=Nothing}
                                                                     ,Amount {commodity=dollar, quantity=10,    price=Just (TotalPrice (Mixed [Amount {commodity=euro, quantity=7, price=Nothing}]))}
                                                                     ,Amount {commodity=dollar, quantity=(-10), price=Nothing}
                                                                     ,Amount {commodity=dollar, quantity=(-10), price=Just (TotalPrice (Mixed [Amount {commodity=euro, quantity=7, price=Nothing}]))}
                                                                     ])

  ,"punctuatethousands 1" ~: punctuatethousands "" `is` ""

  ,"punctuatethousands 2" ~: punctuatethousands "1234567.8901" `is` "1,234,567.8901"

  ,"punctuatethousands 3" ~: punctuatethousands "-100" `is` "-100"

  ,"costOfAmount" ~: do
    costOfAmount (euros 1) `is` euros 1
    costOfAmount (euros 2){price=Just $ UnitPrice $ Mixed [dollars 2]} `is` dollars 4
    costOfAmount (euros 1){price=Just $ TotalPrice $ Mixed [dollars 2]} `is` dollars 2
    costOfAmount (euros (-1)){price=Just $ TotalPrice $ Mixed [dollars 2]} `is` dollars (-2)

  ]
