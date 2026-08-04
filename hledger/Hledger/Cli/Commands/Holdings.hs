{-|

The @holdings@ command shows a report of investment holdings (lot-tracked assets).

Work in progress; see doc/SPEC-holdings.md.
Currently it shows the Date, Age, Quantity, Unit/Avg cost, Cost, Price,
Value and Gain columns in list mode, with lot subaccounts aggregated by
default or shown as rows with --lots.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Holdings (
  holdingsmode
 ,holdings
) where

import Control.Applicative ((<|>))
import Data.Default (def)
import Data.List.Extra (intercalate, nubSort)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time.Calendar (addDays, diffDays)
import System.Console.CmdArgs.Explicit (flagNone, flagReq)
import Text.Printf (printf)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands.Print (roundFromRawOpts)
import Hledger.Cli.Utils (writeOutputLazyText)
import Text.Tabular.AsciiWide

-- | Command line options for this command.
holdingsmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Holdings.txt")
  (flattreeflags True ++
   [flagNone ["no-total","N"] (setboolopt "no-total") "omit the final total row"
   ,flagReq ["round"] (\s opts -> Right $ setopt "round" s opts) "TYPE" $
     intercalate "\n"
     ["how much rounding or padding should be done when displaying amounts ?"
     ,"none - show original decimal digits"
     ,"soft - just add or remove decimal zeros"
     ,"       to match precision"
     ,"hard - round amounts to precision (default)"
     ,"all  - also round cost amounts to precision"
     ]
   ,outputFileFlag])
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | Show the holdings report: the assets held in lot-tracked accounts
-- as of the report end date, one row per account (or per lot, with --lots).
--
-- This command receives the journal with lot detail (lot subaccounts and
-- synthetic postings) uncollapsed, regardless of --lots
-- (see maybeCollapseLotDetail); it aggregates lots itself.
holdings :: CliOpts -> Journal -> IO ()
holdings opts@CliOpts{rawopts_=rawopts, reportspec_=rspec@ReportSpec{_rsQuery=q, _rsReportOpts=ropts}} j = do
  if accountlistmode_ ropts == ALTree then error' "holdings: --tree is not yet supported"
  else if (case mvalue of Just (AtThen _) -> True; _ -> False)
  then error' "holdings: --value=then is not supported"
  else rounding `seq`  -- validate the --round value before any output
    writeOutputLazyText opts $
      "Holdings on " <> TL.fromStrict (showDate reportdate) <> "\n\n" <>
      if null rows
      then "(no holdings)\n"
      else renderTable
        def{tableBorders=False}
        (textCell TopLeft)
        (textCell TopRight)
        (textCell TopRight)
        tbl
  where
    showlots = boolopt "lots" rawopts

    -- The date this report shows holdings at: the day before the (exclusive)
    -- report end date if specified, otherwise today.
    mend = queryEndDate False q
    reportdate = maybe (_rsDay rspec) (addDays (-1)) mend

    -- The quantity held in each lot subaccount, from its postings.
    -- Keyed by account and commodity, so amounts in different commodities
    -- (not expected in a lot subaccount, but possible) don't merge wrongly.
    -- Postings are restricted by the report query's non-date terms and its
    -- end date (but not its begin date; holdings are cumulative).
    lotmap :: M.Map (AccountName, CommoditySymbol) Amount
    lotmap = M.fromListWith (+)
      [ ((paccount p, acommodity a), amountStripCost a)
      | p <- journalPostings j
      , isJust $ lotSubaccountName $ paccount p
      , endq `matchesPosting` p
      , a <- amountsRaw $ pamount p
      ]
      where
        endq = And [filterQuery (not . queryIsDateOrDate2) q
                   ,Date $ DateSpan Nothing (Exact <$> mend)]

    -- A lot subaccount's cost basis, parsed from its name
    -- (which by construction contains the acquisition date and unit cost).
    -- The cost gets its commodity's display style, including display
    -- precision (lot names can have more precision, eg from inferred
    -- per-unit costs), so derived amounts (Unit cost, Cost) are displayed
    -- with the standard display precision.
    lotBasis :: AccountName -> Maybe CostBasis
    lotBasis acct = do
      name <- lotSubaccountName acct
      cb <- either (const Nothing) Just $ parseLotName parseAmt name
      Just cb{cbCost = styleAmounts styles <$> cbCost cb}
      where parseAmt = either (const Nothing) Just . parseamount

    -- Amounts are displayed normalised to their commodity's display
    -- precision by default; --round can choose another rounding strategy.
    rounding = fromMaybe HardRounding $ roundFromRawOpts rawopts
    styles = journalCommodityStylesWith rounding j

    priceoracle = journalPriceOracle (infer_prices_ ropts) j

    -- The valuation strategy requested with -V/-X/--value, if any.
    -- It selects the valuation date and/or the valuation commodity;
    -- --value=then is rejected above.
    mvalue = value_ ropts
    (valuationdate, mtargetcomm) = case mvalue of
      Nothing            -> (reportdate, Nothing)
      Just (AtEnd  mc)   -> (reportdate, mc)
      Just (AtNow  mc)   -> (_rsDay rspec, mc)
      Just (AtDate d mc) -> (d, mc)
      Just (AtThen mc)   -> (reportdate, mc)  -- not supported, rejected above

    -- Value a row's quantities at the valuation date: Just (price amounts,
    -- total value) if all of the row's commodities have a market price,
    -- otherwise Nothing. Without -V/-X/--value, each holding is valued in
    -- its cost commodity when known; with them, in the requested or
    -- default valuation commodity.
    rowValuation :: PeriodicReportRow DisplayName MixedAmount -> Maybe ([Amount], MixedAmount)
    rowValuation r = do
        pvs <- mapM lookup1 qas
        Just (map fst pvs, mixed (map snd pvs))
      where
        -- strip costs so each commodity appears as one amount
        qas = filter (not . amountLooksZero) $ amounts $ mixedAmountStripCosts $ prrTotal r
        mto = case mvalue of
          Nothing -> listToMaybe [acommodity c | (_, mcb) <- lotsUnder (prrFullName r), Just c <- [cbCost =<< mcb]]
          Just _  -> mtargetcomm
        lookup1 qa = do
          (pcomm, rate) <- priceoracle (valuationdate, acommodity qa, mto)
          let mkamt n = styleAmounts styles $ amountSetFullPrecisionUpTo Nothing
                          nullamt{acommodity=pcomm, aquantity=n}
          Just (mkamt rate, mkamt (rate * aquantity qa))

    -- How to convert a row's cost amounts for display, so that the Cost,
    -- Unit/Avg cost and Gain columns follow the valuation commodity when
    -- -V/-X/--value is in effect: convert to the requested commodity, or
    -- to the commodity the row's value came out in, at the valuation date.
    -- Costs already in the target commodity, or with no target or no
    -- market price, are left unchanged.
    rowCostValuer :: PeriodicReportRow DisplayName MixedAmount -> Amount -> Amount
    rowCostValuer r = case mvalue of
      Nothing -> id
      Just _ -> case mtargetcomm <|> mrowvaluecomm of
        Nothing -> id
        -- styleAmounts is reapplied after conversion, since
        -- amountValueAtDate leaves full precision displayed
        Just tc -> \a -> if acommodity a == tc then a
                         else styleAmounts styles $
                              amountValueAtDate priceoracle styles (Just tc) valuationdate a
      where
        mrowvaluecomm = case rowValuation r of
          Just (_, val) | [v] <- amounts val -> Just $ acommodity v
          _ -> Nothing

    -- Render a gain (and percent gain) from single-commodity value and
    -- cost amounts, if their commodities match.
    showgain :: [Amount] -> [Amount] -> T.Text
    showgain [v] [c] | acommodity v == acommodity c =
      T.pack $ showAmountWith noCostFmt{displayZeroCommodity=True} gainamt ++ pct
      where
        gain = aquantity v - aquantity c
        gainamt = styleAmounts styles $ amountSetFullPrecisionUpTo Nothing
                    nullamt{acommodity=acommodity v, aquantity=gain}
        pct | aquantity c /= 0 = printf " (%+.1f%%)" (realToFrac (100 * gain / aquantity c) :: Double)
            | otherwise        = ""
    showgain _ _ = ""

    -- The lots held at or under the given account, excluding empty ones.
    lotsUnder :: AccountName -> [(Amount, Maybe CostBasis)]
    lotsUnder acct =
      [ (a, lotBasis sub) | ((sub, _), a) <- M.toAscList lotmap
      , acct == sub || acct `isAccountNamePrefixOf` sub
      , not $ amountLooksZero a
      ]

    -- Report rows come from a single-period, end-balances multiBalanceReport:
    -- on the lot-detailed journal with --lots (rows are lot subaccounts),
    -- on the collapsed journal otherwise (rows are the base accounts).
    -- Non-holding accounts are filtered out.
    -- Cost conversion and valuation (-B/-V/--value) are disabled:
    -- holdings does its own valuation, and quantities should stay quantities.
    mbr = multiBalanceReport rspec' j'
      where
        rspec' = rspec{_rsReportOpts=ropts{balanceaccum_=Historical, interval_=NoInterval
                                          ,conversionop_=Just NoConversionOp, value_=Nothing}}
        j' = if showlots then j else journalCollapseLotDetail j
    rows = filter keeprow $ prRows mbr
      where
        keeprow r
          | showlots  = isJust $ lotSubaccountName $ prrFullName r
          | otherwise = not $ null $ lotsUnder $ prrFullName r

    tbl = maybe id addtotalrow mtotalrow $ Table
      (Group NoLine $ map (Header . renderacct) rows)
      (Group NoLine $ map Header colheadings)
      (map rowcells rows)
      where
        addtotalrow totalrow tbl' = concatTables SingleLine tbl' $
          Table (Group NoLine [Header ""]) (Header []) [totalrow]
    colheadings = ["Date", "Age", "Quantity", if showlots then "Unit cost" else "Avg cost", "Cost", "Price", "Value", "Gain"]
    renderacct r = T.replicate (prrIndent r * 2) " " <> prrDisplayName r

    rowLotCosts r = [rowCostValuer r $ multiplyAmount (aquantity a) c
                    | (a, mcb) <- lotsUnder $ prrFullName r, Just c <- [cbCost =<< mcb]]

    rowcells r = [datecell, agecell, qtycell, unitcostcell, costcell, pricecell, valuecell, gaincell]
      where
        (pricecell, valuecell, gaincell) = case rowValuation r of
          Nothing -> ("", "", "")
          Just (prices, val) ->
            ( T.intercalate "\n" $ map (T.pack . showAmountWith noCostFmt) prices
            , T.pack $ showMixedAmountWith oneLineNoCostFmt val
            , showgain (amounts val) (amounts $ mixed costs)
            )
        rowlots = lotsUnder $ prrFullName r
        dates = nubSort [cbDate =<< mcb | (_, mcb) <- rowlots]
        -- Date and Age are shown when the row's lots all have the same date.
        (datecell, agecell) = case dates of
          [Just dt] -> (showDate dt, T.pack (show $ diffDays reportdate dt) <> "d")
          _         -> ("", "")
        qtycell = T.pack $ showMixedAmountWith oneLineNoCostFmt $ styleAmounts styles $ prrTotal r
        costs = rowLotCosts r
        costcell = showamts costs
        unitcostcell = case (rowlots, costs) of
          ([(_, mcb)], _) -> maybe "" (T.pack . showAmountWith noCostFmt . rowCostValuer r) (cbCost =<< mcb)
          (_, _:_) | [totcost] <- amounts (mixed costs)
                   , [totqty] <- amounts (mixed $ map fst rowlots)
                   , not $ amountLooksZero totqty
                   -> T.pack $ showAmountWith noCostFmt $ avgcost totqty totcost
          _ -> ""
        -- An average cost: total cost / total quantity, showing significant
        -- decimal digits up to the cost commodity's display precision
        -- (at least 2), without trailing zeros.
        avgcost qtya costa = amountSetPrecision (Precision (min pdiv (max 2 pstyle))) avg
          where
            avg  = divideAmountAndUpdatePrecision (aquantity qtya) costa
            pdiv = case asprecision (astyle avg) of Precision n -> n; _ -> defaultMaxDisplayPrecision
            pstyle = case asprecision (astyle costa) of Precision n -> n; _ -> 2

    -- Grand totals row: the Cost, Value and Gain columns.
    -- Value and Gain are blank unless all rows have a market price.
    mtotalrow
      | no_total_ ropts || length rows < 2 = Nothing
      | otherwise = Just ["", "", "", "", showamts totcosts, "", totvaluecell, totgaincell]
      where
        totcosts = concatMap rowLotCosts rows
        mrowvals = map rowValuation rows
        (totvaluecell, totgaincell) = case sequence mrowvals of
          Nothing -> ("", "")
          Just rowvals -> ( T.pack $ showMixedAmountWith oneLineNoCostFmt totvalue
                          , showgain (amounts totvalue) (amounts $ mixed totcosts))
            where totvalue = mixed $ concatMap (amounts . snd) rowvals

    showamts = T.pack . showMixedAmountWith oneLineNoCostFmt . mixed
