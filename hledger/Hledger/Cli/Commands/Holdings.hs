{-|

The @holdings@ command shows a report of investment holdings (lot-tracked assets).

Work in progress; see doc/SPEC-holdings.md.
Currently it shows the Date, Age, Units, Unit/Avg cost, Cost, Price,
Value, Weight, UGain, UGain%, RGain and XIRR columns, with lot
subaccounts aggregated by default or shown as rows with --lots.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Holdings (
  holdingsmode
 ,holdings
) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Aeson (Value, object, (.=))
import Data.Decimal (roundTo)
import Data.Default (def)
import Data.List.Extra (intercalate, intersperse, nubSort, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Ord (Down(..))
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time.Calendar (Day, addDays, diffDays)
import System.Console.CmdArgs.Explicit (flagNone, flagReq)
import Text.Printf (printf)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands.Balance (addTotalBorders)
import Hledger.Cli.Commands.Print (roundFromRawOpts)
import Hledger.Cli.Utils (unsupportedOutputFormatError, writeOutputLazyText)
import Hledger.Write.Csv (CSV, printCSV, printTSV)
import Hledger.Write.Html (Html, htmlAsLazyText, styledTableHtml, toHtml)
import Hledger.Write.Ods (printFods)
import Hledger.Write.Spreadsheet (addHeaderBorders, headerCell)
import Hledger.Write.Spreadsheet qualified as Ods
import Lucid qualified as L
import Numeric.RootFinding (RiddersParam(..), Root(..), Tolerance(..), ridders)
import System.IO qualified as IO
import Text.Tabular.AsciiWide

-- | Command line options for this command.
holdingsmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Holdings.txt")
  (flattreeflags True ++
   [flagNone ["no-elide"] (setboolopt "no-elide") "in tree mode, don't squash boring parent accounts"
   ,flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "in list mode, omit N leading account name parts"
   ,flagNone ["sort-amount","S"] (setboolopt "sort-amount") "sort by value (or cost) instead of account name, largest first"
   ,flagNone ["no-total","N"] (setboolopt "no-total") "omit the final total row"
   ,flagReq ["round"] (\s opts -> Right $ setopt "round" s opts) "TYPE" $
     intercalate "\n"
     ["how much rounding or padding should be done when displaying amounts ?"
     ,"none - show original decimal digits"
     ,"soft - just add or remove decimal zeros"
     ,"       to match precision"
     ,"hard - round amounts to precision (default)"
     ,"all  - also round cost amounts to precision"
     ]
   ,outputFormatFlag ["txt","csv","tsv","html","fods","json"]
   ,outputFileFlag])
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | One holding: a displayed report row and commodity,
-- as machine-readable data for the csv/tsv/json output.
-- Money amounts are display strings (in machine format: no digit group
-- marks); dates, ages, units and gain percents are typed.
data Holding = Holding {
   hAccount   :: AccountName
  ,hCommodity :: CommoditySymbol
  ,hDate      :: Maybe Day        -- ^ acquisition date, when the lots share one
  ,hAge       :: Maybe Integer    -- ^ days held at the report date
  ,hUnits     :: Amount           -- ^ units held, styled
  ,hUnitCost  :: Maybe T.Text     -- ^ unit or average cost
  ,hCost      :: T.Text           -- ^ total cost basis
  ,hPrice     :: Maybe T.Text     -- ^ market price at the valuation date
  ,hValue     :: Maybe T.Text     -- ^ market value
  ,hWeight    :: Maybe Quantity   -- ^ percentage of the portfolio's value, rounded to 1 decimal
  ,hUgain     :: Maybe T.Text     -- ^ unrealised gain
  ,hUgainPct  :: Maybe Quantity   -- ^ unrealised gain percent, rounded to 1 decimal
  ,hRgain     :: Maybe T.Text     -- ^ realised gain, from disposals so far
  ,hXirr      :: Maybe Double     -- ^ annualised internal rate of return percent
  }

holdingCsv :: Holding -> [T.Text]
holdingCsv h =
  [hAccount h
  ,hCommodity h
  ,maybe "" showDate (hDate h)
  ,maybe "" (T.pack . show) (hAge h)
  ,T.pack $ showAmountWith machineFmt{displayCommodity=False} (hUnits h)
  ,fromMaybe "" (hUnitCost h)
  ,hCost h
  ,fromMaybe "" (hPrice h)
  ,fromMaybe "" (hValue h)
  ,maybe "" (T.pack . show) (hWeight h)
  ,fromMaybe "" (hUgain h)
  ,maybe "" (T.pack . show) (hUgainPct h)
  ,fromMaybe "" (hRgain h)
  ,maybe "" (T.pack . printf "%.1f") (hXirr h)
  ]

holdingJson :: Holding -> Value
holdingJson h = object
  ["account"   .= hAccount h
  ,"commodity" .= hCommodity h
  ,"date"      .= hDate h
  ,"age"       .= hAge h
  ,"units"     .= aquantity (hUnits h)
  ,"unitcost"  .= hUnitCost h
  ,"cost"      .= hCost h
  ,"price"     .= hPrice h
  ,"value"     .= hValue h
  ,"weight"    .= hWeight h
  ,"ugain"     .= hUgain h
  ,"ugainpct"  .= hUgainPct h
  ,"rgain"     .= hRgain h
  ,"xirr"      .= hXirr h
  ]

-- | Show an age in days compactly: in days, or if a year or more,
-- in years with one decimal digit (approximating years as 365 days):
-- eg 44d, 1.1y.
showage :: Integer -> T.Text
showage d
  | d >= 365  = T.pack (show (roundTo 1 (fromIntegral d / 365))) <> "y"
  | otherwise = T.pack (show d) <> "d"

-- | Show the holdings report: the assets held in lot-tracked accounts
-- as of the report end date, one row per account (or per lot, with --lots).
--
-- This command receives the journal with lot detail (lot subaccounts and
-- synthetic postings) uncollapsed, regardless of --lots
-- (see maybeCollapseLotDetail); it aggregates lots itself.
holdings :: CliOpts -> Journal -> IO ()
holdings opts@CliOpts{rawopts_=rawopts, reportspec_=rspec@ReportSpec{_rsQuery=q, _rsReportOpts=ropts}} j = do
  if (case mvalue of Just (AtThen _) -> True; _ -> False)
  then error' "holdings: --value=then is not supported"
  else rounding `seq`  -- validate the --round value before any output
    writeOutputLazyText opts $ case outputFormatFromOpts opts of
      "txt"  -> txtoutput
      "csv"  -> printCSV csvoutput
      "tsv"  -> printTSV csvoutput
      "html" -> (<>"\n") $ htmlAsLazyText $ styledTableHtml htmltable
      "fods" -> printFods IO.localeEncoding $ M.singleton "Holdings" ((1,0), fodstable)
      "json" -> (<>"\n") $ toJsonText $ map holdingJson holdingrecords
      fmt    -> error' $ unsupportedOutputFormatError fmt
  where
    txtoutput =
      "Holdings on " <> TL.fromStrict (showDate reportdate) <> "\n\n" <>
      if null rows
      then "(no holdings)\n"
      else renderTable
        def{tableBorders=False}
        (textCell TopLeft)
        (textCell TopRight)
        (textCell TopRight)
        tbl
    showlots = boolopt "lots" rawopts
    tree = accountlistmode_ ropts == ALTree

    -- The date this report shows holdings at: the day before the (exclusive)
    -- report end date if specified, otherwise today.
    mend = queryEndDate False q
    reportdate = maybe (_rsDay rspec) (addDays (-1)) mend

    -- The query used to select lot subaccount postings: the report query
    -- without its date terms (holdings are cumulative to the end date,
    -- added here) and depth terms (--depth only clips the displayed rows;
    -- the lots beneath still count).
    endq = And [filterQuery (\x -> not $ queryIsDateOrDate2 x || queryIsDepth x) q
               ,Date $ DateSpan Nothing (Exact <$> mend)]

    -- The postings contributing to each lot subaccount, keyed by account
    -- and commodity (so amounts in different commodities, not expected in
    -- a lot subaccount but possible, don't merge wrongly).
    lotpostings :: [((AccountName, CommoditySymbol), (Day, Amount))]
    lotpostings =
      [ ((paccount p, acommodity a), (postingDate p, a))
      | p <- journalPostings j
      , isJust $ lotSubaccountName $ paccount p
      , endq `matchesPosting` p
      , a <- amountsRaw $ pamount p
      ]

    -- The units held in each lot subaccount.
    lotmap :: M.Map (AccountName, CommoditySymbol) Amount
    lotmap = M.fromListWith (+) [(k, amountStripCost a) | (k, (_, a)) <- lotpostings]

    -- Each lot subaccount's cashflows in the cost commodity, for XIRR:
    -- each posting's transacted cost if any (so, proceeds when disposing),
    -- otherwise its cost basis value. Negative = money invested.
    flowmap :: M.Map (AccountName, CommoditySymbol) [(Day, Amount)]
    flowmap = M.fromListWith (++)
      [ (k, [(d, negate flowamt)])
      | (k, (d, a)) <- lotpostings
      , Just flowamt <- [case acost a of
          Just _  -> Just $ amountCost a
          Nothing -> multiplyAmount (aquantity a) <$> (cbCost =<< acostbasis a)]
      ]

    -- Each lot subaccount's realised gains, in the cost commodity:
    -- for each dispose posting (negative, with a transacted price and a
    -- cost basis), the proceeds minus the cost basis of the disposed units.
    rgainmap :: M.Map (AccountName, CommoditySymbol) Amount
    rgainmap = M.fromListWith (+)
      [ (k, proceeds - basis)
      | (k, (_, a)) <- lotpostings
      , aquantity a < 0
      , isJust $ acost a
      , let proceeds = negate $ amountCost a
      , Just ub <- [cbCost =<< acostbasis a]
      , let basis = multiplyAmount (negate $ aquantity a) ub
      , acommodity proceeds == acommodity basis
      ]

    -- The values in a map whose keys are at or under the given account
    -- (and in the given held commodity, if specified).
    underIn :: M.Map (AccountName, CommoditySymbol) v -> AccountName -> Maybe CommoditySymbol -> [v]
    underIn m acct mc =
      [ v | ((sub, c), v) <- M.toAscList m
      , acct == sub || acct `isAccountNamePrefixOf` sub
      , maybe True (== c) mc
      ]

    -- The cashflows of the lots at or under an account, optionally of one held commodity.
    flowsUnder :: AccountName -> Maybe CommoditySymbol -> [(Day, Amount)]
    flowsUnder acct mc = concat $ underIn flowmap acct mc

    -- The realised gains of the lots at or under an account, optionally of one held commodity.
    rgainsUnder :: AccountName -> Maybe CommoditySymbol -> [Amount]
    rgainsUnder = underIn rgainmap

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

    -- Value a row's units at the valuation date: Just (price amounts,
    -- total value) if all of the row's commodities have a market price,
    -- otherwise Nothing. Without -V/-X/--value, each holding is valued in
    -- its cost commodity when known; with them, in the requested or
    -- default valuation commodity.
    rowValuation :: PeriodicReportRow DisplayName MixedAmount -> Maybe ([Amount], MixedAmount)
    rowValuation r = do
        pvs <- mapM lookup1 qas
        Just (map fst pvs, mixed (map snd pvs))
      where
        qas = rowUnitAmounts r
        mto = case mvalue of
          Nothing -> listToMaybe [acommodity c | (_, mcb) <- lotsUnder (prrFullName r), Just c <- [cbCost =<< mcb]]
          Just _  -> mtargetcomm
        lookup1 qa = do
          (pcomm, rate) <- priceoracle (valuationdate, acommodity qa, mto)
          let mkamt n = styleAmounts styles $ amountSetFullPrecisionUpTo Nothing
                          nullamt{acommodity=pcomm, aquantity=n}
          Just (mkamt rate, mkamt (rate * aquantity qa))

    -- How to convert cost amounts (Cost, Unit/Avg cost, RGain, and the
    -- cost side of UGain) for display when -V/-X/--value is in effect:
    -- convert to the requested commodity, or to the given fallback
    -- commodity (the row's or portfolio's value commodity), at the
    -- valuation date. Costs already in the target commodity, or with no
    -- target or no market price, are left unchanged.
    costValuerTo :: Maybe CommoditySymbol -> Amount -> Amount
    costValuerTo mfallback = case mvalue of
      Nothing -> id
      Just _ -> case mtargetcomm <|> mfallback of
        Nothing -> id
        -- styleAmounts is reapplied after conversion, since
        -- amountValueAtDate leaves full precision displayed
        Just tc -> \a -> if acommodity a == tc then a
                         else styleAmounts styles $
                              amountValueAtDate priceoracle styles (Just tc) valuationdate a

    rowCostValuer :: PeriodicReportRow DisplayName MixedAmount -> Amount -> Amount
    rowCostValuer r = costValuerTo mrowvaluecomm
      where
        mrowvaluecomm = case rowValuation r of
          Just (_, val) | [v] <- amounts val -> Just $ acommodity v
          _ -> Nothing

    -- The annualised internal rate of return implied by dated cashflows
    -- (negative = money invested) up to the report date, as a percentage,
    -- calculated like roi's IRR. Nothing if it can not be solved.
    -- Note: this duplicates the solver setup and rate convention of
    -- Roi.hs's solveIRR/interestSum (not exported); keep them in sync,
    -- or extract a shared helper.
    xirrPct :: [(Day, Quantity)] -> Maybe Double
    xirrPct cf =
      case ridders (RiddersParam 100 (AbsTol 0.00001)) (0.000000000001, 10000) npv of
        Root rate -> Just $ (rate - 1) * 100
        _         -> Nothing
      where
        npv rate = sum [realToFrac n * rate ** (fromIntegral (diffDays reportdate t) / 365.25) | (t, n) <- cf]

    -- XIRR from cashflows plus a final value amount at the report date,
    -- when they are all in one commodity.
    xirrOf :: [(Day, Amount)] -> Amount -> Maybe Double
    xirrOf flows finalv = do
      guard $ not $ null flows
      guard $ all ((== acommodity finalv) . acommodity . snd) flows
      xirrPct $ (reportdate, aquantity finalv) : [(d, aquantity a) | (d, a) <- flows]

    -- The total value of the displayed holdings, when all are priced;
    -- and its commodity, when it has just one.
    mportfoliovalue :: Maybe MixedAmount
    mportfoliovalue = do
      rowvals <- traverse rowValuation toprows
      Just $ mixed $ concatMap (amounts . snd) rowvals
    mportvaluecomm = case amounts <$> mportfoliovalue of
      Just [v] -> Just $ acommodity v
      _        -> Nothing

    -- The distinct base accounts of the displayed rows (excluding any
    -- contained in another). Account-level totals (RGain, XIRR) are
    -- computed from these, so that they include fully disposed lots,
    -- which have no displayed row of their own (eg with --lots).
    topbases :: [AccountName]
    topbases = [ b | b <- bases, not $ any (`isAccountNamePrefixOf` b) bases ]
      where bases = nubSort $ map (lotBaseAccount . prrFullName) toprows

    -- A value's percentage of the portfolio's total value, when both are
    -- single amounts in the same commodity.
    weightPct :: MixedAmount -> Maybe Quantity
    weightPct val = do
      tot <- mportfoliovalue
      [t] <- Just $ amounts tot
      [v] <- Just $ amounts val
      guard $ acommodity v == acommodity t && aquantity t /= 0
      Just $ 100 * aquantity v / aquantity t

    -- The commodity display styles, plus a default style for the "%"
    -- commodity if none is declared or inferred: one decimal digit,
    -- and the % sign on the right with no space.
    pctstyles = M.union styles $ M.singleton "%" $
      amountstyle{ascommodityside=R, asprecision=Precision 1, asrounding=HardRounding}

    -- Show a percentage (for the Weight, UGain% and XIRR columns) as a
    -- "%" commodity amount, using the display style of "%" (eg from a
    -- commodity directive or -c) or the default above: eg 64.3%.
    showpct :: Quantity -> T.Text
    showpct p =
      T.pack $ showAmountWith noCostFmt{displayZeroCommodity=True} $
      styleAmounts pctstyles nullamt{acommodity="%", aquantity=p}

    -- Show an XIRR percentage, like showpct: eg 12.3%.
    showxirr :: Double -> T.Text
    showxirr = showpct . realToFrac

    -- Render a gain amount and percent gain, as separate texts, from
    -- single-commodity value and cost amounts, if their commodities match.
    showgain :: [Amount] -> [Amount] -> (T.Text, T.Text)
    showgain [v] [c] | acommodity v == acommodity c =
      (T.pack $ showAmountWith noCostFmt{displayZeroCommodity=True} gainamt, pct)
      where
        gain = aquantity v - aquantity c
        gainamt = styleAmounts styles $ amountSetFullPrecisionUpTo Nothing
                    nullamt{acommodity=acommodity v, aquantity=gain}
        pct | aquantity c /= 0 = showpct $ 100 * gain / aquantity c
            | otherwise        = ""
    showgain _ _ = ("", "")

    -- A row's units of lot-tracked commodities: its balance restricted
    -- to the commodities of the lots at or beneath it. This excludes other
    -- commodities (eg cash) from parent account rows in tree mode, and
    -- strips costs so each commodity appears as one amount.
    rowUnitAmounts :: PeriodicReportRow DisplayName MixedAmount -> [Amount]
    rowUnitAmounts r =
      filter (\a -> acommodity a `elem` lotcomms && not (amountLooksZero a)) $
      amounts $ mixedAmountStripCosts $ prrTotal r
      where lotcomms = [acommodity a | (a, _) <- lotsUnder $ prrFullName r]

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
    -- holdings does its own valuation, and units should stay units.
    mbr = multiBalanceReport rspec' j'
      where
        rspec' = rspec{_rsReportOpts=ropts{balanceaccum_=Historical, interval_=NoInterval
                                          ,conversionop_=Just NoConversionOp, value_=Nothing
                                          ,sort_amount_=False}}  -- -S sorts by value/cost below, not by units
        j' = if showlots then j else journalCollapseLotDetail j
    -- Rows to display: those with lots at or beneath them. In list mode,
    -- also drop rows whose lots all appear in a deeper displayed row
    -- (eg a base account posted to directly, when its lot subaccounts
    -- are shown); in tree mode such parent rows are wanted.
    rows = filter keeprow candidates
      where
        candidates = filter (not . null . lotsUnder . prrFullName) $ prRows mbr
        keeprow r = tree ||
          not (any (\r2 -> prrFullName r `isAccountNamePrefixOf` prrFullName r2) candidates)

    -- The topmost displayed rows: those not contained in another displayed
    -- row. Totals are computed from these, to avoid double counting.
    toprows = [ r | r <- rows
              , not $ any (\r2 -> prrFullName r2 `isAccountNamePrefixOf` prrFullName r) rows ]

    -- The display order: by account name, or with -S by each row's Value
    -- (falling back to Cost), largest first. Sorting compares the keys
    -- along each row's chain of displayed ancestors, so in tree mode each
    -- level is sorted and subtrees stay together. Ties (and rows mixing
    -- commodities, which are summed crudely) keep the account name order.
    sortedrows
      | not $ sort_amount_ ropts = rows
      | otherwise = sortOn keypath rows
      where
        keymap = M.fromList [(prrFullName r, Down $ rowSortKey r) | r <- rows]
        keypath r = mapMaybe (`M.lookup` keymap) $ reverse (parentAccountNames a) ++ [a]
          where a = prrFullName r
        rowSortKey r = case rowValuation r of
          Just (_, val) -> sumq val
          Nothing       -> sumq $ mixed $ rowLotCosts r
          where sumq = sum . map aquantity . amounts

    tbl = maybe id addtotalrow (map (T.intercalate ", ") <$> mtotalrowparts) $ Table
      (Group NoLine $ map (Header . renderacct) sortedrows)
      (Group NoLine $ map Header colheadings)
      (map rowcells sortedrows)
      where
        addtotalrow totalrow tbl' = concatTables SingleLine tbl' $
          Table (Group NoLine [Header ""]) (Header []) [totalrow]
    colheadings = ["Date", "Age", "Units", if showlots then "Unit cost" else "Avg cost", "Cost", "Price", "Value", "Weight", "UGain", "UGain%", "RGain", "XIRR"]
    renderacct r = T.replicate (prrIndent r * 2) " " <> prrDisplayName r

    rowLotCosts r = [rowCostValuer r $ multiplyAmount (aquantity a) c
                    | (a, mcb) <- lotsUnder $ prrFullName r, Just c <- [cbCost =<< mcb]]

    -- The text table's cells: each cell's parts joined,
    -- multi-line in Units and Price, one-line elsewhere.
    rowcells = zipWith T.intercalate cellseps . rowCellParts
    cellseps = [", ", ", ", "\n", ", ", ", ", "\n", ", ", ", ", ", ", ", ", ", ", ", "]

    -- A row's cells, each as a list of parts:
    -- one part per commodity amount in the amount cells, at most one part elsewhere.
    rowCellParts :: PeriodicReportRow DisplayName MixedAmount -> [[T.Text]]
    rowCellParts r = [[datecell], [agecell], unitparts, [unitcostcell], costparts, priceparts, valueparts, [weightcell], [ugaincell], [ugainpctcell], rgainparts, [xirrcell]]
      where
        acct = prrFullName r
        (priceparts, valueparts, (ugaincell, ugainpctcell), weightcell) = case rowValuation r of
          Nothing -> ([], [], ("", ""), "")
          Just (prices, val) ->
            ( map showamt prices
            , map showamt $ amounts val
            , showgain (amounts val) (amounts $ mixed costs)
            , maybe "" showpct $ weightPct val
            )
        rgainparts = case map (rowCostValuer r) $ rgainsUnder acct Nothing of
          [] -> []
          rs -> map showamt $ amounts $ mixed rs
        xirrcell = fromMaybe "" $ do
          (_, val) <- rowValuation r
          [v] <- Just $ amounts val
          showxirr <$> xirrOf (flowsUnder acct Nothing) v
        rowlots = lotsUnder acct
        dates = nubSort [cbDate =<< mcb | (_, mcb) <- rowlots]
        -- Date and Age are shown when the row's lots all have the same date.
        (datecell, agecell) = case dates of
          [Just dt] -> (showDate dt, showage $ diffDays reportdate dt)
          _         -> ("", "")
        unitparts = map (showamt . styleAmounts styles) $ rowUnitAmounts r
        costs = rowLotCosts r
        costparts = map showamt $ amounts $ mixed costs
        unitcostcell = case (rowlots, costs) of
          ([(_, mcb)], _) -> maybe "" (showamt . rowCostValuer r) (cbCost =<< mcb)
          (_, _:_) | [totcost] <- amounts (mixed costs)
                   , [totqty] <- amounts (mixed $ map fst rowlots)
                   , not $ amountLooksZero totqty
                   -> showamt $ avgcost totqty totcost
          _ -> ""
        showamt = T.pack . showAmountWith noCostFmt

    -- Spreadsheet-shaped tables for the html and fods output: like the
    -- text table, but with single-line cells, an Account column heading,
    -- and a Total: row heading. Parameterised on how to convert plain
    -- text, and a cell's list of (possibly amount) parts, to content.
    spreadsheetWith :: forall content. (T.Text -> content) -> (Bool -> [T.Text] -> content)
                    -> [[Ods.Cell Ods.NumLines content]]
    spreadsheetWith plain parts =
      addHeaderBorders (zipWith hcell colclasses ("Account" : colheadings))
      : [ zipWith3 bodycell [0..] colclasses
            (plain (acctcell r) : zipWith parts amountcols (rowCellParts r))
        | r <- sortedrows ]
      ++ maybe [] (\tot -> addTotalBorders
           [zipWith3 totalcell [0..] colclasses
              (plain "Total:" : zipWith parts amountcols tot) :: [Ods.Cell () content]])
           mtotalrowparts
      where
        -- per-column css classes, so the html cells can be styled
        colclasses = ["account","date","age","units","unitcost","cost","price","value","weight","ugain","ugainpct","rgain","xirr"]
        -- which of the other columns' cell parts are amounts
        amountcols = [False, False, True, True, True, True, True, False, True, False, True, False]
        hcell cls t = plain <$> (headerCell t){Ods.cellClass = Ods.Class cls}
        -- body cells are right-aligned, except the first two columns
        -- (Account and Date); headings are unaffected
        bodycell :: Ods.Lines border => Int -> T.Text -> content' -> Ods.Cell border content'
        bodycell i cls t = (Ods.defaultCell t)
          {Ods.cellType = if i < 2 then Ods.TypeString else Ods.TypeMixedAmount
          ,Ods.cellClass = Ods.Class cls}
        totalcell i cls = bodycell i (cls <> " coltotal")
        -- indent tree-mode account names with no-break spaces
        acctcell r = T.replicate (prrIndent r * 2) "\160" <> prrDisplayName r

    -- Each commodity amount gets its own span with an "amount" class,
    -- so eg wrapping within amounts can be prevented with css.
    htmltable :: [[Ods.Cell Ods.NumLines Html]]
    htmltable = spreadsheetWith toHtml partsHtml
      where
        partsHtml isamount parts =
          mconcat $ intersperse (toHtml (", "::T.Text)) $
          map (\p -> if isamount then L.span_ [L.class_ "amount"] (toHtml p) else toHtml p) $
          filter (not . T.null) parts

    fodstable :: [[Ods.Cell Ods.NumLines T.Text]]
    fodstable = spreadsheetWith id (\_ -> T.intercalate ", " . filter (not . T.null))

    -- Machine-readable records, one per displayed row and commodity,
    -- for the csv/tsv/json output: with full account names, age in days,
    -- bare units and gain percent numbers, and gain and gain percent
    -- separate. No totals records.
    holdingrecords :: [Holding]
    holdingrecords = concatMap rowrecords sortedrows
      where
        rowrecords r = map rec $ rowUnitAmounts r
          where
            acct = prrFullName r
            rec qa = Holding
              { hAccount   = acct
              , hCommodity = c
              , hDate      = mdate
              , hAge       = diffDays reportdate <$> mdate
              , hUnits     = styleAmounts styles qa
              , hUnitCost  = mucoststr
              , hCost      = coststr
              , hPrice     = mpricestr
              , hValue     = mvalstr
              , hWeight    = mweight
              , hUgain     = mgainstr
              , hUgainPct  = mpct
              , hRgain     = mrgainstr
              , hXirr      = mxirr
              }
              where
                c = acommodity qa
                showamt  = T.pack . showAmountWith machineFmt
                showamts' = T.pack . showMixedAmountWith machineFmt . mixed
                clots = filter ((==c) . acommodity . fst) $ lotsUnder acct
                dates = nubSort [cbDate =<< mcb | (_, mcb) <- clots]
                mdate = case dates of
                  [Just dt] -> Just dt
                  _         -> Nothing
                ccosts = [rowCostValuer r $ multiplyAmount (aquantity a) cb | (a, mcb) <- clots, Just cb <- [cbCost =<< mcb]]
                coststr = showamts' ccosts
                mucoststr = case (clots, ccosts) of
                  ([(_, mcb)], _) -> showamt . rowCostValuer r <$> (cbCost =<< mcb)
                  (_, _:_) | [totcost] <- amounts (mixed ccosts)
                           , not $ amountLooksZero qa
                           -> Just $ showamt $ avgcost qa totcost
                  _ -> Nothing
                mto = case mvalue of
                  Nothing -> listToMaybe [acommodity cb | (_, mcb) <- clots, Just cb <- [cbCost =<< mcb]]
                  Just _  -> mtargetcomm
                mrgainstr = case map (rowCostValuer r) $ rgainsUnder acct (Just c) of
                  [] -> Nothing
                  rs -> Just $ showamts' rs
                (mpricestr, mvalstr, mgainstr, mpct, mweight, mxirr) =
                  case priceoracle (valuationdate, c, mto) of
                    Nothing -> (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                    Just (pcomm, rate) -> (Just $ showamt price, Just $ showamt val, mgainstr', mpct'
                                          ,roundTo 1 <$> weightPct (mixedAmount val)
                                          ,xirrOf (flowsUnder acct (Just c)) val)
                      where
                        mkamt n = styleAmounts styles $ amountSetFullPrecisionUpTo Nothing
                                    nullamt{acommodity=pcomm, aquantity=n}
                        price = mkamt rate
                        val   = mkamt (rate * aquantity qa)
                        (mgainstr', mpct') = case amounts (mixed ccosts) of
                          [costamt] | acommodity costamt == pcomm ->
                            ( Just $ showamt $ mkamt gainq
                            , if aquantity costamt /= 0
                              then Just $ roundTo 1 $ 100 * gainq / aquantity costamt
                              else Nothing )
                            where gainq = aquantity val - aquantity costamt
                          _ -> (Nothing, Nothing)

    csvoutput :: CSV
    csvoutput =
      ["account","commodity","date","age","units","unitcost","cost","price","value","weight","ugain","ugainpct","rgain","xirr"]
      : map holdingCsv holdingrecords

    -- Grand totals row (as cell parts, like rowCellParts): the Cost,
    -- Value and gain columns, summed over the topmost displayed rows
    -- (which include everything below them).
    -- Value and gains are blank unless all rows have a market price.
    mtotalrowparts :: Maybe [[T.Text]]
    mtotalrowparts
      | no_total_ ropts = Nothing
      | otherwise = Just [[], [], [], [], costparts, [], valueparts, [weightcell], [ugaincell], [ugainpctcell], rgainparts, [xirrcell]]
      where
        totcosts = concatMap rowLotCosts toprows
        costparts = map showamt $ amounts $ mixed totcosts
        (valueparts, weightcell, (ugaincell, ugainpctcell)) = case mportfoliovalue of
          Nothing -> ([], "", ("", ""))
          Just totvalue -> ( map showamt $ amounts totvalue
                           , maybe "" showpct $ weightPct totvalue
                           , showgain (amounts totvalue) (amounts $ mixed totcosts))
        rgainparts = case map (costValuerTo mportvaluecomm) $ concatMap (\b -> rgainsUnder b Nothing) topbases of
          [] -> []
          rs -> map showamt $ amounts $ mixed rs
        xirrcell = fromMaybe "" $ do
          totvalue <- mportfoliovalue
          [tv] <- Just $ amounts totvalue
          showxirr <$> xirrOf (concatMap (\b -> flowsUnder b Nothing) topbases) tv
        showamt = T.pack . showAmountWith noCostFmt

    -- An average cost: total cost / total units, showing significant
    -- decimal digits up to the cost commodity's display precision
    -- (at least 2), without trailing zeros.
    avgcost qtya costa = amountSetPrecision (Precision (min pdiv (max 2 pstyle))) avg
      where
        avg  = divideAmountAndUpdatePrecision (aquantity qtya) costa
        pdiv = case asprecision (astyle avg) of Precision n -> n; _ -> defaultMaxDisplayPrecision
        pstyle = case asprecision (astyle costa) of Precision n -> n; _ -> 2
