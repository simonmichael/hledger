{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-|

This module is used by the 'balancesheet', 'incomestatement', and
'cashflow' commands to print out acocunt balances based on a specific
"view", which consists of a title and multiple named queries that are
aggregated and totaled.

-}

module Hledger.Cli.BalanceView (
  BalanceView(..)
 ,balanceviewmode
 ,balanceviewReport
) where

import Control.Monad (unless)
import Data.List (intercalate, foldl', isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..), (<>))
import System.Console.CmdArgs.Explicit as C
import Text.Tabular as T

import Hledger
import Hledger.Cli.Balance
import Hledger.Cli.CliOptions

-- | Describes a view for the balance, which can consist of multiple
-- separate named queries that are aggregated and totaled.
data BalanceView = BalanceView {
      bvmode     :: String,                        -- ^ command line mode of the view
      bvaliases  :: [String],                      -- ^ command line aliases
      bvhelp     :: String,                        -- ^ command line help message
      bvtitle    :: String,                        -- ^ title of the view
      bvqueries  :: [(String, Journal -> Query)],  -- ^ named queries that make up the view
      bvdeftype  :: BalanceType,                   -- ^ the type of balance this view shows.
      bvtypes    :: [BalanceType]                  -- ^ alternatively allowed types
}

balanceviewmode :: BalanceView -> Mode RawOpts
balanceviewmode BalanceView{..} = (defCommandMode $ bvmode : bvaliases) {
  modeHelp = bvhelp `withAliases` bvaliases
 ,modeGroupFlags = C.Group {
     groupUnnamed = typeFlag True bvdeftype : map (typeFlag False) bvtypes ++ [
      flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show accounts as a list"
     ,flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "flat mode: omit N leading account name parts"
     ,flagNone ["no-total","N"] (\opts -> setboolopt "no-total" opts) "omit the final total row"
     ,flagNone ["tree"] (\opts -> setboolopt "tree" opts) "show accounts as a tree; amounts include subaccounts (default in simple reports)"
     ,flagNone ["average","A"] (\opts -> setboolopt "average" opts) "show a row average column (in multicolumn reports)"
     ,flagNone ["row-total","T"] (\opts -> setboolopt "row-total" opts) "show a row total column (in multicolumn reports)"
     ,flagNone ["no-elide"] (\opts -> setboolopt "no-elide" opts) "don't squash boring parent accounts (in tree mode)"
     ,flagReq  ["format"] (\s opts -> Right $ setopt "format" s opts) "FORMATSTR" "use this custom line format (in simple reports)"
     ,flagNone ["pretty-tables"] (\opts -> setboolopt "pretty-tables" opts) "use unicode when displaying tables"
     ]
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }
 where
   typeFlag :: Bool -> BalanceType -> Flag RawOpts
   typeFlag dt bt = case bt of
       PeriodChange ->
         flagNone ["change"] (\opts -> setboolopt "change" opts)
           ("show balance change in each period" ++ isDef)
       HistoricalBalance ->
         flagNone ["historical","H"] (\opts -> setboolopt "historical" opts)
           ("show historical ending balance in each period (includes postings before report start date)"
               ++ isDef
           )
       CumulativeChange ->
         flagNone ["cumulative"] (\opts -> setboolopt "cumulative" opts)
           ("show balance change accumulated across periods (in multicolumn reports)"
               ++ isDef
           )
     where
       isDef | dt        = " (default)"
             | otherwise = ""

balanceviewQueryReport
    :: ReportOpts
    -> Query
    -> Journal
    -> String
    -> (Journal -> Query)
    -> ([String], Sum MixedAmount)
balanceviewQueryReport ropts q0 j t q = ([view], Sum amt)
    where
      q' = And [q0, q j]
      rep@(_ , amt)
        -- For --historical/--cumulative, we must use multiBalanceReport.
        -- (This forces --no-elide.)
        -- See Balance.hs's implementation of 'balance' for more information
        | balancetype_ ropts `elem` [HistoricalBalance, CumulativeChange]
            = singleBalanceReport ropts q' j
        | otherwise
            = balanceReport ropts q' j
      view = intercalate "\n" [t <> ":", balanceReportAsText ropts rep]

multiBalanceviewQueryReport
    :: ReportOpts
    -> Query
    -> Journal
    -> String
    -> (Journal -> Query)
    -> ([Table String String MixedAmount], [[MixedAmount]], Sum MixedAmount)
multiBalanceviewQueryReport ropts q0 j t q = ([tabl], [coltotals], Sum tot)
    where
      singlesection = "Cash" `isPrefixOf` t -- TODO temp
      ropts' = ropts { no_total_ = singlesection && no_total_ ropts, empty_ = True }
      q' = And [q0, q j]
      MultiBalanceReport (dates, rows, (coltotals,tot,avg)) =
          multiBalanceReport ropts' q' j
      rows' | empty_ ropts = rows
            | otherwise    = filter (not . emptyRow) rows
        where
          emptyRow (_,_,_,amts,_,_) = all isZeroMixedAmount amts
      r = MultiBalanceReport (dates, rows', (coltotals, tot, avg))
      Table hLeft hTop dat = balanceReportAsTable ropts' r
      tabl = Table (T.Group SingleLine [Header t, hLeft]) hTop ([]:dat)

-- | Prints out a balance report according to a given view
balanceviewReport :: BalanceView -> CliOpts -> Journal -> IO ()
balanceviewReport BalanceView{..} CliOpts{command_=cmd, reportopts_=ropts, rawopts_=raw} j = do
    currDay   <- getCurrentDay
    let q0 = queryFromOpts currDay ropts'
    let title = bvtitle ++ maybe "" (' ':) balanceclarification
    case interval_ ropts' of
      NoInterval -> do
        let (views, amt) =
              foldMap (uncurry (balanceviewQueryReport ropts' q0 j))
                 bvqueries
        mapM_ putStrLn (title : "" : views)

        unless (no_total_ ropts' || cmd=="cashflow") . mapM_ putStrLn $ -- TODO temp
          [ "Total:"
          , "--------------------"
          , padLeftWide 20 $ showamt (getSum amt)
          ]
      _ -> do
        let (tabls, amts, Sum totsum)
              = foldMap (uncurry (multiBalanceviewQueryReport ropts' q0 j)) bvqueries
            sumAmts = case amts of
              a1:as -> foldl' (zipWith (+)) a1 as
              []    -> []
            totavg = totsum `divideMixedAmount` fromIntegral (length sumAmts)
            mergedTabl = case tabls of
              t1:ts -> foldl' merging t1 ts
              []    -> T.empty
            totTabl
              | no_total_ ropts' || length bvqueries == 1 =
                  mergedTabl
              | otherwise =
                  mergedTabl
                  +====+
                  row "Total"
                      (sumAmts ++ (if row_total_ ropts' && not (null sumAmts) then [totsum] else [])
                               ++ (if average_ ropts' && not (null sumAmts)   then [totavg] else [])
                      )
        putStrLn title
        putStrLn $ renderBalanceReportTable ropts totTabl
  where
    showamt | color_ ropts = cshowMixedAmountWithoutPrice
            | otherwise    = showMixedAmountWithoutPrice
    overwriteBalanceType =
      case reverse $ filter (`elem` map typeCmd bvtypes) $ map fst raw of
        "historical":_ -> Just HistoricalBalance
        "cumulative":_ -> Just CumulativeChange
        "change":_     -> Just PeriodChange
        _              -> Nothing
    balancetype = fromMaybe bvdeftype overwriteBalanceType
    -- we must clarify that the statements aren't actual income statements,
    -- etc. if the user overrides the balance type
    balanceclarification = flip fmap overwriteBalanceType $ \t ->
      case t of
        PeriodChange      -> "(Balance Changes)"
        CumulativeChange  -> "(Cumulative Ending Balances)"
        HistoricalBalance -> "(Historical Ending Balances)"
    ropts' = treeIfNotPeriod $ ropts { balancetype_ = balancetype }
        -- For --historical/--cumulative, we must use multiBalanceReport.
        -- (This forces --no-elide.)
        -- These settings format the output in a way that we can convert to
        -- a normal balance report using singleBalanceReport.  See
        -- Balance.hs for more information.
    treeIfNotPeriod
      | flat_ ropts = id
      | otherwise   = case (balancetype, interval_ ropts) of
          (HistoricalBalance, NoInterval) -> \o ->
              o { accountlistmode_ = ALTree }
          (CumulativeChange , NoInterval) -> \o ->
              o { accountlistmode_ = ALTree }
          _                               -> id
    merging (Table hLeft hTop dat) (Table hLeft' _ dat') =
        Table (T.Group DoubleLine [hLeft, hLeft']) hTop (dat ++ dat')

typeCmd :: BalanceType -> String
typeCmd HistoricalBalance = "historical"
typeCmd CumulativeChange  = "cumulative"
typeCmd PeriodChange      = "change"
