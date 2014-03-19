{-# LANGUAGE RecordWildCards, DeriveDataTypeable, FlexibleInstances #-}
{-|

Generate several common kinds of report from a journal, as \"*Report\" -
simple intermediate data structures intended to be easily rendered as
text, html, json, csv etc. by hledger commands, hamlet templates,
javascript, or whatever.

-}

module Hledger.Reports (
  -- * Report options
  -- | 
  ReportOpts(..),
  BalanceType(..),
  DisplayExp,
  FormatStr,
  defreportopts,
  dateSpanFromOpts,
  intervalFromOpts,
  clearedValueFromOpts,
  whichDateFromOpts,
  journalSelectingAmountFromOpts,
  queryFromOpts,
  queryFromOptsOnly,
  queryOptsFromOpts,
  reportSpans,
  -- * Entries report
  -- | 
  EntriesReport,
  EntriesReportItem,
  entriesReport,
  -- * Postings report
  -- | 
  PostingsReport,
  PostingsReportItem,
  postingsReport,
  mkpostingsReportItem, -- XXX for showPostingWithBalanceForVty in Hledger.Cli.Register
  -- * Transactions report
  -- | 
  TransactionsReport,
  TransactionsReportItem,
  triDate,
  triBalance,
  triSimpleBalance,
  transactionsReportByCommodity,
  journalTransactionsReport,
  accountTransactionsReport,

  -- * Balance reports
  {-|
  These are used for the various modes of the balance command
  (see "Hledger.Cli.Balance").
  -}
  BalanceReport,
  BalanceReportItem,
  balanceReport,
  MultiBalanceReport(..),
  MultiBalanceReportRow,
  RenderableAccountName,
  periodBalanceReport,
  cumulativeOrHistoricalBalanceReport,

  -- * Other reports
  -- | 
  accountBalanceHistory,

  -- * Tests
  tests_Hledger_Reports
)
where

import Control.Monad
import Data.List
import Data.Maybe
-- import qualified Data.Map as M
import Data.Ord
import Data.Time.Calendar
-- import Data.Tree
import Safe ({- headDef, -} headMay, lastMay)
import System.Console.CmdArgs  -- for defaults support
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.Printf

import Hledger.Data
import Hledger.Read (mamountp')
import Hledger.Query
import Hledger.Utils

------------------------------------------------------------------------------
-- report options handling

-- | Standard options for customising report filtering and output,
-- corresponding to hledger's command-line options and query language
-- arguments. Used in hledger-lib and above.
data ReportOpts = ReportOpts {
     begin_          :: Maybe Day
    ,end_            :: Maybe Day
    ,period_         :: Maybe (Interval,DateSpan)
    ,cleared_        :: Bool
    ,uncleared_      :: Bool
    ,cost_           :: Bool
    ,depth_          :: Maybe Int
    ,display_        :: Maybe DisplayExp
    ,date2_          :: Bool
    ,empty_          :: Bool
    ,no_elide_       :: Bool
    ,real_           :: Bool
    ,balancetype_    :: BalanceType -- for balance command
    ,flat_           :: Bool -- for balance command
    ,drop_           :: Int  -- "
    ,no_total_       :: Bool -- "
    ,daily_          :: Bool
    ,weekly_         :: Bool
    ,monthly_        :: Bool
    ,quarterly_      :: Bool
    ,yearly_         :: Bool
    ,format_         :: Maybe FormatStr
    ,related_        :: Bool
    ,average_        :: Bool
    ,query_          :: String -- all arguments, as a string
 } deriving (Show, Data, Typeable)

type DisplayExp = String
type FormatStr = String

-- | Which balance is being shown in a multi-column balance report.
data BalanceType = PeriodBalance     -- ^ The change of balance in each period.
                 | CumulativeBalance -- ^ The accumulated balance at each period's end, starting from zero at the report start date.
                 | HistoricalBalance -- ^ The historical balance at each period's end, starting from the account balances at the report start date.
  deriving (Eq,Show,Data,Typeable)
instance Default BalanceType where def = PeriodBalance

defreportopts = ReportOpts
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def
    def

instance Default ReportOpts where def = defreportopts

-- | Figure out the date span we should report on, based on any
-- begin/end/period options provided. A period option will cause begin and
-- end options to be ignored.
dateSpanFromOpts :: Day -> ReportOpts -> DateSpan
dateSpanFromOpts _ ReportOpts{..} =
    case period_ of Just (_,span) -> span
                    Nothing -> DateSpan begin_ end_

-- | Figure out the reporting interval, if any, specified by the options.
-- --period overrides --daily overrides --weekly overrides --monthly etc.
intervalFromOpts :: ReportOpts -> Interval
intervalFromOpts ReportOpts{..} =
    case period_ of
      Just (interval,_) -> interval
      Nothing -> i
          where i | daily_ = Days 1
                  | weekly_ = Weeks 1
                  | monthly_ = Months 1
                  | quarterly_ = Quarters 1
                  | yearly_ = Years 1
                  | otherwise =  NoInterval

-- | Get a maybe boolean representing the last cleared/uncleared option if any.
clearedValueFromOpts :: ReportOpts -> Maybe Bool
clearedValueFromOpts ReportOpts{..} | cleared_   = Just True
                                    | uncleared_ = Just False
                                    | otherwise  = Nothing

-- depthFromOpts :: ReportOpts -> Int
-- depthFromOpts opts = min (fromMaybe 99999 $ depth_ opts) (queryDepth $ queryFromOpts nulldate opts)

-- | Report which date we will report on based on --date2.
whichDateFromOpts :: ReportOpts -> WhichDate
whichDateFromOpts ReportOpts{..} = if date2_ then SecondaryDate else PrimaryDate

-- | Select the Transaction date accessor based on --date2.
transactionDateFn :: ReportOpts -> (Transaction -> Day)
transactionDateFn ReportOpts{..} = if date2_ then transactionDate2 else tdate

-- | Select the Posting date accessor based on --date2.
postingDateFn :: ReportOpts -> (Posting -> Day)
postingDateFn ReportOpts{..} = if date2_ then postingDate2 else postingDate


-- | Convert this journal's postings' amounts to the cost basis amounts if
-- specified by options.
journalSelectingAmountFromOpts :: ReportOpts -> Journal -> Journal
journalSelectingAmountFromOpts opts
    | cost_ opts = journalConvertAmountsToCost
    | otherwise = id

-- | Convert report options and arguments to a query.
queryFromOpts :: Day -> ReportOpts -> Query
queryFromOpts d opts@ReportOpts{..} = simplifyQuery $ And $ [flagsq, argsq]
  where
    flagsq = And $
              [(if date2_ then Date2 else Date) $ dateSpanFromOpts d opts]
              ++ (if real_ then [Real True] else [])
              ++ (if empty_ then [Empty True] else []) -- ?
              ++ (maybe [] ((:[]) . Status) (clearedValueFromOpts opts))
              ++ (maybe [] ((:[]) . Depth) depth_)
    argsq = fst $ parseQuery d query_

-- | Convert report options to a query, ignoring any non-flag command line arguments.
queryFromOptsOnly :: Day -> ReportOpts -> Query
queryFromOptsOnly d opts@ReportOpts{..} = simplifyQuery flagsq
  where
    flagsq = And $
              [(if date2_ then Date2 else Date) $ dateSpanFromOpts d opts]
              ++ (if real_ then [Real True] else [])
              ++ (if empty_ then [Empty True] else []) -- ?
              ++ (maybe [] ((:[]) . Status) (clearedValueFromOpts opts))
              ++ (maybe [] ((:[]) . Depth) depth_)

tests_queryFromOpts = [
 "queryFromOpts" ~: do
  assertEqual "" Any (queryFromOpts nulldate defreportopts)
  assertEqual "" (Acct "a") (queryFromOpts nulldate defreportopts{query_="a"})
  assertEqual "" (Desc "a a") (queryFromOpts nulldate defreportopts{query_="desc:'a a'"})
  assertEqual "" (Date $ mkdatespan "2012/01/01" "2013/01/01")
                 (queryFromOpts nulldate defreportopts{begin_=Just (parsedate "2012/01/01")
                                                      ,query_="date:'to 2013'"
                                                      })
  assertEqual "" (Date2 $ mkdatespan "2012/01/01" "2013/01/01")
                 (queryFromOpts nulldate defreportopts{query_="edate:'in 2012'"})
  assertEqual "" (Or [Acct "a a", Acct "'b"])
                 (queryFromOpts nulldate defreportopts{query_="'a a' 'b"})
 ]

-- | Convert report options and arguments to query options.
queryOptsFromOpts :: Day -> ReportOpts -> [QueryOpt]
queryOptsFromOpts d ReportOpts{..} = flagsqopts ++ argsqopts
  where
    flagsqopts = []
    argsqopts = snd $ parseQuery d query_

tests_queryOptsFromOpts = [
 "queryOptsFromOpts" ~: do
  assertEqual "" [] (queryOptsFromOpts nulldate defreportopts)
  assertEqual "" [] (queryOptsFromOpts nulldate defreportopts{query_="a"})
  assertEqual "" [] (queryOptsFromOpts nulldate defreportopts{begin_=Just (parsedate "2012/01/01")
                                                             ,query_="date:'to 2013'"
                                                             })
 ]

-------------------------------------------------------------------------------

-- | A journal entries report is a list of whole transactions as
-- originally entered in the journal (mostly). This is used by eg
-- hledger's print command and hledger-web's journal entries view.
type EntriesReport = [EntriesReportItem]
type EntriesReportItem = Transaction

-- | Select transactions for an entries report.
entriesReport :: ReportOpts -> Query -> Journal -> EntriesReport
entriesReport opts q j =
  sortBy (comparing date) $ filter (q `matchesTransaction`) ts
    where
      date = transactionDateFn opts
      ts = jtxns $ journalSelectingAmountFromOpts opts j

tests_entriesReport = [
  "entriesReport" ~: do
    assertEqual "not acct" 1 (length $ entriesReport defreportopts (Not $ Acct "bank") samplejournal)
    let span = mkdatespan "2008/06/01" "2008/07/01"
    assertEqual "date" 3 (length $ entriesReport defreportopts (Date $ span) samplejournal)
 ]

-------------------------------------------------------------------------------

-- | A postings report is a list of postings with a running total, a label
-- for the total field, and a little extra transaction info to help with rendering.
-- This is used eg for the register command.
type PostingsReport = (String               -- label for the running balance column XXX remove
                      ,[PostingsReportItem] -- line items, one per posting
                      )
type PostingsReportItem = (Maybe Day    -- posting date, if this is the first posting in a transaction or if it's different from the previous posting's date
                          ,Maybe String -- transaction description, if this is the first posting in a transaction
                          ,Posting      -- the posting, possibly with account name depth-clipped
                          ,MixedAmount  -- the running total after this posting (or with --average, the running average)
                          )

-- | Select postings from the journal and add running balance and other
-- information to make a postings report. Used by eg hledger's register command.
postingsReport :: ReportOpts -> Query -> Journal -> PostingsReport
postingsReport opts q j = -- trace ("q: "++show q++"\nq': "++show q') $
                          (totallabel, postingsReportItems ps nullposting wd depth startbal runningcalcfn 1)
    where
      ps | interval == NoInterval = displayableps
         | otherwise              = summarisePostingsByInterval interval depth empty reportspan displayableps
      j' = journalSelectingAmountFromOpts opts j
      wd = whichDateFromOpts opts
      -- delay depth filtering until the end
      (depth, q') = (queryDepth q, filterQuery (not . queryIsDepth) q)
      (precedingps, displayableps, _) =
          dbg "ps5" $
          postingsMatchingDisplayExpr displayexpr opts $ -- filter and group by the -d display expression
          dbg "ps4" $
          map (filterPostingAmount (filterQuery queryIsSym q)) $ -- remove amount parts which the query's sym: terms would exclude
          dbg "ps3" $
          (if related_ opts then concatMap relatedPostings else id) $ -- with --related, replace each with its sibling postings
          dbg "ps2" $ 
          filter (q' `matchesPosting`) $ -- filter postings by the query, ignoring depth
          dbg "ps1" $ 
          journalPostings j'

      -- to debug just this function without the noise of --debug, uncomment:
      -- dbg :: Show a => String -> a -> a
      -- dbg = lstrace

      empty = queryEmpty q
      displayexpr = display_ opts  -- XXX
      interval = intervalFromOpts opts -- XXX
      journalspan = journalDateSpan j'
      -- requestedspan should be the intersection of any span specified
      -- with period options and any span specified with display option.
      -- The latter is not easily available, fake it for now.
      requestedspan = periodspan `spanIntersect` displayspan
      periodspan = queryDateSpan secondarydate q
      secondarydate = whichDateFromOpts opts == SecondaryDate
      displayspan = postingsDateSpan ps
          where (_,ps,_) = postingsMatchingDisplayExpr displayexpr opts $ journalPostings j'
      matchedspan = postingsDateSpan displayableps
      reportspan | empty     = requestedspan `orDatesFrom` journalspan
                 | otherwise = requestedspan `spanIntersect` matchedspan
      startbal = sumPostings precedingps
      runningcalcfn | average_ opts = \i avg amt -> avg + (amt - avg) `divideMixedAmount` (fromIntegral i)
                    | otherwise     = \_ bal amt -> bal + amt

totallabel = "Total"
balancelabel = "Balance"

-- | Generate postings report line items.
postingsReportItems :: [Posting] -> Posting -> WhichDate -> Int -> MixedAmount -> (Int -> MixedAmount -> MixedAmount -> MixedAmount) -> Int -> [PostingsReportItem]
postingsReportItems [] _ _ _ _ _ _ = []
postingsReportItems (p:ps) pprev wd d b runningcalcfn itemnum = i:(postingsReportItems ps p wd d b' runningcalcfn (itemnum+1))
    where
      i = mkpostingsReportItem showdate showdesc wd p' b'
      showdate = isfirstintxn || isdifferentdate
      showdesc = isfirstintxn
      isfirstintxn = ptransaction p /= ptransaction pprev
      isdifferentdate = case wd of PrimaryDate   -> postingDate p  /= postingDate pprev
                                   SecondaryDate -> postingDate2 p /= postingDate2 pprev
      p' = p{paccount=clipAccountName d $ paccount p}
      b' = runningcalcfn itemnum b (pamount p)

-- | Generate one postings report line item, containing the posting,
-- the current running balance, and optionally the posting date and/or
-- the transaction description.
mkpostingsReportItem :: Bool -> Bool -> WhichDate -> Posting -> MixedAmount -> PostingsReportItem
mkpostingsReportItem showdate showdesc wd p b = (if showdate then Just date else Nothing, if showdesc then Just desc else Nothing, p, b)
    where
      date = case wd of PrimaryDate   -> postingDate p
                        SecondaryDate -> postingDate2 p
      desc = maybe "" tdescription $ ptransaction p

-- | Date-sort and split a list of postings into three spans - postings matched
-- by the given display expression, and the preceding and following postings.
-- XXX always sorts by primary date, should sort by secondary date if expression is about that
postingsMatchingDisplayExpr :: Maybe String -> ReportOpts -> [Posting] -> ([Posting],[Posting],[Posting])
postingsMatchingDisplayExpr d opts ps = (before, matched, after)
    where
      sorted = sortBy (comparing (postingDateFn opts)) ps
      (before, rest) = break (displayExprMatches d) sorted
      (matched, after) = span (displayExprMatches d) rest

-- | Does this display expression allow this posting to be displayed ?
-- Raises an error if the display expression can't be parsed.
displayExprMatches :: Maybe String -> Posting -> Bool
displayExprMatches Nothing  _ = True
displayExprMatches (Just d) p = (fromparse $ parsewith datedisplayexpr d) p

-- | Parse a hledger display expression, which is a simple date test like
-- "d>[DATE]" or "d<=[DATE]", and return a "Posting"-matching predicate.
datedisplayexpr :: GenParser Char st (Posting -> Bool)
datedisplayexpr = do
  char 'd'
  op <- compareop
  char '['
  (y,m,d) <- smartdate
  char ']'
  let date    = parsedate $ printf "%04s/%02s/%02s" y m d
      test op = return $ (`op` date) . postingDate
  case op of
    "<"  -> test (<)
    "<=" -> test (<=)
    "="  -> test (==)
    "==" -> test (==)
    ">=" -> test (>=)
    ">"  -> test (>)
    _    -> mzero
 where
  compareop = choice $ map (try . string) ["<=",">=","==","<","=",">"]

-- -- | Clip the account names to the specified depth in a list of postings.
-- depthClipPostings :: Maybe Int -> [Posting] -> [Posting]
-- depthClipPostings depth = map (depthClipPosting depth)

-- -- | Clip a posting's account name to the specified depth.
-- depthClipPosting :: Maybe Int -> Posting -> Posting
-- depthClipPosting Nothing p = p
-- depthClipPosting (Just d) p@Posting{paccount=a} = p{paccount=clipAccountName d a}

-- XXX confusing, refactor

-- | Convert a list of postings into summary postings. Summary postings
-- are one per account per interval and aggregated to the specified depth
-- if any.
summarisePostingsByInterval :: Interval -> Int -> Bool -> DateSpan -> [Posting] -> [Posting]
summarisePostingsByInterval interval depth empty reportspan ps = concatMap summarisespan $ splitSpan interval reportspan
    where
      summarisespan s = summarisePostingsInDateSpan s depth empty (postingsinspan s)
      postingsinspan s = filter (isPostingInDateSpan s) ps

tests_summarisePostingsByInterval = [
  "summarisePostingsByInterval" ~: do
    summarisePostingsByInterval (Quarters 1) 99999 False (DateSpan Nothing Nothing) [] ~?= []
 ]

-- | Given a date span (representing a reporting interval) and a list of
-- postings within it: aggregate the postings so there is only one per
-- account, and adjust their date/description so that they will render
-- as a summary for this interval.
--
-- As usual with date spans the end date is exclusive, but for display
-- purposes we show the previous day as end date, like ledger.
--
-- When a depth argument is present, postings to accounts of greater
-- depth are aggregated where possible.
--
-- The showempty flag includes spans with no postings and also postings
-- with 0 amount.
summarisePostingsInDateSpan :: DateSpan -> Int -> Bool -> [Posting] -> [Posting]
summarisePostingsInDateSpan (DateSpan b e) depth showempty ps
    | null ps && (isNothing b || isNothing e) = []
    | null ps && showempty = [summaryp]
    | otherwise = summaryps'
    where
      summaryp = summaryPosting b' ("- "++ showDate (addDays (-1) e'))
      b' = fromMaybe (maybe nulldate postingDate $ headMay ps) b
      e' = fromMaybe (maybe (addDays 1 nulldate) postingDate $ lastMay ps) e
      summaryPosting date desc = nullposting{ptransaction=Just nulltransaction{tdate=date,tdescription=desc}}
      summaryps' = (if showempty then id else filter (not . isZeroMixedAmount . pamount)) summaryps
      summaryps = [summaryp{paccount=a,pamount=balance a} | a <- clippedanames]
      clippedanames = nub $ map (clipAccountName depth) anames
      anames = sort $ nub $ map paccount ps
      -- aggregate balances by account, like ledgerFromJournal, then do depth-clipping
      accts = accountsFromPostings ps
      balance a = maybe nullmixedamt bal $ lookupAccount a accts 
        where
          bal = if isclipped a then aibalance else aebalance
          isclipped a = accountNameLevel a >= depth

-------------------------------------------------------------------------------

-- | A transactions report includes a list of transactions
-- (posting-filtered and unfiltered variants), a running balance, and some
-- other information helpful for rendering a register view (a flag
-- indicating multiple other accounts and a display string describing
-- them) with or without a notion of current account(s).
-- Two kinds of report use this data structure, see journalTransactionsReport
-- and accountTransactionsReport below for detais.
type TransactionsReport = (String                   -- label for the balance column, eg "balance" or "total"
                          ,[TransactionsReportItem] -- line items, one per transaction
                          )
type TransactionsReportItem = (Transaction -- the corresponding transaction
                              ,Transaction -- the transaction with postings to the current account(s) removed
                              ,Bool        -- is this a split, ie more than one other account posting
                              ,String      -- a display string describing the other account(s), if any
                              ,MixedAmount -- the amount posted to the current account(s) (or total amount posted)
                              ,MixedAmount -- the running balance for the current account(s) after this transaction
                              )

triDate (t,_,_,_,_,_) = tdate t
triAmount (_,_,_,_,a,_) = a
triBalance (_,_,_,_,_,a) = a
triSimpleBalance (_,_,_,_,_,Mixed a) = case a of [] -> "0"
                                                 (Amount{aquantity=q}):_ -> show q

-- Split a transactions report whose items may involve several commodities,
-- into one or more single-commodity transactions reports.
transactionsReportByCommodity :: TransactionsReport -> [TransactionsReport]
transactionsReportByCommodity tr =
  [filterTransactionsReportByCommodity c tr | c <- transactionsReportCommodities tr]
  where
    transactionsReportCommodities (_,items) =
      nub $ sort $ map acommodity $ concatMap (amounts . triAmount) items

-- Remove transaction report items and item amount (and running
-- balance amount) components that don't involve the specified
-- commodity. Other item fields such as the transaction are left unchanged.
filterTransactionsReportByCommodity :: Commodity -> TransactionsReport -> TransactionsReport
filterTransactionsReportByCommodity c (label,items) =
  (label, fixTransactionsReportItemBalances $ concat [filterTransactionsReportItemByCommodity c i | i <- items])
  where
    filterTransactionsReportItemByCommodity c (t,t2,s,o,a,bal)
      | c `elem` cs = [item']
      | otherwise   = []
      where
        cs = map acommodity $ amounts a
        item' = (t,t2,s,o,a',bal)
        a' = filterMixedAmountByCommodity c a

    fixTransactionsReportItemBalances [] = []
    fixTransactionsReportItemBalances [i] = [i]
    fixTransactionsReportItemBalances items = reverse $ i:(go startbal is)
      where
        i:is = reverse items
        startbal = filterMixedAmountByCommodity c $ triBalance i
        go _ [] = []
        go bal ((t,t2,s,o,amt,_):is) = (t,t2,s,o,amt,bal'):go bal' is
          where bal' = bal + amt

-- | Filter out all but the specified commodity from this amount.
filterMixedAmountByCommodity :: Commodity -> MixedAmount -> MixedAmount
filterMixedAmountByCommodity c (Mixed as) = Mixed $ filter ((==c). acommodity) as

-- -- | Filter out all parts of this amount which do not match the query.
-- filterMixedAmount :: Query -> MixedAmount -> MixedAmount
-- filterMixedAmount q (Mixed as) = Mixed $ filter (q `matchesAmount`) as

-- | Select transactions from the whole journal. This is similar to a
-- "postingsReport" except with transaction-based report items which
-- are ordered most recent first. This is used by eg hledger-web's journal view.
journalTransactionsReport :: ReportOpts -> Journal -> Query -> TransactionsReport
journalTransactionsReport _ Journal{jtxns=ts} m = (totallabel, items)
   where
     ts' = sortBy (comparing tdate) $ filter (not . null . tpostings) $ map (filterTransactionPostings m) ts
     items = reverse $ accountTransactionsReportItems m Nothing nullmixedamt id ts'
     -- XXX items' first element should be the full transaction with all postings

-------------------------------------------------------------------------------

-- | Select transactions within one or more current accounts, and make a
-- transactions report relative to those account(s). This means:
--
-- 1. it shows transactions from the point of view of the current account(s).
--    The transaction amount is the amount posted to the current account(s).
--    The other accounts' names are provided. 
--
-- 2. With no transaction filtering in effect other than a start date, it
--    shows the accurate historical running balance for the current account(s).
--    Otherwise it shows a running total starting at 0.
--
-- This is used by eg hledger-web's account register view. Currently,
-- reporting intervals are not supported, and report items are most
-- recent first.
accountTransactionsReport :: ReportOpts -> Journal -> Query -> Query -> TransactionsReport
accountTransactionsReport opts j m thisacctquery = (label, items)
 where
     -- transactions affecting this account, in date order
     ts = sortBy (comparing tdate) $ filter (matchesTransaction thisacctquery) $ jtxns $
          journalSelectingAmountFromOpts opts j
     -- starting balance: if we are filtering by a start date and nothing else,
     -- the sum of postings to this account before that date; otherwise zero.
     (startbal,label) | queryIsNull m                           = (nullmixedamt,        balancelabel)
                      | queryIsStartDateOnly (date2_ opts) m = (sumPostings priorps, balancelabel)
                      | otherwise                                 = (nullmixedamt,        totallabel)
                      where
                        priorps = -- ltrace "priorps" $
                                  filter (matchesPosting
                                          (-- ltrace "priormatcher" $
                                           And [thisacctquery, tostartdatequery]))
                                         $ transactionsPostings ts
                        tostartdatequery = Date (DateSpan Nothing startdate)
                        startdate = queryStartDate (date2_ opts) m
     items = reverse $ accountTransactionsReportItems m (Just thisacctquery) startbal negate ts

-- | Generate transactions report items from a list of transactions,
-- using the provided query and current account queries, starting balance,
-- sign-setting function and balance-summing function.
accountTransactionsReportItems :: Query -> Maybe Query -> MixedAmount -> (MixedAmount -> MixedAmount) -> [Transaction] -> [TransactionsReportItem]
accountTransactionsReportItems _ _ _ _ [] = []
accountTransactionsReportItems query thisacctquery bal signfn (t:ts) =
    -- This is used for both accountTransactionsReport and journalTransactionsReport,
    -- which makes it a bit overcomplicated
    case i of Just i' -> i':is
              Nothing -> is
    where
      tmatched@Transaction{tpostings=psmatched} = filterTransactionPostings query t
      (psthisacct,psotheracct) = case thisacctquery of Just m  -> partition (matchesPosting m) psmatched
                                                       Nothing -> ([],psmatched)
      numotheraccts = length $ nub $ map paccount psotheracct
      amt = negate $ sum $ map pamount psthisacct
      acct | isNothing thisacctquery = summarisePostings psmatched -- journal register
           | numotheraccts == 0 = "transfer between " ++ summarisePostingAccounts psthisacct
           | otherwise          = prefix              ++ summarisePostingAccounts psotheracct
           where prefix = maybe "" (\b -> if b then "from " else "to ") $ isNegativeMixedAmount amt
      (i,bal') = case psmatched of
           [] -> (Nothing,bal)
           _  -> (Just (t, tmatched, numotheraccts > 1, acct, a, b), b)
                 where
                  a = signfn amt
                  b = bal + a
      is = accountTransactionsReportItems query thisacctquery bal' signfn ts

-- | Generate a short readable summary of some postings, like
-- "from (negatives) to (positives)".
summarisePostings :: [Posting] -> String
summarisePostings ps =
    case (summarisePostingAccounts froms, summarisePostingAccounts tos) of
       ("",t) -> "to "++t
       (f,"") -> "from "++f
       (f,t)  -> "from "++f++" to "++t
    where
      (froms,tos) = partition (fromMaybe False . isNegativeMixedAmount . pamount) ps

-- | Generate a simplified summary of some postings' accounts.
summarisePostingAccounts :: [Posting] -> String
summarisePostingAccounts = intercalate ", " . map accountLeafName . nub . map paccount

filterTransactionPostings :: Query -> Transaction -> Transaction
filterTransactionPostings m t@Transaction{tpostings=ps} = t{tpostings=filter (m `matchesPosting`) ps}

-------------------------------------------------------------------------------

-- | A simple single-column balance report. It has:
--
-- 1. a list of rows, each containing a renderable account name and a corresponding amount
--
-- 2. the final total of the amounts
type BalanceReport = ([BalanceReportItem], MixedAmount)
type BalanceReportItem = (RenderableAccountName, MixedAmount)

-- | A renderable account name includes some additional hints for rendering accounts in a balance report.
-- It has:
--
-- * The full account name
-- 
-- * The ledger-style short elided account name (the leaf name, prefixed by any boring parents immediately above)
-- 
-- * The number of indentation steps to use when rendering a ledger-style account tree
--   (normally the 0-based depth of this account excluding boring parents, or 0 with --flat).
type RenderableAccountName = (AccountName, AccountName, Int)

-- | Select accounts, and get their balances at the end of the selected
-- period, and misc. display information, for an accounts report.
balanceReport :: ReportOpts -> Query -> Journal -> BalanceReport
balanceReport opts q j = (items, total)
    where
      l =  ledgerFromJournal q $ journalSelectingAmountFromOpts opts j
      accts =
          dbg "accts1" $ 
          clipAccounts (queryDepth q) $ -- exclude accounts deeper than specified depth
          ledgerRootAccount l
      accts'
          | flat_ opts = filterzeros $ tail $ flattenAccounts accts
          | otherwise  = filter (not.aboring) $ tail $ flattenAccounts $ markboring $ prunezeros accts
          where
            filterzeros | empty_ opts = id
                        | otherwise = filter (not . isZeroMixedAmount . aebalance)
            prunezeros | empty_ opts = id
                       | otherwise   = fromMaybe nullacct . pruneAccounts (isZeroMixedAmount.aibalance)
            markboring | no_elide_ opts = id
                       | otherwise      = markBoringParentAccounts
      items = map (balanceReportItem opts) accts'
      total = sum [amt | ((a,_,indent),amt) <- items, if flat_ opts then accountNameLevel a == 1 else indent == 0]
              -- XXX check account level == 1 is valid when top-level accounts excluded

-- | In an account tree with zero-balance leaves removed, mark the
-- elidable parent accounts (those with one subaccount and no balance
-- of their own).
markBoringParentAccounts :: Account -> Account
markBoringParentAccounts = tieAccountParents . mapAccounts mark
  where
    mark a | length (asubs a) == 1 && isZeroMixedAmount (aebalance a) = a{aboring=True}
           | otherwise = a

balanceReportItem :: ReportOpts -> Account -> BalanceReportItem
balanceReportItem opts a@Account{aname=name, aibalance=ibal}
  | flat_ opts = ((name, name,       0),      ibal)
  | otherwise  = ((name, elidedname, indent), ibal)
  where
    elidedname = accountNameFromComponents (adjacentboringparentnames ++ [accountLeafName name])
    adjacentboringparentnames = reverse $ map (accountLeafName.aname) $ takeWhile aboring $ parents
    indent = length $ filter (not.aboring) parents
    parents = init $ parentAccounts a

-- -- the above using the newer multi balance report code:
-- balanceReport' opts q j = (items, total)
--   where
--     MultiBalanceReport (_,mbrrows,mbrtotals) = periodBalanceReport opts q j
--     items = [(a,a',n, headDef 0 bs) | ((a,a',n), bs) <- mbrrows]
--     total = headDef 0 mbrtotals

-------------------------------------------------------------------------------

-- | A multi balance report is a balance report with one or more columns. It has:
--
-- 1. a list of each column's date span
--
-- 2. a list of rows, each containing a renderable account name and the amounts to show in each column
--
-- 3. a list of each column's final total
--
-- The meaning of the amounts depends on the type of balance report (see
-- 'BalanceType' and "Hledger.Cli.Balance").
newtype MultiBalanceReport = MultiBalanceReport ([DateSpan]
                                                ,[MultiBalanceReportRow]
                                                ,[MixedAmount]
                                                )

-- | A row in a multi balance report has
--
-- * An account name, with rendering hints
--
-- * A list of amounts to be shown in each of the report's columns.
type MultiBalanceReportRow = (RenderableAccountName, [MixedAmount])

instance Show MultiBalanceReport where
    -- use ppShow to break long lists onto multiple lines
    -- we have to add some bogus extra shows here to help ppShow parse the output
    -- and wrap tuples and lists properly
    show (MultiBalanceReport (spans, items, totals)) =
        "MultiBalanceReport (ignore extra quotes):\n" ++ ppShow (show spans, map show items, totals)

-- | Select accounts and get their period balance (change of balance) in each
-- period, plus misc. display information, for a period balance report.
periodBalanceReport :: ReportOpts -> Query -> Journal -> MultiBalanceReport
periodBalanceReport opts q j = MultiBalanceReport (spans, items, totals)
    where
      (q',depthq)  = (filterQuery (not . queryIsDepth) q, filterQuery queryIsDepth q)
      clip = filter (depthq `matchesAccount`)
      j' = filterJournalPostings q' $ journalSelectingAmountFromOpts opts j
      ps = journalPostings $
           filterJournalPostingAmounts (filterQuery queryIsSym q) -- remove amount parts which the query's sym: terms would exclude
           j'

      -- the requested span is the span of the query (which is
      -- based on -b/-e/-p opts and query args IIRC).
      requestedspan = queryDateSpan (date2_ opts) q

      -- the report's span will be the requested span intersected with
      -- the selected data's span; or with -E, the requested span
      -- limited by the journal's overall span.
      reportspan | empty_ opts = requestedspan `orDatesFrom` journalspan
                 | otherwise   = requestedspan `spanIntersect` matchedspan
        where
          journalspan = journalDateSpan j'
          matchedspan = postingsDateSpan ps

      -- first implementation, probably inefficient
      spans               = dbg "1 " $ splitSpan (intervalFromOpts opts) reportspan
      psPerSpan           = dbg "3"  $ [filter (isPostingInDateSpan s) ps | s <- spans]
      acctnames           = dbg "4"  $ sort $ clip $ 
                            -- expandAccountNames $ 
                            accountNamesFromPostings ps
      allAcctsZeros       = dbg "5"  $ [(a, nullmixedamt) | a <- acctnames]
      someAcctBalsPerSpan = dbg "6"  $ [[(aname a, aibalance a) | a <- drop 1 $ accountsFromPostings ps, depthq `matchesAccount` aname a, aname a `elem` acctnames] | ps <- psPerSpan]
      balsPerSpan         = dbg "7"  $ [sortBy (comparing fst) $ unionBy (\(a,_) (a',_) -> a == a') acctbals allAcctsZeros | acctbals <- someAcctBalsPerSpan]
      balsPerAcct         = dbg "8"  $ transpose balsPerSpan
      acctsAndBals        = dbg "8.5" $ zip acctnames (map (map snd) balsPerAcct)
      items               = dbg "9"  $ [((a, a, accountNameLevel a), bs) | (a,bs) <- acctsAndBals, empty_ opts || any (not . isZeroMixedAmount) bs]
      highestLevelBalsPerSpan =
                            dbg "9.5" $ [[b | (a,b) <- spanbals, not $ any (`elem` acctnames) $ init $ expandAccountName a] | spanbals <- balsPerSpan]
      totals              = dbg "10" $ map sum highestLevelBalsPerSpan

-------------------------------------------------------------------------------

-- | Calculate the overall span and per-period date spans for a report
-- based on command-line options, the parsed search query, and the
-- journal data. If a reporting interval is specified, the report span
-- will be enlarged to include a whole number of report periods.
-- Reports will sometimes trim these spans further when appropriate.
reportSpans ::  ReportOpts -> Query -> Journal -> (DateSpan, [DateSpan])
reportSpans opts q j = (reportspan, spans)
  where
    -- get the requested span from the query, which is based on
    -- -b/-e/-p opts and query args.
    requestedspan = queryDateSpan (date2_ opts) q

    -- set the start and end date to the journal's if not specified
    requestedspan' = requestedspan `orDatesFrom` journalDateSpan j

    -- if there's a reporting interval, calculate the report periods
    -- which enclose the requested span
    spans = dbg "spans" $ splitSpan (intervalFromOpts opts) requestedspan'

    -- the overall report span encloses the periods
    reportspan = DateSpan
                 (maybe Nothing spanStart $ headMay spans)
                 (maybe Nothing spanEnd   $ lastMay spans)

-- | Select accounts and get their ending balance in each period, plus
-- account name display information, for a cumulative or historical balance report.
cumulativeOrHistoricalBalanceReport :: ReportOpts -> Query -> Journal -> MultiBalanceReport
cumulativeOrHistoricalBalanceReport opts q j = MultiBalanceReport (periodbalancespans, items, totals)
    where
      -- select/adjust basic report dates
      (reportspan, _) = reportSpans opts q j

      -- rewrite query to use adjusted dates
      dateless  = filterQuery (not . queryIsDate)
      depthless = filterQuery (not . queryIsDepth)
      q' = dateless $ depthless q
      -- reportq = And [q', Date reportspan]

      -- get starting balances and accounts from preceding txns
      precedingq = And [q', Date $ DateSpan Nothing (spanStart reportspan)]
      (startbalanceitems,_) = balanceReport opts{flat_=True,empty_=True} precedingq j
      startacctbals = dbg "startacctbals"   $ map (\((a,_,_),b) -> (a,b)) startbalanceitems
      -- acctsWithStartingBalance = map fst $ filter (not . isZeroMixedAmount . snd) startacctbals
      startingBalanceFor a | balancetype_ opts == HistoricalBalance = fromMaybe nullmixedamt $ lookup a startacctbals
                           | otherwise = nullmixedamt

      -- get balance changes by period
      MultiBalanceReport (periodbalancespans,periodbalanceitems,_) = dbg "changes" $ periodBalanceReport opts q j
      balanceChangesByAcct = map (\((a,_,_),bs) -> (a,bs)) periodbalanceitems
      acctsWithBalanceChanges = map fst $ filter ((any (not . isZeroMixedAmount)) . snd) balanceChangesByAcct
      balanceChangesFor a = fromMaybe (error $ "no data for account: a") $ -- XXX
                            lookup a balanceChangesByAcct

      -- accounts to report on
      reportaccts -- = dbg' "reportaccts" $ (dbg' "acctsWithStartingBalance" acctsWithStartingBalance) `union` (dbg' "acctsWithBalanceChanges" acctsWithBalanceChanges)
                  = acctsWithBalanceChanges

      -- sum balance changes to get ending balances for each period
      endingBalancesFor a = 
          dbg "ending balances" $ drop 1 $ scanl (+) (startingBalanceFor a) $
          dbg "balance changes" $ balanceChangesFor a

      items  = dbg "items"  $ [((a,a,0), endingBalancesFor a) | a <- reportaccts]

      -- sum highest-level account balances in each column for column totals
      totals = dbg "totals" $ map sum highestlevelbalsbycol
          where
            highestlevelbalsbycol = transpose $ map endingBalancesFor highestlevelaccts
            highestlevelaccts =
                dbg "highestlevelaccts" $
                [a | a <- reportaccts, not $ any (`elem` reportaccts) $ init $ expandAccountName a]

      -- enable to debug just this function
      -- dbg :: Show a => String -> a -> a
      -- dbg = lstrace
        
-------------------------------------------------------------------------------

-- | Get the historical running inclusive balance of a particular account,
-- from earliest to latest posting date.
-- XXX Accounts should know the Ledger & Journal they came from
accountBalanceHistory :: ReportOpts -> Journal -> Account -> [(Day, MixedAmount)]
accountBalanceHistory ropts j a = [(getdate t, bal) | (t,_,_,_,_,bal) <- items]
  where
    (_,items) = journalTransactionsReport ropts j acctquery
    inclusivebal = True
    acctquery = Acct $ (if inclusivebal then accountNameToAccountRegex else accountNameToAccountOnlyRegex) $ aname a
    getdate = if date2_ ropts then transactionDate2 else tdate


-------------------------------------------------------------------------------
-- TESTS

tests_postingsReport = [
  "postingsReport" ~: do

   -- with the query specified explicitly
   let (query, journal) `gives` n = (length $ snd $ postingsReport defreportopts query journal) `is` n
   (Any, nulljournal) `gives` 0
   (Any, samplejournal) `gives` 11
   -- register --depth just clips account names
   (Depth 2, samplejournal) `gives` 11
   (And [Depth 1, Status True, Acct "expenses"], samplejournal) `gives` 2
   (And [And [Depth 1, Status True], Acct "expenses"], samplejournal) `gives` 2

   -- with query and/or command-line options
   assertEqual "" 11 (length $ snd $ postingsReport defreportopts Any samplejournal)
   assertEqual ""  9 (length $ snd $ postingsReport defreportopts{monthly_=True} Any samplejournal)
   assertEqual "" 19 (length $ snd $ postingsReport defreportopts{monthly_=True} (Empty True) samplejournal)
   assertEqual ""  4 (length $ snd $ postingsReport defreportopts (Acct "assets:bank:checking") samplejournal)

   -- (defreportopts, And [Acct "a a", Acct "'b"], samplejournal2) `gives` 0
   -- [(Just (parsedate "2008-01-01","income"),assets:bank:checking             $1,$1)
   -- ,(Nothing,income:salary                   $-1,0)
   -- ,(Just (2008-06-01,"gift"),assets:bank:checking             $1,$1)
   -- ,(Nothing,income:gifts                    $-1,0)
   -- ,(Just (2008-06-02,"save"),assets:bank:saving               $1,$1)
   -- ,(Nothing,assets:bank:checking            $-1,0)
   -- ,(Just (2008-06-03,"eat & shop"),expenses:food                    $1,$1)
   -- ,(Nothing,expenses:supplies                $1,$2)
   -- ,(Nothing,assets:cash                     $-2,0)
   -- ,(Just (2008-12-31,"pay off"),liabilities:debts                $1,$1)
   -- ,(Nothing,assets:bank:checking            $-1,0)
   -- ]

{-
    let opts = defreportopts
    (postingsReportAsText opts $ postingsReport opts (queryFromOpts date1 opts) j) `is` unlines
     ["2008/01/01 income               assets:bank:checking             $1           $1"
     ,"                                income:salary                   $-1            0"
     ,"2008/06/01 gift                 assets:bank:checking             $1           $1"
     ,"                                income:gifts                    $-1            0"
     ,"2008/06/02 save                 assets:bank:saving               $1           $1"
     ,"                                assets:bank:checking            $-1            0"
     ,"2008/06/03 eat & shop           expenses:food                    $1           $1"
     ,"                                expenses:supplies                $1           $2"
     ,"                                assets:cash                     $-2            0"
     ,"2008/12/31 pay off              liabilities:debts                $1           $1"
     ,"                                assets:bank:checking            $-1            0"
     ]

  ,"postings report with cleared option" ~:
   do 
    let opts = defreportopts{cleared_=True}
    j <- readJournal' sample_journal_str
    (postingsReportAsText opts $ postingsReport opts (queryFromOpts date1 opts) j) `is` unlines
     ["2008/06/03 eat & shop           expenses:food                    $1           $1"
     ,"                                expenses:supplies                $1           $2"
     ,"                                assets:cash                     $-2            0"
     ,"2008/12/31 pay off              liabilities:debts                $1           $1"
     ,"                                assets:bank:checking            $-1            0"
     ]

  ,"postings report with uncleared option" ~:
   do 
    let opts = defreportopts{uncleared_=True}
    j <- readJournal' sample_journal_str
    (postingsReportAsText opts $ postingsReport opts (queryFromOpts date1 opts) j) `is` unlines
     ["2008/01/01 income               assets:bank:checking             $1           $1"
     ,"                                income:salary                   $-1            0"
     ,"2008/06/01 gift                 assets:bank:checking             $1           $1"
     ,"                                income:gifts                    $-1            0"
     ,"2008/06/02 save                 assets:bank:saving               $1           $1"
     ,"                                assets:bank:checking            $-1            0"
     ]

  ,"postings report sorts by date" ~:
   do 
    j <- readJournal' $ unlines
        ["2008/02/02 a"
        ,"  b  1"
        ,"  c"
        ,""
        ,"2008/01/01 d"
        ,"  e  1"
        ,"  f"
        ]
    let opts = defreportopts
    registerdates (postingsReportAsText opts $ postingsReport opts (queryFromOpts date1 opts) j) `is` ["2008/01/01","2008/02/02"]

  ,"postings report with account pattern" ~:
   do
    j <- samplejournal
    let opts = defreportopts{patterns_=["cash"]}
    (postingsReportAsText opts $ postingsReport opts (queryFromOpts date1 opts) j) `is` unlines
     ["2008/06/03 eat & shop           assets:cash                     $-2          $-2"
     ]

  ,"postings report with account pattern, case insensitive" ~:
   do 
    j <- samplejournal
    let opts = defreportopts{patterns_=["cAsH"]}
    (postingsReportAsText opts $ postingsReport opts (queryFromOpts date1 opts) j) `is` unlines
     ["2008/06/03 eat & shop           assets:cash                     $-2          $-2"
     ]

  ,"postings report with display expression" ~:
   do 
    j <- samplejournal
    let gives displayexpr = 
            (registerdates (postingsReportAsText opts $ postingsReport opts (queryFromOpts date1 opts) j) `is`)
                where opts = defreportopts{display_=Just displayexpr}
    "d<[2008/6/2]"  `gives` ["2008/01/01","2008/06/01"]
    "d<=[2008/6/2]" `gives` ["2008/01/01","2008/06/01","2008/06/02"]
    "d=[2008/6/2]"  `gives` ["2008/06/02"]
    "d>=[2008/6/2]" `gives` ["2008/06/02","2008/06/03","2008/12/31"]
    "d>[2008/6/2]"  `gives` ["2008/06/03","2008/12/31"]

  ,"postings report with period expression" ~:
   do 
    j <- samplejournal
    let periodexpr `gives` dates = do
          j' <- samplejournal
          registerdates (postingsReportAsText opts $ postingsReport opts (queryFromOpts date1 opts) j') `is` dates
              where opts = defreportopts{period_=maybePeriod date1 periodexpr}
    ""     `gives` ["2008/01/01","2008/06/01","2008/06/02","2008/06/03","2008/12/31"]
    "2008" `gives` ["2008/01/01","2008/06/01","2008/06/02","2008/06/03","2008/12/31"]
    "2007" `gives` []
    "june" `gives` ["2008/06/01","2008/06/02","2008/06/03"]
    "monthly" `gives` ["2008/01/01","2008/06/01","2008/12/01"]
    "quarterly" `gives` ["2008/01/01","2008/04/01","2008/10/01"]
    let opts = defreportopts{period_=maybePeriod date1 "yearly"}
    (postingsReportAsText opts $ postingsReport opts (queryFromOpts date1 opts) j) `is` unlines
     ["2008/01/01 - 2008/12/31         assets:bank:saving               $1           $1"
     ,"                                assets:cash                     $-2          $-1"
     ,"                                expenses:food                    $1            0"
     ,"                                expenses:supplies                $1           $1"
     ,"                                income:gifts                    $-1            0"
     ,"                                income:salary                   $-1          $-1"
     ,"                                liabilities:debts                $1            0"
     ]
    let opts = defreportopts{period_=maybePeriod date1 "quarterly"}
    registerdates (postingsReportAsText opts $ postingsReport opts (queryFromOpts date1 opts) j) `is` ["2008/01/01","2008/04/01","2008/10/01"]
    let opts = defreportopts{period_=maybePeriod date1 "quarterly",empty_=True}
    registerdates (postingsReportAsText opts $ postingsReport opts (queryFromOpts date1 opts) j) `is` ["2008/01/01","2008/04/01","2008/07/01","2008/10/01"]

  ]

  , "postings report with depth arg" ~:
   do 
    j <- samplejournal
    let opts = defreportopts{depth_=Just 2}
    (postingsReportAsText opts $ postingsReport opts (queryFromOpts date1 opts) j) `is` unlines
     ["2008/01/01 income               assets:bank                      $1           $1"
     ,"                                income:salary                   $-1            0"
     ,"2008/06/01 gift                 assets:bank                      $1           $1"
     ,"                                income:gifts                    $-1            0"
     ,"2008/06/02 save                 assets:bank                      $1           $1"
     ,"                                assets:bank                     $-1            0"
     ,"2008/06/03 eat & shop           expenses:food                    $1           $1"
     ,"                                expenses:supplies                $1           $2"
     ,"                                assets:cash                     $-2            0"
     ,"2008/12/31 pay off              liabilities:debts                $1           $1"
     ,"                                assets:bank                     $-1            0"
     ]

-}
 ]

tests_balanceReport =
  let (opts,journal) `gives` r = do
         let (eitems, etotal) = r
             (aitems, atotal) = balanceReport opts (queryFromOpts nulldate opts) journal
         assertEqual "items" eitems aitems
         -- assertEqual "" (length eitems) (length aitems)
         -- mapM (\(e,a) -> assertEqual "" e a) $ zip eitems aitems
         assertEqual "total" etotal atotal
  in [

   "balanceReport with no args on null journal" ~: do
   (defreportopts, nulljournal) `gives` ([], Mixed [nullamt])

  ,"balanceReport with no args on sample journal" ~: do
   (defreportopts, samplejournal) `gives`
    ([
      (("assets","assets",0), mamountp' "$-1.00")
     ,(("assets:bank:saving","bank:saving",1), mamountp' "$1.00")
     ,(("assets:cash","cash",1), mamountp' "$-2.00")
     ,(("expenses","expenses",0), mamountp' "$2.00")
     ,(("expenses:food","food",1), mamountp' "$1.00")
     ,(("expenses:supplies","supplies",1), mamountp' "$1.00")
     ,(("income","income",0), mamountp' "$-2.00")
     ,(("income:gifts","gifts",1), mamountp' "$-1.00")
     ,(("income:salary","salary",1), mamountp' "$-1.00")
     ,(("liabilities:debts","liabilities:debts",0), mamountp' "$1.00")
     ],
     Mixed [nullamt])

  ,"balanceReport with --depth=N" ~: do
   (defreportopts{depth_=Just 1}, samplejournal) `gives`
    ([
      (("assets",      "assets",      0), mamountp' "$-1.00")
     ,(("expenses",    "expenses",    0), mamountp'  "$2.00")
     ,(("income",      "income",      0), mamountp' "$-2.00")
     ,(("liabilities", "liabilities", 0), mamountp'  "$1.00")
     ],
     Mixed [nullamt])

  ,"balanceReport with depth:N" ~: do
   (defreportopts{query_="depth:1"}, samplejournal) `gives`
    ([
      (("assets",      "assets",      0), mamountp' "$-1.00")
     ,(("expenses",    "expenses",    0), mamountp'  "$2.00")
     ,(("income",      "income",      0), mamountp' "$-2.00")
     ,(("liabilities", "liabilities", 0), mamountp'  "$1.00")
     ],
     Mixed [nullamt])

  ,"balanceReport with a date or secondary date span" ~: do
   (defreportopts{query_="date:'in 2009'"}, samplejournal2) `gives`
    ([],
     Mixed [nullamt])
   (defreportopts{query_="edate:'in 2009'"}, samplejournal2) `gives`
    ([
      (("assets:bank:checking","assets:bank:checking",0),mamountp' "$1.00")
     ,(("income:salary","income:salary",0),mamountp' "$-1.00")
     ],
     Mixed [nullamt])

  ,"balanceReport with desc:" ~: do
   (defreportopts{query_="desc:income"}, samplejournal) `gives`
    ([
      (("assets:bank:checking","assets:bank:checking",0),mamountp' "$1.00")
     ,(("income:salary","income:salary",0), mamountp' "$-1.00")
     ],
     Mixed [nullamt])

  ,"balanceReport with not:desc:" ~: do
   (defreportopts{query_="not:desc:income"}, samplejournal) `gives`
    ([
      (("assets","assets",0), mamountp' "$-2.00")
     ,(("assets:bank","bank",1), Mixed [nullamt])
     ,(("assets:bank:checking","checking",2),mamountp' "$-1.00")
     ,(("assets:bank:saving","saving",2), mamountp' "$1.00")
     ,(("assets:cash","cash",1), mamountp' "$-2.00")
     ,(("expenses","expenses",0), mamountp' "$2.00")
     ,(("expenses:food","food",1), mamountp' "$1.00")
     ,(("expenses:supplies","supplies",1), mamountp' "$1.00")
     ,(("income:gifts","income:gifts",0), mamountp' "$-1.00")
     ,(("liabilities:debts","liabilities:debts",0), mamountp' "$1.00")
     ],
     Mixed [nullamt])


{-
    ,"accounts report with account pattern o" ~:
     defreportopts{patterns_=["o"]} `gives`
     ["                  $1  expenses:food"
     ,"                 $-2  income"
     ,"                 $-1    gifts"
     ,"                 $-1    salary"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with account pattern o and --depth 1" ~:
     defreportopts{patterns_=["o"],depth_=Just 1} `gives`
     ["                  $1  expenses"
     ,"                 $-2  income"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with account pattern a" ~:
     defreportopts{patterns_=["a"]} `gives`
     ["                 $-1  assets"
     ,"                  $1    bank:saving"
     ,"                 $-2    cash"
     ,"                 $-1  income:salary"
     ,"                  $1  liabilities:debts"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with account pattern e" ~:
     defreportopts{patterns_=["e"]} `gives`
     ["                 $-1  assets"
     ,"                  $1    bank:saving"
     ,"                 $-2    cash"
     ,"                  $2  expenses"
     ,"                  $1    food"
     ,"                  $1    supplies"
     ,"                 $-2  income"
     ,"                 $-1    gifts"
     ,"                 $-1    salary"
     ,"                  $1  liabilities:debts"
     ,"--------------------"
     ,"                   0"
     ]

    ,"accounts report with unmatched parent of two matched subaccounts" ~: 
     defreportopts{patterns_=["cash","saving"]} `gives`
     ["                 $-1  assets"
     ,"                  $1    bank:saving"
     ,"                 $-2    cash"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with multi-part account name" ~: 
     defreportopts{patterns_=["expenses:food"]} `gives`
     ["                  $1  expenses:food"
     ,"--------------------"
     ,"                  $1"
     ]

    ,"accounts report with negative account pattern" ~:
     defreportopts{patterns_=["not:assets"]} `gives`
     ["                  $2  expenses"
     ,"                  $1    food"
     ,"                  $1    supplies"
     ,"                 $-2  income"
     ,"                 $-1    gifts"
     ,"                 $-1    salary"
     ,"                  $1  liabilities:debts"
     ,"--------------------"
     ,"                  $1"
     ]

    ,"accounts report negative account pattern always matches full name" ~: 
     defreportopts{patterns_=["not:e"]} `gives`
     ["--------------------"
     ,"                   0"
     ]

    ,"accounts report negative patterns affect totals" ~: 
     defreportopts{patterns_=["expenses","not:food"]} `gives`
     ["                  $1  expenses:supplies"
     ,"--------------------"
     ,"                  $1"
     ]

    ,"accounts report with -E shows zero-balance accounts" ~:
     defreportopts{patterns_=["assets"],empty_=True} `gives`
     ["                 $-1  assets"
     ,"                  $1    bank"
     ,"                   0      checking"
     ,"                  $1      saving"
     ,"                 $-2    cash"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with cost basis" ~: do
       j <- (readJournal Nothing Nothing Nothing $ unlines
              [""
              ,"2008/1/1 test           "
              ,"  a:b          10h @ $50"
              ,"  c:d                   "
              ]) >>= either error' return
       let j' = journalCanonicaliseAmounts $ journalConvertAmountsToCost j -- enable cost basis adjustment
       balanceReportAsText defreportopts (balanceReport defreportopts Any j') `is`
         ["                $500  a:b"
         ,"               $-500  c:d"
         ,"--------------------"
         ,"                   0"
         ]
-}
 ]

Right samplejournal2 = journalBalanceTransactions $ 
         nulljournal
         {jtxns = [
           txnTieKnot $ Transaction {
             tdate=parsedate "2008/01/01",
             tdate2=Just $ parsedate "2009/01/01",
             tstatus=False,
             tcode="",
             tdescription="income",
             tcomment="",
             ttags=[],
             tpostings=
                 [posting {paccount="assets:bank:checking", pamount=Mixed [usd 1]}
                 ,posting {paccount="income:salary", pamount=missingmixedamt}
                 ],
             tpreceding_comment_lines=""
           }
          ]
         }
         
-- tests_isInterestingIndented = [
--   "isInterestingIndented" ~: do 
--    let (opts, journal, acctname) `gives` r = isInterestingIndented opts l acctname `is` r
--           where l = ledgerFromJournal (queryFromOpts nulldate opts) journal
     
--    (defreportopts, samplejournal, "expenses") `gives` True
--  ]

tests_Hledger_Reports :: Test
tests_Hledger_Reports = TestList $
    tests_queryFromOpts
 ++ tests_queryOptsFromOpts
 ++ tests_entriesReport
 ++ tests_summarisePostingsByInterval
 ++ tests_postingsReport
 -- ++ tests_isInterestingIndented
 ++ tests_balanceReport
 ++ [
  -- ,"summarisePostingsInDateSpan" ~: do
  --   let gives (b,e,depth,showempty,ps) =
  --           (summarisePostingsInDateSpan (mkdatespan b e) depth showempty ps `is`)
  --   let ps =
  --           [
  --            nullposting{lpdescription="desc",lpaccount="expenses:food:groceries",lpamount=Mixed [usd 1]}
  --           ,nullposting{lpdescription="desc",lpaccount="expenses:food:dining",   lpamount=Mixed [usd 2]}
  --           ,nullposting{lpdescription="desc",lpaccount="expenses:food",          lpamount=Mixed [usd 4]}
  --           ,nullposting{lpdescription="desc",lpaccount="expenses:food:dining",   lpamount=Mixed [usd 8]}
  --           ]
  --   ("2008/01/01","2009/01/01",0,9999,False,[]) `gives`
  --    []
  --   ("2008/01/01","2009/01/01",0,9999,True,[]) `gives`
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31"}
  --    ]
  --   ("2008/01/01","2009/01/01",0,9999,False,ts) `gives`
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses:food",          lpamount=Mixed [usd 4]}
  --    ,nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses:food:dining",   lpamount=Mixed [usd 10]}
  --    ,nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses:food:groceries",lpamount=Mixed [usd 1]}
  --    ]
  --   ("2008/01/01","2009/01/01",0,2,False,ts) `gives`
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses:food",lpamount=Mixed [usd 15]}
  --    ]
  --   ("2008/01/01","2009/01/01",0,1,False,ts) `gives`
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses",lpamount=Mixed [usd 15]}
  --    ]
  --   ("2008/01/01","2009/01/01",0,0,False,ts) `gives`
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="",lpamount=Mixed [usd 15]}
  --    ]

 ]
