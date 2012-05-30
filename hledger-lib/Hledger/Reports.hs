{-# LANGUAGE RecordWildCards #-}
{-|

Generate several common kinds of report from a journal, as \"*Report\" -
simple intermediate data structures intended to be easily rendered as
text, html, json, csv etc. by hledger commands, hamlet templates,
javascript, or whatever. This is under Hledger.Cli since it depends
on the command-line options, should move to hledger-lib later.

-}

module Hledger.Reports (
  ReportOpts(..),
  DisplayExp,
  FormatStr,
  defreportopts,
  dateSpanFromOpts,
  intervalFromOpts,
  clearedValueFromOpts,
  whichDateFromOpts,
  journalSelectingDateFromOpts,
  journalSelectingAmountFromOpts,
  queryFromOpts,
  queryOptsFromOpts,
  -- * Entries report
  EntriesReport,
  EntriesReportItem,
  entriesReport,
  -- * Postings report
  PostingsReport,
  PostingsReportItem,
  postingsReport,
  mkpostingsReportItem, -- XXX for showPostingWithBalanceForVty in Hledger.Cli.Register
  -- * Transactions report
  TransactionsReport,
  TransactionsReportItem,
  triDate,
  triBalance,
  journalTransactionsReport,
  accountTransactionsReport,
  -- * Accounts report
  AccountsReport,
  AccountsReportItem,
  accountsReport,
  isInteresting,
  -- * Tests
  tests_Hledger_Reports
)
where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time.Calendar
-- import Data.Tree
import Safe (headMay, lastMay)
import System.Console.CmdArgs  -- for defaults support
import System.Time (ClockTime(TOD))
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.Printf

import Hledger.Data
import Hledger.Read (amount')
import Hledger.Query
import Hledger.Utils

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
    ,effective_      :: Bool
    ,empty_          :: Bool
    ,no_elide_       :: Bool
    ,real_           :: Bool
    ,flat_           :: Bool -- for balance command
    ,drop_           :: Int  -- "
    ,no_total_       :: Bool -- "
    ,daily_          :: Bool
    ,weekly_         :: Bool
    ,monthly_        :: Bool
    ,quarterly_      :: Bool
    ,yearly_         :: Bool
    ,format_         :: Maybe FormatStr
    ,query_          :: String -- all arguments, as a string
 } deriving (Show)

type DisplayExp = String
type FormatStr = String

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

-- | Report which date we will report on based on --effective.
whichDateFromOpts :: ReportOpts -> WhichDate
whichDateFromOpts ReportOpts{..} = if effective_ then EffectiveDate else ActualDate

-- | Select a Transaction date accessor based on --effective.
transactionDateFn :: ReportOpts -> (Transaction -> Day)
transactionDateFn ReportOpts{..} = if effective_ then transactionEffectiveDate else transactionActualDate

-- | Convert this journal's transactions' primary date to either the
-- actual or effective date, as per options.
journalSelectingDateFromOpts :: ReportOpts -> Journal -> Journal
journalSelectingDateFromOpts opts = journalSelectingDate (whichDateFromOpts opts)

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
              [Date $ dateSpanFromOpts d opts]
              ++ (if real_ then [Real True] else [])
              ++ (if empty_ then [Empty True] else []) -- ?
              ++ (maybe [] ((:[]) . Status) (clearedValueFromOpts opts))
              ++ (maybe [] ((:[]) . Depth) depth_)
    argsq = fst $ parseQuery d query_

tests_queryFromOpts = [
 "queryFromOpts" ~: do
  assertEqual "" Any (queryFromOpts nulldate defreportopts)
  assertEqual "" (Acct "a") (queryFromOpts nulldate defreportopts{query_="a"})
  assertEqual "" (Desc "a a") (queryFromOpts nulldate defreportopts{query_="desc:'a a'"})
  assertEqual "" (Date $ mkdatespan "2012/01/01" "2013/01/01")
                 (queryFromOpts nulldate defreportopts{begin_=Just (parsedate "2012/01/01")
                                                      ,query_="date:'to 2013'"
                                                      })
  assertEqual "" (EDate $ mkdatespan "2012/01/01" "2013/01/01")
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
-- originally entered in the journal (mostly). Used by eg hledger's print
-- command and hledger-web's journal entries view.
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
type PostingsReport = (String               -- label for the running balance column XXX remove
                      ,[PostingsReportItem] -- line items, one per posting
                      )
type PostingsReportItem = (Maybe (Day, String) -- transaction date and description if this is the first posting
                          ,Posting             -- the posting, possibly with account name depth-clipped
                          ,MixedAmount         -- the running total after this posting
                          )

-- | Select postings from the journal and add running balance and other
-- information to make a postings report. Used by eg hledger's register command.
postingsReport :: ReportOpts -> Query -> Journal -> PostingsReport
postingsReport opts q j = -- trace ("q: "++show q++"\nq': "++show q') $
                          (totallabel, postingsReportItems ps nullposting depth startbal (+))
    where
      ps | interval == NoInterval = displayableps
         | otherwise              = summarisePostingsByInterval interval depth empty reportspan displayableps
      j' = journalSelectingDateFromOpts opts $ journalSelectingAmountFromOpts opts j
      -- don't do depth filtering until the end
      (depth, q') = (queryDepth q, filterQuery (not . queryIsDepth) q)
      (precedingps, displayableps, _) =   dbg "ps3" $ postingsMatchingDisplayExpr (display_ opts)
                                        $ dbg "ps2" $ filter (q' `matchesPosting`)
                                        $ dbg "ps1" $ journalPostings j'
      dbg :: Show a => String -> a -> a
      -- dbg = ltrace
      dbg = flip const

      empty = queryEmpty q
      displayexpr = display_ opts  -- XXX
      interval = intervalFromOpts opts -- XXX
      journalspan = journalDateSpan j'
      -- requestedspan should be the intersection of any span specified
      -- with period options and any span specified with display option.
      -- The latter is not easily available, fake it for now.
      requestedspan = periodspan `spanIntersect` displayspan
      periodspan = queryDateSpan effectivedate q
      effectivedate = whichDateFromOpts opts == EffectiveDate
      displayspan = postingsDateSpan ps
          where (_,ps,_) = postingsMatchingDisplayExpr displayexpr $ journalPostings j'
      matchedspan = postingsDateSpan displayableps
      reportspan | empty     = requestedspan `orDatesFrom` journalspan
                 | otherwise = requestedspan `spanIntersect` matchedspan
      startbal = sumPostings precedingps

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

totallabel = "Total"
balancelabel = "Balance"

-- | Generate postings report line items.
postingsReportItems :: [Posting] -> Posting -> Int -> MixedAmount -> (MixedAmount -> MixedAmount -> MixedAmount) -> [PostingsReportItem]
postingsReportItems [] _ _ _ _ = []
postingsReportItems (p:ps) pprev d b sumfn = i:(postingsReportItems ps p d b' sumfn)
    where
      i = mkpostingsReportItem isfirst p' b'
      p' = p{paccount=clipAccountName d $ paccount p}
      isfirst = ptransaction p /= ptransaction pprev
      b' = b `sumfn` pamount p

-- | Generate one postings report line item, given a flag indicating
-- whether to include transaction info, the posting, and the current
-- running balance.
mkpostingsReportItem :: Bool -> Posting -> MixedAmount -> PostingsReportItem
mkpostingsReportItem False p b = (Nothing, p, b)
mkpostingsReportItem True p b = (ds, p, b)
    where ds = case ptransaction p of Just (Transaction{tdate=da,tdescription=de}) -> Just (da,de)
                                      Nothing -> Just (nulldate,"")

-- | Date-sort and split a list of postings into three spans - postings matched
-- by the given display expression, and the preceding and following postings.
postingsMatchingDisplayExpr :: Maybe String -> [Posting] -> ([Posting],[Posting],[Posting])
postingsMatchingDisplayExpr d ps = (before, matched, after)
    where
      sorted = sortBy (comparing postingDate) ps
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
      summaryps = [summaryp{paccount=a,pamount=balancetoshowfor a} | a <- clippedanames]
      anames = sort $ nub $ map paccount ps
      -- aggregate balances by account, like journalToLedger, then do depth-clipping
      (_,_,exclbalof,inclbalof) = groupPostings ps
      clippedanames = nub $ map (clipAccountName depth) anames
      isclipped a = accountNameLevel a >= depth
      balancetoshowfor a =
          (if isclipped a then inclbalof else exclbalof) (if null a then "top" else a)

-------------------------------------------------------------------------------

-- | A transactions report includes a list of transactions
-- (posting-filtered and unfiltered variants), a running balance, and some
-- other information helpful for rendering a register view (a flag
-- indicating multiple other accounts and a display string describing
-- them) with or without a notion of current account(s).
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
triBalance (_,_,_,_,_,Mixed a) = case a of [] -> "0"
                                           (Amount{quantity=q}):_ -> show q

-- | Select transactions from the whole journal for a transactions report,
-- with no \"current\" account. The end result is similar to
-- "postingsReport" except it uses queries and transaction-based report
-- items and the items are most recent first. Used by eg hledger-web's
-- journal view.
journalTransactionsReport :: ReportOpts -> Journal -> Query -> TransactionsReport
journalTransactionsReport _ Journal{jtxns=ts} m = (totallabel, items)
   where
     ts' = sortBy (comparing tdate) $ filter (not . null . tpostings) $ map (filterTransactionPostings m) ts
     items = reverse $ accountTransactionsReportItems m Nothing nullmixedamt id ts'
     -- XXX items' first element should be the full transaction with all postings

-------------------------------------------------------------------------------

-- | Select transactions within one or more \"current\" accounts, and make a
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
-- Currently, reporting intervals are not supported, and report items are
-- most recent first. Used by eg hledger-web's account register view.
--
accountTransactionsReport :: ReportOpts -> Journal -> Query -> Query -> TransactionsReport
accountTransactionsReport opts j m thisacctquery = (label, items)
 where
     -- transactions affecting this account, in date order
     ts = sortBy (comparing tdate) $ filter (matchesTransaction thisacctquery) $ jtxns $
          journalSelectingDateFromOpts opts $ journalSelectingAmountFromOpts opts j
     -- starting balance: if we are filtering by a start date and nothing else,
     -- the sum of postings to this account before that date; otherwise zero.
     (startbal,label) | queryIsNull m                           = (nullmixedamt,        balancelabel)
                      | queryIsStartDateOnly (effective_ opts) m = (sumPostings priorps, balancelabel)
                      | otherwise                                 = (nullmixedamt,        totallabel)
                      where
                        priorps = -- ltrace "priorps" $
                                  filter (matchesPosting
                                          (-- ltrace "priormatcher" $
                                           And [thisacctquery, tostartdatequery]))
                                         $ transactionsPostings ts
                        tostartdatequery = Date (DateSpan Nothing startdate)
                        startdate = queryStartDate (effective_ opts) m
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

-- | An accounts report is a list of account names (full and short
-- variants) with their balances, appropriate indentation for rendering as
-- a hierarchy, and grand total.
type AccountsReport = ([AccountsReportItem] -- line items, one per account
                      ,MixedAmount          -- total balance of all accounts
                      )
type AccountsReportItem = (AccountName  -- full account name
                          ,AccountName  -- short account name for display (the leaf name, prefixed by any boring parents immediately above)
                          ,Int          -- how many steps to indent this account (0-based account depth excluding boring parents)
                          ,MixedAmount) -- account balance, includes subs unless --flat is present

-- | Select accounts, and get their balances at the end of the selected
-- period, and misc. display information, for an accounts report.
accountsReport :: ReportOpts -> Query -> Journal -> AccountsReport
accountsReport opts q j = (items, total)
    where
      -- don't do depth filtering until the end
      q1 = filterQuery (not . queryIsDepth) q
      q2 = filterQuery queryIsDepth q
      l =  journalToLedger q1 $ journalSelectingDateFromOpts opts $ journalSelectingAmountFromOpts opts j
      acctnames = filter (q2 `matchesAccount`) $ ledgerAccountNames l
      interestingaccts | no_elide_ opts = acctnames
                       | otherwise = filter (isInteresting opts l) acctnames
      items = map mkitem interestingaccts
      total = sum $ map abalance $ ledgerTopAccounts l

      -- | Get data for one balance report line item.
      mkitem :: AccountName -> AccountsReportItem
      mkitem a = (a, adisplay, indent, abal)
          where
            adisplay | flat_ opts = a
                     | otherwise = accountNameFromComponents $ reverse (map accountLeafName ps) ++ [accountLeafName a]
                where ps = takeWhile boring parents where boring = not . (`elem` interestingparents)
            indent | flat_ opts = 0
                   | otherwise = length interestingparents
            interestingparents = filter (`elem` interestingaccts) parents
            parents = parentAccountNames a
            abal | flat_ opts = exclusiveBalance acct
                 | otherwise = abalance acct
                 where acct = ledgerAccount l a

tests_accountsReport =
  let (opts,journal) `gives` r = do
         let (eitems, etotal) = r
             (aitems, atotal) = accountsReport opts (queryFromOpts nulldate opts) journal
         assertEqual "items" eitems aitems
         -- assertEqual "" (length eitems) (length aitems)
         -- mapM (\(e,a) -> assertEqual "" e a) $ zip eitems aitems
         assertEqual "total" etotal atotal
  in [

   "accountsReport with no args on null journal" ~: do
   (defreportopts, nulljournal) `gives` ([], Mixed [nullamt])

  ,"accountsReport with no args on sample journal" ~: do
   (defreportopts, samplejournal) `gives`
    ([
      ("assets","assets",0, amount' "$-1.00")
     ,("assets:bank:saving","bank:saving",1, amount' "$1.00")
     ,("assets:cash","cash",1, amount' "$-2.00")
     ,("expenses","expenses",0, amount' "$2.00")
     ,("expenses:food","food",1, amount' "$1.00")
     ,("expenses:supplies","supplies",1, amount' "$1.00")
     ,("income","income",0, amount' "$-2.00")
     ,("income:gifts","gifts",1, amount' "$-1.00")
     ,("income:salary","salary",1, amount' "$-1.00")
     ,("liabilities:debts","liabilities:debts",0, amount' "$1.00")
     ],
     Mixed [nullamt])

  ,"accountsReport with --depth=N" ~: do
   (defreportopts{depth_=Just 1}, samplejournal) `gives`
    ([
      ("assets",      "assets",      0, amount' "$-1.00")
     ,("expenses",    "expenses",    0, amount'  "$2.00")
     ,("income",      "income",      0, amount' "$-2.00")
     ,("liabilities", "liabilities", 0, amount'  "$1.00")
     ],
     Mixed [nullamt])

  ,"accountsReport with depth:N" ~: do
   (defreportopts{query_="depth:1"}, samplejournal) `gives`
    ([
      ("assets",      "assets",      0, amount' "$-1.00")
     ,("expenses",    "expenses",    0, amount'  "$2.00")
     ,("income",      "income",      0, amount' "$-2.00")
     ,("liabilities", "liabilities", 0, amount'  "$1.00")
     ],
     Mixed [nullamt])

  ,"accountsReport with a date or effective date span" ~: do
   (defreportopts{query_="date:'in 2009'"}, samplejournal2) `gives`
    ([],
     Mixed [nullamt])
   (defreportopts{query_="edate:'in 2009'"}, samplejournal2) `gives`
    ([
      ("assets:bank:checking","assets:bank:checking",0,amount' "$1.00")
     ,("income:salary","income:salary",0,amount' "$-1.00")
     ],
     Mixed [nullamt])

  ,"accountsReport with desc:" ~: do
   (defreportopts{query_="desc:income"}, samplejournal) `gives`
    ([
      ("assets:bank:checking","assets:bank:checking",0,amount' "$1.00")
     ,("income:salary","income:salary",0, amount' "$-1.00")
     ],
     Mixed [nullamt])

  ,"accountsReport with not:desc:" ~: do
   (defreportopts{query_="not:desc:income"}, samplejournal) `gives`
    ([
      ("assets","assets",0, amount' "$-2.00")
     ,("assets:bank","bank",1, Mixed [nullamt])
     ,("assets:bank:checking","checking",2,amount' "$-1.00")
     ,("assets:bank:saving","saving",2, amount' "$1.00")
     ,("assets:cash","cash",1, amount' "$-2.00")
     ,("expenses","expenses",0, amount' "$2.00")
     ,("expenses:food","food",1, amount' "$1.00")
     ,("expenses:supplies","supplies",1, amount' "$1.00")
     ,("income:gifts","income:gifts",0, amount' "$-1.00")
     ,("liabilities:debts","liabilities:debts",0, amount' "$1.00")
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
       accountsReportAsText defreportopts (accountsReport defreportopts Any j') `is`
         ["                $500  a:b"
         ,"               $-500  c:d"
         ,"--------------------"
         ,"                   0"
         ]
-}
 ]

Right samplejournal2 = journalBalanceTransactions $ Journal
          [] 
          [] 
          [
           txnTieKnot $ Transaction {
             tdate=parsedate "2008/01/01",
             teffectivedate=Just $ parsedate "2009/01/01",
             tstatus=False,
             tcode="",
             tdescription="income",
             tcomment="",
             ttags=[],
             tpostings=[
              Posting {
                pstatus=False,
                paccount="assets:bank:checking",
                pamount=(Mixed [dollars 1]),
                pcomment="",
                ptype=RegularPosting,
                ptags=[],
                ptransaction=Nothing
              },
              Posting {
                pstatus=False,
                paccount="income:salary",
                pamount=(missingmixedamt),
                pcomment="",
                ptype=RegularPosting,
                ptags=[],
                ptransaction=Nothing
              }
             ],
             tpreceding_comment_lines=""
           }
          ]
          []
          []
          ""
          nullctx
          []
          (TOD 0 0)

exclusiveBalance :: Account -> MixedAmount
exclusiveBalance = sumPostings . apostings

-- | Is the named account considered interesting for this ledger's accounts report,
-- following the eliding style of ledger's balance command ?
isInteresting :: ReportOpts -> Ledger -> AccountName -> Bool
isInteresting opts l a | flat_ opts = isInterestingFlat opts l a
                       | otherwise = isInterestingIndented opts l a

-- | Determine whether an account should get its own line in the --flat balance report.
isInterestingFlat :: ReportOpts -> Ledger -> AccountName -> Bool
isInterestingFlat opts l a = notempty || emptyflag
    where
      acct = ledgerAccount l a
      notempty = not $ isZeroMixedAmount $ exclusiveBalance acct
      emptyflag = empty_ opts

-- | Determine whether an account should get its own line in the indented
-- balance report.  Cf Balance module doc.
isInterestingIndented :: ReportOpts -> Ledger -> AccountName -> Bool
isInterestingIndented opts l a
    | numinterestingsubs == 1 && samebalanceassub && not atmaxdepth = False
    | numinterestingsubs < 2 && zerobalance && not emptyflag = False
    | otherwise = True
    where
      atmaxdepth = accountNameLevel a == depthFromOpts opts
      emptyflag = empty_ opts
      acct = ledgerAccount l a
      zerobalance = isZeroMixedAmount inclbalance where inclbalance = abalance acct
      samebalanceassub = isZeroMixedAmount exclbalance where exclbalance = sumPostings $ apostings acct
      numinterestingsubs = length $ filter isInterestingTree subtrees
          where
            isInterestingTree = treeany (isInteresting opts l . aname)
            subtrees = map (fromJust . ledgerAccountTreeAt l) $ ledgerSubAccounts l $ ledgerAccount l a

tests_isInterestingIndented = [
  "isInterestingIndented" ~: do 
   let (opts, journal, acctname) `gives` r = isInterestingIndented opts l acctname `is` r
          where l = journalToLedger (queryFromOpts nulldate opts) journal
     
   (defreportopts, samplejournal, "expenses") `gives` True
 ]

depthFromOpts :: ReportOpts -> Int
depthFromOpts opts = min (fromMaybe 99999 $ depth_ opts) (queryDepth $ queryFromOpts nulldate opts)

-------------------------------------------------------------------------------

tests_Hledger_Reports :: Test
tests_Hledger_Reports = TestList $
    tests_queryFromOpts
 ++ tests_queryOptsFromOpts
 ++ tests_entriesReport
 ++ tests_summarisePostingsByInterval
 ++ tests_postingsReport
 ++ tests_isInterestingIndented
 ++ tests_accountsReport
 ++ [
  -- ,"summarisePostingsInDateSpan" ~: do
  --   let gives (b,e,depth,showempty,ps) =
  --           (summarisePostingsInDateSpan (mkdatespan b e) depth showempty ps `is`)
  --   let ps =
  --           [
  --            nullposting{lpdescription="desc",lpaccount="expenses:food:groceries",lpamount=Mixed [dollars 1]}
  --           ,nullposting{lpdescription="desc",lpaccount="expenses:food:dining",   lpamount=Mixed [dollars 2]}
  --           ,nullposting{lpdescription="desc",lpaccount="expenses:food",          lpamount=Mixed [dollars 4]}
  --           ,nullposting{lpdescription="desc",lpaccount="expenses:food:dining",   lpamount=Mixed [dollars 8]}
  --           ]
  --   ("2008/01/01","2009/01/01",0,9999,False,[]) `gives`
  --    []
  --   ("2008/01/01","2009/01/01",0,9999,True,[]) `gives`
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31"}
  --    ]
  --   ("2008/01/01","2009/01/01",0,9999,False,ts) `gives`
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses:food",          lpamount=Mixed [dollars 4]}
  --    ,nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses:food:dining",   lpamount=Mixed [dollars 10]}
  --    ,nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses:food:groceries",lpamount=Mixed [dollars 1]}
  --    ]
  --   ("2008/01/01","2009/01/01",0,2,False,ts) `gives`
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses:food",lpamount=Mixed [dollars 15]}
  --    ]
  --   ("2008/01/01","2009/01/01",0,1,False,ts) `gives`
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses",lpamount=Mixed [dollars 15]}
  --    ]
  --   ("2008/01/01","2009/01/01",0,0,False,ts) `gives`
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="",lpamount=Mixed [dollars 15]}
  --    ]

 ]
