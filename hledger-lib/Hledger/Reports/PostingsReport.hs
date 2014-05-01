{-# LANGUAGE RecordWildCards, DeriveDataTypeable, FlexibleInstances #-}
{-|

Postings report, used by the register command.

-}

module Hledger.Reports.PostingsReport (
  PostingsReport,
  PostingsReportItem,
  postingsReport,
  mkpostingsReportItem,

  -- * Tests
  tests_Hledger_Reports_PostingsReport
)
where

import Data.List
import Data.Maybe
import Data.Time.Calendar
import Safe (headMay, lastMay)
import Test.HUnit

import Hledger.Data
import Hledger.Query
import Hledger.Utils
import Hledger.Reports.ReportOptions


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
postingsReport opts q j = (totallabel, items)
    where
      -- figure out adjusted queries & spans like multiBalanceReport
      symq = dbg "symq"   $ filterQuery queryIsSym $ dbg "requested q" q
      depth = queryDepth q
      depthless = filterQuery (not . queryIsDepth)
      datelessq = filterQuery (not . queryIsDate) q
      dateqcons  = if date2_ opts then Date2 else Date
      requestedspan  = dbg "requestedspan"  $ queryDateSpan (date2_ opts) q                              -- span specified by -b/-e/-p options and query args
      requestedspan' = dbg "requestedspan'" $ requestedspan `spanDefaultsFrom` journalDateSpan (date2_ opts) j  -- if open-ended, close it using the journal's end dates
      intervalspans  = dbg "intervalspans"  $ splitSpan (intervalFromOpts opts) requestedspan' -- interval spans enclosing it
      reportstart    = dbg "reportstart"    $ maybe Nothing spanStart $ headMay intervalspans
      reportend      = dbg "reportend"      $ maybe Nothing spanEnd   $ lastMay intervalspans
      reportspan     = dbg "reportspan"     $ DateSpan reportstart reportend  -- the requested span enlarged to a whole number of intervals
      beforestartq   = dbg "beforestartq"   $ dateqcons $ DateSpan Nothing reportstart
      beforeendq     = dbg "beforeendq"     $ dateqcons $ DateSpan Nothing reportend
      reportq        = dbg "reportq"        $ depthless $ And [datelessq, beforeendq] -- user's query with no start date, end date on an interval boundary and no depth limit

      pstoend = 
          dbg "ps4" $ map (filterPostingAmount symq) $                            -- remove amount parts which the query's cur: terms would exclude
          dbg "ps3" $ (if related_ opts then concatMap relatedPostings else id) $ -- with -r, replace each with its sibling postings
          dbg "ps2" $ filter (reportq `matchesPosting`) $                         -- filter postings by the query, including before the report start date, ignoring depth
          dbg "ps1" $ journalPostings $ journalSelectingAmountFromOpts opts j
      (precedingps, reportps) = dbg "precedingps, reportps" $ span (beforestartq `matchesPosting`) pstoend

      empty = queryEmpty q
      -- displayexpr = display_ opts  -- XXX
      interval = intervalFromOpts opts -- XXX

      whichdate = whichDateFromOpts opts
      itemps | interval == NoInterval = reportps
             | otherwise              = summarisePostingsByInterval interval whichdate depth empty reportspan reportps
      items = postingsReportItems itemps nullposting whichdate depth startbal runningcalc 1
        where
          startbal = if balancetype_ opts == HistoricalBalance then sumPostings precedingps else 0
          runningcalc | average_ opts = \i avg amt -> avg + (amt - avg) `divideMixedAmount` (fromIntegral i) -- running average
                      | otherwise     = \_ bal amt -> bal + amt                                              -- running total

      dbg s = let p = "postingsReport" in Hledger.Utils.dbg (p++" "++s)  -- add prefix in debug output
      -- dbg = const id  -- exclude from debug output

totallabel = "Total"

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

-- | Convert a list of postings into summary postings. Summary postings
-- are one per account per interval and aggregated to the specified depth
-- if any.
summarisePostingsByInterval :: Interval -> WhichDate -> Int -> Bool -> DateSpan -> [Posting] -> [Posting]
summarisePostingsByInterval interval wd depth empty reportspan ps = concatMap summarisespan $ splitSpan interval reportspan
    where
      summarisespan s = summarisePostingsInDateSpan s wd depth empty (postingsinspan s)
      postingsinspan s = filter (isPostingInDateSpan' wd s) ps

tests_summarisePostingsByInterval = [
  "summarisePostingsByInterval" ~: do
    summarisePostingsByInterval (Quarters 1) PrimaryDate 99999 False (DateSpan Nothing Nothing) [] ~?= []
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
summarisePostingsInDateSpan :: DateSpan -> WhichDate -> Int -> Bool -> [Posting] -> [Posting]
summarisePostingsInDateSpan (DateSpan b e) wd depth showempty ps
    | null ps && (isNothing b || isNothing e) = []
    | null ps && showempty = [summaryp]
    | otherwise = summaryps'
    where
      summaryp = summaryPosting b' ("- "++ showDate (addDays (-1) e'))
      b' = fromMaybe (maybe nulldate postingdate $ headMay ps) b
      e' = fromMaybe (maybe (addDays 1 nulldate) postingdate $ lastMay ps) e
      postingdate = if wd == PrimaryDate then postingDate else postingDate2
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

-- tests_summarisePostingsInDateSpan = [
  --  "summarisePostingsInDateSpan" ~: do
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

tests_Hledger_Reports_PostingsReport :: Test
tests_Hledger_Reports_PostingsReport = TestList $
    tests_summarisePostingsByInterval
 ++ tests_postingsReport

