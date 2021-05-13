{-|

Postings report, used by the register command.

-}

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Hledger.Reports.PostingsReport (
  PostingsReport,
  PostingsReportItem,
  postingsReport,
  mkpostingsReportItem,

  -- * Tests
  tests_PostingsReport
)
where

import Data.List (nub, sortOn)
import Data.List.Extra (nubSort)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Time.Calendar (Day, addDays)
import Safe (headMay, lastMay)

import Hledger.Data
import Hledger.Query
import Hledger.Utils
import Hledger.Reports.ReportOptions


-- | A postings report is a list of postings with a running total, and a little extra
-- transaction info to help with rendering.
-- This is used eg for the register command.
type PostingsReport = [PostingsReportItem] -- line items, one per posting
type PostingsReportItem = (Maybe Day    -- The posting date, if this is the first posting in a
                                        -- transaction or if it's different from the previous
                                        -- posting's date. Or if this a summary posting, the
                                        -- report interval's start date if this is the first
                                        -- summary posting in the interval.
                          ,Maybe Day    -- If this is a summary posting, the report interval's
                                        -- end date if this is the first summary posting in
                                        -- the interval.
                          ,Maybe Text   -- The posting's transaction's description, if this is the first posting in the transaction.
                          ,Posting      -- The posting, possibly with the account name depth-clipped.
                          ,MixedAmount  -- The running total after this posting, or with --average,
                                        -- the running average posting amount. With --historical,
                                        -- postings before the report start date are included in
                                        -- the running total/average.
                          )

-- | A summary posting summarises the activity in one account within a report
-- interval. It is kludgily represented by a regular Posting with no description,
-- the interval's start date stored as the posting date, and the interval's end
-- date attached with a tuple.
type SummaryPosting = (Posting, Day)

-- | Select postings from the journal and add running balance and other
-- information to make a postings report. Used by eg hledger's register command.
postingsReport :: ReportSpec -> Journal -> PostingsReport
postingsReport rspec@ReportSpec{rsOpts=ropts@ReportOpts{..}} j = items
    where
      reportspan  = reportSpanBothDates j rspec
      whichdate   = whichDateFromOpts ropts
      mdepth      = queryDepth $ rsQuery rspec
      multiperiod = interval_ /= NoInterval

      -- postings to be included in the report, and similarly-matched postings before the report start date
      (precedingps, reportps) = matchedPostingsBeforeAndDuring rspec j reportspan

      -- Postings, or summary postings with their subperiod's end date, to be displayed.
      displayps :: [(Posting, Maybe Day)]
        | multiperiod = [(p, Just periodend) | (p, periodend) <- summariseps reportps]
        | otherwise   = [(p, Nothing) | p <- reportps]
        where
          summariseps = summarisePostingsByInterval interval_ whichdate mdepth showempty reportspan
          showempty = empty_ || average_

      -- Posting report items ready for display.
      items =
        dbg4 "postingsReport items" $
        postingsReportItems displayps (nullposting,Nothing) whichdate mdepth startbal runningcalc startnum
        where
          -- In historical mode we'll need a starting balance, which we
          -- may be converting to value per hledger_options.m4.md "Effect
          -- of --value on reports".
          -- XXX balance report doesn't value starting balance.. should this ?
          historical = balancetype_ == HistoricalBalance
          startbal | average_  = if historical then precedingavg else nullmixedamt
                   | otherwise = if historical then precedingsum else nullmixedamt
            where
              precedingsum = sumPostings precedingps
              precedingavg = divideMixedAmount (fromIntegral $ length precedingps) precedingsum

          runningcalc = registerRunningCalculationFn ropts
          startnum = if historical then length precedingps + 1 else 1

-- | Based on the given report options, return a function that does the appropriate
-- running calculation for the register report, ie a running average or running total.
-- This function will take the item number, previous average/total, and new posting amount,
-- and return the new average/total.
registerRunningCalculationFn :: ReportOpts -> (Int -> MixedAmount -> MixedAmount -> MixedAmount)
registerRunningCalculationFn ropts
  | average_ ropts = \i avg amt -> avg `maPlus` divideMixedAmount (fromIntegral i) (amt `maMinus` avg)
  | otherwise      = \_ bal amt -> bal `maPlus` amt

-- | Find postings matching a given query, within a given date span,
-- and also any similarly-matched postings before that date span.
-- Date restrictions and depth restrictions in the query are ignored.
-- A helper for the postings report.
matchedPostingsBeforeAndDuring :: ReportSpec -> Journal -> DateSpan -> ([Posting],[Posting])
matchedPostingsBeforeAndDuring rspec@ReportSpec{rsOpts=ropts,rsQuery=q} j reportspan =
  dbg5 "beforeps, duringps" $ span (beforestartq `matchesPosting`) beforeandduringps
  where
    beforestartq = dbg3 "beforestartq" $ dateqtype $ DateSpan Nothing $ spanStart reportspan
    beforeandduringps =
      dbg5 "ps5" $ sortOn sortdate $                                             -- sort postings by date or date2
      dbg5 "ps4" $ (if invert_ ropts then map negatePostingAmount else id) $     -- with --invert, invert amounts
      dbg5 "ps3" $ map (filterPostingAmount symq) $                              -- remove amount parts which the query's cur: terms would exclude
      dbg5 "ps2" $ (if related_ ropts then concatMap relatedPostings else id) $  -- with -r, replace each with its sibling postings
      dbg5 "ps1" $ filter (beforeandduringq `matchesPosting`) $                  -- filter postings by the query, with no start date or depth limit
                  journalPostings $
                  journalApplyValuationFromOpts rspec j                          -- convert to cost and apply valuation
      where
        beforeandduringq = dbg4 "beforeandduringq" $ And [depthless $ dateless q, beforeendq]
          where
            depthless  = filterQuery (not . queryIsDepth)
            dateless   = filterQuery (not . queryIsDateOrDate2)
            beforeendq = dateqtype $ DateSpan Nothing $ spanEnd reportspan
        sortdate = if date2_ ropts then postingDate2 else postingDate
        symq = dbg4 "symq" $ filterQuery queryIsSym q
    dateqtype
      | queryIsDate2 dateq || (queryIsDate dateq && date2_ ropts) = Date2
      | otherwise = Date
      where
        dateq = dbg4 "dateq" $ filterQuery queryIsDateOrDate2 $ dbg4 "q" q  -- XXX confused by multiple date:/date2: ?

-- | Generate postings report line items from a list of postings or (with
-- non-Nothing dates attached) summary postings.
postingsReportItems :: [(Posting,Maybe Day)] -> (Posting,Maybe Day) -> WhichDate -> Maybe Int -> MixedAmount -> (Int -> MixedAmount -> MixedAmount -> MixedAmount) -> Int -> [PostingsReportItem]
postingsReportItems [] _ _ _ _ _ _ = []
postingsReportItems ((p,menddate):ps) (pprev,menddateprev) wd d b runningcalcfn itemnum =
    i:(postingsReportItems ps (p,menddate) wd d b' runningcalcfn (itemnum+1))
  where
    i = mkpostingsReportItem showdate showdesc wd menddate p' b'
    (showdate, showdesc) | isJust menddate = (menddate /= menddateprev,        False)
                         | otherwise       = (isfirstintxn || isdifferentdate, isfirstintxn)
    isfirstintxn = ptransaction p /= ptransaction pprev
    isdifferentdate = case wd of PrimaryDate   -> postingDate p  /= postingDate pprev
                                 SecondaryDate -> postingDate2 p /= postingDate2 pprev
    p' = p{paccount= clipOrEllipsifyAccountName d $ paccount p}
    b' = runningcalcfn itemnum b $ pamount p

-- | Generate one postings report line item, containing the posting,
-- the current running balance, and optionally the posting date and/or
-- the transaction description.
mkpostingsReportItem :: Bool -> Bool -> WhichDate -> Maybe Day -> Posting -> MixedAmount -> PostingsReportItem
mkpostingsReportItem showdate showdesc wd menddate p b =
  (if showdate then Just date else Nothing
  ,menddate
  ,if showdesc then tdescription <$> ptransaction p else Nothing
  ,p
  ,b
  )
  where
    date = case wd of PrimaryDate   -> postingDate p
                      SecondaryDate -> postingDate2 p

-- | Convert a list of postings into summary postings, one per interval,
-- aggregated to the specified depth if any.
-- Each summary posting will have a non-Nothing interval end date.
summarisePostingsByInterval :: Interval -> WhichDate -> Maybe Int -> Bool -> DateSpan -> [Posting] -> [SummaryPosting]
summarisePostingsByInterval interval wd mdepth showempty reportspan ps = concatMap summarisespan $ splitSpan interval reportspan
  where
    summarisespan s = summarisePostingsInDateSpan s wd mdepth showempty (postingsinspan s)
    postingsinspan s = filter (isPostingInDateSpan' wd s) ps

-- | Given a date span (representing a report interval) and a list of
-- postings within it, aggregate the postings into one summary posting per
-- account. Each summary posting will have a non-Nothing interval end date.
--
-- When a depth argument is present, postings to accounts of greater
-- depth are also aggregated where possible. If the depth is 0, all
-- postings in the span are aggregated into a single posting with
-- account name "...".
--
-- The showempty flag includes spans with no postings and also postings
-- with 0 amount.
--
summarisePostingsInDateSpan :: DateSpan -> WhichDate -> Maybe Int -> Bool -> [Posting] -> [SummaryPosting]
summarisePostingsInDateSpan (DateSpan b e) wd mdepth showempty ps
  | null ps && (isNothing b || isNothing e) = []
  | null ps && showempty = [(summaryp, e')]
  | otherwise = summarypes
  where
    postingdate = if wd == PrimaryDate then postingDate else postingDate2
    b' = fromMaybe (maybe nulldate postingdate $ headMay ps) b
    e' = fromMaybe (maybe (addDays 1 nulldate) postingdate $ lastMay ps) e
    summaryp = nullposting{pdate=Just b'}
    clippedanames = nub $ map (clipAccountName mdepth) anames
    summaryps | mdepth == Just 0 = [summaryp{paccount="...",pamount=sumPostings ps}]
              | otherwise        = [summaryp{paccount=a,pamount=balance a} | a <- clippedanames]
    summarypes = map (, e') $ (if showempty then id else filter (not . mixedAmountLooksZero . pamount)) summaryps
    anames = nubSort $ map paccount ps
    -- aggregate balances by account, like ledgerFromJournal, then do depth-clipping
    accts = accountsFromPostings ps
    balance a = maybe nullmixedamt bal $ lookupAccount a accts
      where
        bal = if isclipped a then aibalance else aebalance
        isclipped a = maybe True (accountNameLevel a >=) mdepth

negatePostingAmount :: Posting -> Posting
negatePostingAmount = postingTransformAmount negate


-- tests

tests_PostingsReport = tests "PostingsReport" [

   test "postingsReport" $ do
    let (query, journal) `gives` n = (length $ postingsReport defreportspec{rsQuery=query} journal) @?= n
    -- with the query specified explicitly
    (Any, nulljournal) `gives` 0
    (Any, samplejournal) `gives` 13
    -- register --depth just clips account names
    (Depth 2, samplejournal) `gives` 13
    (And [Depth 1, StatusQ Cleared, Acct (toRegex' "expenses")], samplejournal) `gives` 2
    (And [And [Depth 1, StatusQ Cleared], Acct (toRegex' "expenses")], samplejournal) `gives` 2
    -- with query and/or command-line options
    (length $ postingsReport defreportspec samplejournal) @?= 13
    (length $ postingsReport defreportspec{rsOpts=defreportopts{interval_=Months 1}} samplejournal) @?= 11
    (length $ postingsReport defreportspec{rsOpts=defreportopts{interval_=Months 1, empty_=True}} samplejournal) @?= 20
    (length $ postingsReport defreportspec{rsQuery=Acct $ toRegex' "assets:bank:checking"} samplejournal) @?= 5

     -- (defreportopts, And [Acct "a a", Acct "'b"], samplejournal2) `gives` 0
     -- [(Just (fromGregorian 2008 01 01,"income"),assets:bank:checking             $1,$1)
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
                    where opts = defreportopts
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

  ,test "summarisePostingsByInterval" $
    summarisePostingsByInterval (Quarters 1) PrimaryDate Nothing False (DateSpan Nothing Nothing) [] @?= []

  -- ,tests_summarisePostingsInDateSpan = [
    --  "summarisePostingsInDateSpan" ~: do
    --   let gives (b,e,depth,showempty,ps) =
    --           (summarisePostingsInDateSpan (DateSpan b e) depth showempty ps `is`)
    --   let ps =
    --           [
    --            nullposting{lpdescription="desc",lpaccount="expenses:food:groceries",lpamount=mixedAmount (usd 1)}
    --           ,nullposting{lpdescription="desc",lpaccount="expenses:food:dining",   lpamount=mixedAmount (usd 2)}
    --           ,nullposting{lpdescription="desc",lpaccount="expenses:food",          lpamount=mixedAmount (usd 4)}
    --           ,nullposting{lpdescription="desc",lpaccount="expenses:food:dining",   lpamount=mixedAmount (usd 8)}
    --           ]
    --   ("2008/01/01","2009/01/01",0,9999,False,[]) `gives`
    --    []
    --   ("2008/01/01","2009/01/01",0,9999,True,[]) `gives`
    --    [
    --     nullposting{lpdate=fromGregorian 2008 01 01,lpdescription="- 2008/12/31"}
    --    ]
    --   ("2008/01/01","2009/01/01",0,9999,False,ts) `gives`
    --    [
    --     nullposting{lpdate=fromGregorian 2008 01 01,lpdescription="- 2008/12/31",lpaccount="expenses:food",          lpamount=mixedAmount (usd 4)}
    --    ,nullposting{lpdate=fromGregorian 2008 01 01,lpdescription="- 2008/12/31",lpaccount="expenses:food:dining",   lpamount=mixedAmount (usd 10)}
    --    ,nullposting{lpdate=fromGregorian 2008 01 01,lpdescription="- 2008/12/31",lpaccount="expenses:food:groceries",lpamount=mixedAmount (usd 1)}
    --    ]
    --   ("2008/01/01","2009/01/01",0,2,False,ts) `gives`
    --    [
    --     nullposting{lpdate=fromGregorian 2008 01 01,lpdescription="- 2008/12/31",lpaccount="expenses:food",lpamount=mixedAmount (usd 15)}
    --    ]
    --   ("2008/01/01","2009/01/01",0,1,False,ts) `gives`
    --    [
    --     nullposting{lpdate=fromGregorian 2008 01 01,lpdescription="- 2008/12/31",lpaccount="expenses",lpamount=mixedAmount (usd 15)}
    --    ]
    --   ("2008/01/01","2009/01/01",0,0,False,ts) `gives`
    --    [
    --     nullposting{lpdate=fromGregorian 2008 01 01,lpdescription="- 2008/12/31",lpaccount="",lpamount=mixedAmount (usd 15)}
    --    ]

 ]
