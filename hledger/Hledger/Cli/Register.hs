{-# LANGUAGE CPP #-}
{-| 

A ledger-compatible @register@ command.

-}

module Hledger.Cli.Register (
  RegisterReport
 ,RegisterReportItem
 ,register
 ,registerReport
 ,accountRegisterReport
 ,registerReportAsText
 ,showPostingWithBalanceForVty
 ,tests_Hledger_Cli_Register
) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time.Calendar
import Safe (headMay, lastMay)
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.Printf

import Hledger.Cli.Options
import Hledger.Cli.Utils
import Hledger.Data
import Hledger.Utils
import Prelude hiding (putStr)
import Hledger.Utils.UTF8 (putStr)


-- | A register report is a list of postings to an account or set of
-- accounts, with a running total. Postings may be actual postings, or
-- virtual postings aggregated over a reporting interval.
-- And also some heading info.
type RegisterReport = (String               -- a possibly null label for the running balance column
                      ,[RegisterReportItem] -- line items, one per posting
                      )

-- | The data for a single register report line item, representing one posting.
type RegisterReportItem = (Maybe (Day, String) -- transaction date and description if this is the first posting
                          ,Posting             -- the posting
                          ,MixedAmount         -- balance so far
                          )

-- | Print a register report.
register :: [Opt] -> [String] -> Journal -> IO ()
register opts args j = do
  d <- getCurrentDay
  putStr $ registerReportAsText opts $ registerReport opts (optsToFilterSpec opts args d) j

-- | Render a register report as plain text suitable for console output.
registerReportAsText :: [Opt] -> RegisterReport -> String
registerReportAsText opts = unlines . map (registerReportItemAsText opts) . snd

-- | Render one register report line item as plain text. Eg:
-- @
-- date (10)  description (20)     account (22)            amount (11)  balance (12)
-- DDDDDDDDDD dddddddddddddddddddd aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
-- ^ displayed for first postings^
--   only, otherwise blank
-- @
registerReportItemAsText :: [Opt] -> RegisterReportItem -> String
registerReportItemAsText _ (dd, p, b) = concatTopPadded [datedesc, pstr, " ", bal]
    where
      datedesc = case dd of Nothing -> replicate datedescwidth ' '
                            Just (da, de) -> printf "%s %s " date desc
                                where
                                  date = showDate da
                                  desc = printf ("%-"++(show descwidth)++"s") $ elideRight descwidth de :: String
          where
            descwidth = datedescwidth - datewidth - 2
            datedescwidth = 32
            datewidth = 10
      pstr = showPostingForRegister p
      bal = padleft 12 (showMixedAmountOrZeroWithoutPrice b)

showPostingWithBalanceForVty showtxninfo p b = registerReportItemAsText [] $ mkitem showtxninfo p b

-- | Get a register report with the specified options for this journal.
-- This is a journal register report, covering the whole journal like
-- ledger's register command; for an account-specific register see
-- accountRegisterReport.
registerReport :: [Opt] -> FilterSpec -> Journal -> RegisterReport
registerReport opts fspec j = (totallabel,postingsToRegisterReportItems ps nullposting startbal (+))
    where
      ps | interval == NoInterval = displayableps
         | otherwise              = summarisePostingsByInterval interval depth empty filterspan displayableps
      (precedingps, displayableps, _) = postingsMatchingDisplayExpr (displayExprFromOpts opts)
                                        $ depthClipPostings depth
                                        $ journalPostings
                                        $ filterJournalPostings fspec{depth=Nothing}
                                        $ journalSelectingDateFromOpts opts
                                        $ journalSelectingAmountFromOpts opts
                                        j
      startbal = sumPostings precedingps
      filterspan = datespan fspec
      (interval, depth, empty) = (intervalFromOpts opts, depthFromOpts opts, Empty `elem` opts)

-- | Get an account register report with the specified options for this
-- journal.  An account register report is like the traditional account
-- register seen in bank statements and personal finance programs.  It is
-- focussed on one account only; it shows this account's transactions'
-- postings to other accounts; and if there is no transaction filtering in
-- effect other than a start date, it shows a historically-accurate
-- running balance for this account. Once additional filters are applied,
-- the running balance reverts to a running total starting at 0.
--
-- Does not handle reporting intervals.
--
accountRegisterReport :: [Opt] -> Journal -> Matcher -> AccountName -> RegisterReport
accountRegisterReport opts j m a = (label, postingsToRegisterReportItems displayps nullposting startbal (-))
 where
      -- displayps' | interval == NoInterval = displayps
      --            | otherwise              = summarisePostingsByInterval interval depth empty filterspan displayps

     -- transactions affecting this account
     a' = accountNameToAccountOnlyRegex a
     thisacctmatcher = MatchAcct True a'
     ts = filter (matchesTransaction thisacctmatcher) $ jtxns j

     -- all postings in these transactions
     ps = transactionsPostings ts

     -- starting balance: if we are filtering by a start date and nothing else
     -- else, the sum of postings to this account before it; otherwise zero.
     (startbal,label) | matcherIsNull m = (nullmixedamt,balancelabel)
                      | matcherIsStartDateOnly effective m = (sumPostings priorps,balancelabel)
                      | otherwise = (nullmixedamt,totallabel)
                      where
                        priorps = -- ltrace "priorps" $
                                  filter (matchesPosting
                                          (-- ltrace "priormatcher" $
                                           MatchAnd [thisacctmatcher, tostartdatematcher])) ps
                        tostartdatematcher = MatchDate True (DateSpan Nothing startdate)
                        startdate = matcherStartDate effective m
                        effective = Effective `elem` opts

     -- postings to display: this account's transactions' "other" postings, with any additional filter applied
     -- XXX would be better to collapse multiple postings from one txn into one (expandable) "split" item
     displayps = -- ltrace "displayps" $
                 filter (matchesPosting $
                         -- ltrace "displaymatcher" $
                         (MatchAnd [negateMatcher thisacctmatcher, m])) ps

totallabel = "Total"
balancelabel = "Balance"

-- | Generate register report line items.
postingsToRegisterReportItems :: [Posting] -> Posting -> MixedAmount -> (MixedAmount -> MixedAmount -> MixedAmount) -> [RegisterReportItem]
postingsToRegisterReportItems [] _ _ _ = []
postingsToRegisterReportItems (p:ps) pprev b sumfn = i:(postingsToRegisterReportItems ps p b' sumfn)
    where
      i = mkitem isfirst p b'
      isfirst = ptransaction p /= ptransaction pprev
      b' = b `sumfn` pamount p

-- | Generate one register report line item, from a flag indicating
-- whether to include transaction info, a posting, and the current running
-- balance.
mkitem :: Bool -> Posting -> MixedAmount -> RegisterReportItem
mkitem False p b = (Nothing, p, b)
mkitem True p b = (ds, p, b)
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

-- XXX confusing, refactor

-- | Convert a list of postings into summary postings. Summary postings
-- are one per account per interval and aggregated to the specified depth
-- if any.
summarisePostingsByInterval :: Interval -> Maybe Int -> Bool -> DateSpan -> [Posting] -> [Posting]
summarisePostingsByInterval interval depth empty filterspan ps = concatMap summarisespan $ splitSpan interval reportspan
    where
      summarisespan s = summarisePostingsInDateSpan s depth empty (postingsinspan s)
      postingsinspan s = filter (isPostingInDateSpan s) ps
      dataspan = postingsDateSpan ps
      reportspan | empty = filterspan `orDatesFrom` dataspan
                 | otherwise = dataspan

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
summarisePostingsInDateSpan :: DateSpan -> Maybe Int -> Bool -> [Posting] -> [Posting]
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
      clippedanames = nub $ map (clipAccountName d) anames
      isclipped a = accountNameLevel a >= d
      d = fromMaybe 99999 $ depth
      balancetoshowfor a =
          (if isclipped a then inclbalof else exclbalof) (if null a then "top" else a)

-- | Clip the account names to the specified depth in a list of postings.
depthClipPostings :: Maybe Int -> [Posting] -> [Posting]
depthClipPostings depth = map (depthClipPosting depth)

-- | Clip a posting's account name to the specified depth.
depthClipPosting :: Maybe Int -> Posting -> Posting
depthClipPosting Nothing p = p
depthClipPosting (Just d) p@Posting{paccount=a} = p{paccount=clipAccountName d a}


tests_Hledger_Cli_Register :: Test
tests_Hledger_Cli_Register = TestList
 [

  "summarisePostingsByInterval" ~: do
    summarisePostingsByInterval (Quarters 1) Nothing False (DateSpan Nothing Nothing) [] ~?= []

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
