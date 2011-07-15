{-# LANGUAGE CPP #-}
{-| 

A ledger-compatible @register@ command.

-}

module Hledger.Cli.Register (
  PostingRegisterReport
 ,PostingRegisterReportItem
 ,AccountRegisterReport
 ,AccountRegisterReportItem
 ,register
 ,postingRegisterReport
 ,accountRegisterReport
 ,journalRegisterReport
 ,postingRegisterReportAsText
 ,showPostingWithBalanceForVty
 ,ariDate
 ,ariBalance
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


-- | A posting register report lists postings to one or more accounts,
-- with a running total. Postings may be actual postings, or aggregate
-- postings corresponding to a reporting interval.
type PostingRegisterReport = (String                      -- label for the running balance column XXX remove
                             ,[PostingRegisterReportItem] -- line items, one per posting
                             )

-- | A single posting register line item, representing one posting.
type PostingRegisterReportItem = (Maybe (Day, String) -- transaction date and description if this is the first posting
                                 ,Posting             -- the posting
                                 ,MixedAmount         -- the running total after this posting
                                 )

-- | An account register report lists transactions to a single account (or
-- possibly subs as well), with the accurate running account balance when
-- possible (otherwise, a running total.)
type AccountRegisterReport = (String                      -- label for the balance column, eg "balance" or "total"
                             ,[AccountRegisterReportItem] -- line items, one per transaction
                             )

-- | A single account register line item, representing one transaction to/from the focussed account.
type AccountRegisterReportItem = (Transaction -- the corresponding transaction
                                 ,Transaction -- the transaction with postings to the focussed account removed
                                 ,Bool        -- is this a split (more than one other-account posting) ?
                                 ,String      -- the (possibly aggregated) account info to display
                                 ,MixedAmount -- the (possibly aggregated) amount to display (sum of the other-account postings)
                                 ,MixedAmount -- the running balance for the focussed account after this transaction
                                 )

ariDate (t,_,_,_,_,_) = tdate t
ariBalance (_,_,_,_,_,Mixed a) = case a of [] -> "0"
                                           (Amount{quantity=q}):_ -> show q

-- | Print a (posting) register report.
register :: [Opt] -> [String] -> Journal -> IO ()
register opts args j = do
  d <- getCurrentDay
  putStr $ postingRegisterReportAsText opts $ postingRegisterReport opts (optsToFilterSpec opts args d) j

-- | Render a register report as plain text suitable for console output.
postingRegisterReportAsText :: [Opt] -> PostingRegisterReport -> String
postingRegisterReportAsText opts = unlines . map (postingRegisterReportItemAsText opts) . snd

-- | Render one register report line item as plain text. Eg:
-- @
-- date (10)  description (20)     account (22)            amount (11)  balance (12)
-- DDDDDDDDDD dddddddddddddddddddd aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
-- ^ displayed for first postings^
--   only, otherwise blank
-- @
postingRegisterReportItemAsText :: [Opt] -> PostingRegisterReportItem -> String
postingRegisterReportItemAsText _ (dd, p, b) = concatTopPadded [datedesc, pstr, " ", bal]
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

showPostingWithBalanceForVty showtxninfo p b = postingRegisterReportItemAsText [] $ mkitem showtxninfo p b

totallabel = "Total"
balancelabel = "Balance"

-- | Get a ledger-style posting register report, with the specified options,
-- for the whole journal. See also "accountRegisterReport".
postingRegisterReport :: [Opt] -> FilterSpec -> Journal -> PostingRegisterReport
postingRegisterReport opts fspec j = (totallabel, postingRegisterItems ps nullposting startbal (+))
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

-- | Generate posting register report line items.
postingRegisterItems :: [Posting] -> Posting -> MixedAmount -> (MixedAmount -> MixedAmount -> MixedAmount) -> [PostingRegisterReportItem]
postingRegisterItems [] _ _ _ = []
postingRegisterItems (p:ps) pprev b sumfn = i:(postingRegisterItems ps p b' sumfn)
    where
      i = mkitem isfirst p b'
      isfirst = ptransaction p /= ptransaction pprev
      b' = b `sumfn` pamount p

-- | Generate one register report line item, from a flag indicating
-- whether to include transaction info, a posting, and the current running
-- balance.
mkitem :: Bool -> Posting -> MixedAmount -> PostingRegisterReportItem
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

-- | Get a ledger-style register report showing all matched transactions and postings.
-- Similar to "postingRegisterReport" except it uses matchers and
-- per-transaction report items like "accountRegisterReport".
journalRegisterReport :: [Opt] -> Journal -> Matcher -> AccountRegisterReport
journalRegisterReport _ Journal{jtxns=ts} m = (totallabel, items)
   where
     ts' = sortBy (comparing tdate) $ filter (not . null . tpostings) $ map (filterTransactionPostings m) ts
     items = reverse $ accountRegisterReportItems m Nothing nullmixedamt id ts'
     -- XXX items' first element should be the full transaction with all postings

-- | Get a conventional account register report, with the specified
-- options, for the currently focussed account (or possibly the focussed
-- account plus sub-accounts.) This differs from "postingRegisterReport"
-- in several ways:
--
-- 1. it shows transactions, from the point of view of the focussed
--    account. The other account's name and posted amount is displayed,
--    aggregated if there is more than one other account posting.
--
-- 2. With no transaction filtering in effect other than a start date, it
--    shows the accurate historical running balance for this
--    account. Otherwise it shows a running total starting at 0 like the posting register report.
--
-- 3. Currently this report does not handle reporting intervals.
--
-- 4. Report items will be most recent first.
--
accountRegisterReport :: [Opt] -> Journal -> Matcher -> Matcher -> AccountRegisterReport
accountRegisterReport opts j m thisacctmatcher = (label, items)
 where
     -- transactions affecting this account, in date order
     ts = sortBy (comparing tdate) $ filter (matchesTransaction thisacctmatcher) $ jtxns j
     -- starting balance: if we are filtering by a start date and nothing else,
     -- the sum of postings to this account before that date; otherwise zero.
     (startbal,label) | matcherIsNull m                    = (nullmixedamt,        balancelabel)
                      | matcherIsStartDateOnly effective m = (sumPostings priorps, balancelabel)
                      | otherwise                          = (nullmixedamt,        totallabel)
                      where
                        priorps = -- ltrace "priorps" $
                                  filter (matchesPosting
                                          (-- ltrace "priormatcher" $
                                           MatchAnd [thisacctmatcher, tostartdatematcher]))
                                         $ transactionsPostings ts
                        tostartdatematcher = MatchDate True (DateSpan Nothing startdate)
                        startdate = matcherStartDate effective m
                        effective = Effective `elem` opts
     items = reverse $ accountRegisterReportItems m (Just thisacctmatcher) startbal negate ts

-- | Generate account register line items from a list of transactions,
-- using the provided query and "this account" matchers, starting balance,
-- sign-setting function and balance-summing function.

-- This is used for both accountRegisterReport and journalRegisterReport,
-- which makes it a bit overcomplicated.
accountRegisterReportItems :: Matcher -> Maybe Matcher -> MixedAmount -> (MixedAmount -> MixedAmount) -> [Transaction] -> [AccountRegisterReportItem]
accountRegisterReportItems _ _ _ _ [] = []
accountRegisterReportItems matcher thisacctmatcher bal signfn (t:ts) =
    case i of Just i' -> i':is
              Nothing -> is
    where
      tmatched@Transaction{tpostings=psmatched} = filterTransactionPostings matcher t
      (psthisacct,psotheracct) = case thisacctmatcher of Just m  -> partition (matchesPosting m) psmatched
                                                         Nothing -> ([],psmatched)
      numthisaccts = length $ nub $ map paccount psthisacct
      numotheraccts = length $ nub $ map paccount psotheracct
      amt = sum $ map pamount psotheracct
      acct | isNothing thisacctmatcher = summarisePostings psmatched -- journal register
           | numotheraccts == 0 = "transfer between " ++ summarisePostingAccounts psthisacct
           | otherwise          = prefix              ++ summarisePostingAccounts psotheracct
           where prefix = maybe "" (\b -> if b then "from " else "to ") $ isNegativeMixedAmount amt
      (i,bal') = case psmatched of
           [] -> (Nothing,bal)
           _  -> (Just (t, tmatched, numotheraccts > 1, acct, a, b), b)
                 where
                  a = signfn amt
                  b = bal + a
      is = accountRegisterReportItems matcher thisacctmatcher bal' signfn ts

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

filterTransactionPostings :: Matcher -> Transaction -> Transaction
filterTransactionPostings m t@Transaction{tpostings=ps} = t{tpostings=filter (m `matchesPosting`) ps}

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
