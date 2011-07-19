{-|

Generate several common kinds of report from a journal, as \"*Report\" -
simple intermediate data structures intended to be easily rendered as
text, html, json, csv etc. by hledger commands, hamlet templates,
javascript, or whatever. This is under Hledger.Cli since it depends
on the command-line options, should move to hledger-lib later.

-}

module Hledger.Cli.Reports (
  -- * Raw journal report
  RawJournalReport,
  RawJournalReportItem,
  rawJournalReport,
  -- * Postings report
  PostingsReport,
  PostingsReportItem,
  postingsReport,
  mkpostingsReportItem, -- XXX for showPostingWithBalanceForVty in Hledger.Cli.Register
  -- * Transactions report
  TransactionsReport,
  TransactionsReportItem,
  ariDate,
  ariBalance,
  journalTransactionsReport,
  accountTransactionsReport,
  -- * Accounts report
  AccountsReport,
  AccountsReportItem,
  accountsReport,
  accountsReport2,
  -- * Tests
  tests_Hledger_Cli_Reports
)
where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time.Calendar
import Data.Tree
import Safe (headMay, lastMay)
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.Printf

import Hledger.Data
import Hledger.Utils
import Hledger.Cli.Options
import Hledger.Cli.Utils

-------------------------------------------------------------------------------

-- | A raw journal report is a list of transactions used to generate a raw journal view.
-- Used by eg hledger's print command.
type RawJournalReport = [RawJournalReportItem]
type RawJournalReportItem = Transaction

-- | Select transactions for a raw journal report.
rawJournalReport :: [Opt] -> FilterSpec -> Journal -> RawJournalReport
rawJournalReport opts fspec j = sortBy (comparing tdate) $ jtxns $ filterJournalTransactions fspec j'
    where
      j' = journalSelectingDateFromOpts opts $ journalSelectingAmountFromOpts opts j

-------------------------------------------------------------------------------

-- | A postings report is a list of postings with a running total, a label
-- for the total field, and a little extra transaction info to help with rendering.
type PostingsReport = (String               -- label for the running balance column XXX remove
                      ,[PostingsReportItem] -- line items, one per posting
                      )
type PostingsReportItem = (Maybe (Day, String) -- transaction date and description if this is the first posting
                                 ,Posting      -- the posting
                                 ,MixedAmount  -- the running total after this posting
                                 )

-- | Select postings from the journal and add running balance and other
-- information to make a postings report. Used by eg hledger's register command.
postingsReport :: [Opt] -> FilterSpec -> Journal -> PostingsReport
postingsReport opts fspec j = (totallabel, postingsReportItems ps nullposting startbal (+))
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

totallabel = "Total"
balancelabel = "Balance"

-- | Generate postings report line items.
postingsReportItems :: [Posting] -> Posting -> MixedAmount -> (MixedAmount -> MixedAmount -> MixedAmount) -> [PostingsReportItem]
postingsReportItems [] _ _ _ = []
postingsReportItems (p:ps) pprev b sumfn = i:(postingsReportItems ps p b' sumfn)
    where
      i = mkpostingsReportItem isfirst p b'
      isfirst = ptransaction p /= ptransaction pprev
      b' = b `sumfn` pamount p

-- | Generate one postings report line item, from a flag indicating
-- whether to include transaction info, a posting, and the current running
-- balance.
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

-- | Clip the account names to the specified depth in a list of postings.
depthClipPostings :: Maybe Int -> [Posting] -> [Posting]
depthClipPostings depth = map (depthClipPosting depth)

-- | Clip a posting's account name to the specified depth.
depthClipPosting :: Maybe Int -> Posting -> Posting
depthClipPosting Nothing p = p
depthClipPosting (Just d) p@Posting{paccount=a} = p{paccount=clipAccountName d a}

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

ariDate (t,_,_,_,_,_) = tdate t
ariBalance (_,_,_,_,_,Mixed a) = case a of [] -> "0"
                                           (Amount{quantity=q}):_ -> show q

-- | Select transactions from the whole journal for a transactions report,
-- with no \"current\" account. The end result is similar to
-- "postingsReport" except it uses matchers and transaction-based report
-- items and the items are most recent first. Used by eg hledger-web's
-- journal view.
journalTransactionsReport :: [Opt] -> Journal -> Matcher -> TransactionsReport
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
accountTransactionsReport :: [Opt] -> Journal -> Matcher -> Matcher -> TransactionsReport
accountTransactionsReport opts j m thisacctmatcher = (label, items)
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
     items = reverse $ accountTransactionsReportItems m (Just thisacctmatcher) startbal negate ts

-- | Generate transactions report items from a list of transactions,
-- using the provided query and current account matchers, starting balance,
-- sign-setting function and balance-summing function.
accountTransactionsReportItems :: Matcher -> Maybe Matcher -> MixedAmount -> (MixedAmount -> MixedAmount) -> [Transaction] -> [TransactionsReportItem]
accountTransactionsReportItems _ _ _ _ [] = []
accountTransactionsReportItems matcher thisacctmatcher bal signfn (t:ts) =
    -- This is used for both accountTransactionsReport and journalTransactionsReport,
    -- which makes it a bit overcomplicated
    case i of Just i' -> i':is
              Nothing -> is
    where
      tmatched@Transaction{tpostings=psmatched} = filterTransactionPostings matcher t
      (psthisacct,psotheracct) = case thisacctmatcher of Just m  -> partition (matchesPosting m) psmatched
                                                         Nothing -> ([],psmatched)
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
      is = accountTransactionsReportItems matcher thisacctmatcher bal' signfn ts

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

-------------------------------------------------------------------------------

-- | An accounts report is a list of account names (full and short
-- variants) with their balances, appropriate indentation for rendering as
-- a hierarchy tree, and grand total.
type AccountsReport = ([AccountsReportItem] -- line items, one per account
                      ,MixedAmount          -- total balance of all accounts
                      )
type AccountsReportItem = (AccountName  -- full account name
                          ,AccountName  -- short account name for display (the leaf name, prefixed by any boring parents immediately above)
                          ,Int          -- how many steps to indent this account (0-based account depth excluding boring parents)
                          ,MixedAmount) -- account balance, includes subs unless --flat is present

-- | Select accounts, and get their balances at the end of the selected
-- period, and misc. display information, for an accounts report. Used by
-- eg hledger's balance command.
accountsReport :: [Opt] -> FilterSpec -> Journal -> AccountsReport
accountsReport opts filterspec j = accountsReport' opts j (journalToLedger filterspec)

-- | Select accounts, and get their balances at the end of the selected
-- period, and misc. display information, for an accounts report. Like
-- "accountsReport" but uses the new matchers. Used by eg hledger-web's
-- accounts sidebar.
accountsReport2 :: [Opt] -> Matcher -> Journal -> AccountsReport
accountsReport2 opts matcher j = accountsReport' opts j (journalToLedger2 matcher)

-- Accounts report helper.
accountsReport' :: [Opt] -> Journal -> (Journal -> Ledger) -> AccountsReport
accountsReport' opts j jtol = (items, total)
    where
      items = map mkitem interestingaccts
      interestingaccts | NoElide `elem` opts = acctnames
                       | otherwise = filter (isInteresting opts l) acctnames
      acctnames = sort $ tail $ flatten $ treemap aname accttree
      accttree = ledgerAccountTree (fromMaybe 99999 $ depthFromOpts opts) l
      total = sum $ map abalance $ ledgerTopAccounts l
      l =  jtol $ journalSelectingDateFromOpts opts $ journalSelectingAmountFromOpts opts j

      -- | Get data for one balance report line item.
      mkitem :: AccountName -> AccountsReportItem
      mkitem a = (a, adisplay, indent, abal)
          where
            adisplay | Flat `elem` opts = a
                     | otherwise = accountNameFromComponents $ reverse (map accountLeafName ps) ++ [accountLeafName a]
                where ps = takeWhile boring parents where boring = not . (`elem` interestingparents)
            indent | Flat `elem` opts = 0
                   | otherwise = length interestingparents
            interestingparents = filter (`elem` interestingaccts) parents
            parents = parentAccountNames a
            abal | Flat `elem` opts = exclusiveBalance acct
                 | otherwise = abalance acct
                 where acct = ledgerAccount l a

exclusiveBalance :: Account -> MixedAmount
exclusiveBalance = sumPostings . apostings

-- | Is the named account considered interesting for this ledger's accounts report,
-- following the eliding style of ledger's balance command ?
isInteresting :: [Opt] -> Ledger -> AccountName -> Bool
isInteresting opts l a | Flat `elem` opts = isInterestingFlat opts l a
                       | otherwise = isInterestingIndented opts l a

isInterestingFlat :: [Opt] -> Ledger -> AccountName -> Bool
isInterestingFlat opts l a = notempty || emptyflag
    where
      acct = ledgerAccount l a
      notempty = not $ isZeroMixedAmount $ exclusiveBalance acct
      emptyflag = Empty `elem` opts

isInterestingIndented :: [Opt] -> Ledger -> AccountName -> Bool
isInterestingIndented opts l a
    | numinterestingsubs==1 && not atmaxdepth = notlikesub
    | otherwise = notzero || emptyflag
    where
      atmaxdepth = isJust d && Just (accountNameLevel a) == d where d = depthFromOpts opts
      emptyflag = Empty `elem` opts
      acct = ledgerAccount l a
      notzero = not $ isZeroMixedAmount inclbalance where inclbalance = abalance acct
      notlikesub = not $ isZeroMixedAmount exclbalance where exclbalance = sumPostings $ apostings acct
      numinterestingsubs = length $ filter isInterestingTree subtrees
          where
            isInterestingTree = treeany (isInteresting opts l . aname)
            subtrees = map (fromJust . ledgerAccountTreeAt l) $ ledgerSubAccounts l $ ledgerAccount l a

-------------------------------------------------------------------------------

tests_Hledger_Cli_Reports :: Test
tests_Hledger_Cli_Reports = TestList
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
