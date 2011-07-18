{-|

Generate several common kinds of report from a journal, as "Reports" -
simple intermediate data structures intended to be easily rendered as
text, html, json, csv etc. by hledger commands, hamlet templates,
javascript, or whatever.

-}

module Hledger.Report (
  tests_Hledger_Report
 ,JournalReport
 ,JournalReportItem
 ,PostingRegisterReport
 ,PostingRegisterReportItem
 ,AccountRegisterReport
 ,AccountRegisterReportItem
 ,BalanceReport
 ,BalanceReportItem
 ,ariDate
 ,ariBalance
 ,journalReport
 ,postingRegisterReport
 ,accountRegisterReport
 ,journalRegisterReport
 ,mkpostingRegisterItem -- for silly showPostingWithBalanceForVty in Hledger.Cli.Register
 ,balanceReport
 ,balanceReport2
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

import Hledger.Cli.Options
import Hledger.Cli.Utils
import Hledger.Data
import Hledger.Utils

-- | A "journal report" is just a list of transactions.
type JournalReport = [JournalReportItem]

type JournalReportItem = Transaction

-- | A posting register report lists postings to one or more accounts,
-- with a running total. Postings may be actual postings, or aggregate
-- postings corresponding to a reporting interval.
type PostingRegisterReport = (String                      -- label for the running balance column XXX remove
                             ,[PostingRegisterReportItem] -- line items, one per posting
                             )

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

-- | A balance report is a chart of accounts with balances, and their grand total.
type BalanceReport = ([BalanceReportItem] -- line items, one per account
                     ,MixedAmount         -- total balance of all accounts
                     )

type BalanceReportItem = (AccountName  -- full account name
                         ,AccountName  -- account name elided for display: the leaf name,
                                       -- prefixed by any boring parents immediately above
                         ,Int          -- how many steps to indent this account (0-based account depth excluding boring parents)
                         ,MixedAmount) -- account balance, includes subs unless --flat is present

-------------------------------------------------------------------------------

-- | Select transactions, as in the print command.
journalReport :: [Opt] -> FilterSpec -> Journal -> JournalReport
journalReport opts fspec j = sortBy (comparing tdate) $ jtxns $ filterJournalTransactions fspec j'
    where
      j' = journalSelectingDateFromOpts opts $ journalSelectingAmountFromOpts opts j

-------------------------------------------------------------------------------

-- | Select postings from the journal and get their running balance, as in
-- the register command.
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

totallabel = "Total"
balancelabel = "Balance"

-- | Generate posting register report line items.
postingRegisterItems :: [Posting] -> Posting -> MixedAmount -> (MixedAmount -> MixedAmount -> MixedAmount) -> [PostingRegisterReportItem]
postingRegisterItems [] _ _ _ = []
postingRegisterItems (p:ps) pprev b sumfn = i:(postingRegisterItems ps p b' sumfn)
    where
      i = mkpostingRegisterItem isfirst p b'
      isfirst = ptransaction p /= ptransaction pprev
      b' = b `sumfn` pamount p

-- | Generate one register report line item, from a flag indicating
-- whether to include transaction info, a posting, and the current running
-- balance.
mkpostingRegisterItem :: Bool -> Posting -> MixedAmount -> PostingRegisterReportItem
mkpostingRegisterItem False p b = (Nothing, p, b)
mkpostingRegisterItem True p b = (ds, p, b)
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

-- | Select postings from the whole journal and get their running balance.
-- Similar to "postingRegisterReport" except it uses matchers and
-- per-transaction report items like "accountRegisterReport".
journalRegisterReport :: [Opt] -> Journal -> Matcher -> AccountRegisterReport
journalRegisterReport _ Journal{jtxns=ts} m = (totallabel, items)
   where
     ts' = sortBy (comparing tdate) $ filter (not . null . tpostings) $ map (filterTransactionPostings m) ts
     items = reverse $ accountRegisterReportItems m Nothing nullmixedamt id ts'
     -- XXX items' first element should be the full transaction with all postings

-------------------------------------------------------------------------------

-- | Select transactions within one (or more) specified accounts, and get
-- their running balance within that (those) account(s). Used for a
-- conventional quicker/gnucash/bank-style account register. Specifically,
-- this differs from "postingRegisterReport" as follows:
--
-- 1. it shows transactions, from the point of view of the focussed
--    account. The other account's name and posted amount is displayed,
--    aggregated if there is more than one other account posting.
--
-- 2. With no transaction filtering in effect other than a start date, it
--    shows the accurate historical running balance for this
--    account. Otherwise it shows a running total starting at 0 like the
--    posting register report.
--
-- 3. It currently does not handle reporting intervals.
--
-- 4. Report items are most recent first.
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

-------------------------------------------------------------------------------

-- | Select accounts, and get their balances at the end of the selected
-- period, as in the balance command.
balanceReport :: [Opt] -> FilterSpec -> Journal -> BalanceReport
balanceReport opts filterspec j = balanceReport' opts j (journalToLedger filterspec)

-- | Select accounts, and get their balances at the end of the selected
-- period. Like "balanceReport" but uses the new matchers.
balanceReport2 :: [Opt] -> Matcher -> Journal -> BalanceReport
balanceReport2 opts matcher j = balanceReport' opts j (journalToLedger2 matcher)

-- Balance report helper.
balanceReport' :: [Opt] -> Journal -> (Journal -> Ledger) -> BalanceReport
balanceReport' opts j jtol = (items, total)
    where
      items = map mkitem interestingaccts
      interestingaccts | NoElide `elem` opts = acctnames
                       | otherwise = filter (isInteresting opts l) acctnames
      acctnames = sort $ tail $ flatten $ treemap aname accttree
      accttree = ledgerAccountTree (fromMaybe 99999 $ depthFromOpts opts) l
      total = sum $ map abalance $ ledgerTopAccounts l
      l =  jtol $ journalSelectingDateFromOpts opts $ journalSelectingAmountFromOpts opts j

      -- | Get data for one balance report line item.
      mkitem :: AccountName -> BalanceReportItem
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

-- | Is the named account considered interesting for this ledger's balance report ?
-- We follow the style of ledger's balance command.
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

tests_Hledger_Report :: Test
tests_Hledger_Report = TestList
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
