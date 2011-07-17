{-# LANGUAGE CPP #-}
{-|

A ledger-compatible @balance@ command.

ledger's balance command is easy to use but not easy to describe
precisely.  In the examples below we'll use sample.journal, which has the
following account tree:

@
 assets
   bank
     checking
     saving
   cash
 expenses
   food
   supplies
 income
   gifts
   salary
 liabilities
   debts
@

The balance command shows accounts with their aggregate balances.
Subaccounts are displayed indented below their parent. Each balance is the
sum of any transactions in that account plus any balances from
subaccounts:

@
 $ hledger -f sample.journal balance
                 $-1  assets
                  $1    bank:saving
                 $-2    cash
                  $2  expenses
                  $1    food
                  $1    supplies
                 $-2  income
                 $-1    gifts
                 $-1    salary
                  $1  liabilities:debts
@

Usually, the non-interesting accounts are elided or omitted. Above,
@checking@ is omitted because it has no subaccounts and a zero balance.
@bank@ is elided because it has only a single displayed subaccount
(@saving@) and it would be showing the same balance as that ($1). Ditto
for @liabilities@. We will return to this in a moment.

The --depth argument can be used to limit the depth of the balance report.
So, to see just the top level accounts:

@
$ hledger -f sample.journal balance --depth 1
                 $-1  assets
                  $2  expenses
                 $-2  income
                  $1  liabilities
@

This time liabilities has no displayed subaccounts (due to --depth) and
is not elided.

With one or more account pattern arguments, the balance command shows
accounts whose name matches one of the patterns, plus their parents
(elided) and subaccounts. So with the pattern o we get:

@
 $ hledger -f sample.journal balance o
                  $1  expenses:food
                 $-2  income
                 $-1    gifts
                 $-1    salary
--------------------
                 $-1
@

The o pattern matched @food@ and @income@, so they are shown. Unmatched
parents of matched accounts are also shown (elided) for context (@expenses@).

Also, the balance report shows the total of all displayed accounts, when
that is non-zero. Here, it is displayed because the accounts shown add up
to $-1.

Here is a more precise definition of \"interesting\" accounts in ledger's
balance report:

- an account which has just one interesting subaccount branch, and which
  is not at the report's maximum depth, is interesting if the balance is
  different from the subaccount's, and otherwise boring.

- any other account is interesting if it has a non-zero balance, or the -E
  flag is used.

-}

module Hledger.Cli.Balance (
  BalanceReport
 ,BalanceReportItem
 ,balance
 ,balanceReport
 ,balanceReport2
 ,balanceReportAsText
 ,tests_Hledger_Cli_Balance
 -- ,tests_Balance
) where
import Data.List
import Data.Maybe
import Data.Tree
import Test.HUnit

import Hledger.Cli.Format
import qualified Hledger.Cli.Format as Format
import Hledger.Cli.Options
import Hledger.Cli.Utils
import Hledger.Data
import Hledger.Utils
import Prelude hiding (putStr)
import Hledger.Utils.UTF8 (putStr)


-- | A balance report is a chart of accounts with balances, and their grand total.
type BalanceReport = ([BalanceReportItem] -- line items, one per account
                     ,MixedAmount         -- total balance of all accounts
                     )

-- | The data for a single balance report line item, representing one account.
type BalanceReportItem = (AccountName  -- full account name
                         ,AccountName  -- account name elided for display: the leaf name,
                                       -- prefixed by any boring parents immediately above
                         ,Int          -- how many steps to indent this account (0-based account depth excluding boring parents)
                         ,MixedAmount) -- account balance, includes subs unless --flat is present

-- | Print a balance report.
balance :: [Opt] -> [String] -> Journal -> IO ()
balance opts args j = do
  d <- getCurrentDay
  let lines = case parseFormatFromOpts opts of
            Left err -> [err]
            Right _ -> balanceReportAsText opts $ balanceReport opts (optsToFilterSpec opts args d) j
  putStr $ unlines lines

-- | Render a balance report as plain text suitable for console output.
balanceReportAsText :: [Opt] -> BalanceReport -> [String]
balanceReportAsText opts (items, total) = concat lines ++ t
    where
      lines = map (balanceReportItemAsText opts format) items
      format = formatFromOpts opts
      t = if NoTotal `elem` opts
             then []
             else ["--------------------"
                    -- TODO: This must use the format somehow
                  , padleft 20 $ showMixedAmountWithoutPrice total
                  ]

{-
This implementation turned out to be a bit convoluted but implements the following algorithm for formatting:

- If there is a single amount, print it with the account name directly:
- Otherwise, only print the account name on the last line.

    a         USD 1   ; Account 'a' has a single amount
              EUR -1
    b         USD -1  ; Account 'b' has two amounts. The account name is printed on the last line.
-}
-- | Render one balance report line item as plain text.
balanceReportItemAsText :: [Opt] -> [FormatString] -> BalanceReportItem -> [String]
balanceReportItemAsText opts format (_, accountName, depth, Mixed amounts) =
    case amounts of
      [] -> []
      [a] -> [formatBalanceReportItem opts (Just accountName) depth a format]
      (as) -> asText as
    where
      asText :: [Amount] -> [String]
      asText []     = []
      asText [a]    = [formatBalanceReportItem opts (Just accountName) depth a format]
      asText (a:as) = (formatBalanceReportItem opts Nothing depth a format) : asText as

formatBalanceReportItem :: [Opt] -> Maybe AccountName -> Int -> Amount -> [FormatString] -> String
formatBalanceReportItem _ _ _ _ [] = ""
formatBalanceReportItem opts accountName depth amount (f:fs) = s ++ (formatBalanceReportItem opts accountName depth amount fs)
  where
    s = case f of
            FormatLiteral l -> l
            FormatField leftJustified min max field  -> formatAccount opts accountName depth amount leftJustified min max field

formatAccount :: [Opt] -> Maybe AccountName -> Int -> Amount -> Bool -> Maybe Int -> Maybe Int -> Field -> String
formatAccount opts accountName depth balance leftJustified min max field = case field of
        Format.Account  -> formatValue leftJustified min max a
        DepthSpacer     -> case min of
                               Just m  -> formatValue leftJustified Nothing max $ replicate (depth * m) ' '
                               Nothing -> formatValue leftJustified Nothing max $ replicate depth ' '
        Total           -> formatValue leftJustified min max $ showAmountWithoutPrice balance
        _	        -> ""
    where
      a = maybe "" (accountNameDrop (dropFromOpts opts)) accountName

-- | Get a balance report with the specified options for this journal.
balanceReport :: [Opt] -> FilterSpec -> Journal -> BalanceReport
balanceReport opts filterspec j = balanceReport' opts j (journalToLedger filterspec)

-- | Get a balance report with the specified options for this
-- journal. Like balanceReport but uses the new matchers.
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

tests_Hledger_Cli_Balance = TestList
 [
 ]
