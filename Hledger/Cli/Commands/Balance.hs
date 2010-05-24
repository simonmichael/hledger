{-# LANGUAGE CPP #-}
{-|

A ledger-compatible @balance@ command.

ledger's balance command is easy to use but not easy to describe
precisely.  In the examples below we'll use sample.ledger, which has the
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
 $ hledger -f sample.ledger balance
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
$ hledger -f sample.ledger balance --depth 1
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
 $ hledger -f sample.ledger balance o
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

module Hledger.Cli.Commands.Balance
where
import Hledger.Data.Utils
import Hledger.Data.Types
import Hledger.Data.Amount
import Hledger.Data.AccountName
import Hledger.Data.Posting
import Hledger.Data.Ledger
import Hledger.Cli.Options
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding ( putStr )
import System.IO.UTF8
#endif


-- | Print a balance report.
balance :: [Opt] -> [String] -> Journal -> IO ()
balance opts args j = do
  t <- getCurrentLocalTime
  putStr $ showBalanceReport opts (optsToFilterSpec opts args t) j

-- | Generate a balance report with the specified options for this ledger.
showBalanceReport :: [Opt] -> FilterSpec -> Journal -> String
showBalanceReport opts filterspec j = acctsstr ++ totalstr
    where
      l = journalToLedger filterspec j
      acctsstr = unlines $ map showacct interestingaccts
          where
            showacct = showInterestingAccount opts l interestingaccts
            interestingaccts = filter (isInteresting opts l) acctnames
            acctnames = sort $ tail $ flatten $ treemap aname accttree
            accttree = ledgerAccountTree (fromMaybe 99999 $ depthFromOpts opts) l
      totalstr | NoTotal `elem` opts = ""
               | otherwise = printf "--------------------\n%s\n" $ padleft 20 $ showMixedAmountWithoutPrice total
          where
            total = sum $ map abalance $ ledgerTopAccounts l

-- | Display one line of the balance report with appropriate indenting and eliding.
showInterestingAccount :: [Opt] -> Ledger -> [AccountName] -> AccountName -> String
showInterestingAccount opts l interestingaccts a = concatTopPadded [amt, "  ", name]
    where
      amt = padleft 20 $ showMixedAmountWithoutPrice bal
      bal | Flat `elem` opts = exclusiveBalance acct
          | otherwise = abalance acct
      acct = ledgerAccount l a
      name | Flat `elem` opts = accountNameDrop (dropFromOpts opts) a
           | otherwise        = depthspacer ++ partialname
      parents = parentAccountNames a
      interestingparents = filter (`elem` interestingaccts) parents
      depthspacer = replicate (indentperlevel * length interestingparents) ' '
      indentperlevel = 2
      -- the partial name is the account's leaf name, prefixed by the
      -- names of any boring parents immediately above
      partialname = accountNameFromComponents $ reverse (map accountLeafName ps) ++ [accountLeafName a]
          where ps = takeWhile boring parents where boring = not . (`elem` interestingparents)

exclusiveBalance :: Account -> MixedAmount
exclusiveBalance = sumPostings . apostings

-- | Is the named account considered interesting for this ledger's balance report ?
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

