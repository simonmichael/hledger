{-| 

A ledger-compatible @balance@ command. Here's how it should work:

A sample account tree (as in the sample.ledger file):

@
 assets
  cash
  checking
  saving
 expenses
  food
  supplies
 income
  gifts
  salary
 liabilities
  debts
@

The balance command shows top-level accounts by default:

@
 \> ledger balance
 $-1  assets
  $2  expenses
 $-2  income
  $1  liabilities
@

With -s (--showsubs), also show the subaccounts:

@
 $-1  assets
 $-2    cash
  $1    saving
  $2  expenses
  $1    food
  $1    supplies
 $-2  income
 $-1    gifts
 $-1    salary
  $1  liabilities:debts
@

- @checking@ is not shown because it has a zero balance and no interesting
  subaccounts.  

- @liabilities@ is displayed only as a prefix because it has no transactions
  of its own and only one subaccount.

With an account pattern, show only the accounts with matching names:

@
 \> ledger balance o
  $1  expenses:food
 $-2  income
--------------------
 $-1  
@

- The o matched @food@ and @income@, so they are shown.

- Parents of matched accounts are also shown for context (@expenses@).

- This time the grand total is also shown, because it is not zero.

Again, -s adds the subaccounts:

@
\> ledger -s balance o
  $1  expenses:food
 $-2  income
 $-1    gifts
 $-1    salary
--------------------
 $-1  
@

- @food@ has no subaccounts. @income@ has two, so they are shown. 

- We do not add the subaccounts of parents included for context (@expenses@).

Here are some rules for account balance display, as seen above:

- grand total is omitted if it is 0

- leaf accounts and branches with 0 balance or 0 transactions are omitted

- inner accounts with 0 transactions and 1 subaccount are displayed inline

- in a filtered report, matched accounts are displayed with their parents
  inline (a consequence of the above)

- in a showsubs report, all subaccounts of matched accounts are displayed

-}
{-
let's start over:

a simple balance report lists top-level non-boring accounts, with their aggregated balances, followed by the total

a balance report with showsubs lists all non-boring accounts, with their aggregated balances, followed by the total

a filtered balance report lists non-boring accounts whose leafname matches the filter, with their aggregated balances, followed by the total

a filtered balance report with showsubs lists non-boring accounts whose leafname matches the filter, plus their subaccounts, with their aggregated balances, followed by the total

the total is the sum of the aggregated balances shown, excluding subaccounts whose parent's balance is shown. If the total is zero it is not shown.

boring accounts are 
- leaf accounts with zero balance; these are never shown
- non-matched parent accounts of matched accounts, when filtering; these are shown inline
- parent accounts with no transactions of their own and a single subaccount; these are shown inline

maxdepth may affect this further

-}

module BalanceCommand
where
import Ledger.Utils
import Ledger.Types
import Ledger.Amount
import Ledger.AccountName
import Ledger.Ledger
import Options
import Utils


-- | Print a balance report.
printbalance :: [Opt] -> [String] -> Ledger -> IO ()
printbalance opts args l = putStr $ balancereport opts args l

balancereport = balancereport1

-- | List the accounts for which we should show balances in the balance
-- report, based on the options.
balancereportaccts :: Bool -> [String] -> Ledger -> [Account]
balancereportaccts False [] l = topAccounts l
balancereportaccts False pats l = accountsMatching (regexFor pats) l
balancereportaccts True pats l = addsubaccts l $ balancereportaccts False pats l

-- | Add (in tree order) any missing subacccounts to a list of accounts.
addsubaccts l as = concatMap addsubs as where addsubs = maybe [] flatten . ledgerAccountTreeAt l

balancereport2 :: [Opt] -> [String] -> Ledger -> String
balancereport2 opts args l = acctsstr ++ totalstr
    where
      accts = balancereportaccts (ShowSubs `elem` opts) args l
      showacct a =
          bal ++ "  " ++ indent ++ prefix ++ fullname ++ "\n"
          where
            bal = printf "%20s" $ show $ abalance a
            indentlevel = 0
            prefix = ""
            indent = replicate (indentlevel * 2) ' '
            fullname = aname a
            leafname = accountLeafName fullname
      acctsstr = concatMap showacct accts
      total = sumAmounts $ map abalance $ removeduplicatebalances accts
      removeduplicatebalances as = filter (not . hasparentshowing) as
          where 
            hasparentshowing a = (parentAccountName $ aname a) `elem` names
            names = map aname as
      totalstr
          | isZeroAmount total = ""
          | otherwise = printf "--------------------\n%20s\n" $ showAmountRounded total

-- | Generate balance report output for a ledger.
balancereport1 :: [Opt] -> [String] -> Ledger -> String
balancereport1 opts args l = acctsstr ++ totalstr
    where 
      showsubs = (ShowSubs `elem` opts)
      pats@(apats,dpats) = parseAccountDescriptionArgs args
      maxdepth = case (pats, showsubs) of
                   (([],[]), False) -> 1 -- with no -s or pattern, show with depth 1
                   otherwise  -> 9999

      acctstoshow = balancereportaccts showsubs apats l
      acctnames = map aname acctstoshow
      treetoshow = pruneZeroBalanceLeaves $ pruneUnmatchedAccounts $ treeprune maxdepth $ ledgerAccountTree 9999 l
      acctforest = subs treetoshow

      acctsstr = concatMap (showAccountTree l maxdepth) acctforest

      totalstr
          | isZeroAmount total = ""
          | otherwise = printf "--------------------\n%20s\n" $ showAmountRounded total
      total = sumAmounts $ map abalance $ nonredundantaccts
      nonredundantaccts = filter (not . hasparentshowing) acctstoshow
      hasparentshowing a = (parentAccountName $ aname a) `elem` acctnames

      -- remove any accounts from the tree which are not one of the acctstoshow, 
      -- or one of their parents, or one of their subaccounts when doing showsubs
      pruneUnmatchedAccounts :: Tree Account -> Tree Account
      pruneUnmatchedAccounts = treefilter matched
          where 
            matched :: Account -> Bool
            matched (Account name _ _)
                | name `elem` acctnames = True
                | any (name `isAccountNamePrefixOf`) acctnames = True
                | showsubs && any (`isAccountNamePrefixOf` name) acctnames = True
                | otherwise = False

      -- remove all zero-balance leaf accounts (recursively)
      pruneZeroBalanceLeaves :: Tree Account -> Tree Account
      pruneZeroBalanceLeaves = treefilter (not . isZeroAmount . abalance)

-- | Get the string representation of a tree of accounts.
-- The ledger from which the accounts come is required so that
-- we can check for boring accounts.
showAccountTree :: Ledger -> Int -> Tree Account -> String
showAccountTree l maxdepth = showAccountTree' l maxdepth 0 ""
    where
      showAccountTree' :: Ledger -> Int -> Int -> String -> Tree Account -> String
      showAccountTree' l maxdepth indentlevel prefix t

          | isBoringParentAccount (length subaccts) (length $ subAccounts l a) maxdepth a = nextwithprefix
          | otherwise = thisline ++ nextwithindent

          where
            a = root t
            subaccts = subs t
            nextwithprefix = showsubs 0 (fullname++":")
            nextwithindent = showsubs 1 ""
            showsubs i p = concatMap (showAccountTree' l maxdepth (indentlevel+i) p) subaccts
            thisline = bal ++ "  " ++ indent ++ prefix ++ leafname ++ "\n"

            bal = printf "%20s" $ show $ abalance $ a
            indent = replicate (indentlevel * 2) ' '
            leafname = accountLeafName fullname
            fullname = aname a
            filtering = filteredaccountnames l /= (accountnames l)
            doesnotmatch = not (containsRegex (acctpat l) leafname)

            -- Boring parent accounts have the same balance as their
            -- single child. In other words they have exactly one child
            -- (which we may not be showing) and no transactions.  Also
            -- their depth is less than the maximum display depth.
            -- ..or some such thing..
            --isBoringParentAccount :: Int -> Int -> Account -> Bool
            isBoringParentAccount numsubs realnumsubs maxdepth a
                | name == "top" = False
                | depth < maxdepth && numtxns == 0 && numsubs == 1 = True
                | otherwise = False
                where      
                  name = aname a
                  depth = accountNameLevel name
                  numtxns = length $ atransactions a
