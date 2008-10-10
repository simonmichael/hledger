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

balancereport :: [Opt] -> [String] -> Ledger -> String
balancereport opts args l = showLedgerAccountBalances l depth
    where 
      showsubs = (ShowSubs `elem` opts)
      pats = parseAccountDescriptionArgs args
      -- when there is no -s or pattern args, show with depth 1
      depth = case (pats, showsubs) of
                (([],[]), False) -> 1
                otherwise  -> 9999

-- | Generate balance report output for a ledger, to the specified depth.
showLedgerAccountBalances :: Ledger -> Int -> String
showLedgerAccountBalances l maxdepth = 
    concatMap (showAccountTree l maxdepth) acctbranches
    ++
    if isZeroAmount total 
    then ""
    else printf "--------------------\n%20s\n" $ showAmountRounded total
    where 
      acctbranches = branches $ pruneZeroBalanceBranches $ ledgerAccountTree maxdepth l
      filteredacctbranches = branches $ ledgerFilteredAccountTree maxdepth (acctpat l) l
      total = sum $ map (abalance . root) filteredacctbranches

-- | Remove all-zero-balance branches and leaves from a tree of accounts.
pruneZeroBalanceBranches :: Tree Account -> Tree Account
pruneZeroBalanceBranches = treefilter (not . isZeroAmount . abalance)

-- | Get the string representation of a tree of accounts.
-- The ledger from which the accounts come is required so that
-- we can check for boring accounts.
showAccountTree :: Ledger -> Int -> Tree Account -> String
showAccountTree l maxdepth = showAccountTree' l maxdepth 0 ""

showAccountTree' :: Ledger -> Int -> Int -> String -> Tree Account -> String
showAccountTree' l maxdepth indentlevel prefix t
    -- merge boring inner account names with the next line
    | isBoringInnerAccount l maxdepth acct = subsindented 0 (fullname++":")
    -- ditto with unmatched parent accounts when filtering by account
    |  filtering && doesnotmatch = subsindented 0 (fullname++":")
    -- otherwise show this account's name & balance
    | otherwise = bal ++ "  " ++ indent ++ prefix ++ leafname ++ "\n" ++ (subsindented 1 "")
    where
      acct = root t
      subs = branches t
      subsindented i p = concatMap (showAccountTree' l maxdepth (indentlevel+i) p) subs
      bal = printf "%20s" $ show $ abalance $ acct
      indent = replicate (indentlevel * 2) ' '
      fullname = aname acct
      leafname = accountLeafName fullname
      filtering = filteredaccountnames l /= (accountnames l)
      doesnotmatch = not (containsRegex (acctpat l) leafname)

-- | Is this account a boring inner account in this ledger ? 
-- Boring inner accounts have no transactions, one subaccount,
-- and depth less than the maximum display depth.
-- Also, they are unmatched parent accounts when account matching is in effect.
isBoringInnerAccount :: Ledger -> Int -> Account -> Bool
isBoringInnerAccount l maxdepth a
    | name == "top" = False
    | depth < maxdepth && numtxns == 0 && numsubs == 1 = True
    | otherwise = False
    where      
      name = aname a
      depth = accountNameLevel name
      numtxns = length $ atransactions a
      -- how many (filter-matching) subaccounts has this account ?
      numsubs = length $ subAccountNamesFrom (filteredaccountnames l) name

-- | Is the named account a boring inner account in this ledger ?
isBoringInnerAccountName :: Ledger -> Int -> AccountName -> Bool
isBoringInnerAccountName l maxdepth = isBoringInnerAccount l maxdepth . ledgerAccount l
