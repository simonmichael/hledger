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

With -s (--subtotal), also show the subaccounts:

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

- @liabilities@ is displayed only as a prefix because it has the same balance
  as its single subaccount.

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

Some notes for the implementation:

- a simple balance report shows top-level accounts

- with an account pattern, it shows accounts whose leafname matches, plus their parents

- with the subtotal option, it also shows all subaccounts of the above

- zero-balance leaf accounts are removed

- the resulting account tree is displayed with each account's aggregated
  balance, with boring parents prefixed to the next line

- a boring parent has the same balance as its child and is not explicitly
  matched by the display options.

- the sum of the balances shown is displayed at the end, if it is non-zero

-}

module BalanceCommand
where
import Ledger.Utils
import Ledger.Types
import Ledger.Amount
import Ledger.AccountName
import Ledger.Transaction
import Ledger.Ledger
import Ledger.Parse
import Options
import Utils


-- | Print a balance report.
balance :: [Opt] -> [String] -> Ledger -> IO ()
balance opts args l = putStr $ showBalanceReport opts args l

-- | Generate a balance report with the specified options for this ledger.
showBalanceReport :: [Opt] -> [String] -> Ledger -> String
showBalanceReport opts args l = acctsstr ++ totalstr
    where 
      acctsstr = unlines $ map showacct interestingaccts
          where
            showacct = showInterestingAccount l interestingaccts
            interestingaccts = filter (isInteresting opts l) acctnames
            acctnames = sort $ tail $ flatten $ treemap aname accttree
            accttree = ledgerAccountTree (depthFromOpts opts) l
      totalstr | isZeroMixedAmount total = ""
               | otherwise = printf "--------------------\n%s\n" $ padleft 20 $ showMixedAmount total
          where
            total = sum $ map abalance $ topAccounts l

-- | Display one line of the balance report with appropriate indenting and eliding.
showInterestingAccount :: Ledger -> [AccountName] -> AccountName -> String
showInterestingAccount l interestingaccts a = concatTopPadded [amt, "  ", depthspacer ++ partialname]
    where
      amt = padleft 20 $ showMixedAmount $ abalance $ ledgerAccount l a
      -- the depth spacer (indent) is two spaces for each interesting parent
      parents = parentAccountNames a
      interestingparents = filter (`elem` interestingaccts) parents
      depthspacer = replicate (2 * length interestingparents) ' '
      -- the partial name is the account's leaf name, prefixed by the
      -- names of any boring parents immediately above
      partialname = accountNameFromComponents $ (reverse $ map accountLeafName ps) ++ [accountLeafName a]
          where ps = takeWhile boring parents where boring = not . (`elem` interestingparents)

-- | Is the named account considered interesting for this ledger's balance report ?
isInteresting :: [Opt] -> Ledger -> AccountName -> Bool
isInteresting opts l a
    | numinterestingsubs==1 && not atmaxdepth = notlikesub
    | otherwise = notzero || emptyflag
    where
      atmaxdepth = accountNameLevel a == depthFromOpts opts
      emptyflag = Empty `elem` opts
      acct = ledgerAccount l a
      notzero = not $ isZeroMixedAmount inclbalance where inclbalance = abalance acct
      notlikesub = not $ isZeroMixedAmount exclbalance where exclbalance = sumTransactions $ atransactions acct
      numinterestingsubs = length $ filter isInterestingTree subtrees
          where
            isInterestingTree t = treeany (isInteresting opts l . aname) t
            subtrees = map (fromJust . ledgerAccountTreeAt l) $ subAccounts l $ ledgerAccount l a

