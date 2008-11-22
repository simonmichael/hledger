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
import Ledger.Ledger
import Options
import Utils


-- | Print a balance report.
balance :: [Opt] -> [String] -> Ledger -> IO ()
balance opts args l = putStr $ showBalanceReport opts args l

-- | Generate balance report output for a ledger.
showBalanceReport :: [Opt] -> [String] -> Ledger -> String
showBalanceReport opts args l = acctsstr ++ (if collapse then "" else totalstr)
    where 
      acctsstr = concatMap showatree $ subs t
      showatree t = showAccountTreeWithBalances matchedacctnames t
      matchedacctnames = balancereportacctnames l sub apats t
      t = (if empty then id else pruneZeroBalanceLeaves) $ ledgerAccountTree maxdepth l
      apats = fst $ parseAccountDescriptionArgs args
      maxdepth = fromMaybe 9999 $ depthFromOpts opts
      sub = SubTotal `elem` opts || (isJust $ depthFromOpts opts)
      empty = Empty `elem` opts
      collapse = Collapse `elem` opts
      totalstr = if isZeroMixedAmount total 
                 then "" 
                 else printf "--------------------\n%s\n" $ padleft 20 $ showMixedAmount total
      total = sum $ map (abalance . ledgerAccount l) $ nonredundantaccts
      nonredundantaccts = filter (not . hasparentshowing) matchedacctnames
      hasparentshowing aname = (parentAccountName $ aname) `elem` matchedacctnames

-- | Identify the accounts we are interested in seeing balances for in the
-- balance report, based on the -s flag and account patterns. See Tests.hs.
balancereportacctnames :: Ledger -> Bool -> [String] -> Tree Account -> [AccountName]
balancereportacctnames l False [] t   = filter (/= "top") $ map aname $ flatten $ treeprune 1 t
balancereportacctnames l False pats t = filter (/= "top") $ ns
    where 
      ns = filter (matchpats_balance pats) $ map aname $ flatten t'
      t' | null $ positivepats pats = treeprune 1 t
         | otherwise = t
balancereportacctnames l True pats t  = nub $ map aname $ addsubaccts l $ as
    where 
      as = map (ledgerAccount l) ns
      ns = balancereportacctnames l False pats t
      -- add (in tree order) any missing subaccounts to a list of accounts
      addsubaccts :: Ledger -> [Account] -> [Account]
      addsubaccts l as = concatMap addsubs as where addsubs = maybe [] flatten . ledgerAccountTreeAt l

-- | Remove all sub-trees whose accounts have a zero balance.
pruneZeroBalanceLeaves :: Tree Account -> Tree Account
pruneZeroBalanceLeaves = treefilter (not . isZeroMixedAmount . abalance)

-- | Show this tree of accounts with balances, eliding boring parent
-- accounts and omitting uninteresting subaccounts based on the provided
-- list of account names we want to see balances for.
showAccountTreeWithBalances :: [AccountName] -> Tree Account -> String
showAccountTreeWithBalances matchednames t = showAccountTreeWithBalances' matchednames 0 "" t
    where
      showAccountTreeWithBalances' :: [AccountName] -> Int -> String -> Tree Account -> String
      showAccountTreeWithBalances' matchednames indent prefix (Node (Account fullname _ bal) subs)
          | isboringparent && hasmatchedsubs = subsprefixed
          | ismatched = this ++ subsindented
          | otherwise = subsnoindent
          where
            subsprefixed = showsubs indent (prefix++leafname++":")
            subsnoindent = showsubs indent ""
            subsindented = showsubs (indent+1) ""
            showsubs i p = concatMap (showAccountTreeWithBalances' matchednames i p) subs
            hasmatchedsubs = any ((`elem` matchednames) . aname) $ concatMap flatten subs
            amt = padleft 20 $ showMixedAmount bal
            this = concatTopPadded [amt, spaces ++ prefix ++ leafname] ++ "\n"
            spaces = "  " ++ replicate (indent * 2) ' '
            leafname = accountLeafName fullname
            ismatched = fullname `elem` matchednames
            isboringparent = numsubs >= 1 && (bal == subbal || not matched)
            numsubs = length subs
            subbal = abalance $ root $ head subs
            matched = fullname `elem` matchednames

