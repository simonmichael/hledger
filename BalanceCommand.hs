{-| 

A ledger-compatible @balance@ command. 

ledger's balance command is easy to use but hard to describe precisely.
Here are some attempts.


I. high level description with examples
---------------------------------------

We'll use sample.ledger, which has the following account tree:

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
Subaccounts are displayed with more indentation. Each balance is the sum
of any transactions in that account plus any balances from subaccounts:

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

Usually, the non-interesting accounts are elided or omitted. To be
precise, an interesting account is one with: a non-zero balance, or a
balance different from its single subaccount, or two or more interesting
subaccounts. (More subtleties to be filled in here.)

So, above, @checking@ is omitted because it has no interesting subaccounts
and a zero balance. @bank@ is elided because it has only a single
interesting subaccount (saving) and it would be showing the same balance
($1). Ditto for @liabilities@.

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


II. Some notes for the implementation
-------------------------------------

- a simple balance report shows top-level accounts

- with an account pattern, it shows accounts whose leafname matches, plus their parents

- with the subtotal option, it also shows all subaccounts of the above

- zero-balance leaf accounts are removed

- the resulting account tree is displayed with each account's aggregated
  balance, with boring parents prefixed to the next line

- a boring parent has the same balance as its child and is not explicitly
  matched by the display options.

- the sum of the balances shown is displayed at the end, if it is non-zero


III. John's description 2009/02
-------------------------------

johnw: \"Since you've been re-implementing the balance report in Haskell, I thought
I'd share with you in pseudocode how I rewrote it for Ledger 3.0, since
the old method never stopped with the bugs.  The new scheme uses a 5 stage
algorithm, with each stage gathering information for the next:

STEP 1

Based on the user's query, walk through all the transactions in their
journal, finding which ones to include in the account subtotals. For each
transaction that matches, mark the account as VISITED.

STEP 2

Recursively walk the accounts tree, depth-first, computing aggregate
totals and aggregate \"counts\" (number of transactions contributing to the
aggregate total).

STEP 3

Walk the account tree again, breadth-first, and for every VISITED account,
check whether it matches the user's \"display predicate\".  If so, mark the
account as MATCHING.

STEP 4

Do an in-order traversal of the account tree.  Except for the top-most
account (which serves strictly as a container for the other accounts):

a. If the account was MATCHING, or two or more of its children are
   MATCHING or had descendents who were MATCHING, display the account.

b. Otherwise, if the account had *any* children or descendants who
   were VISITED and *no* children or descendants who were MATCHING,
   then apply the display predicate from STEP 3 to the account.  If
   it matches, also print this account.  (This step allows -E to
   report empty accounts which otherwise did match the original
   query).

STEP 5

When printing an account, display a \"depth spacer\" followed by the \"partial name\".
tal
The partial name is found by taking the base account's name, then
prepending to it every non-MATCHING parent until a MATCHING parent is
found.

The depth spacer is found by outputting two spaces for every MATCHING parent.

This way, \"Assets:Bank:Checking\" might be reported as:

 Assets
   Bank
     Checking

or

 Assets
   Bank:Checking

or

 Assets:Bank:Checking

Depending on whether the parents were or were not reported for their own reasons.
\"

\"I just had to add one more set of tree traversals, to correctly determine
whether a final balance should be displayed

without --flat, sort at each level in the hierarchy
with --flat, sort across all accounts\"

IV. A functional description
-----------------------------

1. filter the transactions, keeping only those included in the calculation.
   Remember the subset of accounts involved. (VISITED)

2. generate a full account & balance tree from all transactions

3. Remember the subset of VISITED accounts which are matched for display.
   (MATCHING)

4. walk through the account tree:

   a. If the account is in MATCHING, or two or more of its children are or
      have descendants who are, display it.

   b. Otherwise, if the account has any children or descendants in VISITED
      but none in MATCHING, and it is matched for display, display it.
      (allows -E to report empty accounts which otherwise did match the
      original query).

5. when printing an account, display a \"depth spacer\" followed by the
   \"partial name\".  The partial name is found by taking the base account's
   name, then prepending to it every non-MATCHING parent until a MATCHING
   parent is found. The depth spacer is two spaces per MATCHING parent.

6. I just had to add one more set of tree traversals, to correctly
   determine whether a final balance should be displayed

7. without --flat, sort at each level in the hierarchy
   with --flat, sort across all accounts

V. Another functional description with new terminology
------------------------------------------------------

- included transactions are those included in the calculation, specified
  by -b, -e, -p, -C, -R, account patterns and description patterns.

- included accounts are the accounts referenced by included transactions.

- matched transactions are the included transactions which match the
  display predicate, specified by -d.

- matched accounts are the included accounts which match the display
  predicate, specified by -d, --depth, -E, -s

- an account name tree is the full hierarchy of account names implied by a
  set of transactions

- an account tree is an account name tree augmented with the aggregate
  balances and transaction counts for each named account

- the included account tree is the account tree for the included transactions

- a matched account tree contains one or more matched accounts

- to generate the balance report, walk through the included account tree
  and display each account if

  - it is matching

  - or it has two more more matching subtrees

  - or it has included offspring but no matching offspring

- to display an account, display an indent then the \"partial name\".  The
  partial name is the account's name, prefixed by each unmatched parent
  until a matched parent is found. The indent is two spaces per matched
  parent.


VI. John's description 2009/03/11
---------------------------------

johnw: \"Well, I had to rewrite the balance reporting code yet again, 
because it wouldn't work with --depth correctly.  Here's the new algorithm.

STEP 1: Walk all postings, looking for those that match the user's query.
       As a match is found, mark its account VISITED.

STEP 2: Do a traversal of all accounts, sorting as need be, and collect
       them all into an ordered list.

STEP 3: Keeping that list on the side, do a *depth-first* traversal of
       the account tree.

       visited    = 0
       to_display = 0

       (visited, to_display) += <recurse for all immediate children>

       if account is VISITED or (no --flat and visited > 0):
           if account matches display predicate and
              (--flat or to_display != 1):
               mark account as TO_DISPLAY
               to_display = 1
           visited = 1

       return (visited, to_display)

STEP 4: top_displayed = 0

       for every account in the ordered list:
           if account has been marked TO_DISPLAY:
               mark account as DISPLAYED
               format the account and print

           if --flat and account is DISPLAYED:
               top_displayed += 1

       if no --flat:
           for every top-most account:
               if account is DISPLAYED or any children or DISPLAYED:
                   top_displayed += 1

       if no --no-total and top_displayed > 1 and
          top-most accounts sum to a non-zero balance:
           output separator
           output balance sum account as DISPLAYED
               format the account and print

           if --flat and account is DISPLAYED:
               top_displayed += 1

       if no --flat:
           for every top-most account:
               if account is DISPLAYED or any children or DISPLAYED:
                   top_displayed += 1

       if no --no-total and top_displayed > 1 and
          top-most accounts sum to a non-zero balance:
           output separator
           output balance sum
\"


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
      totalstr | NoTotal `elem` opts = ""
               | not (Empty `elem` opts) && isZeroMixedAmount total = ""
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

