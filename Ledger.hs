module Ledger
where
import qualified Data.Map as Map
import Data.Map ((!))
import Utils
import Types
import Amount
import Account
import AccountName
import Transaction
import LedgerFile


rawLedgerTransactions :: LedgerFile -> [Transaction]
rawLedgerTransactions l = entryTransactionsFrom $ entries l

rawLedgerAccountNamesUsed :: LedgerFile -> [AccountName]
rawLedgerAccountNamesUsed l = accountNamesFromTransactions $ entryTransactionsFrom $ entries l

rawLedgerAccountNames :: LedgerFile -> [AccountName]
rawLedgerAccountNames = sort . expandAccountNames . rawLedgerAccountNamesUsed

rawLedgerAccountNameTree :: LedgerFile -> Tree AccountName
rawLedgerAccountNameTree l = accountNameTreeFrom $ rawLedgerAccountNames l


instance Show Ledger where
    show l = printf "Ledger with %d entries, %d accounts"
             ((length $ entries $ rawledger l) +
              (length $ modifier_entries $ rawledger l) +
              (length $ periodic_entries $ rawledger l))
             (length $ accountnames l)

-- at startup, we augment the parsed ledger entries with an account map
-- and other things useful for performance
cacheLedger :: LedgerFile -> Ledger
cacheLedger l = 
    let 
        ant = rawLedgerAccountNameTree l
        ans = flatten ant
        ts = rawLedgerTransactions l
        sortedts = sortBy (comparing account) ts
        groupedts = groupBy (\t1 t2 -> account t1 == account t2) sortedts
        tmap = Map.union 
               (Map.fromList [(account $ head g, g) | g <- groupedts])
               (Map.fromList [(a,[]) | a <- ans])
        txns a = tmap ! a
        subaccts a = filter (isAccountNamePrefixOf a) ans
        subtxns a = concat [txns a | a <- [a] ++ subaccts a]
        lprecision = maximum $ map (precision . tamount . transaction) ts
        bmap = Map.union 
               (Map.fromList [(a, (sumEntryTransactions $ subtxns a){precision=lprecision}) | a <- ans])
               (Map.fromList [(a,nullamt) | a <- ans])
        amap = Map.fromList [(a, Account a (tmap ! a) (bmap ! a)) | a <- ans]
    in
      Ledger l ant amap lprecision

accountnames :: Ledger -> [AccountName]
accountnames l = flatten $ accountnametree l

ledgerAccount :: Ledger -> AccountName -> Account
ledgerAccount l a = (accounts l) ! a

-- This sets all amount precisions to that of the highest-precision
-- amount, to help with report output. It should perhaps be done in the
-- display functions, but those are far removed from the ledger. Keep in
-- mind if doing more arithmetic with these.
ledgerTransactions :: Ledger -> [Transaction]
ledgerTransactions l = 
    setprecisions $ rawLedgerTransactions $ rawledger l
    where
      setprecisions = map (entryTransactionSetPrecision (lprecision l))

ledgerTransactionsMatching :: ([String],[String]) -> Ledger -> [Transaction]
ledgerTransactionsMatching ([],[]) l = ledgerTransactionsMatching ([".*"],[".*"]) l
ledgerTransactionsMatching (rs,[]) l = ledgerTransactionsMatching (rs,[".*"]) l
ledgerTransactionsMatching ([],rs) l = ledgerTransactionsMatching ([".*"],rs) l
ledgerTransactionsMatching (acctpats,descpats) l =
    intersect 
    (concat [filter (matchTransactionAccount r) ts | r <- acctregexps])
    (concat [filter (matchTransactionDescription r) ts | r <- descregexps])
    where 
      ts = ledgerTransactions l
      acctregexps = map mkRegex acctpats
      descregexps = map mkRegex descpats

ledgerAccountTreeMatching :: Ledger -> [String] -> Bool -> Int -> Tree Account
ledgerAccountTreeMatching l [] showsubs maxdepth = 
    ledgerAccountTreeMatching l [".*"] showsubs maxdepth
ledgerAccountTreeMatching l acctpats showsubs maxdepth = 
    addDataToAccountNameTree l $ 
    filterAccountNameTree acctpats showsubs maxdepth $ 
    accountnametree l

addDataToAccountNameTree :: Ledger -> Tree AccountName -> Tree Account
addDataToAccountNameTree = treemap . ledgerAccount

-- balance report support
--
-- examples: here is a sample account tree:
--
-- assets
--  cash
--  checking
--  saving
-- equity
-- expenses
--  food
--  shelter
-- income
--  salary
-- liabilities
--  debts
--
-- standard balance command shows all top-level accounts:
--
-- > ledger bal
-- $ assets      
-- $ equity
-- $ expenses    
-- $ income      
-- $ liabilities 
--
-- with an account pattern, show only the ones with matching names:
--
-- > ledger bal asset
-- $ assets      
--
-- with -s, show all subaccounts of matched accounts:
--
-- > ledger -s bal asset
-- $ assets      
-- $  cash       
-- $  checking   
-- $  saving
--
-- we elide boring accounts in two ways:
-- - leaf accounts and branches with 0 balance or 0 transactions are omitted
-- - inner accounts with 0 transactions and 1 subaccount are displayed inline
-- so this:
--
-- a (0 txns)
--   b (0 txns)
--     c
--       d
-- e (0 txns)
--   f
--   g
-- h (0 txns)
--   i (0 balance)
--
-- is displayed like:
--
-- a:b:c
--   d
-- e
--   f
--   g

showLedgerAccounts :: Ledger -> [String] -> Bool -> Int -> String
showLedgerAccounts l acctpats showsubs maxdepth = 
    concatMap 
    (showAccountTree l) 
    (branches (ledgerAccountTreeMatching l acctpats showsubs maxdepth))

showAccountTree :: Ledger -> Tree Account -> String
showAccountTree l = showAccountTree' l 0 . pruneBoringBranches

showAccountTree' :: Ledger -> Int -> Tree Account -> String
showAccountTree' l indentlevel t
    -- skip a boring inner account
    | length subs > 0 && isBoringAccount l acct = subsindented 0
    -- otherwise show normal indented account name with balance, 
    -- prefixing the names of any boring parents
    | otherwise = 
        bal ++ "  " ++ indent ++ prefix ++ leafname ++ "\n" ++ (subsindented 1)
    where
      acct = root t
      subs = branches t
      subsindented i = concatMap (showAccountTree' l (indentlevel+i)) subs
      bal = printf "%20s" $ show $ abalance $ acct
      indent = replicate (indentlevel * 2) ' '
      prefix = concatMap (++ ":") $ map accountLeafName boringparents
      boringparents = takeWhile (isBoringAccountName l) $ parentAccountNames $ aname acct
      leafname = accountLeafName $ aname acct

isBoringAccount :: Ledger -> Account -> Bool
isBoringAccount l a
    | name == "top" = False
    | (length txns == 0) && ((length subs) == 1) = True
    | otherwise = False
    where      
      name = aname a
      txns = atransactions a
      subs = subAccountNamesFrom (accountnames l) name

isBoringAccountName :: Ledger -> AccountName -> Bool
isBoringAccountName l = isBoringAccount l . ledgerAccount l

pruneBoringBranches :: Tree Account -> Tree Account
pruneBoringBranches =
    treefilter hastxns . treefilter hasbalance
    where 
      hasbalance = (/= 0) . abalance
      hastxns = (> 0) . length . atransactions

