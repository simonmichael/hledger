
module Models -- data types & behaviours
where

import Debug.Trace
import Text.Printf
import Text.Regex
import Data.List

import Utils
import Account

-- basic types

type Date = String
type Status = Bool

-- amounts
-- amount arithmetic currently ignores currency conversion

data Amount = Amount {
                      currency :: String,
                      quantity :: Double
                     } deriving (Eq,Ord)

instance Num Amount where
    abs (Amount c q) = Amount c (abs q)
    signum (Amount c q) = Amount c (signum q)
    fromInteger i = Amount "$" (fromInteger i)
    (+) = amountAdd
    (-) = amountSub
    (*) = amountMult
Amount ca qa `amountAdd` Amount cb qb = Amount ca (qa + qb)
Amount ca qa `amountSub` Amount cb qb = Amount ca (qa - qb)
Amount ca qa `amountMult` Amount cb qb = Amount ca (qa * qb)

instance Show Amount where show = amountRoundedOrZero

amountRoundedOrZero :: Amount -> String
amountRoundedOrZero (Amount cur qty) =
    let rounded = printf "%.2f" qty in
    case rounded of
      "0.00"    -> "0"
      "-0.00"   -> "0"
      otherwise -> cur ++ rounded

-- modifier & periodic entries

data ModifierEntry = ModifierEntry { -- aka "automated entry"
                    valueexpr :: String,
                    m_transactions :: [Transaction]
                   } deriving (Eq)

instance Show ModifierEntry where 
    show e = "= " ++ (valueexpr e) ++ "\n" ++ unlines (map show (m_transactions e))

data PeriodicEntry = PeriodicEntry {
                    periodexpr :: String,
                    p_transactions :: [Transaction]
                   } deriving (Eq)

instance Show PeriodicEntry where 
    show e = "~ " ++ (periodexpr e) ++ "\n" ++ unlines (map show (p_transactions e))

-- entries
-- a register entry is displayed as two or more lines like this:
-- date       description          account                 amount       balance
-- DDDDDDDDDD dddddddddddddddddddd aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
--                                 aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
--                                 ...                     ...         ...
-- dateWidth = 10
-- descWidth = 20
-- acctWidth = 22
-- amtWidth  = 11
-- balWidth  = 12

data Entry = Entry {
                    edate :: Date,
                    estatus :: Status,
                    ecode :: String,
                    edescription :: String,
                    etransactions :: [Transaction]
                   } deriving (Eq,Ord)

instance Show Entry where show = showEntry

showEntry e = (showDate $ edate e) ++ " " ++ (showDescription $ edescription e) ++ " "
showDate d = printf "%-10s" d
showDescription s = printf "%-20s" (elideRight 20 s)

isEntryBalanced :: Entry -> Bool
isEntryBalanced e = (sumTransactions . etransactions) e == 0

autofillEntry :: Entry -> Entry
autofillEntry e = 
    Entry (edate e) (estatus e) (ecode e) (edescription e)
              (autofillTransactions (etransactions e))

-- transactions

data Transaction = Transaction {
                                taccount :: AccountName,
                                tamount :: Amount
                               } deriving (Eq,Ord)

instance Show Transaction where show = showTransaction

showTransaction t = (showAccountName $ taccount t) ++ "  " ++ (showAmount $ tamount t) 
showAmount amt = printf "%11s" (show amt)
showAccountName s = printf "%-22s" (elideRight 22 s)

elideRight width s =
    case length s > width of
      True -> take (width - 2) s ++ ".."
      False -> s

-- elideAccountRight width abbrevlen a = 
--     case length a > width of
--       False -> a
--       True -> abbreviateAccountComponent abbrevlen a 
        
-- abbreviateAccountComponent abbrevlen a =
--     let components = splitAtElement ':' a in
--     case 
    
autofillTransactions :: [Transaction] -> [Transaction]
autofillTransactions ts =
    let (ns, as) = partition isNormal ts
            where isNormal t = (currency $ tamount t) /= "AUTO" in
    case (length as) of
      0 -> ns
      1 -> ns ++ [balanceTransaction $ head as]
          where balanceTransaction t = t{tamount = -(sumTransactions ns)}
      otherwise -> error "too many blank transactions in this entry"

sumTransactions :: [Transaction] -> Amount
sumTransactions ts = sum [tamount t | t <- ts]

-- entrytransactions
-- We parse Entries containing Transactions and flatten them into
-- (entry,transaction) pairs (entrytransactions, hereafter referred to as
-- "transactions") for easier processing. (So far, these types have
-- morphed through E->T; (T,E); ET; E<->T; (E,T)).

type EntryTransaction = (Entry,Transaction)

entry       (e,t) = e
transaction (e,t) = t
date        (e,t) = edate e
status      (e,t) = estatus e
code        (e,t) = ecode e
description (e,t) = edescription e
account     (e,t) = taccount t
amount      (e,t) = tamount t
                                         
flattenEntry :: Entry -> [EntryTransaction]
flattenEntry e = [(e,t) | t <- etransactions e]

entryTransactionsFrom :: [Entry] -> [EntryTransaction]
entryTransactionsFrom es = concat $ map flattenEntry es

sumEntryTransactions :: [EntryTransaction] -> Amount
sumEntryTransactions ets = 
    sumTransactions $ map transaction ets

matchTransactionAccount :: String -> EntryTransaction -> Bool
matchTransactionAccount s t =
    case matchRegex (mkRegex s) (account t) of
      Nothing -> False
      otherwise -> True

matchTransactionDescription :: String -> EntryTransaction -> Bool
matchTransactionDescription s t =
    case matchRegex (mkRegex s) (description t) of
      Nothing -> False
      otherwise -> True

showTransactionsWithBalances :: [EntryTransaction] -> Amount -> String
showTransactionsWithBalances [] _ = []
showTransactionsWithBalances ts b =
    unlines $ showTransactionsWithBalances' ts dummyt b
        where
          dummyt = (Entry "" False "" "" [], Transaction "" (Amount "" 0))
          showTransactionsWithBalances' [] _ _ = []
          showTransactionsWithBalances' (t:ts) tprev b =
              (if (entry t /= (entry tprev))
               then [showTransactionDescriptionAndBalance t b']
               else [showTransactionAndBalance t b'])
              ++ (showTransactionsWithBalances' ts t b')
                  where b' = b + (amount t)

showTransactionDescriptionAndBalance :: EntryTransaction -> Amount -> String
showTransactionDescriptionAndBalance t b =
    (showEntry $ entry t) ++ (showTransaction $ transaction t) ++ (showBalance b)

showTransactionAndBalance :: EntryTransaction -> Amount -> String
showTransactionAndBalance t b =
    (replicate 32 ' ') ++ (showTransaction $ transaction t) ++ (showBalance b)

showBalance :: Amount -> String
showBalance b = printf " %12s" (amountRoundedOrZero b)

-- more account functions

accountNamesFromTransactions :: [EntryTransaction] -> [AccountName]
accountNamesFromTransactions ts = nub $ map account ts

-- like expandAccountNames, but goes from the top down and elides accountNames
-- with only one child and no transactions. Returns accountNames paired with
-- the appropriate indented name. Eg
-- [("assets","assets"),("assets:cash:gifts","  cash:gifts"),("assets:checking","  checking")]
expandAccountNamesMostly :: Ledger -> [AccountName] -> [(AccountName, String)]
expandAccountNamesMostly l as = concat $ map (expandAccountNameMostly l) as
    where 
      expandAccountNameMostly :: Ledger -> AccountName -> [(AccountName, String)]
      expandAccountNameMostly l a =
          [(acct, acctname)] ++ (concat $ map (expandAccountNameMostly l) subs)
              where 
                subs = subAccountNames l a
                txns = accountTransactionsNoSubs l a
                (acct, acctname) = 
                    case (length subs == 1) && (length txns == 0) of
                      False -> (a, indentAccountName a)
                      True -> (a, indentAccountName a ++ ":" ++ subname)
                        where 
                          sub = head subs
                          subname = (reverse . takeWhile (/= ':') . reverse) sub

subAccountNames :: Ledger -> AccountName -> [AccountName]
subAccountNames l a = [a' | a' <- ledgerAccountNames l, a `isSubAccountNameOf` a']

showAccountNamesWithBalances :: [(AccountName,String)] -> Ledger -> String
showAccountNamesWithBalances as l =
    unlines $ map (showAccountNameAndBalance l) as

showAccountNameAndBalance :: Ledger -> (AccountName, String) -> String
showAccountNameAndBalance l (a, adisplay) =
    printf "%20s  %s" (showBalance $ accountBalance l a) adisplay

accountBalance :: Ledger -> AccountName -> Amount
accountBalance l a =
    sumEntryTransactions (accountTransactions l a)

accountTransactions :: Ledger -> AccountName -> [EntryTransaction]
accountTransactions l a = ledgerTransactionsMatching (["^" ++ a ++ "(:.+)?$"], []) l

accountBalanceNoSubs :: Ledger -> AccountName -> Amount
accountBalanceNoSubs l a =
    sumEntryTransactions (accountTransactionsNoSubs l a)

accountTransactionsNoSubs :: Ledger -> AccountName -> [EntryTransaction]
accountTransactionsNoSubs l a = ledgerTransactionsMatching (["^" ++ a ++ "$"], []) l

addDataToAccounts :: Ledger -> (Tree AccountName) -> (Tree AccountData)
addDataToAccounts l acct = 
    Tree (acctdata, map (addDataToAccounts l) (atsubs acct))
        where 
          aname = atacct acct
          atxns = accountTransactionsNoSubs l aname
          abal = accountBalance l aname
          acctdata = (aname, atxns, abal)

-- an AccountData tree adds some other things we want to cache for
-- convenience, like the account's balance and transactions.
type AccountData = (AccountName,[EntryTransaction],Amount)
type AccountDataTree = Tree AccountData
adtdata = fst . unTree
adtsubs = snd . unTree
nullad = Tree (("", [], 0), [])
adname (a,_,_) = a
adtxns (_,ts,_) = ts
adamt (_,_,amt) = amt

-- a (2 txns)
--   b (boring acct - 0 txns, exactly 1 sub)
--     c (5 txns)
--       d
-- to:
-- a (2 txns)
--   b:c (5 txns)
--     d

-- elideAccount adt = adt

-- elideAccount :: Tree AccountData -> Tree AccountData
-- elideAccount adt = adt
    

-- a
--   b
--     c
-- d
-- to:
-- $7  a
-- $5    b
-- $5      c
-- $0  d
showAccountWithBalances :: Ledger -> (Tree AccountData) -> String
showAccountWithBalances l adt = (showAccountsWithBalance l) (adtsubs adt)

showAccountsWithBalance :: Ledger -> [Tree AccountData] -> String
showAccountsWithBalance l adts =
    concatMap showAccountDataBranch adts
        where
          showAccountDataBranch :: Tree AccountData -> String
          showAccountDataBranch adt = 
              case boring of
                True  -> 
                False -> topacct ++ "\n" ++ subs
              where
                topacct = (showAmount abal) ++ "  " ++ (indentAccountName aname)
                showAmount amt = printf "%11s" (show amt)
                aname = adname $ adtdata adt
                atxns = adtxns $ adtdata adt
                abal = adamt $ adtdata adt
                subs = (showAccountsWithBalance l) $ adtsubs adt
                boring = (length atxns == 0) && ((length subs) == 1)

    


-- ledger

data Ledger = Ledger {
                      modifier_entries :: [ModifierEntry],
                      periodic_entries :: [PeriodicEntry],
                      entries :: [Entry]
                     } deriving (Eq)

instance Show Ledger where
    show l = "Ledger with " ++ m ++ " modifier, " ++ p ++ " periodic, " ++ e ++ " normal entries:\n"
                     ++ (concat $ map show (modifier_entries l))
                     ++ (concat $ map show (periodic_entries l))
                     ++ (concat $ map show (entries l))
                     where 
                       m = show $ length $ modifier_entries l
                       p = show $ length $ periodic_entries l
                       e = show $ length $ entries l

ledgerTransactions :: Ledger -> [EntryTransaction]
ledgerTransactions l = entryTransactionsFrom $ entries l

ledgerTransactionsMatching :: ([String],[String]) -> Ledger -> [EntryTransaction]
ledgerTransactionsMatching ([],[]) l = ledgerTransactionsMatching ([".*"],[".*"]) l
ledgerTransactionsMatching (rs,[]) l = ledgerTransactionsMatching (rs,[".*"]) l
ledgerTransactionsMatching ([],rs) l = ledgerTransactionsMatching ([".*"],rs) l
ledgerTransactionsMatching (acctregexps,descregexps) l =
    intersect 
    (concat [filter (matchTransactionAccount r) ts | r <- acctregexps])
    (concat [filter (matchTransactionDescription r) ts | r <- descregexps])
    where ts = ledgerTransactions l

ledgerAccountNamesUsed :: Ledger -> [AccountName]
ledgerAccountNamesUsed l = accountNamesFromTransactions $ entryTransactionsFrom $ entries l

ledgerAccountNames :: Ledger -> [AccountName]
ledgerAccountNames = sort . expandAccountNames . ledgerAccountNamesUsed

ledgerTopAccountNames :: Ledger -> [AccountName]
ledgerTopAccountNames l = filter (notElem ':') (ledgerAccountNames l)

ledgerAccountNamesMatching :: [String] -> Ledger -> [AccountName]
ledgerAccountNamesMatching [] l = ledgerAccountNamesMatching [".*"] l
ledgerAccountNamesMatching acctregexps l =
    concat [filter (matchAccountName r) accountNames | r <- acctregexps]
        where accountNames = ledgerTopAccountNames l

ledgerAccounts :: Ledger -> Tree AccountName
ledgerAccounts l = accountFrom $ ledgerAccountNames l

ledgerAccountsData :: Ledger -> Tree AccountData
ledgerAccountsData l = addDataToAccounts l (ledgerAccounts l)

showLedgerAccountsWithBalances :: Ledger -> String
showLedgerAccountsWithBalances l =
    showAccountWithBalances l (ledgerAccountsData l)
