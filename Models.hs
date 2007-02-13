
module Models -- data types & behaviours
where

import Text.Printf
import Data.List

-- basic types

type Date = String
type Status = Bool
type Account = String

-- amounts
-- amount arithmetic currently ignores currency conversion

data Amount = Amount {
                      currency :: String,
                      quantity :: Double
                     } deriving (Eq)

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

instance Show Amount where
    show (Amount cur qty) = 
        let roundedqty = printf "%.2f" qty in
        case roundedqty of
          "0.00" -> "0"
          otherwise -> cur ++ roundedqty

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
-- date       description          account                    amount     balance
-- DDDDDDDDDD dddddddddddddddddddd aaaaaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAA AAAAAAAAAA
--                                 aaaaaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAA AAAAAAAAAA
--                                 ...                        ...        ...
-- dateWidth = 10
-- descWidth = 20
-- acctWidth = 25
-- amtWidth  = 10
-- balWidth  = 10

data Entry = Entry {
                    edate :: Date,
                    estatus :: Status,
                    ecode :: String,
                    edescription :: String,
                    etransactions :: [Transaction]
                   } deriving (Eq)

instance Show Entry where show = showEntry

showEntry e = printf "%-10s %-20s " (edate e) (take 20 $ edescription e)

isEntryBalanced :: Entry -> Bool
isEntryBalanced e = (sumTransactions . etransactions) e == 0

autofillEntry :: Entry -> Entry
autofillEntry e = 
    Entry (edate e) (estatus e) (ecode e) (edescription e)
              (autofillTransactions (etransactions e))

-- transactions

data Transaction = Transaction {
                                taccount :: Account,
                                tamount :: Amount
                               } deriving (Eq)

instance Show Transaction where show = showTransaction

showTransaction t = printf "%-25s  %10s" (take 25 $ taccount t) (show $ tamount t)

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

matchTransactionAccount :: String -> EntryTransaction -> Bool
matchTransactionAccount s t = s `isInfixOf` (account t)

matchTransactionDescription :: String -> EntryTransaction -> Bool
matchTransactionDescription s t = s `isInfixOf` (description t)

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

showBalance b = printf " %10.2s" (show b)

-- accounts

accountsFromTransactions :: [EntryTransaction] -> [Account]
accountsFromTransactions ts = nub $ map account ts

-- ["a:b:c","d:e"] -> ["a","a:b","a:b:c","d","d:e"]
expandAccounts :: [Account] -> [Account]
expandAccounts l = nub $ concat $ map expand l
                where
                  expand l' = map (concat . intersperse ":") (tail $ inits $ splitAtElement ':' l')

splitAtElement :: Eq a => a -> [a] -> [[a]]
splitAtElement e l = 
    case dropWhile (e==) l of
      [] -> []
      l' -> first : splitAtElement e rest
        where
          (first,rest) = break (e==) l'

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

ledgerAccountsUsed :: Ledger -> [Account]
ledgerAccountsUsed l = accountsFromTransactions $ entryTransactionsFrom $ entries l

ledgerAccountTree :: Ledger -> [Account]
ledgerAccountTree = sort . expandAccounts . ledgerAccountsUsed

ledgerTransactions :: Ledger -> [EntryTransaction]
ledgerTransactions l = entryTransactionsFrom $ entries l

ledgerTransactionsMatching :: String -> Ledger -> [EntryTransaction]
ledgerTransactionsMatching s l = filter (\t -> matchTransactionAccount s t) (ledgerTransactions l)

