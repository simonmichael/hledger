
module Models -- data types & behaviours
where

import Text.Printf
import Data.List

-- types

data Ledger = Ledger {
                      modifier_entries :: [ModifierEntry],
                      periodic_entries :: [PeriodicEntry],
                      entries :: [Entry]
                     } deriving (Eq)
data ModifierEntry = ModifierEntry { -- aka "automated entry"
                    valueexpr :: String,
                    m_transactions :: [Transaction]
                   } deriving (Eq)
data PeriodicEntry = PeriodicEntry {
                    periodexpr :: String,
                    p_transactions :: [Transaction]
                   } deriving (Eq)
data Entry = Entry {
                    date :: Date,
                    status :: Status,
                    code :: String,
                    description :: String,
                    transactions :: [Transaction]
                   } deriving (Eq)
data Transaction = Transaction {
                                account :: Account,
                                amount :: Amount
                               } deriving (Eq)
data Amount = Amount {
                      currency :: String,
                      quantity :: Double
                     } deriving (Eq)
type Date = String
type Status = Bool
type Account = String

-- Amount arithmetic - ignores currency conversion

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

-- show & display methods

instance Show Ledger where
    show l = "Ledger with " ++ m ++ " modifier, " ++ p ++ " periodic, " ++ e ++ " normal entries:\n"
                     ++ (concat $ map show (modifier_entries l))
                     ++ (concat $ map show (periodic_entries l))
                     ++ (concat $ map show (entries l))
                     where 
                       m = show $ length $ modifier_entries l
                       p = show $ length $ periodic_entries l
                       e = show $ length $ entries l

instance Show ModifierEntry where 
    show e = "= " ++ (valueexpr e) ++ "\n" ++ unlines (map show (m_transactions e))

instance Show PeriodicEntry where 
    show e = "~ " ++ (periodexpr e) ++ "\n" ++ unlines (map show (p_transactions e))

instance Show Entry where show = showEntry

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

showEntry :: Entry -> String
showEntry e = unlines $ map fst (entryLines e)

-- convert an Entry to entry lines (string, amount pairs)
entryLines :: Entry -> [(String,Amount)]
entryLines e =
    [firstline] ++ otherlines
        where 
          t:ts = transactions e
          entrydesc = printf "%-10s %-20s " (date e) (take 20 $ description e)
          firstline = (entrydesc ++ (show t), amount t)
          otherlines = map (\t -> (prependSpace $ show t, amount t)) ts
          prependSpace = (replicate 32 ' ' ++)

instance Show Transaction where 
    show t = printf "%-25s  %10s" (take 25 $ account t) (show $ amount t)

instance Show Amount where
    show (Amount cur qty) = 
        let roundedqty = printf "%.2f" qty in
        case roundedqty of
          "0.00" -> "0"
          otherwise -> cur ++ roundedqty

-- in the register report we show entries plus a running balance

showEntriesWithBalances :: [Entry] -> Amount -> String
showEntriesWithBalances [] _ = ""
showEntriesWithBalances (e:es) b =
    showEntryWithBalances e b ++ (showEntriesWithBalances es b')
        where b' = b + (entryBalance e)

entryBalance :: Entry -> Amount
entryBalance = sumTransactions . transactions

showEntryWithBalances :: Entry -> Amount -> String
showEntryWithBalances e b =
    unlines [s | (s,a,b) <- entryLinesWithBalances (entryLines e) b]

entryLinesWithBalances :: [(String,Amount)] -> Amount -> [(String,Amount,Amount)]
entryLinesWithBalances [] _ = []
entryLinesWithBalances ((str,amt):els) bal = 
    [(str',amt,bal')] ++ entryLinesWithBalances els bal'
        where
          bal' = bal + amt
          str' = str ++ (printf " %10.2s" (show bal'))

-- misc

autofillEntry :: Entry -> Entry
autofillEntry e = 
    Entry (date e) (status e) (code e) (description e)
              (autofillTransactions (transactions e))

autofillTransactions :: [Transaction] -> [Transaction]
autofillTransactions ts =
    let (ns, as) = normalAndAutoTransactions ts in
    case (length as) of
      0 -> ns
      1 -> ns ++ [balanceTransaction $ head as]
          where balanceTransaction t = t{amount = -(sumTransactions ns)}
      otherwise -> error "too many blank transactions in this entry"

normalAndAutoTransactions :: [Transaction] -> ([Transaction], [Transaction])
normalAndAutoTransactions ts = 
    partition isNormal ts
        where isNormal t = (currency $ amount t) /= "AUTO"

sumTransactions :: [Transaction] -> Amount
sumTransactions ts = sum [amount t | t <- ts]

transactionsFromEntries :: [Entry] -> [Transaction]
transactionsFromEntries es = concat $ map transactions es

accountsFromTransactions :: [Transaction] -> [Account]
accountsFromTransactions ts = nub $ map account ts

accountsUsed :: Ledger -> [Account]
accountsUsed l = accountsFromTransactions $ transactionsFromEntries $ entries l

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

accountTree :: Ledger -> [Account]
accountTree = sort . expandAccounts . accountsUsed

entriesMatching :: String -> Ledger -> [Entry]
entriesMatching s l = filterEntriesByAccount s (entries l)

filterEntriesByAccount :: String -> [Entry] -> [Entry]
filterEntriesByAccount s es = filter (matchEntryAccount s) es

matchEntryAccount :: String -> Entry -> Bool
matchEntryAccount s e = any (matchTransactionAccount s) (transactions e)

matchTransactionAccount :: String -> Transaction -> Bool
matchTransactionAccount s t = s `isInfixOf` (account t)
