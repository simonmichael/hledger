-- a data model
module Types where

import Text.Printf

data Ledger = Ledger {
                      modifier_entries :: [ModifierEntry],
                      periodic_entries :: [PeriodicEntry],
                      entries :: [Entry]
                     } deriving (Eq)
data ModifierEntry = ModifierEntry { -- aka automated entry
                    valueexpr :: String,
                    m_transactions :: [Transaction]
                   } deriving (Eq)
data PeriodicEntry = PeriodicEntry {
                    periodexpr :: String,
                    p_transactions :: [Transaction]
                   } deriving (Eq)
data Entry = Entry {
                    date :: Date,
                    status :: Bool,
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
type Account = String

-- Amount arithmetic
-- ignores currency conversion
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

showEntryOld :: Entry -> String
showEntryOld e = date e ++ " " ++ s ++ c ++ d ++ "\n" ++ unlines (map show (transactions e))
        where 
          d = description e
          s = case (status e) of {True -> "* "; False -> ""}
          c = case (length(code e) > 0) of {True -> (code e ++ " "); False -> ""}

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

-- convert an Entry to entry lines (string, amount pairs)
entryLines :: Entry -> [(String,Amount)]
entryLines e =
    [(entrydesc ++ (show t), amount t)]
    ++ map (\t -> (prependSpace $ show t, amount t)) ts
        where 
          t:ts = transactions e
          entrydesc = printf "%-10s %-20s " (date e) (take 20 $ description e)
          prependSpace = (printf (take 32 (repeat ' ')) ++)

instance Show Transaction where 
    show t = printf "%-25s  %10s " (take 25 $ account t) (show $ amount t)

instance Show Amount where
    show (Amount cur qty) = 
        let roundedqty = printf "%.2f" qty in
        case roundedqty of
          "0.00" -> "0"
          otherwise -> cur ++ roundedqty

showEntry :: Entry -> String
showEntry e = unlines $ map fst (entryLines e)

-- add balances to entry lines, given a starting balance
entryLinesWithBalances :: [(String,Amount)] -> Amount -> [(String,Amount,Amount)]
entryLinesWithBalances [] _ = []
entryLinesWithBalances ((str,amt):els) bal = 
    [(str',amt,bal')] ++ entryLinesWithBalances els bal'
        where
          bal' = bal + amt
          str' = str ++ (printf "%10.2s" (show bal'))

showEntryWithBalances :: Entry -> Amount -> String
showEntryWithBalances e b = unlines $ 
  [s | (s,a,b) <- entryLinesWithBalances (entryLines e) b]

-- show register entries, keeping a running balance
showRegisterEntries :: [Entry] -> Amount -> String
showRegisterEntries [] _ = ""
showRegisterEntries (e:es) b =
    showEntryWithBalances e b ++ (showRegisterEntries es b')
        where b' = b + (sumTransactions (transactions e))

printRegister :: Ledger -> IO ()
printRegister l = putStr $ showRegisterEntries (entries l) 0

-- misc

transactionsFrom :: [Entry] -> [Transaction]
transactionsFrom es = concat $ map transactions es

-- fill in missing amounts etc., as far as possible
autofill :: Entry -> Entry
autofill e = Entry (date e) (status e) (code e) (description e)
             (autofillTransactions (transactions e))

autofillTransactions :: [Transaction] -> [Transaction]
autofillTransactions ts =
    let (ns,as) = normalAndAutoTransactions ts in
    case (length as) of
      0 -> ns
      1 -> let t = head as 
               newamt = -(sumTransactions ns)
           in 
             ns ++ [Transaction (account t) newamt]
      otherwise -> error "too many blank transactions in this entry"

normalAndAutoTransactions :: [Transaction] -> ([Transaction], [Transaction])
normalAndAutoTransactions ts =
    ([t | t <- ts, (currency $ amount t) /= "AUTO"],
     [t | t <- ts, (currency $ amount t) == "AUTO"])

sumTransactions :: [Transaction] -> Amount
sumTransactions ts = sum [amount t | t <- ts]