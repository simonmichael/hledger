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
                      quantity :: Float
                     } deriving (Eq)
type Date = String
type Account = String

-- show methods

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

instance Show Entry where show = showEntry2

showEntry1 e = date e ++ " " ++ s ++ c ++ d ++ "\n" ++ unlines (map show (transactions e))
        where 
          d = description e
          s = case (status e) of {True -> "* "; False -> ""}
          c = case (length(code e) > 0) of {True -> (code e ++ " "); False -> ""}

dateWidth = 10
descWidth = 20
acctWidth = 25
amtWidth = 11

showEntry2 e = 
    unlines (
             [printf "%-10s %-20s " (date e) (take 20 $ description e)
                         ++ (show $ head $ transactions e)]
             ++ map ((printf (take 32 (repeat ' ')) ++) . show) (tail $ transactions e))

instance Show Transaction where 
    show t = printf "%-25s  %8.2s %8.2s" (take 25 $ account t) (show $ amount t) (show 0)

instance Show Amount where show a = (currency a) ++ (show $ quantity a)

-- more display methods

printRegister :: Ledger -> IO ()
printRegister l = do
    putStr $ concat $ map show $ entries l
