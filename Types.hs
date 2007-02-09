-- a data model
module Types where

import Text.Printf

data Ledger = Ledger {
                      modifier_entries :: [ModifierEntry],
                      periodic_entries :: [PeriodicEntry],
                      entries :: [Entry]
                     } deriving (Show, Eq)
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
                     } deriving (Read, Eq)
type Date = String
type Account = String

-- show methods

showLedger :: Ledger -> String
showLedger l = "Ledger has\n"
               ++ (showModifierEntries $ modifier_entries l)
               ++ (showPeriodicEntries $ periodic_entries l)
               ++ (showEntries $ entries l)

showModifierEntries :: [ModifierEntry] -> String
showModifierEntries [] = ""
showModifierEntries es =
    (show n) ++ " modifier " ++ (inflectEntries n) ++ ":\n" ++ unlines (map show es)
            where n = length es

showPeriodicEntries :: [PeriodicEntry] -> String
showPeriodicEntries [] = ""
showPeriodicEntries es =
    (show n) ++ " periodic " ++ (inflectEntries n) ++ ":\n" ++ unlines (map show es)
            where n = length es

showEntries :: [Entry] -> String
showEntries [] = ""
showEntries es =
    (show n) ++ " " ++ (inflectEntries n) ++ ":\n" ++ unlines (map show es)
            where n = length es

inflectEntries 1 = "entry"
inflectEntries _ = "entries"

instance Show ModifierEntry where 
    show e = "= " ++ (valueexpr e) ++ "\n" ++ unlines (map show (m_transactions e))

instance Show PeriodicEntry where 
    show e = "~ " ++ (periodexpr e) ++ "\n" ++ unlines (map show (p_transactions e))

instance Show Entry where 
    show e = date e ++ " " ++ s ++ c ++ d ++ "\n" ++ unlines (map show (transactions e))
        where 
          d = description e
          s = case (status e) of {True -> "* "; False -> ""}
          c = case (length(code e) > 0) of {True -> (code e ++ " "); False -> ""}

instance Show Transaction where 
    show t = printf "    %-40s  %20.2s" (take 40 $ account t) (show $ amount t)

instance Show Amount where show a = (currency a) ++ (show $ quantity a)

