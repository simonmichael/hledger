module Types 
where
import Utils
import qualified Data.Map as Map

{-
Here is the approximate module hierarchy. The early code defined types in
each module and so was strictly layered. Now, all data types have been
moved to the bottom. The modules are still used to group related
functions/methods (" make overview " to list those).

hledger
 Options
 Tests
  Parse
   Models
    TimeLog
     TimeLogEntry
    Ledger
     Account
      Transaction
     LedgerFile
      LedgerEntry
       LedgerTransaction
        AccountName
        Amount
         Currency
          Types
           Utils

-}

-- account and description-matching patterns
type FilterPatterns = (Maybe Regex, Maybe Regex)
                       
type Date = String

type DateTime = String

data Currency = Currency {
      symbol :: String,
      rate :: Double -- relative to the dollar.. 0 rates not supported yet
    } deriving (Eq,Show)

-- some amount of money, time, stock, oranges, etc.
data Amount = Amount {
      currency :: Currency,
      quantity :: Double,
      precision :: Int -- number of significant decimal places
    } deriving (Eq)

-- AccountNames are strings like "assets:cash:petty", from which we derive
-- the chart of accounts
type AccountName = String

-- a line item in a ledger entry
data LedgerTransaction = LedgerTransaction {
      taccount :: AccountName,
      tamount :: Amount,
      tcomment :: String
    } deriving (Eq)

-- a ledger entry, with two or more balanced transactions
data LedgerEntry = LedgerEntry {
      edate :: Date,
      estatus :: Bool,
      ecode :: String,
      edescription :: String,
      ecomment :: String,
      etransactions :: [LedgerTransaction]
    } deriving (Eq)

-- an automated ledger entry
data ModifierEntry = ModifierEntry {
      valueexpr :: String,
      m_transactions :: [LedgerTransaction]
    } deriving (Eq)

-- a periodic ledger entry
data PeriodicEntry = PeriodicEntry {
      periodexpr :: String,
      p_transactions :: [LedgerTransaction]
    } deriving (Eq)

-- we also parse timeclock.el timelogs
data TimeLogEntry = TimeLogEntry {
      tlcode :: Char,
      tldatetime :: DateTime,
      tlcomment :: String
    } deriving (Eq,Ord)

data TimeLog = TimeLog {
      timelog_entries :: [TimeLogEntry]
    } deriving (Eq)

-- a parsed ledger file
data LedgerFile = LedgerFile {
      modifier_entries :: [ModifierEntry],
      periodic_entries :: [PeriodicEntry],
      entries :: [LedgerEntry]
    } deriving (Eq)

-- we flatten LedgerEntries and LedgerTransactions into Transactions,
-- which are simpler to query at the cost of some data duplication
data Transaction = Transaction {
      entryno :: Int,
      date :: Date,
      description :: String,
      account :: AccountName,
      amount :: Amount
    } deriving (Eq)

-- cached information for a particular account
data Account = Account {
      aname :: AccountName, 
      atransactions :: [Transaction], -- excludes sub-accounts
      abalance :: Amount              -- includes sub-accounts
    }

-- a ledger with account information cached for faster queries
data Ledger = Ledger {
      rawledger :: LedgerFile,
      accountnametree :: Tree AccountName,
      accounts :: Map.Map AccountName Account,
      lprecision :: Int
    }

