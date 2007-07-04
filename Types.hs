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

-- AccountNames are strings like "assets:cash:petty"; from these we figure
-- out the chart of accounts
type AccountName = String

-- a flow of some amount to some account (see also Transaction)
data LedgerTransaction = LedgerTransaction {
      taccount :: AccountName,
      tamount :: Amount
    } deriving (Eq)

-- a ledger entry, with two or more balanced transactions
data LedgerEntry = LedgerEntry {
      edate :: Date,
      estatus :: EntryStatus,
      ecode :: String,
      edescription :: String,
      etransactions :: [LedgerTransaction]
    } deriving (Eq)

type EntryStatus = Bool

-- an "=" automated entry (ignored)
data ModifierEntry = ModifierEntry {
      valueexpr :: String,
      m_transactions :: [LedgerTransaction]
    } deriving (Eq)

-- a "~" periodic entry (ignored)
data PeriodicEntry = PeriodicEntry {
      periodexpr :: String,
      p_transactions :: [LedgerTransaction]
    } deriving (Eq)

-- we also parse timeclock.el timelogs
data TimeLogEntry = TimeLogEntry {
      tcode :: Char,
      tdatetime :: DateTime,
      tcomment :: String
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

-- We convert Transactions into EntryTransactions, which are (entry,
-- transaction) pairs, since I couldn't see how to have transactions
-- reference their entry like in OO.  These are referred to as just
-- "transactions" in modules above Transaction.
type Transaction = (LedgerEntry,LedgerTransaction)

-- all information for a particular account, derived from a LedgerFile
data Account = Account {
      aname :: AccountName, 
      atransactions :: [Transaction], -- excludes sub-accounts
      abalance :: Amount                   -- includes sub-accounts
    }

-- a ledger with account info cached for faster queries
data Ledger = Ledger {
      rawledger :: LedgerFile, 
      accountnametree :: Tree AccountName,
      accounts :: Map.Map AccountName Account,
      lprecision :: Int
    }

