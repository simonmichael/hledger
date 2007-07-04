module Types 
where
import Utils
import qualified Data.Map as Map

{-

First, here is the module hierarchy. The initial implementation defined
types in each module and was strictly layered. Now, all types have been
moved to the bottom, with modules still used to group related functions
(aka methods - make overview to list those).

hledger
 Options
 Tests
  Parse
   Models
    TimeLog
     TimeLogEntry
    Ledger
     Account
      RawLedger
       EntryTransaction
        Entry
         Transaction
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
      rate :: Double -- relative to the dollar
    } deriving (Eq,Show)

-- some amount of money, time, stock, oranges, etc.
data Amount = Amount {
      currency :: Currency,
      quantity :: Double,
      precision :: Int -- number of significant decimal places
    } deriving (Eq)

-- AccountNames are strings like "assets:cash:petty"; from these we build
-- the chart of accounts, which should be a simple hierarchy. 
type AccountName = String

-- a flow of an amount to an account
data Transaction = Transaction {
      taccount :: AccountName,
      tamount :: Amount
    } deriving (Eq)

-- cleared ?
type EntryStatus = Bool

-- a ledger entry, with two or more balanced transactions
data Entry = Entry {
      edate :: Date,
      estatus :: EntryStatus,
      ecode :: String,
      edescription :: String,
      etransactions :: [Transaction]
    } deriving (Eq)

-- an "automated" entry (see = in ledger manual) 
data ModifierEntry = ModifierEntry {
      valueexpr :: String,
      m_transactions :: [Transaction]
    } deriving (Eq)

-- a periodic entry (see ~ in ledger manual)
data PeriodicEntry = PeriodicEntry {
      periodexpr :: String,
      p_transactions :: [Transaction]
    } deriving (Eq)

-- we also parse timeclock.el's timelogs (as a ledger)
data TimeLogEntry = TimeLogEntry {
      tcode :: Char,
      tdatetime :: DateTime,
      tcomment :: String
    } deriving (Eq,Ord)

data TimeLog = TimeLog {
      timelog_entries :: [TimeLogEntry]
    } deriving (Eq)

-- a parsed ledger file
data RawLedger = RawLedger {
      modifier_entries :: [ModifierEntry],
      periodic_entries :: [PeriodicEntry],
      entries :: [Entry]
    } deriving (Eq)

-- We convert Transactions into EntryTransactions, which are (entry,
-- transaction) pairs, since I couldn't see how to have transactions
-- reference their entry like in OO.  These are referred to as just
-- "transactions" in modules above EntryTransaction.
type EntryTransaction = (Entry,Transaction)

-- all information for a particular account, derived from a RawLedger
data Account = Account {
      aname :: AccountName, 
      atransactions :: [EntryTransaction], -- excludes sub-accounts
      abalance :: Amount                   -- includes sub-accounts
    }

-- a ledger with account info cached for faster queries
data Ledger = Ledger {
      rawledger :: RawLedger, 
      accountnametree :: Tree AccountName,
      accounts :: Map.Map AccountName Account,
      lprecision :: Int
    }

