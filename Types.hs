module Types
where
import Utils

{-

First, here is the module hierarchy. The initial implementation defined
types in each module and so was strictly layered. Now, all types have been
moved to the bottom, with modules still used to group related functions
(aka methods - "make overview" to list these).

hledger
 Options
 Tests
  Parse
   Models
    TimeLog
     TimeLogEntry
    Account
     Ledger
      EntryTransaction
       Entry
        Transaction
         AccountName
         Amount
          Currency
           Types
            Utils

(Will this allow a more muddled design ?  Possibly, though starting out
layered probably helped, but note previous comment:

  Each layer can only reference things below it.  A seeming problem:
  CookedLedger must be at the top so it can cache any of the others. Code
  below it can not use its fast functions, and code above it should use
  only its functions for good performance. Upper-level code loses the
  benefit of many lower-level functions and has to reimplement them as
  fast versions.)

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
      quantity :: Double
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

-- a parsed ledger file
data Ledger = Ledger {
      modifier_entries :: [ModifierEntry],
      periodic_entries :: [PeriodicEntry],
      entries :: [Entry]
    } deriving (Eq)

-- we also process timeclock.el's timelogs
data TimeLogEntry = TimeLogEntry {
      tcode :: Char,
      tdatetime :: DateTime,
      tcomment :: String
    } deriving (Eq,Ord)

data TimeLog = TimeLog {
      timelog_entries :: [TimeLogEntry]
    } deriving (Eq)

-- We convert Transactions into EntryTransactions, which are (entry,
-- transaction) pairs, since I couldn't see how to have transactions
-- reference their entry like in OO.  These are referred to as just
-- "transactions" in modules above EntryTransaction.
type EntryTransaction = (Entry,Transaction)

-- an Account caches a particular account's name, balance and transactions
-- from a Ledger
data Account = Account {
      aname :: AccountName, 
      atransactions :: [EntryTransaction], -- excludes sub-accounts
      abalance :: Amount                   -- includes sub-accounts
    }

