{-| 
All the basic data types, defined here for easy re-use. See "Main".
-}
module Types 
where
import Utils
import qualified Data.Map as Map

type Date = String

type DateTime = String

data Currency = Currency {
      symbol :: String,
      rate :: Double -- relative to the dollar.. 0 rates not supported yet
    } deriving (Eq,Show)

-- | some amount of money, time, stock, oranges, or whatever.
data Amount = Amount {
      currency :: Currency,
      quantity :: Double,
      precision :: Int -- number of significant decimal places
    } deriving (Eq)

-- | AccountNames are strings like assets:cash:petty, from which we derive
-- the chart of accounts
type AccountName = String

-- | a line item in a ledger entry
data LedgerTransaction = LedgerTransaction {
      taccount :: AccountName,
      tamount :: Amount,
      tcomment :: String
    } deriving (Eq)

-- | a ledger entry, containing two or more balanced transactions
data LedgerEntry = LedgerEntry {
      edate :: Date,
      estatus :: Bool,
      ecode :: String,
      edescription :: String,
      ecomment :: String,
      etransactions :: [LedgerTransaction],
      epreceding_comment_lines :: String
    } deriving (Eq)

-- | an automated ledger entry
data ModifierEntry = ModifierEntry {
      valueexpr :: String,
      m_transactions :: [LedgerTransaction]
    } deriving (Eq)

-- | a periodic ledger entry
data PeriodicEntry = PeriodicEntry {
      periodexpr :: String,
      p_transactions :: [LedgerTransaction]
    } deriving (Eq)

-- | a timelog entry (we also parse timeclock.el timelogs)
data TimeLogEntry = TimeLogEntry {
      tlcode :: Char,
      tldatetime :: DateTime,
      tlcomment :: String
    } deriving (Eq,Ord)

-- | a parsed timelog file
data TimeLog = TimeLog {
      timelog_entries :: [TimeLogEntry]
    } deriving (Eq)

-- | a parsed ledger file
data LedgerFile = LedgerFile {
      modifier_entries :: [ModifierEntry],
      periodic_entries :: [PeriodicEntry],
      entries :: [LedgerEntry],
      final_comment_lines :: String
    } deriving (Eq)

-- | we flatten LedgerEntries and LedgerTransactions into Transactions,
-- which are simpler to query at the cost of some data duplication
data Transaction = Transaction {
      entryno :: Int,
      date :: Date,
      description :: String,
      account :: AccountName,
      amount :: Amount
    } deriving (Eq)

-- | cached information for a particular account
data Account = Account {
      aname :: AccountName,           -- ^ the account name
      atransactions :: [Transaction], -- ^ the transactions, excluding sub-accounts
      abalance :: Amount              -- ^ the total balance, including sub-accounts
    }

-- | a ledger with account information cached for faster queries
data Ledger = Ledger {
      rawledger :: LedgerFile,
      accountnametree :: Tree AccountName,
      accounts :: Map.Map AccountName Account,
      lprecision :: Int
    }

