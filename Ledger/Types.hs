{-|

All the main data types, defined here to avoid import cycles.
See the corresponding modules for documentation.

-}

module Ledger.Types 
where
import Ledger.Utils
import qualified Data.Map as Map


type Date = String

type DateTime = String

data Currency = Currency {
      symbol :: String,
      rate :: Double
    } deriving (Eq,Show)

data Amount = Amount {
      currency :: Currency,
      quantity :: Double,
      precision :: Int           -- ^ number of significant decimal places
    } deriving (Eq)

type AccountName = String

data RawTransaction = RawTransaction {
      taccount :: AccountName,
      tamount :: Amount,
      tcomment :: String
    } deriving (Eq)

-- | a ledger "modifier" entry. Currently ignored.
data ModifierEntry = ModifierEntry {
      valueexpr :: String,
      m_transactions :: [RawTransaction]
    } deriving (Eq)

-- | a ledger "periodic" entry. Currently ignored.
data PeriodicEntry = PeriodicEntry {
      periodexpr :: String,
      p_transactions :: [RawTransaction]
    } deriving (Eq)

data Entry = Entry {
      edate :: Date,
      estatus :: Bool,
      ecode :: String,
      edescription :: String,
      ecomment :: String,
      etransactions :: [RawTransaction],
      epreceding_comment_lines :: String
    } deriving (Eq)

data RawLedger = RawLedger {
      modifier_entries :: [ModifierEntry],
      periodic_entries :: [PeriodicEntry],
      entries :: [Entry],
      final_comment_lines :: String
    } deriving (Eq)

data TimeLogEntry = TimeLogEntry {
      tlcode :: Char,
      tldatetime :: DateTime,
      tlcomment :: String
    } deriving (Eq,Ord)

data TimeLog = TimeLog {
      timelog_entries :: [TimeLogEntry]
    } deriving (Eq)

data Transaction = Transaction {
      entryno :: Int,
      date :: Date,
      description :: String,
      account :: AccountName,
      amount :: Amount
    } deriving (Eq)

data Account = Account {
      aname :: AccountName,
      atransactions :: [Transaction],
      abalance :: Amount
    }

data Ledger = Ledger {
      rawledger :: RawLedger,
      accountnametree :: Tree AccountName,
      accountmap :: Map.Map AccountName Account,
      lprecision :: Int -- the preferred display precision
    }

