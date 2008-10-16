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

data Side = L | R deriving (Eq,Show) 

data Commodity = Commodity {
      symbol :: String,  -- ^ the commodity's symbol

      -- display preferences for amounts of this commodity
      side :: Side,      -- ^ should the symbol appear on the left or the right
      spaced :: Bool,    -- ^ should there be a space between symbol and quantity
      comma :: Bool,     -- ^ should thousands be comma-separated
      precision :: Int,  -- ^ number of decimal places to display

      rate :: Double     -- ^ the current (hard-coded) conversion rate against the dollar
    } deriving (Eq,Show)

data Amount = Amount {
      commodity :: Commodity,
      quantity :: Double
    } deriving (Eq)

type AccountName = String

data TransactionType = RegularTransaction | VirtualTransaction | BalancedVirtualTransaction
                       deriving (Eq,Show)

data RawTransaction = RawTransaction {
      taccount :: AccountName,
      tamount :: Amount,
      tcomment :: String,
      rttype :: TransactionType
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
      amount :: Amount,
      ttype :: TransactionType
    } deriving (Eq)

data Account = Account {
      aname :: AccountName,
      atransactions :: [Transaction],
      abalance :: Amount
    }

data Ledger = Ledger {
      rawledger :: RawLedger,
      accountnametree :: Tree AccountName,
      accountmap :: Map.Map AccountName Account
    }

