{-|

This is the next layer up from Ledger.Utils. All main data types are
defined here to avoid import cycles; see the corresponding modules for
documentation.

-}

module Ledger.Types 
where
import Ledger.Utils
import qualified Data.Map as Map


type SmartDate = (String,String,String)

data DateSpan = DateSpan (Maybe Day) (Maybe Day) deriving (Eq,Show,Ord)

data Interval = NoInterval | Daily | Weekly | Monthly | Quarterly | Yearly 
                deriving (Eq,Show,Ord)

type AccountName = String

data Side = L | R deriving (Eq,Show,Ord) 

data Commodity = Commodity {
      symbol :: String,  -- ^ the commodity's symbol

      -- display preferences for amounts of this commodity
      side :: Side,      -- ^ should the symbol appear on the left or the right
      spaced :: Bool,    -- ^ should there be a space between symbol and quantity
      comma :: Bool,     -- ^ should thousands be comma-separated
      precision :: Int   -- ^ number of decimal places to display
    } deriving (Eq,Show,Ord)

data Amount = Amount {
      commodity :: Commodity,
      quantity :: Double,
      price :: Maybe MixedAmount  -- ^ optional per-unit price for this amount at the time of entry
    } deriving (Eq)

newtype MixedAmount = Mixed [Amount] deriving (Eq)

data TransactionType = RegularTransaction | VirtualTransaction | BalancedVirtualTransaction
                       deriving (Eq,Show)

data RawTransaction = RawTransaction {
      taccount :: AccountName,
      tamount :: MixedAmount,
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
      periodicexpr :: String,
      p_transactions :: [RawTransaction]
    } deriving (Eq)

data Entry = Entry {
      edate :: Day,
      estatus :: Bool,
      ecode :: String,
      edescription :: String,
      ecomment :: String,
      etransactions :: [RawTransaction],
      epreceding_comment_lines :: String
    } deriving (Eq)

data HistoricalPrice = HistoricalPrice {
     hdate :: Day,
     hsymbol1 :: String,
     hsymbol2 :: String,
     hprice :: Double
} deriving (Eq,Show)

data RawLedger = RawLedger {
      modifier_entries :: [ModifierEntry],
      periodic_entries :: [PeriodicEntry],
      entries :: [Entry],
      open_timelog_entries :: [TimeLogEntry],
      historical_prices :: [HistoricalPrice],
      final_comment_lines :: String
    } deriving (Eq)

data TimeLogEntry = TimeLogEntry {
      tlcode :: Char,
      tldatetime :: UTCTime,
      tlcomment :: String
    } deriving (Eq,Ord)

data TimeLog = TimeLog {
      timelog_entries :: [TimeLogEntry]
    } deriving (Eq)

data Transaction = Transaction {
      entryno :: Int,
      date :: Day,
      description :: String,
      account :: AccountName,
      amount :: MixedAmount,
      ttype :: TransactionType
    } deriving (Eq)

data Account = Account {
      aname :: AccountName,
      atransactions :: [Transaction],
      abalance :: MixedAmount
    }

data Ledger = Ledger {
      rawledgertext :: String,
      rawledger :: RawLedger,
      accountnametree :: Tree AccountName,
      accountmap :: Map.Map AccountName Account
    }

