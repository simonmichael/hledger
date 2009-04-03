{-|

This is the next layer up from Ledger.Utils. All main data types are
defined here to avoid import cycles; see the corresponding modules for
documentation.

On the current use of terminology:

- ledger 2 has Entrys containing Transactions.

- hledger 0.4 has Entrys containing RawTransactions, and Transactions
  which are a RawTransaction with its parent Entry's info added.
  Transactions are what we most work with when reporting and are
  ubiquitous in the code and docs.

- ledger 3 has Transactions containing Postings.

- hledger 0.5 has LedgerTransactions containing Postings, with
  Transactions kept just as in hledger 0.4 (a Posting with it's parent's
  info added). They could be named PartialTransactions or
  TransactionPostings, but that just gets too verbose and obscure for devs
  and users.

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

data PostingType = RegularPosting | VirtualPosting | BalancedVirtualPosting
                       deriving (Eq,Show)

data Posting = Posting {
      pstatus :: Bool,
      paccount :: AccountName,
      pamount :: MixedAmount,
      pcomment :: String,
      ptype :: PostingType
    } deriving (Eq)

data ModifierTransaction = ModifierTransaction {
      mtvalueexpr :: String,
      mtpostings :: [Posting]
    } deriving (Eq)

data PeriodicTransaction = PeriodicTransaction {
      ptperiodicexpr :: String,
      ptpostings :: [Posting]
    } deriving (Eq)

data LedgerTransaction = LedgerTransaction {
      ltdate :: Day,
      ltstatus :: Bool,
      ltcode :: String,
      ltdescription :: String,
      ltcomment :: String,
      ltpostings :: [Posting],
      ltpreceding_comment_lines :: String
    } deriving (Eq)

data HistoricalPrice = HistoricalPrice {
      hdate :: Day,
      hsymbol1 :: String,
      hsymbol2 :: String,
      hprice :: Double
    } deriving (Eq,Show)

data RawLedger = RawLedger {
      modifier_txns :: [ModifierTransaction],
      periodic_txns :: [PeriodicTransaction],
      ledger_txns :: [LedgerTransaction],
      open_timelog_entries :: [TimeLogEntry],
      historical_prices :: [HistoricalPrice],
      final_comment_lines :: String,
      filepath :: FilePath
    } deriving (Eq)

data TimeLogEntry = TimeLogEntry {
      tlcode :: Char,
      tldatetime :: LocalTime,
      tlcomment :: String
    } deriving (Eq,Ord)

data Transaction = Transaction {
      tnum :: Int,
      status :: Bool,
      date :: Day,
      description :: String,
      account :: AccountName,
      amount :: MixedAmount,
      ttype :: PostingType
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

