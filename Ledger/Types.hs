{-|

Most data types are defined here to avoid import cycles. See the
corresponding modules for each type's documentation.

A note about entry\/transaction\/posting terminology:

  - ledger 2 had Entrys containing Transactions.
  
  - hledger 0.4 had Entrys containing RawTransactions, plus Transactions
    which were a RawTransaction with its parent Entry's info added.
    The latter are what we most work with when reporting and are
    ubiquitous in the code and docs.
  
  - ledger 3 has Transactions containing Postings.
  

  - hledger 0.5 has LedgerTransactions containing Postings, plus
    Transactions as before (a Posting plus it's parent's info).  The
    \"transaction\" term is pretty ingrained in the code, docs and with
    users, so we've kept it. 

-}

module Ledger.Types 
where
import Ledger.Utils
import qualified Data.Map as Map
import System.Time (ClockTime)


type SmartDate = (String,String,String)

data WhichDate = ActualDate | EffectiveDate

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
      price :: Maybe MixedAmount  -- ^ unit price for this amount at posting time, if known (from @ or P)
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
      lteffectivedate :: Maybe Day,
      ltstatus :: Bool,
      ltcode :: String,
      ltdescription :: String,
      ltcomment :: String,
      ltpostings :: [Posting],
      ltpreceding_comment_lines :: String
    } deriving (Eq)

data TimeLogCode = SetBalance | SetRequiredHours | In | Out | FinalOut deriving (Eq,Ord) 

data TimeLogEntry = TimeLogEntry {
      tlcode :: TimeLogCode,
      tldatetime :: LocalTime,
      tlcomment :: String
    } deriving (Eq,Ord)

data HistoricalPrice = HistoricalPrice {
      hdate :: Day,
      hsymbol :: String,
      hamount :: MixedAmount
    } deriving (Eq) -- & Show (in Amount.hs)

data RawLedger = RawLedger {
      modifier_txns :: [ModifierTransaction],
      periodic_txns :: [PeriodicTransaction],
      ledger_txns :: [LedgerTransaction],
      open_timelog_entries :: [TimeLogEntry],
      historical_prices :: [HistoricalPrice],
      final_comment_lines :: String,
      filepath :: FilePath,
      filereadtime :: ClockTime
    } deriving (Eq)

-- | A generic, pure specification of how to filter raw ledger transactions.
data FilterSpec = FilterSpec {
     datespan  :: DateSpan   -- ^ only include transactions in this date span
    ,cleared   :: Maybe Bool -- ^ only include if cleared\/uncleared\/don't care
    ,real      :: Bool       -- ^ only include if real\/don't care
    ,costbasis :: Bool       -- ^ convert all amounts to cost basis
    ,acctpats  :: [String]   -- ^ only include if matching these account patterns
    ,descpats  :: [String]   -- ^ only include if matching these description patterns
    ,whichdate :: WhichDate  -- ^ which dates to use (transaction or effective)
    }

data Transaction = Transaction {
      tnum :: Int,
      tstatus :: Bool,           -- ^ posting status
      tdate :: Day,              -- ^ transaction date
      tdescription :: String,    -- ^ ledger transaction description
      taccount :: AccountName,   -- ^ posting account
      tamount :: MixedAmount,    -- ^ posting amount
      ttype :: PostingType       -- ^ posting type
    } deriving (Eq)

data Account = Account {
      aname :: AccountName,
      atransactions :: [Transaction], -- ^ transactions in this account
      abalance :: MixedAmount         -- ^ sum of transactions in this account and subaccounts
    }

data Ledger = Ledger {
      rawledgertext :: String,
      rawledger :: RawLedger,
      accountnametree :: Tree AccountName,
      accountmap :: Map.Map AccountName Account
    }

