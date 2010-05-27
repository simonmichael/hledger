{-# LANGUAGE DeriveDataTypeable #-}
{-|

Most data types are defined here to avoid import cycles.
Here is an overview of the hledger data model:

> Journal                  -- a journal is derived from one or more data files. It contains..
>  [Transaction]           -- journal transactions, which have date, status, code, description and..
>   [Posting]              -- multiple account postings (entries), which have account name and amount.
>  [HistoricalPrice]       -- historical commodity prices
>
> Ledger                   -- a ledger is derived from a journal, by applying a filter specification. It contains..
>  Journal                 -- the filtered journal, containing only the transactions and postings we are interested in
>  Tree AccountName        -- account names referenced in the journal's transactions, as a tree
>  Map AccountName Account -- per-account postings and balances from the journal's transactions, as a  map from account name to account info

For more detailed documentation on each type, see the corresponding modules.

Evolution of transaction\/entry\/posting terminology:

  - ledger 2:    entries contain transactions

  - hledger 0.4: Entrys contain RawTransactions (which are flattened to Transactions)

  - ledger 3:    transactions contain postings

  - hledger 0.5: LedgerTransactions contain Postings (which are flattened to Transactions)

  - hledger 0.8: Transactions contain Postings (referencing Transactions, corecursively)

  - hledger 0.10: Postings should be called Entrys, but are left as-is for now

-}

module Hledger.Data.Types
where
import Hledger.Data.Utils
import qualified Data.Map as Map
import System.Time (ClockTime)
import Data.Typeable (Typeable)


type SmartDate = (String,String,String)

data WhichDate = ActualDate | EffectiveDate deriving (Eq,Show)

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
      price :: Maybe MixedAmount  -- ^ unit price/conversion rate for this amount at posting time
    } deriving (Eq)

newtype MixedAmount = Mixed [Amount] deriving (Eq)

data PostingType = RegularPosting | VirtualPosting | BalancedVirtualPosting
                   deriving (Eq,Show)

data Posting = Posting {
      pstatus :: Bool,
      paccount :: AccountName,
      pamount :: MixedAmount,
      pcomment :: String,
      ptype :: PostingType,
      ptransaction :: Maybe Transaction  -- ^ this posting's parent transaction (co-recursive types).
                                        -- Tying this knot gets tedious, Maybe makes it easier/optional.
    }

-- The equality test for postings ignores the parent transaction's
-- identity, to avoid infinite loops.
instance Eq Posting where
    (==) (Posting a1 b1 c1 d1 e1 _) (Posting a2 b2 c2 d2 e2 _) =  a1==a2 && b1==b2 && c1==c2 && d1==d2 && e1==e2

data Transaction = Transaction {
      tdate :: Day,
      teffectivedate :: Maybe Day,
      tstatus :: Bool,  -- XXX tcleared ?
      tcode :: String,
      tdescription :: String,
      tcomment :: String,
      tpostings :: [Posting],            -- ^ this transaction's postings (co-recursive types).
      tpreceding_comment_lines :: String
    } deriving (Eq)

data ModifierTransaction = ModifierTransaction {
      mtvalueexpr :: String,
      mtpostings :: [Posting]
    } deriving (Eq)

data PeriodicTransaction = PeriodicTransaction {
      ptperiodicexpr :: String,
      ptpostings :: [Posting]
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

data Journal = Journal {
      jmodifiertxns :: [ModifierTransaction],
      jperiodictxns :: [PeriodicTransaction],
      jtxns :: [Transaction],
      open_timelog_entries :: [TimeLogEntry],
      historical_prices :: [HistoricalPrice],
      final_comment_lines :: String,
      filepath :: FilePath,
      filereadtime :: ClockTime,
      jtext :: String
    } deriving (Eq, Typeable)

data Ledger = Ledger {
      journal :: Journal,
      accountnametree :: Tree AccountName,
      accountmap :: Map.Map AccountName Account
    }

data Account = Account {
      aname :: AccountName,
      apostings :: [Posting],    -- ^ postings in this account
      abalance :: MixedAmount    -- ^ sum of postings in this account and subaccounts
    }

-- | A generic, pure specification of how to filter transactions and postings.
data FilterSpec = FilterSpec {
     datespan  :: DateSpan   -- ^ only include if in this date span
    ,cleared   :: Maybe Bool -- ^ only include if cleared\/uncleared\/don't care
    ,real      :: Bool       -- ^ only include if real\/don't care
    ,empty     :: Bool       -- ^ include if empty (ie amount is zero)
    ,costbasis :: Bool       -- ^ convert all amounts to cost basis
    ,acctpats  :: [String]   -- ^ only include if matching these account patterns
    ,descpats  :: [String]   -- ^ only include if matching these description patterns
    ,whichdate :: WhichDate  -- ^ which dates to use (actual or effective)
    ,depth     :: Maybe Int
    } deriving (Show)

