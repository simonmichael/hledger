{-# LANGUAGE DeriveDataTypeable #-}
{-|

Most data types are defined here to avoid import cycles.
Here is an overview of the hledger data model:

> Journal                  -- a journal is read from one or more data files. It contains..
>  [Transaction]           -- journal transactions (aka entries), which have date, status, code, description and..
>   [Posting]              -- multiple account postings, which have account name and amount
>  [HistoricalPrice]       -- historical commodity prices
>
> Ledger                   -- a ledger is derived from a journal, by applying a filter specification and doing some further processing. It contains..
>  Journal                 -- a filtered copy of the original journal, containing only the transactions and postings we are interested in
>  Tree AccountName        -- all accounts named by the journal's transactions, as a hierarchy
>  Map AccountName Account -- the postings, and resulting balances, in each account

For more detailed documentation on each type, see the corresponding modules.

Evolution of transaction\/entry\/posting terminology:

  - ledger 2:    entries contain transactions

  - hledger 0.4: Entrys contain RawTransactions (which are flattened to Transactions)

  - ledger 3:    transactions contain postings

  - hledger 0.5: LedgerTransactions contain Postings (which are flattened to Transactions)

  - hledger 0.8: Transactions contain Postings (referencing Transactions..)

-}

module Hledger.Data.Types
where
import Control.Monad.Error (ErrorT)
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Tree
import Data.Typeable
import qualified Data.Map as Map
import System.Time (ClockTime)


type SmartDate = (String,String,String)

data WhichDate = ActualDate | EffectiveDate deriving (Eq,Show)

data DateSpan = DateSpan (Maybe Day) (Maybe Day) deriving (Eq,Show,Ord)

data Interval = NoInterval
              | Days Int | Weeks Int | Months Int | Quarters Int | Years Int
              | DayOfMonth Int | DayOfWeek Int
              -- WeekOfYear Int | MonthOfYear Int | QuarterOfYear Int
                deriving (Eq,Show,Ord)

type AccountName = String

data Side = L | R deriving (Eq,Show,Read,Ord)

data Commodity = Commodity {
      symbol :: String,            -- ^ the commodity's symbol
      -- display preferences for amounts of this commodity
      side :: Side,                -- ^ should the symbol appear on the left or the right
      spaced :: Bool,              -- ^ should there be a space between symbol and quantity
      precision :: Int,            -- ^ number of decimal places to display
      -- XXX these three might be better belonging to Journal
      decimalpoint :: Char,        -- ^ character to use as decimal point
      separator :: Char,           -- ^ character to use for separating digit groups (eg thousands)
      separatorpositions :: [Int]  -- ^ positions of separators, counting leftward from decimal point
    } deriving (Eq,Ord,Show,Read)

type Quantity = Double

-- | An amount's price in another commodity may be written as \@ unit
-- price or \@\@ total price.  Note although a MixedAmount is used, it
-- should be in a single commodity, also the amount should be positive;
-- these are not enforced currently.
data Price = UnitPrice MixedAmount | TotalPrice MixedAmount
             deriving (Eq,Ord)

data Amount = Amount {
      commodity :: Commodity,
      quantity :: Quantity,
      price :: Maybe Price  -- ^ the price for this amount at posting time
    } deriving (Eq,Ord)

newtype MixedAmount = Mixed [Amount] deriving (Eq,Ord)

data PostingType = RegularPosting | VirtualPosting | BalancedVirtualPosting
                   deriving (Eq,Show)

data Posting = Posting {
      pstatus :: Bool,
      paccount :: AccountName,
      pamount :: MixedAmount,
      pcomment :: String,
      ptype :: PostingType,
      pmetadata :: [(String,String)],
      ptransaction :: Maybe Transaction  -- ^ this posting's parent transaction (co-recursive types).
                                        -- Tying this knot gets tedious, Maybe makes it easier/optional.
    }

-- The equality test for postings ignores the parent transaction's
-- identity, to avoid infinite loops.
instance Eq Posting where
    (==) (Posting a1 b1 c1 d1 e1 f1 _) (Posting a2 b2 c2 d2 e2 f2 _) =  a1==a2 && b1==b2 && c1==c2 && d1==d2 && e1==e2 && f1==f2

data Transaction = Transaction {
      tdate :: Day,
      teffectivedate :: Maybe Day,
      tstatus :: Bool,  -- XXX tcleared ?
      tcode :: String,
      tdescription :: String,
      tcomment :: String,
      tmetadata :: [(String,String)],
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

type Year = Integer

-- | A journal "context" is some data which can change in the course of
-- parsing a journal. An example is the default year, which changes when a
-- Y directive is encountered.  At the end of parsing, the final context
-- is saved for later use by eg the add command.
data JournalContext = Ctx {
      ctxYear      :: !(Maybe Year)      -- ^ the default year most recently specified with Y
    , ctxCommodity :: !(Maybe Commodity) -- ^ the default commodity most recently specified with D
    , ctxAccount   :: ![AccountName]     -- ^ the current stack of parent accounts/account name components
                                        --   specified with "account" directive(s). Concatenated, these
                                        --   are the account prefix prepended to parsed account names.
    , ctxAliases   :: ![(AccountName,AccountName)] -- ^ the current list of account name aliases in effect
    } deriving (Read, Show, Eq)

data Journal = Journal {
      jmodifiertxns :: [ModifierTransaction],
      jperiodictxns :: [PeriodicTransaction],
      jtxns :: [Transaction],
      open_timelog_entries :: [TimeLogEntry],
      historical_prices :: [HistoricalPrice],
      final_comment_lines :: String,        -- ^ any trailing comments from the journal file
      jContext :: JournalContext,           -- ^ the context (parse state) at the end of parsing
      files :: [(FilePath, String)],        -- ^ the file path and raw text of the main and
                                           -- any included journal files. The main file is
                                           -- first followed by any included files in the
                                           -- order encountered.
      filereadtime :: ClockTime             -- ^ when this journal was last read from its file(s)
    } deriving (Eq, Typeable)

-- | A JournalUpdate is some transformation of a Journal. It can do I/O or
-- raise an error.
type JournalUpdate = ErrorT String IO (Journal -> Journal)

-- | The id of a data format understood by hledger, eg @journal@ or @csv@.
type Format = String

-- | A hledger journal reader is a triple of format name, format-detecting
-- predicate, and a parser to Journal.
data Reader = Reader {
     -- name of the format this reader handles
     rFormat   :: Format
     -- quickly check if this reader can probably handle the given file path and file content
    ,rDetector :: FilePath -> String -> Bool
     -- parse the given string, using the given parse rules file if any, returning a journal or error aware of the given file path
    ,rParser   :: Maybe FilePath -> FilePath -> String -> ErrorT String IO Journal
    }

-- data format parse/conversion rules

-- currently the only parse (conversion) rules are those for the CSV format
type ParseRules = CsvRules

-- XXX copied from Convert.hs
{- |
A set of data definitions and account-matching patterns sufficient to
convert a particular CSV data file into meaningful journal transactions. See above.
-}
data CsvRules = CsvRules {
      dateField :: Maybe FieldPosition,
      dateFormat :: Maybe String,
      statusField :: Maybe FieldPosition,
      codeField :: Maybe FieldPosition,
      descriptionField :: [FormatString],
      amountField :: Maybe FieldPosition,
      amountInField :: Maybe FieldPosition,
      amountOutField :: Maybe FieldPosition,
      currencyField :: Maybe FieldPosition,
      baseCurrency :: Maybe String,
      accountField :: Maybe FieldPosition,
      account2Field :: Maybe FieldPosition,
      effectiveDateField :: Maybe FieldPosition,
      baseAccount :: AccountName,
      accountRules :: [AccountRule]
} deriving (Show, Eq)

type FieldPosition = Int

type AccountRule = (
   [(String, Maybe String)] -- list of regex match patterns with optional replacements
  ,AccountName              -- account name to use for a transaction matching this rule
  )

-- format strings

data HledgerFormatField =
    AccountField
  | DefaultDateField
  | DescriptionField
  | TotalField
  | DepthSpacerField
  | FieldNo Int
    deriving (Show, Eq)

data FormatString =
    FormatLiteral String
  | FormatField Bool        -- Left justified ?
                (Maybe Int) -- Min width
                (Maybe Int) -- Max width
                HledgerFormatField       -- Field
  deriving (Show, Eq)


data Ledger = Ledger {
      ledgerJournal :: Journal,
      ledgerAccountNameTree :: Tree AccountName,
      ledgerAccountMap :: Map.Map AccountName Account
    }

data Account = Account {
      aname :: AccountName,
      apostings :: [Posting],    -- ^ postings in this account
      abalance :: MixedAmount    -- ^ sum of postings in this account and subaccounts
    }

-- | A generic, pure specification of how to filter (or search) transactions and postings.
data FilterSpec = FilterSpec {
     datespan  :: DateSpan   -- ^ only include if in this date span
    ,cleared   :: Maybe Bool -- ^ only include if cleared\/uncleared\/don't care
    ,real      :: Bool       -- ^ only include if real\/don't care
    ,empty     :: Bool       -- ^ include if empty (ie amount is zero)
    ,acctpats  :: [String]   -- ^ only include if matching these account patterns
    ,descpats  :: [String]   -- ^ only include if matching these description patterns
    ,depth     :: Maybe Int
    ,fMetadata  :: [(String,String)] -- ^ only include if matching these metadata
    } deriving (Show)

