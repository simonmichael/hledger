{-# LANGUAGE CPP, DeriveDataTypeable, StandaloneDeriving, DeriveGeneric, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-|

Most data types are defined here to avoid import cycles.
Here is an overview of the hledger data model:

> Journal                  -- a journal is read from one or more data files. It contains..
>  [Transaction]           -- journal transactions (aka entries), which have date, cleared status, code, description and..
>   [Posting]              -- multiple account postings, which have account name and amount
>  [MarketPrice]           -- historical market prices for commodities
>
> Ledger                   -- a ledger is derived from a journal, by applying a filter specification and doing some further processing. It contains..
>  Journal                 -- a filtered copy of the original journal, containing only the transactions and postings we are interested in
>  [Account]               -- all accounts, in tree order beginning with a "root" account", with their balances and sub/parent accounts

For more detailed documentation on each type, see the corresponding modules.

-}

module Hledger.Data.Types
where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Monad.Except (ExceptT)
import Data.Data
import Data.Decimal
import Data.Default
import Text.Blaze (ToMarkup(..))
import qualified Data.Map as M
import Data.Text (Text)
-- import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import System.Time (ClockTime(..))
import Data.Aeson

import Hledger.Utils.Regex


-- todo: see if this needs to be moved to another file
instance ToJSON Decimal where
  toJSON = toJSON . show

type SmartDate = (String,String,String)

data WhichDate = PrimaryDate | SecondaryDate deriving (Eq,Show)

data DateSpan = DateSpan (Maybe Day) (Maybe Day) deriving (Eq,Ord,Data,Generic,Typeable)

instance Default DateSpan where def = DateSpan Nothing Nothing

instance NFData DateSpan

-- synonyms for various date-related scalars
type Year = Integer
type Month = Int     -- 1-12
type Quarter = Int   -- 1-4
type YearWeek = Int  -- 1-52
type MonthWeek = Int -- 1-5
type YearDay = Int   -- 1-366
type MonthDay = Int  -- 1-31
type WeekDay = Int   -- 1-7

-- Typical report periods (spans of time), both finite and open-ended.
-- A richer abstraction than DateSpan.
data Period =
    DayPeriod Day
  | WeekPeriod Day
  | MonthPeriod Year Month
  | QuarterPeriod Year Quarter
  | YearPeriod Year
  | PeriodBetween Day Day
  | PeriodFrom Day
  | PeriodTo Day
  | PeriodAll
  deriving (Eq,Ord,Show,Data,Generic,Typeable)

instance Default Period where def = PeriodAll

---- Typical report period/subperiod durations, from a day to a year.
--data Duration =
--    DayLong
--   WeekLong
--   MonthLong
--   QuarterLong
--   YearLong
--  deriving (Eq,Ord,Show,Data,Generic,Typeable)

-- Ways in which a period can be divided into subperiods.
data Interval =
    NoInterval
  | Days Int
  | Weeks Int
  | Months Int
  | Quarters Int
  | Years Int
  | DayOfMonth Int
  | WeekdayOfMonth Int Int
  | DayOfWeek Int
  | DayOfYear Int Int -- Month, Day
  -- WeekOfYear Int
  -- MonthOfYear Int
  -- QuarterOfYear Int
  deriving (Eq,Show,Ord,Data,Generic,Typeable)

instance Default Interval where def = NoInterval

instance NFData Interval

type AccountName = Text
type AccountCode = Int

data AccountAlias = BasicAlias AccountName AccountName
                  | RegexAlias Regexp Replacement
  deriving (Eq, Read, Show, Ord, Data, Generic, Typeable)

instance NFData AccountAlias

data Side = L | R deriving (Eq,Show,Read,Ord,Typeable,Data,Generic)

instance NFData Side
instance ToJSON Side where toJSON = genericToJSON defaultOptions

-- | The basic numeric type used in amounts.
type Quantity = Decimal
deriving instance Data (Quantity)
-- The following is for hledger-web, and requires blaze-markup.
-- Doing it here avoids needing a matching flag on the hledger-web package.
instance ToMarkup (Quantity)
 where
   toMarkup = toMarkup . show

-- | An amount's price (none, per unit, or total) in another commodity.
-- Note the price should be a positive number, although this is not enforced.
data Price = NoPrice | UnitPrice Amount | TotalPrice Amount deriving (Eq,Ord,Typeable,Data,Generic)

instance NFData Price
instance ToJSON Price where toJSON = genericToJSON defaultOptions

-- | Display style for an amount.
data AmountStyle = AmountStyle {
      ascommodityside   :: Side,                 -- ^ does the symbol appear on the left or the right ?
      ascommodityspaced :: Bool,                 -- ^ space between symbol and quantity ?
      asprecision       :: !Int,                 -- ^ number of digits displayed after the decimal point
      asdecimalpoint    :: Maybe Char,           -- ^ character used as decimal point: period or comma. Nothing means "unspecified, use default"
      asdigitgroups     :: Maybe DigitGroupStyle -- ^ style for displaying digit groups, if any
} deriving (Eq,Ord,Read,Show,Typeable,Data,Generic)

instance NFData AmountStyle
instance ToJSON AmountStyle where toJSON = genericToJSON defaultOptions

-- | A style for displaying digit groups in the integer part of a
-- floating point number. It consists of the character used to
-- separate groups (comma or period, whichever is not used as decimal
-- point), and the size of each group, starting with the one nearest
-- the decimal point. The last group size is assumed to repeat. Eg,
-- comma between thousands is DigitGroups ',' [3].
data DigitGroupStyle = DigitGroups Char [Int]
  deriving (Eq,Ord,Read,Show,Typeable,Data,Generic)

instance NFData DigitGroupStyle
instance ToJSON DigitGroupStyle where toJSON = genericToJSON defaultOptions

type CommoditySymbol = Text

data Commodity = Commodity {
  csymbol :: CommoditySymbol,
  cformat :: Maybe AmountStyle
  } deriving (Show,Eq,Data,Generic) --,Ord,Typeable,Data,Generic)

instance NFData Commodity

data Amount = Amount {
      acommodity  :: CommoditySymbol,
      aquantity   :: Quantity,
      aprice      :: Price,           -- ^ the (fixed) price for this amount, if any
      astyle      :: AmountStyle,
      amultiplier :: Bool             -- ^ amount is a multipier for AutoTransactions
    } deriving (Eq,Ord,Typeable,Data,Generic)

instance NFData Amount
instance ToJSON Amount where toJSON = genericToJSON defaultOptions

newtype MixedAmount = Mixed [Amount] deriving (Eq,Ord,Typeable,Data,Generic)

instance NFData MixedAmount
instance ToJSON MixedAmount where toJSON = genericToJSON defaultOptions

data PostingType = RegularPosting | VirtualPosting | BalancedVirtualPosting
                   deriving (Eq,Show,Typeable,Data,Generic)

instance NFData PostingType
instance ToJSON PostingType where toJSON = genericToJSON defaultOptions

type TagName = Text
type TagValue = Text
type Tag = (TagName, TagValue)  -- ^ A tag name and (possibly empty) value.

-- | The status of a transaction or posting, recorded with a status mark
-- (nothing, !, or *). What these mean is ultimately user defined.
data Status = Unmarked | Pending | Cleared
  deriving (Eq,Ord,Bounded,Enum,Typeable,Data,Generic)

instance NFData Status
instance ToJSON Status where toJSON = genericToJSON defaultOptions

instance Show Status where -- custom show.. bad idea.. don't do it..
  show Unmarked = ""
  show Pending   = "!"
  show Cleared   = "*"

type BalanceAssertion = Maybe (Amount, GenericSourcePos)

data Posting = Posting {
      pdate             :: Maybe Day,         -- ^ this posting's date, if different from the transaction's
      pdate2            :: Maybe Day,         -- ^ this posting's secondary date, if different from the transaction's
      pstatus           :: Status,
      paccount          :: AccountName,
      pamount           :: MixedAmount,
      pcomment          :: Text,              -- ^ this posting's comment lines, as a single non-indented multi-line string
      ptype             :: PostingType,
      ptags             :: [Tag],             -- ^ tag names and values, extracted from the comment
      pbalanceassertion :: BalanceAssertion,  -- ^ optional: the expected balance in this commodity in the account after this posting
      ptransaction      :: Maybe Transaction, -- ^ this posting's parent transaction (co-recursive types).
                                              -- Tying this knot gets tedious, Maybe makes it easier/optional.
      porigin           :: Maybe Posting      -- ^ original posting if this one is result of any transformations (one level only)
    } deriving (Typeable,Data,Generic)

instance NFData Posting
instance ToJSON Posting where
  toJSON Posting{..} =
    object
    ["pdate"             .= toJSON pdate
    ,"pdate2"            .= toJSON pdate2
    ,"pstatus"           .= toJSON pstatus
    ,"paccount"          .= toJSON paccount
    ,"pamount"           .= toJSON pamount
    ,"pcomment"          .= toJSON pcomment
    ,"ptype"             .= toJSON ptype
    ,"ptags"             .= toJSON ptags
    ,"pbalanceassertion" .= toJSON pbalanceassertion
    ,"ptransactionidx"   .= toJSON (maybe "" (show.tindex) ptransaction)
    ]

-- The equality test for postings ignores the parent transaction's
-- identity, to avoid infinite loops.
instance Eq Posting where
    (==) (Posting a1 b1 c1 d1 e1 f1 g1 h1 i1 _ _) (Posting a2 b2 c2 d2 e2 f2 g2 h2 i2 _ _) =  a1==a2 && b1==b2 && c1==c2 && d1==d2 && e1==e2 && f1==f2 && g1==g2 && h1==h2 && i1==i2

-- TODO: needs renaming, or removal if no longer needed. See also TextPosition in Hledger.UI.Editor
-- | The position of parse errors (eg), like parsec's SourcePos but generic.
data GenericSourcePos = GenericSourcePos FilePath Int Int    -- ^ name, 1-based line number and 1-based column number.
                      | JournalSourcePos FilePath (Int, Int) -- ^ file name, inclusive range of 1-based line numbers (first, last).
  deriving (Eq, Read, Show, Ord, Data, Generic, Typeable)

instance NFData GenericSourcePos
instance ToJSON GenericSourcePos where toJSON = genericToJSON defaultOptions

data Transaction = Transaction {
      tindex                   :: Integer,   -- ^ this transaction's 1-based position in the input stream, or 0 when not available
      tsourcepos               :: GenericSourcePos,
      tdate                    :: Day,
      tdate2                   :: Maybe Day,
      tstatus                  :: Status,
      tcode                    :: Text,
      tdescription             :: Text,
      tcomment                 :: Text,      -- ^ this transaction's comment lines, as a single non-indented multi-line string
      ttags                    :: [Tag],     -- ^ tag names and values, extracted from the comment
      tpostings                :: [Posting], -- ^ this transaction's postings
      tpreceding_comment_lines :: Text       -- ^ any comment lines immediately preceding this transaction
    } deriving (Eq,Typeable,Data,Generic)

instance NFData Transaction
instance ToJSON Transaction where toJSON = genericToJSON defaultOptions

data ModifierTransaction = ModifierTransaction {
      mtvalueexpr :: Text,
      mtpostings  :: [Posting]
    } deriving (Eq,Typeable,Data,Generic)

instance NFData ModifierTransaction

data PeriodicTransaction = PeriodicTransaction {
      ptperiodicexpr :: Text,
      ptpostings     :: [Posting]
    } deriving (Eq,Typeable,Data,Generic)

instance NFData PeriodicTransaction

data TimeclockCode = SetBalance | SetRequiredHours | In | Out | FinalOut deriving (Eq,Ord,Typeable,Data,Generic)

instance NFData TimeclockCode

data TimeclockEntry = TimeclockEntry {
      tlsourcepos   :: GenericSourcePos,
      tlcode        :: TimeclockCode,
      tldatetime    :: LocalTime,
      tlaccount     :: AccountName,
      tldescription :: Text
    } deriving (Eq,Ord,Typeable,Data,Generic)

instance NFData TimeclockEntry

data MarketPrice = MarketPrice {
      mpdate      :: Day,
      mpcommodity :: CommoditySymbol,
      mpamount    :: Amount
    } deriving (Eq,Ord,Typeable,Data,Generic) -- & Show (in Amount.hs)

instance NFData MarketPrice
instance ToJSON MarketPrice where toJSON = genericToJSON defaultOptions

-- | A Journal, containing transactions and various other things.
-- The basic data model for hledger.
--
-- This is used during parsing (as the type alias ParsedJournal), and
-- then finalised/validated for use as a Journal. Some extra
-- parsing-related fields are included for convenience, at least for
-- now. In a ParsedJournal these are updated as parsing proceeds, in a
-- Journal they represent the final state at end of parsing (used eg
-- by the add command).
--
data Journal = Journal {
  -- parsing-related data
   jparsedefaultyear      :: Maybe Year                            -- ^ the current default year, specified by the most recent Y directive (or current date)
  ,jparsedefaultcommodity :: Maybe (CommoditySymbol,AmountStyle)   -- ^ the current default commodity and its format, specified by the most recent D directive
  ,jparseparentaccounts   :: [AccountName]                         -- ^ the current stack of parent account names, specified by apply account directives
  ,jparsealiases          :: [AccountAlias]                        -- ^ the current account name aliases in effect, specified by alias directives (& options ?)
  -- ,jparsetransactioncount :: Integer                               -- ^ the current count of transactions parsed so far (only journal format txns, currently)
  ,jparsetimeclockentries :: [TimeclockEntry]                   -- ^ timeclock sessions which have not been clocked out
  -- principal data
  ,jaccounts              :: [(AccountName, Maybe AccountCode)]     -- ^ accounts that have been declared by account directives
  ,jcommodities           :: M.Map CommoditySymbol Commodity        -- ^ commodities and formats declared by commodity directives
  ,jinferredcommodities   :: M.Map CommoditySymbol AmountStyle      -- ^ commodities and formats inferred from journal amounts
  ,jmarketprices          :: [MarketPrice]
  ,jmodifiertxns          :: [ModifierTransaction]
  ,jperiodictxns          :: [PeriodicTransaction]
  ,jtxns                  :: [Transaction]
  ,jfinalcommentlines     :: Text                                   -- ^ any final trailing comments in the (main) journal file
  ,jfiles                 :: [(FilePath, Text)]                     -- ^ the file path and raw text of the main and
                                                                    --   any included journal files. The main file is first,
                                                                    --   followed by any included files in the order encountered.
  ,jlastreadtime          :: ClockTime                              -- ^ when this journal was last read from its file(s)
  } deriving (Eq, Typeable, Data, Generic)

deriving instance Data (ClockTime)
deriving instance Typeable (ClockTime)
deriving instance Generic (ClockTime)
instance NFData ClockTime
instance NFData Journal

-- | A journal in the process of being parsed, not yet finalised.
-- The data is partial, and list fields are in reverse order.
type ParsedJournal = Journal

-- | The id of a data format understood by hledger, eg @journal@ or @csv@.
-- The --output-format option selects one of these for output.
type StorageFormat = String

-- | A hledger journal reader is a triple of storage format name, a
-- detector of that format, and a parser from that format to Journal.
data Reader = Reader {

     -- The canonical name of the format handled by this reader
     rFormat   :: StorageFormat

     -- The file extensions recognised as containing this format
    ,rExtensions :: [String]

     -- A text parser for this format, accepting an optional rules file,
     -- assertion-checking flag, and file path for error messages,
     -- producing an exception-raising IO action that returns a journal
     -- or error message.
    ,rParser   :: Maybe FilePath -> Bool -> FilePath -> Text -> ExceptT String IO Journal

     -- Experimental readers are never tried automatically.
    ,rExperimental :: Bool
    }

instance Show Reader where show r = rFormat r ++ " reader"

-- | An account, with name, balances and links to parent/subaccounts
-- which let you walk up or down the account tree.
data Account = Account {
  aname                     :: AccountName,   -- ^ this account's full name
  acode                     :: Maybe AccountCode,   -- ^ this account's numeric code, if any (not always set) 
  aebalance                 :: MixedAmount,   -- ^ this account's balance, excluding subaccounts
  asubs                     :: [Account],     -- ^ sub-accounts
  anumpostings              :: Int,           -- ^ number of postings to this account
  -- derived from the above :
  aibalance                 :: MixedAmount,   -- ^ this account's balance, including subaccounts
  aparent                   :: Maybe Account, -- ^ parent account
  aboring                   :: Bool           -- ^ used in the accounts report to label elidable parents
  } deriving (Typeable, Data, Generic)

instance ToJSON Account where
  toJSON a =
    object
    ["aname"        .= toJSON (aname a)
    ,"aebalance"    .= toJSON (aebalance a)
    ,"aibalance"    .= toJSON (aibalance a)
    ,"anumpostings" .= toJSON (anumpostings a)
    ,"aboring"      .= toJSON (aboring a)
    ,"aparentname"  .= toJSON (maybe "" aname $ aparent a)
    ,"asubs"        .= toJSON (map toJSON $ asubs a)
    ]

-- | Whether an account's balance is normally a positive number (in 
-- accounting terms, a debit balance) or a negative number (credit balance). 
-- Assets and expenses are normally positive (debit), while liabilities, equity
-- and income are normally negative (credit).
-- https://en.wikipedia.org/wiki/Normal_balance
data NormalSign = NormallyPositive | NormallyNegative deriving (Show, Data, Eq) 

-- | A Ledger has the journal it derives from, and the accounts
-- derived from that. Accounts are accessible both list-wise and
-- tree-wise, since each one knows its parent and subs; the first
-- account is the root of the tree and always exists.
data Ledger = Ledger {
  ljournal  :: Journal,
  laccounts :: [Account]
}

