{-# LANGUAGE CPP, DeriveDataTypeable, StandaloneDeriving, DeriveGeneric, TypeSynonymInstances, FlexibleInstances #-}
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
#ifndef DOUBLE
import Data.Decimal
import Text.Blaze (ToMarkup(..))
#endif
import qualified Data.Map as M
import Data.Time.Calendar
import Data.Time.LocalTime
import System.Time (ClockTime(..))

import Hledger.Utils.Regex


type SmartDate = (String,String,String)

data WhichDate = PrimaryDate | SecondaryDate deriving (Eq,Show)

data DateSpan = DateSpan (Maybe Day) (Maybe Day) deriving (Eq,Ord,Data,Generic,Typeable)

instance NFData DateSpan

data Interval = NoInterval
              | Days Int | Weeks Int | Months Int | Quarters Int | Years Int
              | DayOfMonth Int | DayOfWeek Int
              -- WeekOfYear Int | MonthOfYear Int | QuarterOfYear Int
                deriving (Eq,Show,Ord,Data,Generic,Typeable)

instance NFData Interval

type AccountName = String

data AccountAlias = BasicAlias AccountName AccountName
                  | RegexAlias Regexp Replacement
  deriving (Eq, Read, Show, Ord, Data, Generic, Typeable)

instance NFData AccountAlias

data Side = L | R deriving (Eq,Show,Read,Ord,Typeable,Data,Generic)

instance NFData Side

type Commodity = String

-- | The basic numeric type used in amounts. Different implementations
-- can be selected via cabal flag for testing and benchmarking purposes.
numberRepresentation :: String
#ifdef DOUBLE
type Quantity = Double
numberRepresentation = "Double"
#else
type Quantity = Decimal
deriving instance Data (Quantity)
-- The following is for hledger-web, and requires blaze-markup.
-- Doing it here avoids needing a matching flag on the hledger-web package.
instance ToMarkup (Quantity) 
 where
   toMarkup = toMarkup . show
numberRepresentation = "Decimal"
#endif

-- | An amount's price (none, per unit, or total) in another commodity.
-- Note the price should be a positive number, although this is not enforced.
data Price = NoPrice | UnitPrice Amount | TotalPrice Amount deriving (Eq,Ord,Typeable,Data,Generic)

instance NFData Price

-- | Display style for an amount.
data AmountStyle = AmountStyle {
      ascommodityside :: Side,       -- ^ does the symbol appear on the left or the right ?
      ascommodityspaced :: Bool,     -- ^ space between symbol and quantity ?
      asprecision :: Int,            -- ^ number of digits displayed after the decimal point
      asdecimalpoint :: Maybe Char,  -- ^ character used as decimal point: period or comma. Nothing means "unspecified, use default"
      asdigitgroups :: Maybe DigitGroupStyle -- ^ style for displaying digit groups, if any
} deriving (Eq,Ord,Read,Show,Typeable,Data,Generic)

instance NFData AmountStyle

-- | A style for displaying digit groups in the integer part of a
-- floating point number. It consists of the character used to
-- separate groups (comma or period, whichever is not used as decimal
-- point), and the size of each group, starting with the one nearest
-- the decimal point. The last group size is assumed to repeat. Eg,
-- comma between thousands is DigitGroups ',' [3].
data DigitGroupStyle = DigitGroups Char [Int]
  deriving (Eq,Ord,Read,Show,Typeable,Data,Generic)

instance NFData DigitGroupStyle

data Amount = Amount {
      acommodity :: Commodity,
      aquantity :: Quantity,
      aprice :: Price,                -- ^ the (fixed) price for this amount, if any
      astyle :: AmountStyle
    } deriving (Eq,Ord,Typeable,Data,Generic)

instance NFData Amount

newtype MixedAmount = Mixed [Amount] deriving (Eq,Ord,Typeable,Data,Generic)

instance NFData MixedAmount

data PostingType = RegularPosting | VirtualPosting | BalancedVirtualPosting
                   deriving (Eq,Show,Typeable,Data,Generic)

instance NFData PostingType

type Tag = (String, String)  -- ^ A tag name and (possibly empty) value.

data ClearedStatus = Uncleared | Pending | Cleared
                   deriving (Eq,Ord,Typeable,Data,Generic)

instance NFData ClearedStatus

instance Show ClearedStatus where -- custom show
  show Uncleared = ""             -- a bad idea
  show Pending   = "!"            -- don't do it
  show Cleared   = "*"

data Posting = Posting {
      pdate :: Maybe Day,  -- ^ this posting's date, if different from the transaction's
      pdate2 :: Maybe Day,  -- ^ this posting's secondary date, if different from the transaction's
      pstatus :: ClearedStatus,
      paccount :: AccountName,
      pamount :: MixedAmount,
      pcomment :: String, -- ^ this posting's comment lines, as a single non-indented multi-line string
      ptype :: PostingType,
      ptags :: [Tag], -- ^ tag names and values, extracted from the comment
      pbalanceassertion :: Maybe MixedAmount,  -- ^ optional: the expected balance in the account after this posting
      ptransaction :: Maybe Transaction    -- ^ this posting's parent transaction (co-recursive types).
                                           -- Tying this knot gets tedious, Maybe makes it easier/optional.
    } deriving (Typeable,Data,Generic)

instance NFData Posting

-- The equality test for postings ignores the parent transaction's
-- identity, to avoid infinite loops.
instance Eq Posting where
    (==) (Posting a1 b1 c1 d1 e1 f1 g1 h1 i1 _) (Posting a2 b2 c2 d2 e2 f2 g2 h2 i2 _) =  a1==a2 && b1==b2 && c1==c2 && d1==d2 && e1==e2 && f1==f2 && g1==g2 && h1==h2 && i1==i2

-- | The position of parse errors (eg), like parsec's SourcePos but generic.
-- File name, 1-based line number and 1-based column number.
data GenericSourcePos = GenericSourcePos FilePath Int Int
  deriving (Eq, Read, Show, Ord, Data, Generic, Typeable)

instance NFData GenericSourcePos

data Transaction = Transaction {
      tsourcepos :: GenericSourcePos,
      tdate :: Day,
      tdate2 :: Maybe Day,
      tstatus :: ClearedStatus,
      tcode :: String,
      tdescription :: String,
      tcomment :: String, -- ^ this transaction's comment lines, as a single non-indented multi-line string
      ttags :: [Tag], -- ^ tag names and values, extracted from the comment
      tpostings :: [Posting],            -- ^ this transaction's postings
      tpreceding_comment_lines :: String -- ^ any comment lines immediately preceding this transaction
    } deriving (Eq,Typeable,Data,Generic)

instance NFData Transaction

data ModifierTransaction = ModifierTransaction {
      mtvalueexpr :: String,
      mtpostings :: [Posting]
    } deriving (Eq,Typeable,Data,Generic)

instance NFData ModifierTransaction

data PeriodicTransaction = PeriodicTransaction {
      ptperiodicexpr :: String,
      ptpostings :: [Posting]
    } deriving (Eq,Typeable,Data,Generic)

instance NFData PeriodicTransaction

data TimeLogCode = SetBalance | SetRequiredHours | In | Out | FinalOut deriving (Eq,Ord,Typeable,Data,Generic)

instance NFData TimeLogCode

data TimeLogEntry = TimeLogEntry {
      tlsourcepos :: GenericSourcePos,
      tlcode :: TimeLogCode,
      tldatetime :: LocalTime,
      tlaccount :: String,
      tldescription :: String
    } deriving (Eq,Ord,Typeable,Data,Generic)

instance NFData TimeLogEntry

data MarketPrice = MarketPrice {
      mpdate :: Day,
      mpcommodity :: Commodity,
      mpamount :: Amount
    } deriving (Eq,Ord,Typeable,Data,Generic) -- & Show (in Amount.hs)

instance NFData MarketPrice

type Year = Integer

-- | A journal "context" is some data which can change in the course of
-- parsing a journal. An example is the default year, which changes when a
-- Y directive is encountered.  At the end of parsing, the final context
-- is saved for later use by eg the add command.
data JournalContext = Ctx {
      ctxYear      :: !(Maybe Year)      -- ^ the default year most recently specified with Y
    , ctxDefaultCommodityAndStyle :: !(Maybe (Commodity,AmountStyle)) -- ^ the default commodity and amount style most recently specified with D
    , ctxAccount   :: ![AccountName]     -- ^ the current stack of parent accounts/account name components
                                        --   specified with "account" directive(s). Concatenated, these
                                        --   are the account prefix prepended to parsed account names.
    , ctxAliases   :: ![AccountAlias]   -- ^ the current list of account name aliases in effect
    } deriving (Read, Show, Eq, Data, Typeable, Generic)

instance NFData JournalContext

deriving instance Data (ClockTime)
deriving instance Typeable (ClockTime)
deriving instance Generic (ClockTime)

instance NFData ClockTime

data Journal = Journal {
      jmodifiertxns :: [ModifierTransaction],
      jperiodictxns :: [PeriodicTransaction],
      jtxns :: [Transaction],
      open_timelog_entries :: [TimeLogEntry],
      jmarketprices :: [MarketPrice],
      final_comment_lines :: String,        -- ^ any trailing comments from the journal file
      jContext :: JournalContext,           -- ^ the context (parse state) at the end of parsing
      files :: [(FilePath, String)],        -- ^ the file path and raw text of the main and
                                            -- any included journal files. The main file is
                                            -- first followed by any included files in the
                                            -- order encountered.
      filereadtime :: ClockTime,            -- ^ when this journal was last read from its file(s)
      jcommoditystyles :: M.Map Commodity AmountStyle  -- ^ how to display amounts in each commodity
    } deriving (Eq, Typeable, Data, Generic)

instance NFData Journal

-- | A JournalUpdate is some transformation of a Journal. It can do I/O or
-- raise an error.
type JournalUpdate = ExceptT String IO (Journal -> Journal)

-- | The id of a data format understood by hledger, eg @journal@ or @csv@.
type StorageFormat = String

-- | A hledger journal reader is a triple of format name, format-detecting
-- predicate, and a parser to Journal.
data Reader = Reader {
     -- name of the format this reader handles
     rFormat   :: StorageFormat
     -- quickly check if this reader can probably handle the given file path and file content
    ,rDetector :: FilePath -> String -> Bool
     -- parse the given string, using the given parse rules file if any, returning a journal or error aware of the given file path
    ,rParser   :: Maybe FilePath -> Bool -> FilePath -> String -> ExceptT String IO Journal
    }

instance Show Reader where show r = rFormat r ++ " reader"

-- format strings

data HledgerFormatField =
    AccountField
  | DefaultDateField
  | DescriptionField
  | TotalField
  | DepthSpacerField
  | FieldNo Int
    deriving (Show, Eq)

data OutputFormat =
    FormatLiteral String
  | FormatField Bool        -- Left justified ?
                (Maybe Int) -- Min width
                (Maybe Int) -- Max width
                HledgerFormatField       -- Field
  deriving (Show, Eq)


-- | An account, with name, balances and links to parent/subaccounts
-- which let you walk up or down the account tree.
data Account = Account {
  aname :: AccountName,     -- ^ this account's full name
  aebalance :: MixedAmount, -- ^ this account's balance, excluding subaccounts
  asubs :: [Account],       -- ^ sub-accounts
  anumpostings :: Int,      -- ^ number of postings to this account
  -- derived from the above:
  aibalance :: MixedAmount, -- ^ this account's balance, including subaccounts
  aparent :: Maybe Account, -- ^ parent account
  aboring :: Bool           -- ^ used in the accounts report to label elidable parents
  }



-- | A Ledger has the journal it derives from, and the accounts
-- derived from that. Accounts are accessible both list-wise and
-- tree-wise, since each one knows its parent and subs; the first
-- account is the root of the tree and always exists.
data Ledger = Ledger {
  ljournal :: Journal,
  laccounts :: [Account]
}
