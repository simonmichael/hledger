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
>  [Account]               -- all accounts, in tree order beginning with a "root" account", with their balances and sub/parent accounts

For more detailed documentation on each type, see the corresponding modules.

-}

module Hledger.Data.Types
where
import Control.Monad.Error (ErrorT)
import qualified Data.Map as M
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Typeable
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

type Commodity = String
      
type Quantity = Double

-- | An amount's price (none, per unit, or total) in another commodity.
-- Note although a MixedAmount is used, it should be in a single
-- commodity, also the amount should be positive; these are not enforced
-- currently.
data Price = NoPrice | UnitPrice MixedAmount | TotalPrice MixedAmount
             deriving (Eq,Ord)

-- | Display style for an amount.
data AmountStyle = AmountStyle {
      ascommodityside :: Side,       -- ^ does the symbol appear on the left or the right ?
      ascommodityspaced :: Bool,     -- ^ space between symbol and quantity ?
      asprecision :: Int,            -- ^ number of digits displayed after the decimal point
      asdecimalpoint :: Char,        -- ^ character used as decimal point
      asseparator :: Char,           -- ^ character used for separating digit groups (eg thousands)
      asseparatorpositions :: [Int]  -- ^ positions of digit group separators, counting leftward from decimal point
} deriving (Eq,Ord,Show,Read)

data Amount = Amount {
      acommodity :: Commodity,
      aquantity :: Quantity,
      aprice :: Price,                -- ^ the (fixed) price for this amount, if any
      astyle :: AmountStyle
    } deriving (Eq,Ord)

newtype MixedAmount = Mixed [Amount] deriving (Eq,Ord)

data PostingType = RegularPosting | VirtualPosting | BalancedVirtualPosting
                   deriving (Eq,Show)

type Tag = (String, String)

data Posting = Posting {
      pstatus :: Bool,
      paccount :: AccountName,
      pamount :: MixedAmount,
      pcomment :: String, -- ^ this posting's non-tag comment lines, as a single non-indented string
      ptype :: PostingType,
      ptags :: [Tag],
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
      tcomment :: String, -- ^ this transaction's non-tag comment lines, as a single non-indented string
      ttags :: [Tag],
      tpostings :: [Posting],            -- ^ this transaction's postings
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
    , ctxCommodityAndStyle :: !(Maybe (Commodity,AmountStyle)) -- ^ the default commodity and amount style most recently specified with D
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
                                            -- order encountered (XXX reversed, cf journalAddFile).
      filereadtime :: ClockTime,            -- ^ when this journal was last read from its file(s)
      jcommoditystyles :: M.Map Commodity AmountStyle  -- ^ how to display amounts in each commodity
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

instance Show Reader where show r = "Reader for "++rFormat r

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


-- | An account, with name, balances and links to parent/subaccounts
-- which let you walk up or down the account tree.
data Account = Account {
  aname :: AccountName,     -- ^ this account's full name
  aebalance :: MixedAmount, -- ^ this account's balance, excluding subaccounts
  asubs :: [Account],       -- ^ sub-accounts
  -- anumpostings :: Int       -- ^ number of postings to this account
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
