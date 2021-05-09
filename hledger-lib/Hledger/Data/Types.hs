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

-- {-# LANGUAGE DeriveAnyClass #-}  -- https://hackage.haskell.org/package/deepseq-1.4.4.0/docs/Control-DeepSeq.html#v:rnf
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}

module Hledger.Data.Types
where

import GHC.Generics (Generic)
import Data.Decimal
import Data.Default
import Data.Functor (($>))
import Data.List (intercalate)
import Text.Blaze (ToMarkup(..))
--XXX https://hackage.haskell.org/package/containers/docs/Data-Map.html
--Note: You should use Data.Map.Strict instead of this module if:
--You will eventually need all the values stored.
--The stored values don't represent large virtual data structures to be lazily computed.
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Word (Word8)
import System.Time (ClockTime(..))

import Hledger.Utils.Regex


-- | A possibly incomplete year-month-day date provided by the user, to be
-- interpreted as either a date or a date span depending on context. Missing
-- parts "on the left" will be filled from the provided reference date, e.g. if
-- the year and month are missing, the reference date's year and month are used.
-- Missing parts "on the right" are assumed, when interpreting as a date, to be
-- 1, (e.g. if the year and month are present but the day is missing, it means
-- first day of that month); or when interpreting as a date span, to be a
-- wildcard (so it would mean all days of that month). See the `smartdate`
-- parser for more examples.
--
-- Or, one of the standard periods and an offset relative to the reference date:
-- (last|this|next) (day|week|month|quarter|year), where "this" means the period
-- containing the reference date.
data SmartDate
  = SmartAssumeStart Year (Maybe (Month, Maybe MonthDay))
  | SmartFromReference (Maybe Month) MonthDay
  | SmartMonth Month
  | SmartRelative SmartSequence SmartInterval
  deriving (Show)

data SmartSequence = Last | This | Next deriving (Show)
data SmartInterval = Day | Week | Month | Quarter | Year deriving (Show)

data WhichDate = PrimaryDate | SecondaryDate deriving (Eq,Show)

data DateSpan = DateSpan (Maybe Day) (Maybe Day) deriving (Eq,Ord,Generic)

instance Default DateSpan where def = DateSpan Nothing Nothing

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
-- A higher-level abstraction than DateSpan.
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
  deriving (Eq,Ord,Show,Generic)

instance Default Period where def = PeriodAll

---- Typical report period/subperiod durations, from a day to a year.
--data Duration =
--    DayLong
--   WeekLong
--   MonthLong
--   QuarterLong
--   YearLong
--  deriving (Eq,Ord,Show,Generic)

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
  deriving (Eq,Show,Ord,Generic)

instance Default Interval where def = NoInterval

type Payee = Text

type AccountName = Text

data AccountType =
    Asset
  | Liability
  | Equity
  | Revenue
  | Expense
  | Cash  -- ^ a subtype of Asset - liquid assets to show in cashflow report
  deriving (Show,Eq,Ord,Generic)

-- not worth the trouble, letters defined in accountdirectivep for now
--instance Read AccountType
--  where
--    readsPrec _ ('A' : xs) = [(Asset,     xs)]
--    readsPrec _ ('L' : xs) = [(Liability, xs)]
--    readsPrec _ ('E' : xs) = [(Equity,    xs)]
--    readsPrec _ ('R' : xs) = [(Revenue,   xs)]
--    readsPrec _ ('X' : xs) = [(Expense,   xs)]
--    readsPrec _ _ = []

data AccountAlias = BasicAlias AccountName AccountName
                  | RegexAlias Regexp Replacement
  deriving (Eq, Read, Show, Ord, Generic)

data Side = L | R deriving (Eq,Show,Read,Ord,Generic)

-- | One of the decimal marks we support: either period or comma.
type DecimalMark = Char

isDecimalMark :: Char -> Bool
isDecimalMark c = c == '.' || c == ','

-- | The basic numeric type used in amounts.
type Quantity = Decimal
-- The following is for hledger-web, and requires blaze-markup.
-- Doing it here avoids needing a matching flag on the hledger-web package.
instance ToMarkup Quantity
 where
   toMarkup = toMarkup . show

-- | An amount's per-unit or total cost/selling price in another
-- commodity, as recorded in the journal entry eg with @ or @@.
-- Docs call this "transaction price". The amount is always positive.
data AmountPrice = UnitPrice !Amount | TotalPrice !Amount
  deriving (Eq,Ord,Generic,Show)

-- | Display style for an amount.
data AmountStyle = AmountStyle {
      ascommodityside   :: !Side,                   -- ^ does the symbol appear on the left or the right ?
      ascommodityspaced :: !Bool,                   -- ^ space between symbol and quantity ?
      asprecision       :: !AmountPrecision,        -- ^ number of digits displayed after the decimal point
      asdecimalpoint    :: !(Maybe Char),           -- ^ character used as decimal point: period or comma. Nothing means "unspecified, use default"
      asdigitgroups     :: !(Maybe DigitGroupStyle) -- ^ style for displaying digit groups, if any
} deriving (Eq,Ord,Read,Generic)

instance Show AmountStyle where
  show AmountStyle{..} = concat
    [ "AmountStylePP \""
    , show ascommodityside
    , show ascommodityspaced
    , show asprecision
    , show asdecimalpoint
    , show asdigitgroups
    , "..\""
    ]

-- | The "display precision" for a hledger amount, by which we mean
-- the number of decimal digits to display to the right of the decimal mark.
-- This can be from 0 to 255 digits (the maximum supported by the Decimal library),
-- or NaturalPrecision meaning "show all significant decimal digits".
data AmountPrecision = Precision !Word8 | NaturalPrecision deriving (Eq,Ord,Read,Show,Generic)

-- | A style for displaying digit groups in the integer part of a
-- floating point number. It consists of the character used to
-- separate groups (comma or period, whichever is not used as decimal
-- point), and the size of each group, starting with the one nearest
-- the decimal point. The last group size is assumed to repeat. Eg,
-- comma between thousands is DigitGroups ',' [3].
data DigitGroupStyle = DigitGroups !Char ![Word8]
  deriving (Eq,Ord,Read,Show,Generic)

type CommoditySymbol = Text

data Commodity = Commodity {
  csymbol :: CommoditySymbol,
  cformat :: Maybe AmountStyle
  } deriving (Show,Eq,Generic) --,Ord)

data Amount = Amount {
      acommodity  :: !CommoditySymbol,     -- commodity symbol, or special value "AUTO"
      aquantity   :: !Quantity,            -- numeric quantity, or zero in case of "AUTO"
      aismultiplier :: !Bool,              -- ^ kludge: a flag marking this amount and posting as a multiplier
                                           --   in a TMPostingRule. In a regular Posting, should always be false.
      astyle      :: !AmountStyle,
      aprice      :: !(Maybe AmountPrice)  -- ^ the (fixed, transaction-specific) price for this amount, if any
    } deriving (Eq,Ord,Generic,Show)

newtype MixedAmount = Mixed (M.Map MixedAmountKey Amount) deriving (Eq,Ord,Generic,Show)

-- | Stores the CommoditySymbol of the Amount, along with the CommoditySymbol of
-- the price, and its unit price if being used.
data MixedAmountKey
  = MixedAmountKeyNoPrice    !CommoditySymbol
  | MixedAmountKeyTotalPrice !CommoditySymbol !CommoditySymbol
  | MixedAmountKeyUnitPrice  !CommoditySymbol !CommoditySymbol !Quantity
  deriving (Eq,Generic,Show)

-- | We don't auto-derive the Ord instance because it would give an undesired ordering.
-- We want the keys to be sorted lexicographically:
-- (1) By the primary commodity of the amount.
-- (2) By the commodity of the price, with no price being first.
-- (3) By the unit price, from most negative to most positive, with total prices
-- before unit prices.
-- For example, we would like the ordering to give
-- MixedAmountKeyNoPrice "X" < MixedAmountKeyTotalPrice "X" "Z" < MixedAmountKeyNoPrice "Y"
instance Ord MixedAmountKey where
  compare = comparing commodity <> comparing pCommodity <> comparing pPrice
    where
      commodity (MixedAmountKeyNoPrice    c)     = c
      commodity (MixedAmountKeyTotalPrice c _)   = c
      commodity (MixedAmountKeyUnitPrice  c _ _) = c

      pCommodity (MixedAmountKeyNoPrice    _)      = Nothing
      pCommodity (MixedAmountKeyTotalPrice _ pc)   = Just pc
      pCommodity (MixedAmountKeyUnitPrice  _ pc _) = Just pc

      pPrice (MixedAmountKeyNoPrice    _)     = Nothing
      pPrice (MixedAmountKeyTotalPrice _ _)   = Nothing
      pPrice (MixedAmountKeyUnitPrice  _ _ q) = Just q

data PostingType = RegularPosting | VirtualPosting | BalancedVirtualPosting
                   deriving (Eq,Show,Generic)

type TagName = Text
type TagValue = Text
type Tag = (TagName, TagValue)  -- ^ A tag name and (possibly empty) value.
type DateTag = (TagName, Day)

-- | The status of a transaction or posting, recorded with a status mark
-- (nothing, !, or *). What these mean is ultimately user defined.
data Status = Unmarked | Pending | Cleared
  deriving (Eq,Ord,Bounded,Enum,Generic)

instance Show Status where -- custom show.. bad idea.. don't do it..
  show Unmarked = ""
  show Pending   = "!"
  show Cleared   = "*"

-- | A balance assertion is a declaration about an account's expected balance
-- at a certain point (posting date and parse order). They provide additional
-- error checking and readability to a journal file.
--
-- The 'BalanceAssertion' type is also used to represent balance assignments,
-- which instruct hledger what an account's balance should become at a certain
-- point.
--
-- Different kinds of balance assertions are discussed eg on #290.
-- Variables include:
--
-- - which postings are to be summed (real/virtual; unmarked/pending/cleared; this account/this account including subs)
--
-- - which commodities within the balance are to be checked
--
-- - whether to do a partial or a total check (disallowing other commodities)
--
-- I suspect we want:
--
-- 1. partial, subaccount-exclusive, Ledger-compatible assertions. Because
--    they're what we've always had, and removing them would break some
--    journals unnecessarily.  Implemented with = syntax.
--
-- 2. total assertions. Because otherwise assertions are a bit leaky.
--    Implemented with == syntax.
--
-- 3. subaccount-inclusive assertions. Because that's something folks need.
--    Not implemented.
--
-- 4. flexible assertions allowing custom criteria (perhaps arbitrary
--    queries). Because power users have diverse needs and want to try out
--    different schemes (assert cleared balances, assert balance from real or
--    virtual postings, etc.). Not implemented.
--
-- 5. multicommodity assertions, asserting the balance of multiple commodities
--    at once. Not implemented, requires #934.
--
data BalanceAssertion = BalanceAssertion {
      baamount    :: Amount,             -- ^ the expected balance in a particular commodity
      batotal     :: Bool,               -- ^ disallow additional non-asserted commodities ?
      bainclusive :: Bool,               -- ^ include subaccounts when calculating the actual balance ?
      baposition  :: GenericSourcePos    -- ^ the assertion's file position, for error reporting
    } deriving (Eq,Generic,Show)

data Posting = Posting {
      pdate             :: Maybe Day,         -- ^ this posting's date, if different from the transaction's
      pdate2            :: Maybe Day,         -- ^ this posting's secondary date, if different from the transaction's
      pstatus           :: Status,
      paccount          :: AccountName,
      pamount           :: MixedAmount,
      pcomment          :: Text,              -- ^ this posting's comment lines, as a single non-indented multi-line string
      ptype             :: PostingType,
      ptags             :: [Tag],                   -- ^ tag names and values, extracted from the comment
      pbalanceassertion :: Maybe BalanceAssertion,  -- ^ an expected balance in the account after this posting,
                                                    --   in a single commodity, excluding subaccounts.
      ptransaction      :: Maybe Transaction,       -- ^ this posting's parent transaction (co-recursive types).
                                                    --   Tying this knot gets tedious, Maybe makes it easier/optional.
      poriginal         :: Maybe Posting            -- ^ When this posting has been transformed in some way
                                                    --   (eg its amount or price was inferred, or the account name was
                                                    --   changed by a pivot or budget report), this references the original
                                                    --   untransformed posting (which will have Nothing in this field).
    } deriving (Generic)

-- The equality test for postings ignores the parent transaction's
-- identity, to avoid recurring ad infinitum.
-- XXX could check that it's Just or Nothing.
instance Eq Posting where
    (==) (Posting a1 b1 c1 d1 e1 f1 g1 h1 i1 _ _) (Posting a2 b2 c2 d2 e2 f2 g2 h2 i2 _ _) =  a1==a2 && b1==b2 && c1==c2 && d1==d2 && e1==e2 && f1==f2 && g1==g2 && h1==h2 && i1==i2

-- | Posting's show instance elides the parent transaction so as not to recurse forever.
instance Show Posting where
  show Posting{..} = "PostingPP {" ++ intercalate ", " [
     "pdate="             ++ show (show pdate)
    ,"pdate2="            ++ show (show pdate2)
    ,"pstatus="           ++ show (show pstatus)
    ,"paccount="          ++ show paccount
    ,"pamount="           ++ show pamount
    ,"pcomment="          ++ show pcomment
    ,"ptype="             ++ show ptype
    ,"ptags="             ++ show ptags
    ,"pbalanceassertion=" ++ show pbalanceassertion
    ,"ptransaction="      ++ show (ptransaction $> "txn")
    ,"poriginal="         ++ show poriginal
    ] ++ "}"

-- TODO: needs renaming, or removal if no longer needed. See also TextPosition in Hledger.UI.Editor
-- | The position of parse errors (eg), like parsec's SourcePos but generic.
data GenericSourcePos = GenericSourcePos FilePath Int Int    -- ^ file path, 1-based line number and 1-based column number.
                      | JournalSourcePos FilePath (Int, Int) -- ^ file path, inclusive range of 1-based line numbers (first, last).
  deriving (Eq, Read, Show, Ord, Generic)

--{-# ANN Transaction "HLint: ignore" #-}
--    Ambiguous type variable ‘p0’ arising from an annotation
--    prevents the constraint ‘(Data p0)’ from being solved.
--    Probable fix: use a type annotation to specify what ‘p0’ should be.
data Transaction = Transaction {
      tindex                   :: Integer,   -- ^ this transaction's 1-based position in the transaction stream, or 0 when not available
      tprecedingcomment        :: Text,      -- ^ any comment lines immediately preceding this transaction
      tsourcepos               :: GenericSourcePos,  -- ^ the file position where the date starts
      tdate                    :: Day,
      tdate2                   :: Maybe Day,
      tstatus                  :: Status,
      tcode                    :: Text,
      tdescription             :: Text,
      tcomment                 :: Text,      -- ^ this transaction's comment lines, as a single non-indented multi-line string
      ttags                    :: [Tag],     -- ^ tag names and values, extracted from the comment
      tpostings                :: [Posting]  -- ^ this transaction's postings
    } deriving (Eq,Generic,Show)

-- | A transaction modifier rule. This has a query which matches postings
-- in the journal, and a list of transformations to apply to those
-- postings or their transactions. Currently there is one kind of transformation:
-- the TMPostingRule, which adds a posting ("auto posting") to the transaction,
-- optionally setting its amount to the matched posting's amount multiplied by a constant.
data TransactionModifier = TransactionModifier {
      tmquerytxt :: Text,
      tmpostingrules :: [TMPostingRule]
    } deriving (Eq,Generic,Show)

nulltransactionmodifier = TransactionModifier{
  tmquerytxt = ""
 ,tmpostingrules = []
}

-- | A transaction modifier transformation, which adds an extra posting
-- to the matched posting's transaction.
-- Can be like a regular posting, or the amount can have the aismultiplier flag set,
-- indicating that it's a multiplier for the matched posting's amount.
type TMPostingRule = Posting

-- | A periodic transaction rule, describing a transaction that recurs.
data PeriodicTransaction = PeriodicTransaction {
      ptperiodexpr   :: Text,     -- ^ the period expression as written
      ptinterval     :: Interval, -- ^ the interval at which this transaction recurs
      ptspan         :: DateSpan, -- ^ the (possibly unbounded) period during which this transaction recurs. Contains a whole number of intervals.
      --
      ptstatus       :: Status,   -- ^ some of Transaction's fields
      ptcode         :: Text,
      ptdescription  :: Text,
      ptcomment      :: Text,
      pttags         :: [Tag],
      ptpostings     :: [Posting]
    } deriving (Eq,Generic) -- , Show in PeriodicTransaction.hs

nullperiodictransaction = PeriodicTransaction{
      ptperiodexpr   = ""
     ,ptinterval     = def
     ,ptspan         = def
     ,ptstatus       = Unmarked
     ,ptcode         = ""
     ,ptdescription  = ""
     ,ptcomment      = ""
     ,pttags         = []
     ,ptpostings     = []
}

data TimeclockCode = SetBalance | SetRequiredHours | In | Out | FinalOut deriving (Eq,Ord,Generic)

data TimeclockEntry = TimeclockEntry {
      tlsourcepos   :: GenericSourcePos,
      tlcode        :: TimeclockCode,
      tldatetime    :: LocalTime,
      tlaccount     :: AccountName,
      tldescription :: Text
    } deriving (Eq,Ord,Generic)

-- | A market price declaration made by the journal format's P directive.
-- It declares two things: a historical exchange rate between two commodities,
-- and an amount display style for the second commodity.
data PriceDirective = PriceDirective {
   pddate      :: Day
  ,pdcommodity :: CommoditySymbol
  ,pdamount    :: Amount
  } deriving (Eq,Ord,Generic,Show)
        -- Show instance derived in Amount.hs (XXX why ?)

-- | A historical market price (exchange rate) from one commodity to another.
-- A more concise form of a PriceDirective, without the amount display info.
data MarketPrice = MarketPrice {
   mpdate :: Day                -- ^ Date on which this price becomes effective.
  ,mpfrom :: CommoditySymbol    -- ^ The commodity being converted from.
  ,mpto   :: CommoditySymbol    -- ^ The commodity being converted to.
  ,mprate :: Quantity           -- ^ One unit of the "from" commodity is worth this quantity of the "to" commodity.
  } deriving (Eq,Ord,Generic)
        -- Show instance derived in Amount.hs (XXX why ?)

-- additional valuation-related types in Valuation.hs

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
  ,jparsedecimalmark      :: Maybe DecimalMark                     -- ^ the character to always parse as decimal point, if set by CsvReader's decimal-mark (or a future journal directive)
  ,jparseparentaccounts   :: [AccountName]                         -- ^ the current stack of parent account names, specified by apply account directives
  ,jparsealiases          :: [AccountAlias]                        -- ^ the current account name aliases in effect, specified by alias directives (& options ?)
  -- ,jparsetransactioncount :: Integer                               -- ^ the current count of transactions parsed so far (only journal format txns, currently)
  ,jparsetimeclockentries :: [TimeclockEntry]                       -- ^ timeclock sessions which have not been clocked out
  ,jincludefilestack      :: [FilePath]
  -- principal data
  ,jdeclaredpayees        :: [(Payee,PayeeDeclarationInfo)]         -- ^ Payees declared by payee directives, in parse order (after journal finalisation)
  ,jdeclaredaccounts      :: [(AccountName,AccountDeclarationInfo)] -- ^ Accounts declared by account directives, in parse order (after journal finalisation)
  ,jdeclaredaccounttypes  :: M.Map AccountType [AccountName]        -- ^ Accounts whose type has been declared in account directives (usually 5 top-level accounts)
  ,jglobalcommoditystyles :: M.Map CommoditySymbol AmountStyle      -- ^ per-commodity display styles declared globally, eg by command line option or import command
  ,jcommodities           :: M.Map CommoditySymbol Commodity        -- ^ commodities and formats declared by commodity directives
  ,jinferredcommodities   :: M.Map CommoditySymbol AmountStyle      -- ^ commodities and formats inferred from journal amounts
  ,jpricedirectives       :: [PriceDirective]                       -- ^ Declarations of market prices by P directives, in parse order (after journal finalisation)
  ,jinferredmarketprices  :: [MarketPrice]                          -- ^ Market prices implied by transactions, in parse order (after journal finalisation)
  ,jtxnmodifiers          :: [TransactionModifier]
  ,jperiodictxns          :: [PeriodicTransaction]
  ,jtxns                  :: [Transaction]
  ,jfinalcommentlines     :: Text                                   -- ^ any final trailing comments in the (main) journal file
  ,jfiles                 :: [(FilePath, Text)]                     -- ^ the file path and raw text of the main and
                                                                    --   any included journal files. The main file is first,
                                                                    --   followed by any included files in the order encountered.
                                                                    --   TODO: FilePath is a sloppy type here, don't assume it's a
                                                                    --   real file; values like "", "-", "(string)" can be seen
  ,jlastreadtime          :: ClockTime                              -- ^ when this journal was last read from its file(s)
  } deriving (Eq, Generic)

deriving instance Generic ClockTime

-- | A journal in the process of being parsed, not yet finalised.
-- The data is partial, and list fields are in reverse order.
type ParsedJournal = Journal

-- | The id of a data format understood by hledger, eg @journal@ or @csv@.
-- The --output-format option selects one of these for output.
type StorageFormat = String

-- | Extra information found in a payee directive.
data PayeeDeclarationInfo = PayeeDeclarationInfo {
   pdicomment :: Text   -- ^ any comment lines following the payee directive
  ,pditags    :: [Tag]  -- ^ tags extracted from the comment, if any
} deriving (Eq,Show,Generic)

nullpayeedeclarationinfo = PayeeDeclarationInfo {
   pdicomment          = ""
  ,pditags             = []
}

-- | Extra information about an account that can be derived from
-- its account directive (and the other account directives).
data AccountDeclarationInfo = AccountDeclarationInfo {
   adicomment          :: Text   -- ^ any comment lines following an account directive for this account
  ,aditags             :: [Tag]  -- ^ tags extracted from the account comment, if any
  ,adideclarationorder :: Int    -- ^ the order in which this account was declared,
                                 --   relative to other account declarations, during parsing (1..)
} deriving (Eq,Show,Generic)

nullaccountdeclarationinfo = AccountDeclarationInfo {
   adicomment          = ""
  ,aditags             = []
  ,adideclarationorder = 0
}

-- | An account, with its balances, parent/subaccount relationships, etc.
-- Only the name is required; the other fields are added when needed.
data Account = Account {
   aname                     :: AccountName    -- ^ this account's full name
  ,adeclarationinfo          :: Maybe AccountDeclarationInfo  -- ^ optional extra info from account directives
  -- relationships in the tree
  ,asubs                     :: [Account]      -- ^ this account's sub-accounts
  ,aparent                   :: Maybe Account  -- ^ parent account
  ,aboring                   :: Bool           -- ^ used in the accounts report to label elidable parents
  -- balance information
  ,anumpostings              :: Int            -- ^ the number of postings to this account
  ,aebalance                 :: MixedAmount    -- ^ this account's balance, excluding subaccounts
  ,aibalance                 :: MixedAmount    -- ^ this account's balance, including subaccounts
  } deriving (Generic)

-- | Whether an account's balance is normally a positive number (in
-- accounting terms, a debit balance) or a negative number (credit balance).
-- Assets and expenses are normally positive (debit), while liabilities, equity
-- and income are normally negative (credit).
-- https://en.wikipedia.org/wiki/Normal_balance
data NormalSign = NormallyPositive | NormallyNegative deriving (Show, Eq)

-- | A Ledger has the journal it derives from, and the accounts
-- derived from that. Accounts are accessible both list-wise and
-- tree-wise, since each one knows its parent and subs; the first
-- account is the root of the tree and always exists.
data Ledger = Ledger {
  ljournal  :: Journal,
  laccounts :: [Account]
}

