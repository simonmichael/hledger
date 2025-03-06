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
{-# LANGUAGE CPP        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}

module Hledger.Data.Types (
  module Hledger.Data.Types,
#if MIN_VERSION_time(1,11,0)
  Year
#endif
)
where

import GHC.Generics (Generic)
import Data.Bifunctor (first)
import Data.Decimal (Decimal, DecimalRaw(..))
import Data.Default (Default(..))
import Data.Functor (($>))
import Data.List (intercalate, sortBy)
--XXX https://hackage.haskell.org/package/containers/docs/Data-Map.html
--Note: You should use Data.Map.Strict instead of this module if:
--You will eventually need all the values stored.
--The stored values don't represent large virtual data structures to be lazily computed.
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Semigroup (Min(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.LocalTime (LocalTime)
import Data.Word (Word8)
import Text.Blaze (ToMarkup(..))
import Text.Megaparsec (SourcePos(SourcePos), mkPos)

import Hledger.Utils.Regex


-- synonyms for various date-related scalars
#if MIN_VERSION_time(1,11,0)
import Data.Time.Calendar (Year)
#else
type Year = Integer
#endif
type Month = Int     -- 1-12
type Quarter = Int   -- 1-4
type YearWeek = Int  -- 1-52
type MonthWeek = Int -- 1-5
type YearDay = Int   -- 1-366
type MonthDay = Int  -- 1-31
type WeekDay = Int   -- 1-7

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
  = SmartCompleteDate Day
  | SmartAssumeStart Year (Maybe Month)         -- XXX improve these constructor names
  | SmartFromReference (Maybe Month) MonthDay   --
  | SmartMonth Month
  | SmartRelative Integer SmartInterval
  deriving (Show)

data SmartInterval = Day | Week | Month | Quarter | Year deriving (Show)

data WhichDate = PrimaryDate | SecondaryDate deriving (Eq,Show)

-- | A date which is either exact or flexible.
-- Flexible dates are allowed to be adjusted in certain situations.
data EFDay = Exact Day | Flex Day deriving (Eq,Generic,Show)

-- EFDay's Ord instance treats them like ordinary dates, ignoring exact/flexible.
instance Ord EFDay where compare d1 d2 = compare (fromEFDay d1) (fromEFDay d2)

-- instance Ord EFDay where compare = maCompare

fromEFDay :: EFDay -> Day
fromEFDay (Exact d) = d
fromEFDay (Flex  d) = d

modifyEFDay :: (Day -> Day) -> EFDay -> EFDay
modifyEFDay f (Exact d) = Exact $ f d
modifyEFDay f (Flex  d) = Flex  $ f d

-- | A possibly open-ended span of time, from an optional inclusive start date
-- to an optional exclusive end date. Each date can be either exact or flexible.
-- An "exact date span" is a Datepan with exact start and end dates.
data DateSpan = DateSpan (Maybe EFDay) (Maybe EFDay) deriving (Eq,Ord,Generic)

instance Default DateSpan where def = DateSpan Nothing Nothing

-- Some common report subperiods, both finite and open-ended.
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

-- All the kinds of report interval allowed in a period expression
-- (to generate periodic reports or periodic transactions).
data Interval =
    NoInterval
  | Days Int
  | Weeks Int
  | Months Int
  | Quarters Int
  | Years Int
  | NthWeekdayOfMonth Int Int  -- n,              weekday 1-7
  | MonthDay Int               -- 1-31
  | MonthAndDay Int Int        -- month 1-12,     monthday 1-31
  | DaysOfWeek [Int]           -- [weekday 1-7]
  deriving (Eq,Show,Ord,Generic)

instance Default Interval where def = NoInterval

type Payee = Text

type AccountName = Text

-- A specification indicating how to depth-limit
data DepthSpec = DepthSpec {
  dsFlatDepth    :: Maybe Int,
  dsRegexpDepths :: [(Regexp, Int)]
  } deriving (Eq,Show)

-- Semigroup instance consider all regular expressions, but take the minimum of the simple flat depths
instance Semigroup DepthSpec where
    DepthSpec d1 l1 <> DepthSpec d2 l2 = DepthSpec (getMin <$> (Min <$> d1) <> (Min <$> d2)) (l1 ++ l2)

instance Monoid DepthSpec where
    mempty = DepthSpec Nothing []

data AccountType =
    Asset
  | Liability
  | Equity
  | Revenue
  | Expense
  | Cash  -- ^ a subtype of Asset - liquid assets to show in cashflow report
  | Conversion -- ^ a subtype of Equity - account with which to balance commodity conversions
  deriving (Eq,Ord,Generic)

instance Show AccountType where
  show Asset      = "A"
  show Liability  = "L"
  show Equity     = "E"
  show Revenue    = "R"
  show Expense    = "X"
  show Cash       = "C"
  show Conversion = "V"

isBalanceSheetAccountType :: AccountType -> Bool
isBalanceSheetAccountType t = t `elem` [
  Asset,
  Liability,
  Equity,
  Cash,
  Conversion
  ]

isIncomeStatementAccountType :: AccountType -> Bool
isIncomeStatementAccountType t = t `elem` [
  Revenue,
  Expense
  ]

-- | Check whether the first argument is a subtype of the second: either equal
-- or one of the defined subtypes.
isAccountSubtypeOf :: AccountType -> AccountType -> Bool
isAccountSubtypeOf Asset      Asset      = True
isAccountSubtypeOf Liability  Liability  = True
isAccountSubtypeOf Equity     Equity     = True
isAccountSubtypeOf Revenue    Revenue    = True
isAccountSubtypeOf Expense    Expense    = True
isAccountSubtypeOf Cash       Cash       = True
isAccountSubtypeOf Cash       Asset      = True
isAccountSubtypeOf Conversion Conversion = True
isAccountSubtypeOf Conversion Equity     = True
isAccountSubtypeOf _          _          = False

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
deriving instance Generic (DecimalRaw a)

-- | An amount's per-unit or total cost/selling price in another
-- commodity, as recorded in the journal entry eg with @ or @@.
-- "Cost", formerly AKA "transaction price". The amount is always positive.
data AmountCost = UnitCost !Amount | TotalCost !Amount
  deriving (Eq,Ord,Generic,Show)

-- | Display styles for amounts - things which can be detected during parsing, such as
-- commodity side and spacing, digit group marks, decimal mark, number of decimal digits etc.
-- Every "Amount" has an AmountStyle.
-- After amounts are parsed from the input, for each "Commodity" a standard style is inferred
-- and then used when displaying amounts in that commodity.
-- Related to "AmountFormat" but higher level.
--
-- See also:
-- - hledger manual > Commodity styles
-- - hledger manual > Amounts
-- - hledger manual > Commodity display style
data AmountStyle = AmountStyle {
  ascommodityside   :: !Side,                     -- ^ show the symbol on the left or the right ?
  ascommodityspaced :: !Bool,                     -- ^ show a space between symbol and quantity ?
  asdigitgroups     :: !(Maybe DigitGroupStyle),  -- ^ show the integer part with these digit group marks, or not
  asdecimalmark     :: !(Maybe Char),             -- ^ show this character (should be . or ,) as decimal mark, or use the default (.)
  asprecision       :: !AmountPrecision,          -- ^ "display precision" - show this number of digits after the decimal point
  asrounding        :: !Rounding                  -- ^ "rounding strategy" - kept here for convenience, for now:
                                                  --   when displaying an amount, it is ignored,
                                                  --   but when applying this style to another amount, it determines 
                                                  --   how hard we should try to adjust that amount's display precision.
} deriving (Eq,Ord,Read,Generic)

instance Show AmountStyle where
  show AmountStyle{..} = unwords
    [ "AmountStylePP"
    , show ascommodityside
    , show ascommodityspaced
    , show asdigitgroups
    , show asdecimalmark
    , show asprecision
    , show asrounding
    ]

-- | The "display precision" for a hledger amount, by which we mean
-- the number of decimal digits to display to the right of the decimal mark.
data AmountPrecision =
    Precision !Word8    -- ^ show this many decimal digits (0..255)
  | NaturalPrecision    -- ^ show all significant decimal digits stored internally
  deriving (Eq,Ord,Read,Show,Generic)

-- | "Rounding strategy" - how to apply an AmountStyle's display precision
-- to a posting amount (and its cost, if any). 
-- Mainly used to customise print's output, with --round=none|soft|hard|all.
data Rounding =
    NoRounding    -- ^ keep display precisions unchanged in amt and cost
  | SoftRounding  -- ^ do soft rounding of amt and cost amounts (show more or fewer decimal zeros to approximate the target precision, but don't hide significant digits)
  | HardRounding  -- ^ do hard rounding of amt (use the exact target precision, possibly hiding significant digits), and soft rounding of cost
  | AllRounding   -- ^ do hard rounding of amt and cost
  deriving (Eq,Ord,Read,Show,Generic)

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
      astyle      :: !AmountStyle,
      acost       :: !(Maybe AmountCost)  -- ^ the (fixed, transaction-specific) cost in another commodity of this amount, if any
    } deriving (Eq,Ord,Generic,Show)

-- | Types with this class have one or more amounts,
-- which can have display styles applied to them.
class HasAmounts a where
  styleAmounts :: M.Map CommoditySymbol AmountStyle -> a -> a

instance HasAmounts a =>
  HasAmounts [a]
  where styleAmounts styles = map (styleAmounts styles)

instance (HasAmounts a, HasAmounts b) =>
  HasAmounts (a,b)
  where styleAmounts styles (aa,bb) = (styleAmounts styles aa, styleAmounts styles bb)

instance HasAmounts a =>
  HasAmounts (Maybe a)
  where styleAmounts styles = fmap (styleAmounts styles)


newtype MixedAmount = Mixed (M.Map MixedAmountKey Amount) deriving (Generic,Show)

instance Eq  MixedAmount where a == b  = maCompare a b == EQ
instance Ord MixedAmount where compare = maCompare

-- | Compare two MixedAmounts, substituting 0 for the quantity of any missing
-- commodities in either.
maCompare :: MixedAmount -> MixedAmount -> Ordering
maCompare (Mixed a) (Mixed b) = go (M.toList a) (M.toList b)
  where
    go xss@((kx,x):xs) yss@((ky,y):ys) = case compare kx ky of
                 EQ -> compareQuantities (Just x) (Just y) <> go xs ys
                 LT -> compareQuantities (Just x) Nothing  <> go xs yss
                 GT -> compareQuantities Nothing  (Just y) <> go xss ys
    go ((_,x):xs) [] = compareQuantities (Just x) Nothing  <> go xs []
    go [] ((_,y):ys) = compareQuantities Nothing  (Just y) <> go [] ys
    go []         [] = EQ
    compareQuantities = comparing (maybe 0 aquantity) <> comparing (maybe 0 totalcost)
    totalcost x = case acost x of
                        Just (TotalCost p) -> aquantity p
                        _                   -> 0

-- | Stores the CommoditySymbol of the Amount, along with the CommoditySymbol of
-- the cost, and its unit cost if being used.
data MixedAmountKey
  = MixedAmountKeyNoCost   !CommoditySymbol
  | MixedAmountKeyTotalCost !CommoditySymbol !CommoditySymbol
  | MixedAmountKeyUnitCost  !CommoditySymbol !CommoditySymbol !Quantity
  deriving (Eq,Generic,Show)

-- | We don't auto-derive the Ord instance because it would give an undesired ordering.
-- We want the keys to be sorted lexicographically:
-- (1) By the primary commodity of the amount.
-- (2) By the commodity of the cost, with no cost being first.
-- (3) By the unit cost, from most negative to most positive, with total costs
-- before unit costs.
-- For example, we would like the ordering to give
-- MixedAmountKeyNoCost "X" < MixedAmountKeyTotalCost "X" "Z" < MixedAmountKeyNoCost "Y"
instance Ord MixedAmountKey where
  compare = comparing commodity <> comparing pCommodity <> comparing pCost
    where
      commodity (MixedAmountKeyNoCost    c)     = c
      commodity (MixedAmountKeyTotalCost c _)   = c
      commodity (MixedAmountKeyUnitCost  c _ _) = c

      pCommodity (MixedAmountKeyNoCost    _)      = Nothing
      pCommodity (MixedAmountKeyTotalCost _ pc)   = Just pc
      pCommodity (MixedAmountKeyUnitCost  _ pc _) = Just pc

      pCost (MixedAmountKeyNoCost    _)     = Nothing
      pCost (MixedAmountKeyTotalCost _ _)   = Nothing
      pCost (MixedAmountKeyUnitCost  _ _ q) = Just q

data PostingType = RegularPosting | VirtualPosting | BalancedVirtualPosting
                   deriving (Eq,Show,Generic)

type TagName = Text
type TagValue = Text
type Tag = (TagName, TagValue)  -- ^ A tag name and (possibly empty) value.
type HiddenTag = Tag            -- ^ A tag whose name begins with _.
type DateTag = (TagName, Day)

-- | Add the _ prefix to a normal visible tag's name, making it a hidden tag.
toHiddenTag :: Tag -> HiddenTag
toHiddenTag = first toHiddenTagName

-- | Drop the _ prefix from a hidden tag's name, making it a normal visible tag.
toVisibleTag :: HiddenTag -> Tag
toVisibleTag = first toVisibleTagName

-- | Does this tag name begin with the hidden tag prefix (_) ?
isHiddenTagName :: TagName -> Bool
isHiddenTagName t =
  case T.uncons t of
    Just ('_',_) -> True
    _ -> False

-- | Add the _ prefix to a normal visible tag's name, making it a hidden tag.
toHiddenTagName :: TagName -> TagName
toHiddenTagName = T.cons '_'

-- | Drop the _ prefix from a hidden tag's name, making it a normal visible tag.
toVisibleTagName :: TagName -> TagName
toVisibleTagName = T.drop 1

-- | The status of a transaction or posting, recorded with a status mark
-- (nothing, !, or *). What these mean is ultimately user defined.
data Status = Unmarked | Pending | Cleared
  deriving (Eq,Ord,Bounded,Enum,Generic)

instance Show Status where -- custom show.. bad idea.. don't do it..
  show Unmarked = ""
  show Pending   = "!"
  show Cleared   = "*"

nullsourcepos :: SourcePos
nullsourcepos = SourcePos "" (mkPos 1) (mkPos 1)

nullsourcepospair :: (SourcePos, SourcePos)
nullsourcepospair = (SourcePos "" (mkPos 1) (mkPos 1), SourcePos "" (mkPos 2) (mkPos 1))

-- | A balance assertion is a declaration about an account's expected balance
-- at a certain point (posting date and parse order). They provide additional
-- error checking and readability to a journal file.
--
-- A balance assignments is an instruction to hledger to adjust an
-- account's balance to a certain amount at a certain point.
--
-- The 'BalanceAssertion' type is used for representing both of these.
--
-- hledger supports multiple kinds of balance assertions/assignments,
-- which differ in whether they refer to a single commodity or all commodities,
-- and the (subaccount-)inclusive or exclusive account balance.
--
data BalanceAssertion = BalanceAssertion {
      baamount    :: Amount,    -- ^ the expected balance in a particular commodity
      batotal     :: Bool,      -- ^ disallow additional non-asserted commodities ?
      bainclusive :: Bool,      -- ^ include subaccounts when calculating the actual balance ?
      baposition  :: SourcePos  -- ^ the assertion's file position, for error reporting
    } deriving (Eq,Generic,Show)

data Posting = Posting {
      pdate             :: Maybe Day,         -- ^ this posting's date, if different from the transaction's
      pdate2            :: Maybe Day,         -- ^ this posting's secondary date, if different from the transaction's
      pstatus           :: Status,
      paccount          :: AccountName,
      pamount           :: MixedAmount,
      pcomment          :: Text,              -- ^ this posting's comment lines, as a single non-indented multi-line string
      ptype             :: PostingType,
      ptags             :: [Tag],                   -- ^ tag names and values, extracted from the posting comment 
                                                    --   and (after finalisation) the posting account's directive if any
      pbalanceassertion :: Maybe BalanceAssertion,  -- ^ an expected balance in the account after this posting,
                                                    --   in a single commodity, excluding subaccounts.
      ptransaction      :: Maybe Transaction,       -- ^ this posting's parent transaction (co-recursive types).
                                                    --   Tying this knot gets tedious, Maybe makes it easier/optional.
      poriginal         :: Maybe Posting            -- ^ When this posting has been transformed in some way
                                                    --   (eg its amount or cost was inferred, or the account name was
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

data Transaction = Transaction {
      tindex                   :: Integer,   -- ^ this transaction's 1-based position in the transaction stream, or 0 when not available
      tprecedingcomment        :: Text,      -- ^ any comment lines immediately preceding this transaction
      tsourcepos               :: (SourcePos, SourcePos),  -- ^ the file position where the date starts, and where the last posting ends
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
-- Can be like a regular posting, or can have the tmprIsMultiplier flag set,
-- indicating that it's a multiplier for the matched posting's amount.
data TMPostingRule = TMPostingRule
  { tmprPosting :: Posting
  , tmprIsMultiplier :: Bool
  } deriving (Eq,Generic,Show)

-- | A periodic transaction rule, describing a transaction that recurs.
data PeriodicTransaction = PeriodicTransaction {
      ptperiodexpr   :: Text,     -- ^ the period expression as written
      ptinterval     :: Interval, -- ^ the interval at which this transaction recurs
      ptspan         :: DateSpan, -- ^ the (possibly unbounded) period during which this transaction recurs. Contains a whole number of intervals.
      --
      ptsourcepos    :: (SourcePos, SourcePos),  -- ^ the file position where the period expression starts, and where the last posting ends
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
     ,ptsourcepos    = (SourcePos "" (mkPos 1) (mkPos 1), SourcePos "" (mkPos 1) (mkPos 1))
     ,ptstatus       = Unmarked
     ,ptcode         = ""
     ,ptdescription  = ""
     ,ptcomment      = ""
     ,pttags         = []
     ,ptpostings     = []
}

data TimeclockCode = SetBalance | SetRequiredHours | In | Out | FinalOut deriving (Eq,Ord,Generic)

data TimeclockEntry = TimeclockEntry {
      tlsourcepos   :: SourcePos,
      tlcode        :: TimeclockCode,
      tldatetime    :: LocalTime,
      tlaccount     :: AccountName,
      tldescription :: Text,
      tlcomment     :: Text,
      tltags        :: [Tag]
    } deriving (Eq,Ord,Generic)

-- | A market price declaration made by the journal format's P directive.
-- It declares two things: a historical exchange rate between two commodities,
-- and an amount display style for the second commodity.
data PriceDirective = PriceDirective {
   pdsourcepos :: SourcePos
  ,pddate      :: Day
  ,pdcommodity :: CommoditySymbol
  ,pdamount    :: Amount
  } deriving (Eq,Ord,Generic,Show)

-- | A historical market price (exchange rate) from one commodity to another.
-- A more concise form of a PriceDirective, without the amount display info.
data MarketPrice = MarketPrice {
   mpdate :: Day                -- ^ Date on which this price becomes effective.
  ,mpfrom :: CommoditySymbol    -- ^ The commodity being converted from.
  ,mpto   :: CommoditySymbol    -- ^ The commodity being converted to.
  ,mprate :: Quantity           -- ^ One unit of the "from" commodity is worth this quantity of the "to" commodity.
  } deriving (Eq,Ord,Generic, Show)

showMarketPrice MarketPrice{..} = unwords [show mpdate, T.unpack mpfrom <> ">" <> T.unpack mpto, show mprate]
showMarketPrices = intercalate "\n" . map ((' ':).showMarketPrice) . sortBy (comparing mpdate)

-- additional valuation-related types in Valuation.hs

-- | A journal, containing general ledger transactions; also directives and various other things.
-- This is hledger's main data model.
--
-- During parsing, it is used as the type alias "ParsedJournal".
-- The jparse* fields are mainly used during parsing and included here for convenience.
-- The list fields described as "in parse order" are usually reversed for efficiency during parsing.
-- After parsing, "journalFinalise" converts ParsedJournal to a finalised "Journal",
-- which has all lists correctly ordered, and much data inference and validation applied.
--
data Journal = Journal {
  -- parsing-related state
   jparsedefaultyear        :: Maybe Year                             -- ^ the current default year, specified by the most recent Y directive (or current date)
  ,jparsedefaultcommodity   :: Maybe (CommoditySymbol,AmountStyle)    -- ^ the current default commodity and its format, specified by the most recent D directive
  ,jparsedecimalmark        :: Maybe DecimalMark                      -- ^ the character to always parse as decimal point, if set by CsvReader's decimal-mark (or a future journal directive)
  ,jparseparentaccounts     :: [AccountName]                          -- ^ the current stack of parent account names, specified by apply account directives
  ,jparsealiases            :: [AccountAlias]                         -- ^ the current account name aliases in effect, specified by alias directives (& options ?)
  -- ,jparsetransactioncount :: Integer                               -- ^ the current count of transactions parsed so far (only journal format txns, currently)
  ,jparsetimeclockentries   :: [TimeclockEntry]                       -- ^ timeclock sessions which have not been clocked out
  ,jincludefilestack        :: [FilePath]
  -- principal data
  ,jdeclaredpayees          :: [(Payee,PayeeDeclarationInfo)]         -- ^ Payees declared by payee directives, in parse order.
  ,jdeclaredtags            :: [(TagName,TagDeclarationInfo)]         -- ^ Tags declared by tag directives, in parse order.
  ,jdeclaredaccounts        :: [(AccountName,AccountDeclarationInfo)] -- ^ Accounts declared by account directives, in parse order.
  ,jdeclaredaccounttags     :: M.Map AccountName [Tag]                -- ^ Accounts which were declared with tags, and those tags.
  ,jdeclaredaccounttypes    :: M.Map AccountType [AccountName]        -- ^ Accounts which were declared with a type: tag, grouped by the type.
  ,jaccounttypes            :: M.Map AccountName AccountType          -- ^ All the account types known, from account declarations or account names or parent accounts.
  ,jdeclaredcommodities     :: M.Map CommoditySymbol Commodity        -- ^ Commodities (and their display styles) declared by commodity directives, in parse order.
  ,jinferredcommoditystyles :: M.Map CommoditySymbol AmountStyle      -- ^ Commodity display styles inferred from amounts in the journal.
  ,jglobalcommoditystyles   :: M.Map CommoditySymbol AmountStyle      -- ^ Commodity display styles declared by command line options (sometimes augmented, see the import command).
  ,jpricedirectives         :: [PriceDirective]                       -- ^ P (market price) directives in the journal, in parse order.
  ,jinferredmarketprices    :: [MarketPrice]                          -- ^ Market prices inferred from transactions in the journal, in parse order.
  ,jtxnmodifiers            :: [TransactionModifier]                  -- ^ Auto posting rules declared in the journal.
  ,jperiodictxns            :: [PeriodicTransaction]                  -- ^ Periodic transaction rules declared in the journal.
  ,jtxns                    :: [Transaction]                          -- ^ Transactions recorded in the journal. The important bit.
  ,jfinalcommentlines       :: Text                                   -- ^ any final trailing comments in the (main) journal file
  ,jfiles                   :: [(FilePath, Text)]                     -- ^ the file path and raw text of the main and
                                                                      --   any included journal files. The main file is first,
                                                                      --   followed by any included files in the order encountered.
                                                                      --   TODO: FilePath is a sloppy type here, don't assume it's a
                                                                      --   real file; values like "" or "-" can be seen
  ,jlastreadtime            :: POSIXTime                              -- ^ when this journal was last read from its file(s)
  -- NOTE: after adding new fields, eg involving account names, consider updating
  -- the Anon instance in Hleger.Cli.Anon
  } deriving (Eq, Generic)

-- | A journal in the process of being parsed, not yet finalised.
-- The data is partial, and list fields are in reverse order.
type ParsedJournal = Journal

-- | One of the standard *-separated value file types known by hledger,
data SepFormat 
  = Csv  -- comma-separated
  | Tsv  -- tab-separated
  | Ssv  -- semicolon-separated
  deriving Eq

-- | The id of a data format understood by hledger, eg @journal@ or @csv@.
-- The --output-format option selects one of these for output.
data StorageFormat 
  = Rules 
  | Journal' 
  | Ledger' 
  | Timeclock 
  | Timedot 
  | Sep SepFormat 
  deriving Eq

instance Show SepFormat where
  show Csv = "csv"
  show Ssv = "ssv"
  show Tsv = "tsv"

instance Show StorageFormat where
  show Rules = "rules"
  show Journal' = "journal"
  show Ledger' = "ledger"
  show Timeclock = "timeclock"
  show Timedot = "timedot"
  show (Sep Csv) = "csv"
  show (Sep Ssv) = "ssv"
  show (Sep Tsv) = "tsv"

-- | Extra information found in a payee directive.
data PayeeDeclarationInfo = PayeeDeclarationInfo {
   pdicomment :: Text   -- ^ any comment lines following the payee directive
  ,pditags    :: [Tag]  -- ^ tags extracted from the comment, if any
} deriving (Eq,Show,Generic)

nullpayeedeclarationinfo = PayeeDeclarationInfo {
   pdicomment          = ""
  ,pditags             = []
}

-- | Extra information found in a tag directive.
newtype TagDeclarationInfo = TagDeclarationInfo {
   tdicomment :: Text   -- ^ any comment lines following the tag directive. No tags allowed here.
} deriving (Eq,Show,Generic)

nulltagdeclarationinfo = TagDeclarationInfo {
   tdicomment          = ""
}

-- | Extra information about an account that can be derived from
-- its account directive (and the other account directives).
data AccountDeclarationInfo = AccountDeclarationInfo {
   adicomment          :: Text   -- ^ any comment lines following an account directive for this account
  ,aditags             :: [Tag]  -- ^ tags extracted from the account comment, if any
  ,adideclarationorder :: Int    -- ^ the order in which this account was declared,
                                 --   relative to other account declarations, during parsing (1..)
  ,adisourcepos        :: SourcePos  -- ^ source file and position
} deriving (Eq,Show,Generic)

nullaccountdeclarationinfo = AccountDeclarationInfo {
   adicomment          = ""
  ,aditags             = []
  ,adideclarationorder = 0
  ,adisourcepos        = SourcePos "" (mkPos 1) (mkPos 1)
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
   ljournal  :: Journal
  ,laccounts :: [Account]
  } deriving (Generic)
