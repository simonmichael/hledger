{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RecordWildCards     #-}

{-|

A 'Journal' is a set of transactions, plus optional related data.  This is
hledger's primary data object. It is usually parsed from a journal file or
other data format (see "Hledger.Read").

-}

module Hledger.Data.Journal (
  -- * Parsing helpers
  JournalParser,
  ErroringJournalParser,
  addPriceDirective,
  addTransactionModifier,
  addPeriodicTransaction,
  addTransaction,
  journalDbg,
  journalInferMarketPricesFromTransactions,
  journalInferCommodityStyles,
  journalStyleAmounts,
  journalCommodityStyles,
  journalCommodityStylesWith,
  journalToCost,
  journalInferEquityFromCosts,
  journalTagCostsAndEquityAndMaybeInferCosts,
  journalReverse,
  journalSetLastReadTime,
  journalRenumberAccountDeclarations,
  journalPivot,
  -- * Filtering
  filterJournalTransactions,
  filterJournalPostings,
  filterJournalRelatedPostings,
  filterJournalAmounts,
  filterTransactionAmounts,
  filterTransactionPostings,
  filterTransactionPostingsExtra,
  filterTransactionRelatedPostings,
  filterPostingAmount,
  -- * Mapping
  journalMapTransactions,
  journalMapPostings,
  journalMapPostingAmounts,
  -- * Querying
  journalAccountNamesUsed,
  journalAccountNamesImplied,
  journalAccountNamesDeclared,
  journalAccountNamesDeclaredOrUsed,
  journalAccountNamesDeclaredOrImplied,
  journalAccountNames,
  journalLeafAccountNames,
  journalAccountNameTree,
  journalAccountTags,
  journalInheritedAccountTags,
  -- journalAmountAndPriceCommodities,
  -- journalAmountStyles,
  -- overJournalAmounts,
  -- traverseJournalAmounts,
  -- journalCanonicalCommodities,
  journalPayeesDeclared,
  journalPayeesUsed,
  journalPayeesDeclaredOrUsed,
  journalTagsDeclared,
  journalTagsUsed,
  journalTagsDeclaredOrUsed,
  journalAmounts,
  journalPostingAmounts,
  journalPostingAndCostAmounts,
  journalCommoditiesDeclared,
  journalCommoditiesUsed,
  journalCommodities,
  journalCommoditiesFromPriceDirectives,
  journalCommoditiesFromTransactions,
  journalDateSpan,
  journalDateSpanBothDates,
  journalStartDate,
  journalEndDate,
  journalLastDay,
  journalDescriptions,
  journalFilePath,
  journalFilePaths,
  journalTransactionAt,
  journalNextTransaction,
  journalPrevTransaction,
  journalPostings,
  showJournalPostingAmountsDebug,
  journalTransactionsSimilarTo,
  -- * Account types
  journalAccountType,
  journalAccountTypes,
  journalAddAccountTypes,
  journalPostingsAddAccountTags,
  defaultBaseConversionAccount,
  -- journalPrices,
  journalBaseConversionAccount,
  journalConversionAccounts,
  -- * Misc
  nulljournal,
  journalConcat,
  journalNumberTransactions,
  journalNumberAndTieTransactions,
  journalUntieTransactions,
  journalModifyTransactions,
  journalApplyAliases,
  dbgJournalAcctDeclOrder,
  -- * Tests
  samplejournal,
  samplejournalMaybeExplicit,
  tests_Journal
  --
)
where

import Control.Applicative ((<|>))
import Control.Monad.Except (ExceptT(..))
import Control.Monad.State.Strict (StateT)
import Data.Char (toUpper, isDigit)
import Data.Default (Default(..))
import Data.Foldable (toList)
import Data.List ((\\), find, sortBy, union, intercalate)
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.List.Extra (nubSort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Safe (headMay, headDef, maximumMay, minimumMay, lastDef)
import Data.Time.Calendar (Day, addDays, fromGregorian, diffDays)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Tree (Tree(..), flatten)
import Text.Printf (printf)
import Text.Megaparsec (ParsecT)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.AccountName
import Hledger.Data.Amount
import Hledger.Data.Posting
import Hledger.Data.Transaction
import Hledger.Data.TransactionModifier
import Hledger.Data.Valuation
import Hledger.Query
import System.FilePath (takeFileName)
import Data.Ord (comparing)
import Hledger.Data.Dates (nulldate)
import Data.List (sort)
import Data.Function ((&))
-- import Data.Function ((&))


-- | A parser of text that runs in some monad, keeping a Journal as state.
type JournalParser m a = StateT Journal (ParsecT HledgerParseErrorData Text m) a

-- | A parser of text that runs in some monad, keeping a Journal as
-- state, that can throw an exception to end parsing, preventing
-- further parser backtracking.
type ErroringJournalParser m a =
  StateT Journal (ParsecT HledgerParseErrorData Text (ExceptT FinalParseError m)) a

-- deriving instance Show Journal
instance Show Journal where
  show j
    | debugLevel < 3 = printf "Journal %s with %d transactions, %d accounts"
             (journalFilePath j)
             (length $ jtxns j)
             (length accounts)
    | debugLevel < 6 = printf "Journal %s with %d transactions, %d accounts: %s"
             (journalFilePath j)
             (length $ jtxns j)
             (length accounts)
             (show accounts)
    | otherwise = printf "Journal %s with %d transactions, %d accounts: %s, commodity styles: %s"
             (journalFilePath j)
             (length $ jtxns j)
             (length accounts)
             (show accounts)
             (show $ jinferredcommoditystyles j)
             -- ++ (show $ journalTransactions l)
             where accounts = filter (/= "root") $ flatten $ journalAccountNameTree j

journalDbg j@Journal{..} = chomp $ unlines $
  ("Journal " ++ takeFileName (journalFilePath j)++":") :  --  ++ " {"
  map (" "<>) [
   "jparsedefaultyear: "         <> shw jparsedefaultyear
  ,"jparsedefaultcommodity: "    <> shw jparsedefaultcommodity
  ,"jparsedecimalmark: "         <> shw jparsedecimalmark
  ,"jparseparentaccounts: "      <> shw jparseparentaccounts
  ,"jparsealiases: "             <> shw jparsealiases
  -- ,"jparsetimeclockentries: " <> shw jparsetimeclockentries
  ,"jincludefilestack: "         <> shw jincludefilestack
  ,"jdeclaredpayees: "           <> shw jdeclaredpayees
  ,"jdeclaredtags: "             <> shw jdeclaredtags
  ,"jdeclaredaccounts: "         <> shw jdeclaredaccounts
  ,"jdeclaredaccounttags: "      <> shw jdeclaredaccounttags
  ,"jdeclaredaccounttypes: "     <> shw jdeclaredaccounttypes
  ,"jaccounttypes: "             <> shw jaccounttypes
  ,"jdeclaredcommodities: "      <> shw jdeclaredcommodities
  ,"jinferredcommoditystyles: "  <> shw jinferredcommoditystyles
  ,"jglobalcommoditystyles: "    <> shw jglobalcommoditystyles
  ,"jpricedirectives: "          <> shw jpricedirectives
  ,"jinferredmarketprices: "     <> shw jinferredmarketprices
  ,"jtxnmodifiers: "             <> shw jtxnmodifiers
  -- ,"jperiodictxns: "          <> shw jperiodictxns
  ,"jtxns: "                     <> shw jtxns
  ,"jfinalcommentlines: "        <> shw jfinalcommentlines
  ,"jfiles: "                    <> shw jfiles
  ,"jlastreadtime: "             <> shw jlastreadtime
  ]
  -- ++ ["}"]
  where
    shw :: Show a => a -> String
    shw = show
    -- shw = pshow

-- The semigroup instance for Journal is useful for two situations.
--
-- 1. concatenating finalised journals, eg with multiple -f options:
-- FIRST <> SECOND.
--
-- 2. merging a child parsed journal, eg with the include directive:
-- CHILD <> PARENT. A parsed journal's data is in reverse order, so
-- this gives what we want.
--
-- Note that (<>) is right-biased, so nulljournal is only a left identity.
-- In particular, this prevents Journal from being a monoid.
instance Semigroup Journal where j1 <> j2 = j1 `journalConcat` j2

-- | Merge two journals into one.
-- Transaction counts are summed, map fields are combined,
-- the second's list fields are appended to the first's,
-- the second's parse state is kept.
journalConcat :: Journal -> Journal -> Journal
journalConcat j1 j2 =
  let
    f1 = takeFileName $ journalFilePath j1
    f2 = maybe "(unknown)" takeFileName $ headMay $ jincludefilestack j2  -- XXX more accurate than journalFilePath for some reason
  in
    dbgJournalAcctDeclOrder ("journalConcat: " <> f1 <> " <> " <> f2 <> ", acct decls renumbered: ") $
    journalRenumberAccountDeclarations $
    dbgJournalAcctDeclOrder ("journalConcat: " <> f1 <> " <> " <> f2 <> ", acct decls           : ") $
    Journal {
     jparsedefaultyear          = jparsedefaultyear          j2
    ,jparsedefaultcommodity     = jparsedefaultcommodity     j2
    ,jparsedecimalmark          = jparsedecimalmark          j2
    ,jparseparentaccounts       = jparseparentaccounts       j2
    ,jparsealiases              = jparsealiases              j2
    -- ,jparsetransactioncount     = jparsetransactioncount     j1 +  jparsetransactioncount     j2
    ,jparsetimeclockentries     = jparsetimeclockentries     j1 <> jparsetimeclockentries     j2
    ,jincludefilestack          = jincludefilestack j2
    ,jdeclaredpayees            = jdeclaredpayees            j1 <> jdeclaredpayees            j2
    ,jdeclaredtags              = jdeclaredtags              j1 <> jdeclaredtags              j2
    ,jdeclaredaccounts          = jdeclaredaccounts          j1 <> jdeclaredaccounts          j2
    --
    -- The next six fields are Maps, which need to be merged carefully for correct semantics,
    -- especially the first two, which have list values. There may still be room for improvement here.
    --
    -- ,jdeclaredaccounttags   :: M.Map AccountName [Tag]
    -- jdeclaredaccounttags can have multiple duplicated/conflicting values for an account's tag.
    ,jdeclaredaccounttags       = M.unionWith (<>) (jdeclaredaccounttags j1) (jdeclaredaccounttags j2)
    --
    -- ,jdeclaredaccounttypes  :: M.Map AccountType [AccountName]
    -- jdeclaredaccounttypes can have multiple duplicated/conflicting values for an account's type.
    ,jdeclaredaccounttypes      = M.unionWith (<>) (jdeclaredaccounttypes j1) (jdeclaredaccounttypes j2)
    --
    -- ,jaccounttypes          :: M.Map AccountName AccountType
    -- jaccounttypes has a single type for any given account. When it had multiple type declarations, the last/rightmost wins.
    ,jaccounttypes              = M.unionWith (const id) (jaccounttypes j1) (jaccounttypes j2)
    --
    -- ,jglobalcommoditystyles :: M.Map CommoditySymbol AmountStyle
    ,jglobalcommoditystyles     = (<>) (jglobalcommoditystyles j1) (jglobalcommoditystyles j2)
    --
    -- ,jdeclaredcommodities           :: M.Map CommoditySymbol Commodity
    ,jdeclaredcommodities               = (<>) (jdeclaredcommodities j1) (jdeclaredcommodities j2)
    --
    -- ,jinferredcommoditystyles   :: M.Map CommoditySymbol AmountStyle
    ,jinferredcommoditystyles       = (<>) (jinferredcommoditystyles j1) (jinferredcommoditystyles j2)
    --
    --
    ,jpricedirectives           = jpricedirectives           j1 <> jpricedirectives           j2
    ,jinferredmarketprices      = jinferredmarketprices      j1 <> jinferredmarketprices      j2
    ,jtxnmodifiers              = jtxnmodifiers              j1 <> jtxnmodifiers              j2
    ,jperiodictxns              = jperiodictxns              j1 <> jperiodictxns              j2
    ,jtxns                      = jtxns                      j1 <> jtxns                      j2
    ,jfinalcommentlines         = jfinalcommentlines j2  -- XXX discards j1's ?
    ,jfiles                     = jfiles                     j1 <> jfiles                     j2
    ,jlastreadtime              = max (jlastreadtime j1) (jlastreadtime j2)
    }

-- | Renumber all the account declarations. This is useful to call when
-- finalising or concatenating Journals, to give account declarations
-- a total order across files.
journalRenumberAccountDeclarations :: Journal -> Journal
journalRenumberAccountDeclarations j = j{jdeclaredaccounts=jdas'}
  where
    jdas' = [(a, adi{adideclarationorder=n}) | (n, (a,adi)) <- zip [1..] $ jdeclaredaccounts j]
    -- the per-file declaration order saved during parsing is discarded,
    -- it seems unneeded except perhaps for debugging

-- | Debug log the ordering of a journal's account declarations
-- (at debug level 7+).
dbgJournalAcctDeclOrder :: String -> Journal -> Journal
dbgJournalAcctDeclOrder prefix =
  dbg7With ((prefix++) . showAcctDeclsSummary . jdeclaredaccounts)
  where
    showAcctDeclsSummary :: [(AccountName,AccountDeclarationInfo)] -> String
    showAcctDeclsSummary adis
      | length adis < (2*n+2) = "[" <> showadis adis <> "]"
      | otherwise =
          "[" <> showadis (take n adis) <> " ... " <> showadis (takelast n adis) <> "]"
      where
        n = 3
        showadis = intercalate ", " . map showadi
        showadi (a,adi) = "("<>show (adideclarationorder adi)<>","<>T.unpack a<>")"
        takelast n' = reverse . take n' . reverse

instance Default Journal where
  def = nulljournal

nulljournal :: Journal
nulljournal = Journal {
   jparsedefaultyear          = Nothing
  ,jparsedefaultcommodity     = Nothing
  ,jparsedecimalmark          = Nothing
  ,jparseparentaccounts       = []
  ,jparsealiases              = []
  -- ,jparsetransactioncount     = 0
  ,jparsetimeclockentries     = []
  ,jincludefilestack          = []
  ,jdeclaredpayees            = []
  ,jdeclaredtags              = []
  ,jdeclaredaccounts          = []
  ,jdeclaredaccounttags       = M.empty
  ,jdeclaredaccounttypes      = M.empty
  ,jaccounttypes              = M.empty
  ,jglobalcommoditystyles     = M.empty
  ,jdeclaredcommodities               = M.empty
  ,jinferredcommoditystyles       = M.empty
  ,jpricedirectives           = []
  ,jinferredmarketprices      = []
  ,jtxnmodifiers              = []
  ,jperiodictxns              = []
  ,jtxns                      = []
  ,jfinalcommentlines         = ""
  ,jfiles                     = []
  ,jlastreadtime              = 0
  }

journalFilePath :: Journal -> FilePath
journalFilePath = fst . mainfile

journalFilePaths :: Journal -> [FilePath]
journalFilePaths = map fst . jfiles

mainfile :: Journal -> (FilePath, Text)
mainfile = headDef ("(unknown)", "") . jfiles

addTransaction :: Transaction -> Journal -> Journal
addTransaction t j = j { jtxns = t : jtxns j }

addTransactionModifier :: TransactionModifier -> Journal -> Journal
addTransactionModifier mt j = j { jtxnmodifiers = mt : jtxnmodifiers j }

addPeriodicTransaction :: PeriodicTransaction -> Journal -> Journal
addPeriodicTransaction pt j = j { jperiodictxns = pt : jperiodictxns j }

addPriceDirective :: PriceDirective -> Journal -> Journal
addPriceDirective h j = j { jpricedirectives = h : jpricedirectives j }  -- XXX #999 keep sorted

-- | Get the transaction with this index (its 1-based position in the input stream), if any.
journalTransactionAt :: Journal -> Integer -> Maybe Transaction
journalTransactionAt Journal{jtxns=ts} i =
  -- it's probably ts !! (i+1), but we won't assume
  headMay [t | t <- ts, tindex t == i]

-- | Get the transaction that appeared immediately after this one in the input stream, if any.
journalNextTransaction :: Journal -> Transaction -> Maybe Transaction
journalNextTransaction j t = journalTransactionAt j (tindex t + 1)

-- | Get the transaction that appeared immediately before this one in the input stream, if any.
journalPrevTransaction :: Journal -> Transaction -> Maybe Transaction
journalPrevTransaction j t = journalTransactionAt j (tindex t - 1)

-- | All postings from this journal's transactions, in order.
journalPostings :: Journal -> [Posting]
journalPostings = concatMap tpostings . jtxns

-- | All posting amounts from this journal, in order.
journalPostingAmounts :: Journal -> [MixedAmount]
journalPostingAmounts = map pamount . journalPostings

-- | Show the journal posting amounts rendered, suitable for debug logging.
showJournalPostingAmountsDebug :: Journal -> String
showJournalPostingAmountsDebug = show . map showMixedAmountOneLine . journalPostingAmounts

-- | All raw amounts used in this journal's postings and costs,
-- with MixedAmounts flattened, in parse order.
journalPostingAndCostAmounts :: Journal -> [Amount]
journalPostingAndCostAmounts = concatMap getAmounts . concatMap (amountsRaw . pamount) . journalPostings

-- | All raw amounts appearing in this journal, with MixedAmounts flattened, in no particular order.
-- (Including from posting amounts, cost amounts, P directives, and the last D directive.)
journalAmounts :: Journal -> S.Set Amount
journalAmounts = S.fromList . journalStyleInfluencingAmounts True

-- | Sorted unique commodity symbols declared by commodity directives in this journal.
journalCommoditiesDeclared :: Journal -> [CommoditySymbol]
journalCommoditiesDeclared = M.keys . jdeclaredcommodities

-- | Sorted unique commodity symbols used anywhere in this journal, including
-- commodity directives, P directives, the last D directive, posting amounts and cost amounts.
journalCommoditiesUsed :: Journal -> [CommoditySymbol]
journalCommoditiesUsed j = S.elems $
  journalCommoditiesFromPriceDirectives j <>
  (S.fromList $ map acommodity $ journalStyleInfluencingAmounts True j)

-- | Sorted unique commodity symbols mentioned anywhere in this journal.
-- (Including commodity directives, P directives, the last D directive, posting amounts and cost amounts.)
journalCommodities :: Journal -> S.Set CommoditySymbol
journalCommodities j =
     M.keysSet (jdeclaredcommodities j)
  <> journalCommoditiesFromPriceDirectives j
  <> S.fromList (map acommodity $ journalStyleInfluencingAmounts True j)

-- | Sorted unique commodity symbols mentioned in this journal's P directives.
journalCommoditiesFromPriceDirectives :: Journal -> S.Set CommoditySymbol
journalCommoditiesFromPriceDirectives = S.fromList . concatMap pdcomms . jpricedirectives
  where pdcomms pd = [pdcommodity pd, acommodity $ pdamount pd]

-- | Sorted unique commodity symbols used in transactions, in either posting or cost amounts.
journalCommoditiesFromTransactions :: Journal -> S.Set CommoditySymbol
journalCommoditiesFromTransactions j = S.fromList $ map acommodity $ journalPostingAndCostAmounts j

-- | Unique transaction descriptions used in this journal.
journalDescriptions :: Journal -> [Text]
journalDescriptions = nubSort . map tdescription . jtxns

-- | Sorted unique payees declared by payee directives in this journal.
journalPayeesDeclared :: Journal -> [Payee]
journalPayeesDeclared = nubSort . map fst . jdeclaredpayees

-- | Sorted unique payees used by transactions in this journal.
journalPayeesUsed :: Journal -> [Payee]
journalPayeesUsed = nubSort . map transactionPayee . jtxns

-- | Sorted unique payees used in transactions or declared by payee directives in this journal.
journalPayeesDeclaredOrUsed :: Journal -> [Payee]
journalPayeesDeclaredOrUsed j = toList $ foldMap S.fromList
    [journalPayeesDeclared j, journalPayeesUsed j]

-- | Sorted unique tag names declared by tag directives in this journal.
journalTagsDeclared :: Journal -> [TagName]
journalTagsDeclared = nubSort . map fst . jdeclaredtags

-- | Sorted unique tag names used in this journal (in account directives, transactions, postings..)
journalTagsUsed :: Journal -> [TagName]
journalTagsUsed j = nubSort $ map fst $ concatMap transactionAllTags $ jtxns j
  -- tags used in all transactions and postings and postings' accounts

-- | Sorted unique tag names used in transactions or declared by tag directives in this journal.
journalTagsDeclaredOrUsed :: Journal -> [TagName]
journalTagsDeclaredOrUsed j = toList $ foldMap S.fromList
    [journalTagsDeclared j, journalTagsUsed j]

-- | Sorted unique account names posted to by this journal's transactions.
journalAccountNamesUsed :: Journal -> [AccountName]
journalAccountNamesUsed = accountNamesFromPostings . journalPostings

-- | Sorted unique account names implied by this journal's transactions -
-- accounts posted to and all their implied parent accounts.
journalAccountNamesImplied :: Journal -> [AccountName]
journalAccountNamesImplied = expandAccountNames . journalAccountNamesUsed

-- | Sorted unique account names declared by account directives in this journal.
journalAccountNamesDeclared :: Journal -> [AccountName]
journalAccountNamesDeclared = nubSort . map fst . jdeclaredaccounts

-- | Sorted unique account names declared by account directives or posted to
-- by transactions in this journal.
journalAccountNamesDeclaredOrUsed :: Journal -> [AccountName]
journalAccountNamesDeclaredOrUsed j = toList $ foldMap S.fromList
    [journalAccountNamesDeclared j, journalAccountNamesUsed j]

-- | Sorted unique account names declared by account directives, or posted to
-- or implied as parents by transactions in this journal.
journalAccountNamesDeclaredOrImplied :: Journal -> [AccountName]
journalAccountNamesDeclaredOrImplied j = toList $ foldMap S.fromList
    [journalAccountNamesDeclared j, expandAccountNames $ journalAccountNamesUsed j]

-- | Convenience/compatibility alias for journalAccountNamesDeclaredOrImplied.
journalAccountNames :: Journal -> [AccountName]
journalAccountNames = journalAccountNamesDeclaredOrImplied

-- | Sorted unique account names declared or implied in this journal
-- which have no children.
journalLeafAccountNames :: Journal -> [AccountName]
journalLeafAccountNames = treeLeaves . journalAccountNameTree

journalAccountNameTree :: Journal -> Tree AccountName
journalAccountNameTree = accountNameTreeFrom . journalAccountNamesDeclaredOrImplied

-- | Which tags have been declared explicitly for this account, if any ?
journalAccountTags :: Journal -> AccountName -> [Tag]
journalAccountTags Journal{jdeclaredaccounttags} a = M.findWithDefault [] a jdeclaredaccounttags

-- | Which tags are in effect for this account, including tags inherited from parent accounts ?
journalInheritedAccountTags :: Journal -> AccountName -> [Tag]
journalInheritedAccountTags j a =
  foldl' (\ts a' -> ts `union` journalAccountTags j a') [] as
  where
    as = a : parentAccountNames a
-- PERF: cache in journal ?

type DateWeightedSimilarityScore = Double
type SimilarityScore = Double
type Age = Integer

-- | Find up to N most similar and most recent transactions matching
-- the given transaction description and query and exceeding the given
-- description similarity score (0 to 1, see compareDescriptions).
-- Returns transactions along with
-- their age in days compared to the latest transaction date,
-- their description similarity score,
-- and a heuristically date-weighted variant of this that favours more recent transactions.
journalTransactionsSimilarTo :: Journal -> Text -> Query -> SimilarityScore -> Int
  -> [(DateWeightedSimilarityScore, Age, SimilarityScore, Transaction)]
journalTransactionsSimilarTo Journal{jtxns} desc q similaritythreshold n =
  take n $
  dbg1With (
    unlines . 
    ("up to 30 transactions above description similarity threshold "<>show similaritythreshold<>" ordered by recency-weighted similarity:":) .
    take 30 .
    map ( \(w,a,s,Transaction{..}) -> printf "weighted:%8.3f  age:%4d similarity:%5.3f  %s %s" w a s (show tdate) tdescription )) $
  sortBy (comparing (negate.first4)) $
  map (\(s,t) -> (weightedScore (s,t), age t, s, t)) $
  filter ((> similaritythreshold).fst)
  [(compareDescriptions desc $ tdescription t, t) | t <- jtxns, q `matchesTransaction` t]
  where
    latest = lastDef nulldate $ sort $ map tdate jtxns
    age = diffDays latest . tdate
    -- Combine similarity and recency heuristically. This gave decent results
    -- in my "find most recent invoice" use case in 2023-03,
    -- but will probably need more attention.
    weightedScore :: (Double, Transaction) -> Double
    weightedScore (s, t) = 100 * s - fromIntegral (age t) / 4

-- | Return a similarity score from 0 to 1.5 for two transaction descriptions. 
-- This is based on compareStrings, with the following modifications:
--
-- - numbers are stripped out before measuring similarity
--
-- - if the (unstripped) first description appears in its entirety within the second,
--   the score is boosted by 0.5.
--
compareDescriptions :: Text -> Text -> Double
compareDescriptions a b =
  (if a `T.isInfixOf` b then (0.5+) else id) $
  compareStrings (simplify a) (simplify b)
  where
    simplify = T.unpack . T.filter (not.isDigit)

-- | Return a similarity score from 0 to 1 for two strings.  This
-- was based on Simon White's string similarity algorithm
-- (http://www.catalysoft.com/articles/StrikeAMatch.html), later found
-- to be https://en.wikipedia.org/wiki/S%C3%B8rensen%E2%80%93Dice_coefficient,
-- and modified to handle short strings better.
-- Todo: check out http://nlp.fi.muni.cz/raslan/2008/raslan08.pdf#page=14 .
compareStrings :: String -> String -> Double
compareStrings "" "" = 1
compareStrings [_] "" = 0
compareStrings "" [_] = 0
compareStrings [a] [b] = if toUpper a == toUpper b then 1 else 0
compareStrings s1 s2 = 2 * commonpairs / totalpairs
  where
    pairs1      = S.fromList $ wordLetterPairs $ uppercase s1
    pairs2      = S.fromList $ wordLetterPairs $ uppercase s2
    commonpairs = fromIntegral $ S.size $ S.intersection pairs1 pairs2
    totalpairs  = fromIntegral $ S.size pairs1 + S.size pairs2

wordLetterPairs :: String -> [String]
wordLetterPairs = concatMap letterPairs . words

letterPairs :: String -> [String]
letterPairs (a:b:rest) = [a,b] : letterPairs (b:rest)
letterPairs _ = []

-- Newer account type code.

journalAccountType :: Journal -> AccountName -> Maybe AccountType
journalAccountType Journal{jaccounttypes} = accountNameType jaccounttypes

-- | Add a map of all known account types to the journal.
journalAddAccountTypes :: Journal -> Journal
journalAddAccountTypes j = j{jaccounttypes = journalAccountTypes j}

-- | An account type inherited from the parent account(s),
-- and whether it was originally declared by an account directive (true) or inferred from an account name (false).
type ParentAccountType = (AccountType, Bool)

-- | Build a map of all known account types, explicitly declared
-- or inferred from the account's parent or name.
journalAccountTypes :: Journal -> M.Map AccountName AccountType
journalAccountTypes j = M.fromList [(a,acctType) | (a, Just (acctType,_)) <- flatten t']
  where
    t = accountNameTreeFrom $ journalAccountNames j :: Tree AccountName
    -- Traverse downward through the account tree, applying any explicitly declared account types,
    -- otherwise inferring account types from account names when possible, and propagating account types downward.
    -- Declared account types (possibly inherited from parent) are preferred, inferred types are used as a fallback.
    t' = setTypeHereAndBelow Nothing t :: Tree (AccountName, Maybe (AccountType, Bool))
      where
        declaredtypes       = M.keys $ jdeclaredaccounttypes j
        declaredtypesbyname = journalDeclaredAccountTypes j & fmap (,True)
        setTypeHereAndBelow :: Maybe ParentAccountType -> Tree AccountName -> Tree (AccountName, Maybe ParentAccountType)
        setTypeHereAndBelow mparenttype (Node a subs) = Node (a, mnewtype) (map (setTypeHereAndBelow mnewtype) subs)
          where
            mnewtype = mthisacctdeclaredtype <|> mparentacctdeclaredtype <|> mthisacctinferredtype <|> mparentacctinferredtype
              where
                mthisacctdeclaredtype   = M.lookup a declaredtypesbyname
                mparentacctdeclaredtype = if       fromMaybe False $ snd <$> mparenttype then mparenttype else Nothing
                mparentacctinferredtype = if not $ fromMaybe True  $ snd <$> mparenttype then mparenttype else Nothing
                mthisacctinferredtype   = accountNameInferTypeExcept declaredtypes a & fmap (,False)  -- XXX not sure about this Except logic.. but for now, tests pass

-- | Build a map from account names to explicitly declared account types.
journalDeclaredAccountTypes :: Journal -> M.Map AccountName AccountType
journalDeclaredAccountTypes Journal{jdeclaredaccounttypes} =
  M.fromList $ concat [map (,t) as | (t,as) <- M.toList jdeclaredaccounttypes]

-- | To all postings in the journal, add any tags from their account
-- (including those inherited from parent accounts).
-- If the same tag exists on posting and account, the latter is ignored.
journalPostingsAddAccountTags :: Journal -> Journal
journalPostingsAddAccountTags j = journalMapPostings addtags j
  where addtags p = p `postingAddTags` (journalInheritedAccountTags j $ paccount p)

-- | The account name to use for conversion postings generated by --infer-equity.
-- This is the first account declared with type V/Conversion,
-- or otherwise the defaultBaseConversionAccount (equity:conversion).
journalBaseConversionAccount :: Journal -> AccountName
journalBaseConversionAccount = headDef defaultBaseConversionAccount . journalConversionAccounts

-- | All the accounts in this journal which are declared or inferred as V/Conversion type.
-- This does not include new account names which might be generated by --infer-equity, currently.
journalConversionAccounts :: Journal -> [AccountName]
journalConversionAccounts = M.keys . M.filter (==Conversion) . jaccounttypes


-- Various kinds of filtering on journals. We do it differently depending
-- on the command.

-------------------------------------------------------------------------------
-- filtering V2

-- | Keep only transactions matching the query expression.
filterJournalTransactions :: Query -> Journal -> Journal
filterJournalTransactions q j@Journal{jtxns} = j{jtxns=filter (matchesTransactionExtra (journalAccountType j) q) jtxns}

-- | Keep only postings matching the query expression.
-- This can leave unbalanced transactions.
filterJournalPostings :: Query -> Journal -> Journal
filterJournalPostings q j@Journal{jtxns=ts} = j{jtxns=map (filterTransactionPostingsExtra (journalAccountType j) q) ts}

-- | Keep only postings which do not match the query expression, but for which a related posting does.
-- This can leave unbalanced transactions.
filterJournalRelatedPostings :: Query -> Journal -> Journal
filterJournalRelatedPostings q j@Journal{jtxns=ts} = j{jtxns=map (filterTransactionRelatedPostings q) ts}

-- | Within each posting's amount, keep only the parts matching the query, and
-- remove any postings with all amounts removed.
-- This can leave unbalanced transactions.
filterJournalAmounts :: Query -> Journal -> Journal
filterJournalAmounts q j@Journal{jtxns=ts} = j{jtxns=map (filterTransactionAmounts q) ts}

-- | Filter out all parts of this transaction's amounts which do not match the
-- query, and remove any postings with all amounts removed.
-- This can leave the transaction unbalanced.
filterTransactionAmounts :: Query -> Transaction -> Transaction
filterTransactionAmounts q t@Transaction{tpostings=ps} = t{tpostings=mapMaybe (filterPostingAmount q) ps}

-- | Filter out all parts of this posting's amount which do not match the query, and remove the posting
-- if this removes all amounts.
filterPostingAmount :: Query -> Posting -> Maybe Posting
filterPostingAmount q p@Posting{pamount=as}
  | null newamt = Nothing
  | otherwise   = Just p{pamount=Mixed newamt}
  where
    Mixed newamt = filterMixedAmount (q `matchesAmount`) as

filterTransactionPostings :: Query -> Transaction -> Transaction
filterTransactionPostings q t@Transaction{tpostings=ps} = t{tpostings=filter (q `matchesPosting`) ps}

-- Like filterTransactionPostings, but is given the map of account types so can also filter by account type.
filterTransactionPostingsExtra :: (AccountName -> Maybe AccountType) -> Query -> Transaction -> Transaction
filterTransactionPostingsExtra atypes q t@Transaction{tpostings=ps} =
  t{tpostings=filter (matchesPostingExtra atypes q) ps}

filterTransactionRelatedPostings :: Query -> Transaction -> Transaction
filterTransactionRelatedPostings q t@Transaction{tpostings=ps} =
    t{tpostings=if null matches then [] else ps \\ matches}
  where matches = filter (matchesPosting q) ps

-- | Apply a transformation to a journal's transactions.
journalMapTransactions :: (Transaction -> Transaction) -> Journal -> Journal
journalMapTransactions f j@Journal{jtxns=ts} = j{jtxns=map f ts}

-- | Apply a transformation to a journal's postings.
journalMapPostings :: (Posting -> Posting) -> Journal -> Journal
journalMapPostings f j@Journal{jtxns=ts} = j{jtxns=map (transactionMapPostings f) ts}

-- | Apply a transformation to a journal's posting amounts.
journalMapPostingAmounts :: (MixedAmount -> MixedAmount) -> Journal -> Journal
journalMapPostingAmounts f = journalMapPostings (postingTransformAmount f)

{-
-------------------------------------------------------------------------------
-- filtering V1

-- | Keep only transactions we are interested in, as described by the
-- filter specification.
filterJournalTransactions :: FilterSpec -> Journal -> Journal
filterJournalTransactions FilterSpec{datespan=datespan
                                    ,cleared=cleared
                                    -- ,real=real
                                    -- ,empty=empty
                                    ,acctpats=apats
                                    ,descpats=dpats
                                    ,depth=depth
                                    ,fMetadata=md
                                    } =
    filterJournalTransactionsByStatus cleared .
    filterJournalPostingsByDepth depth .
    filterJournalTransactionsByAccount apats .
    filterJournalTransactionsByMetadata md .
    filterJournalTransactionsByDescription dpats .
    filterJournalTransactionsByDate datespan

-- | Keep only postings we are interested in, as described by the filter
-- specification. This can leave unbalanced transactions.
filterJournalPostings :: FilterSpec -> Journal -> Journal
filterJournalPostings FilterSpec{datespan=datespan
                                ,cleared=cleared
                                ,real=real
                                ,empty=empty
                                ,acctpats=apats
                                ,descpats=dpats
                                ,depth=depth
                                ,fMetadata=md
                                } =
    filterJournalPostingsByRealness real .
    filterJournalPostingsByStatus cleared .
    filterJournalPostingsByEmpty empty .
    filterJournalPostingsByDepth depth .
    filterJournalPostingsByAccount apats .
    filterJournalTransactionsByMetadata md .
    filterJournalTransactionsByDescription dpats .
    filterJournalTransactionsByDate datespan

-- | Keep only transactions whose metadata matches all metadata specifications.
filterJournalTransactionsByMetadata :: [(String,String)] -> Journal -> Journal
filterJournalTransactionsByMetadata pats j@Journal{jtxns=ts} = j{jtxns=filter matchmd ts}
    where matchmd t = all (`elem` tmetadata t) pats

-- | Keep only transactions whose description matches the description patterns.
filterJournalTransactionsByDescription :: [String] -> Journal -> Journal
filterJournalTransactionsByDescription pats j@Journal{jtxns=ts} = j{jtxns=filter matchdesc ts}
    where matchdesc = matchpats pats . tdescription

-- | Keep only transactions which fall between begin and end dates.
-- We include transactions on the begin date and exclude transactions on the end
-- date, like ledger.  An empty date string means no restriction.
filterJournalTransactionsByDate :: DateSpan -> Journal -> Journal
filterJournalTransactionsByDate (DateSpan begin end) j@Journal{jtxns=ts} = j{jtxns=filter match ts}
    where match t = maybe True (tdate t>=) begin && maybe True (tdate t<) end

-- | Keep only transactions which have the requested cleared/uncleared
-- status, if there is one.
filterJournalTransactionsByStatus :: Maybe Bool -> Journal -> Journal
filterJournalTransactionsByStatus Nothing j = j
filterJournalTransactionsByStatus (Just val) j@Journal{jtxns=ts} = j{jtxns=filter match ts}
    where match = (==val).tstatus

-- | Keep only postings which have the requested cleared/uncleared status,
-- if there is one.
filterJournalPostingsByStatus :: Maybe Bool -> Journal -> Journal
filterJournalPostingsByStatus Nothing j = j
filterJournalPostingsByStatus (Just c) j@Journal{jtxns=ts} = j{jtxns=map filterpostings ts}
    where filterpostings t@Transaction{tpostings=ps} = t{tpostings=filter ((==c) . postingCleared) ps}

-- | Strip out any virtual postings, if the flag is true, otherwise do
-- no filtering.
filterJournalPostingsByRealness :: Bool -> Journal -> Journal
filterJournalPostingsByRealness False j = j
filterJournalPostingsByRealness True j@Journal{jtxns=ts} = j{jtxns=map filterpostings ts}
    where filterpostings t@Transaction{tpostings=ps} = t{tpostings=filter isReal ps}

-- | Strip out any postings with zero amount, unless the flag is true.
filterJournalPostingsByEmpty :: Bool -> Journal -> Journal
filterJournalPostingsByEmpty True j = j
filterJournalPostingsByEmpty False j@Journal{jtxns=ts} = j{jtxns=map filterpostings ts}
    where filterpostings t@Transaction{tpostings=ps} = t{tpostings=filter (not . isEmptyPosting) ps}

-- -- | Keep only transactions which affect accounts deeper than the specified depth.
-- filterJournalTransactionsByDepth :: Maybe Int -> Journal -> Journal
-- filterJournalTransactionsByDepth Nothing j = j
-- filterJournalTransactionsByDepth (Just d) j@Journal{jtxns=ts} =
--     j{jtxns=(filter (any ((<= d+1) . accountNameLevel . paccount) . tpostings) ts)}

-- | Strip out any postings to accounts deeper than the specified depth
-- (and any transactions which have no postings as a result).
filterJournalPostingsByDepth :: Maybe Int -> Journal -> Journal
filterJournalPostingsByDepth Nothing j = j
filterJournalPostingsByDepth (Just d) j@Journal{jtxns=ts} =
    j{jtxns=filter (not . null . tpostings) $ map filtertxns ts}
    where filtertxns t@Transaction{tpostings=ps} =
              t{tpostings=filter ((<= d) . accountNameLevel . paccount) ps}

-- | Keep only postings which affect accounts matched by the account patterns.
-- This can leave transactions unbalanced.
filterJournalPostingsByAccount :: [String] -> Journal -> Journal
filterJournalPostingsByAccount apats j@Journal{jtxns=ts} = j{jtxns=map filterpostings ts}
    where filterpostings t@Transaction{tpostings=ps} = t{tpostings=filter (matchpats apats . paccount) ps}

-- | Keep only transactions which affect accounts matched by the account patterns.
-- More precisely: each positive account pattern excludes transactions
-- which do not contain a posting to a matched account, and each negative
-- account pattern excludes transactions containing a posting to a matched
-- account.
filterJournalTransactionsByAccount :: [String] -> Journal -> Journal
filterJournalTransactionsByAccount apats j@Journal{jtxns=ts} = j{jtxns=filter tmatch ts}
    where
      tmatch t = (null positives || any positivepmatch ps) && (null negatives || not (any negativepmatch ps)) where ps = tpostings t
      positivepmatch p = any (`amatch` a) positives where a = paccount p
      negativepmatch p = any (`amatch` a) negatives where a = paccount p
      amatch pat a = regexMatchesCI (abspat pat) a
      (negatives,positives) = partition isnegativepat apats

-}

-- | Reverse all lists of parsed items, which during parsing were
-- prepended to, so that the items are in parse order. Part of
-- post-parse finalisation.
journalReverse :: Journal -> Journal
journalReverse j =
  j {jfiles            = reverse $ jfiles j
    ,jdeclaredaccounts = reverse $ jdeclaredaccounts j
    ,jtxns             = reverse $ jtxns j
    ,jtxnmodifiers     = reverse $ jtxnmodifiers j
    ,jperiodictxns     = reverse $ jperiodictxns j
    ,jpricedirectives  = reverse $ jpricedirectives j
    }

-- | Set this journal's last read time, ie when its files were last read.
journalSetLastReadTime :: POSIXTime -> Journal -> Journal
journalSetLastReadTime t j = j{ jlastreadtime = t }


journalNumberAndTieTransactions = journalTieTransactions . journalNumberTransactions

-- | Number (set the tindex field) this journal's transactions, counting upward from 1.
journalNumberTransactions :: Journal -> Journal
journalNumberTransactions j@Journal{jtxns=ts} = j{jtxns=zipWith (\i t -> t{tindex=i}) [1..] ts}

-- | Tie the knot in all of this journal's transactions, ensuring their postings
-- refer to them. This should be done last, after any other transaction-modifying operations.
journalTieTransactions :: Journal -> Journal
journalTieTransactions j@Journal{jtxns=ts} = j{jtxns=map txnTieKnot ts}

-- | Untie all transaction-posting knots in this journal, so that eg
-- recursiveSize and GHCI's :sprint can work on it.
journalUntieTransactions :: Transaction -> Transaction
journalUntieTransactions t@Transaction{tpostings=ps} = t{tpostings=map (\p -> p{ptransaction=Nothing}) ps}

-- | Apply any transaction modifier rules in the journal (adding automated
-- postings to transactions, eg). Or if a modifier rule fails to parse,
-- return the error message. A reference date is provided to help interpret
-- relative dates in transaction modifier queries.
-- The first argument selects whether to add visible tags to generated postings & modified transactions.
journalModifyTransactions :: Bool -> Day -> Journal -> Either String Journal
journalModifyTransactions verbosetags d j =
  case modifyTransactions (journalAccountType j) (journalInheritedAccountTags j) (journalCommodityStyles j) d verbosetags (jtxnmodifiers j) (jtxns j) of
    Right ts -> Right j{jtxns=ts}
    Left err -> Left err

-- | Apply this journal's commodity display styles to all of its amounts.
-- This does no display rounding, keeping decimal digits as they were;
-- it is suitable for an early cleanup pass before calculations.
-- Reports may want to do additional rounding/styling at render time.
-- This can return an error message eg if inconsistent number formats are found.
journalStyleAmounts :: Journal -> Either String Journal
journalStyleAmounts = fmap journalapplystyles . journalInferCommodityStyles
  where
    journalapplystyles j@Journal{jpricedirectives=pds} =
      journalMapPostings (styleAmounts styles) j{jpricedirectives=map fixpricedirective pds}
      where
        styles = journalCommodityStylesWith NoRounding j  -- defer rounding, in case of print --round=none
        fixpricedirective pd@PriceDirective{pdamount=a} = pd{pdamount=styleAmounts styles a}

-- | Get the canonical amount styles for this journal, whether (in order of precedence):
-- set globally in InputOpts,
-- declared by commodity directives, 
-- declared by a default commodity (D) directive, 
-- or inferred from posting amounts, 
-- as a map from symbol to style. 
-- Styles from directives are assumed to specify the decimal mark.
journalCommodityStyles :: Journal -> M.Map CommoditySymbol AmountStyle
journalCommodityStyles j =
  -- XXX could be some redundancy here, cf journalStyleInfluencingAmounts
  globalstyles <> declaredstyles <> defaultcommoditystyle <> inferredstyles
  where
    globalstyles          = jglobalcommoditystyles j
    declaredstyles        = M.mapMaybe cformat $ jdeclaredcommodities j
    defaultcommoditystyle = M.fromList $ catMaybes [jparsedefaultcommodity j]
    inferredstyles        = jinferredcommoditystyles j

-- | Like journalCommodityStyles, but attach a particular rounding strategy to the styles,
-- affecting how they will affect display precisions when applied.
journalCommodityStylesWith :: Rounding -> Journal -> M.Map CommoditySymbol AmountStyle
journalCommodityStylesWith r = amountStylesSetRounding r . journalCommodityStyles

-- | Collect and save inferred amount styles for each commodity based on
-- P directive amounts, posting amounts but not cost amounts, and maybe the last D amount, in that commodity.
-- Can return an error message eg if inconsistent number formats are found.
journalInferCommodityStyles :: Journal -> Either String Journal
journalInferCommodityStyles j =
  case commodityStylesFromAmounts $ journalStyleInfluencingAmounts False j of
    Left e   -> Left e
    Right cs -> Right j{jinferredcommoditystyles = dbg7 "journalInferCommodityStyles" cs}

-- -- | Apply this journal's historical price records to unpriced amounts where possible.
-- journalApplyPriceDirectives :: Journal -> Journal
-- journalApplyPriceDirectives j@Journal{jtxns=ts} = j{jtxns=map fixtransaction ts}
--     where
--       fixtransaction t@Transaction{tdate=d, tpostings=ps} = t{tpostings=map fixposting ps}
--        where
--         fixposting p@Posting{pamount=a} = p{pamount=fixmixedamount a}
--         fixmixedamount = mapMixedAmount fixamount
--         fixamount = fixprice
--         fixprice a@Amount{price=Just _} = a
--         fixprice a@Amount{commodity=c} = a{price=maybe Nothing (Just . UnitCost) $ journalPriceDirectiveFor j d c}

-- -- | Get the price for a commodity on the specified day from the price database, if known.
-- -- Does only one lookup step, ie will not look up the price of a price.
-- journalPriceDirectiveFor :: Journal -> Day -> CommoditySymbol -> Maybe MixedAmount
-- journalPriceDirectiveFor j d CommoditySymbol{symbol=s} = do
--   let ps = reverse $ filter ((<= d).pddate) $ filter ((s==).hsymbol) $ sortBy (comparing pddate) $ jpricedirectives j
--   case ps of (PriceDirective{pdamount=a}:_) -> Just a
--              _ -> Nothing

-- | Infer transaction-implied market prices from commodity-exchanging
-- transactions, if any. It's best to call this after transactions have
-- been balanced and posting amounts have appropriate prices attached.
journalInferMarketPricesFromTransactions :: Journal -> Journal
journalInferMarketPricesFromTransactions j =
  j{jinferredmarketprices =
       dbg4With (("jinferredmarketprices:\n"<>) . showMarketPrices) $
       map priceDirectiveToMarketPrice .
       concatMap postingPriceDirectivesFromCost $
       journalPostings j
   }

-- | Convert all this journal's amounts to cost using their attached prices, if any.
journalToCost :: ConversionOp -> Journal -> Journal
journalToCost cost j@Journal{jtxns=ts} = j{jtxns=map (transactionToCost cost) ts}

-- | Identify and tag (1) equity conversion postings and (2) postings which have (or could have ?) redundant costs.
-- And if the addcosts flag is true, also add any costs which can be inferred from equity conversion postings.
-- This is always called before transaction balancing to tag the redundant-cost postings so they can be ignored.
-- With --infer-costs, it is called again after transaction balancing (when it has more information to work with) to infer costs from equity postings.
-- See transactionTagCostsAndEquityAndMaybeInferCosts for more details, and hledger manual > Cost reporting for more background.
journalTagCostsAndEquityAndMaybeInferCosts :: Bool -> Bool -> Journal -> Either String Journal
journalTagCostsAndEquityAndMaybeInferCosts verbosetags addcosts j = do
  let conversionaccts = journalConversionAccounts j
  ts <- mapM (transactionTagCostsAndEquityAndMaybeInferCosts verbosetags addcosts conversionaccts) $ jtxns j
  return j{jtxns=ts}

-- | Add equity postings inferred from costs, where needed and possible.
-- See hledger manual > Cost reporting.
journalInferEquityFromCosts :: Bool -> Journal -> Journal
journalInferEquityFromCosts verbosetags j =
  journalMapTransactions (transactionInferEquityPostings verbosetags equityAcct) j
  where equityAcct = journalBaseConversionAccount j

-- -- | Get this journal's unique, display-preference-canonicalised commodities, by symbol.
-- journalCanonicalCommodities :: Journal -> M.Map String CommoditySymbol
-- journalCanonicalCommodities j = canonicaliseCommodities $ journalAmountCommodities j

-- -- | Get all this journal's amounts' commodities, in the order parsed.
-- journalAmountCommodities :: Journal -> [CommoditySymbol]
-- journalAmountCommodities = map acommodity . concatMap amounts . journalAmounts

-- -- | Get all this journal's amount and price commodities, in the order parsed.
-- journalAmountAndPriceCommodities :: Journal -> [CommoditySymbol]
-- journalAmountAndPriceCommodities = concatMap amountCommodities . concatMap amounts . journalAmounts

-- -- | Get this amount's commodity and any commodities referenced in its price.
-- amountCommodities :: Amount -> [CommoditySymbol]
-- amountCommodities Amount{acommodity=c,acost=p} =
--     case p of Nothing -> [c]
--               Just (UnitCost ma)  -> c:(concatMap amountCommodities $ amounts ma)
--               Just (TotalCost ma) -> c:(concatMap amountCommodities $ amounts ma)

-- | Get an ordered list of amounts in this journal which can influence
-- canonical amount display styles (excluding the ones in commodity directives). 
-- They are, in the following order:
--
-- * amounts in market price (P) directives (in parse order)
-- * posting amounts and optionally cost amounts (in parse order)
-- * the amount in the final default commodity (D) directive
--
journalStyleInfluencingAmounts :: Bool -> Journal -> [Amount]
journalStyleInfluencingAmounts includecost j =
  dbg7 "journalStyleInfluencingAmounts" $
  catMaybes $ concat [
   [mdefaultcommodityamt]
  ,map (Just . pdamount) $ jpricedirectives j
  ,map Just $ if includecost
    then journalPostingAndCostAmounts j
    else concatMap amountsRaw $ journalPostingAmounts j
  ]
  where
    -- D's amount style isn't actually stored as an amount, make it into one
    mdefaultcommodityamt =
      case jparsedefaultcommodity j of
        Just (symbol,style) -> Just nullamt{acommodity=symbol,astyle=style}
        Nothing -> Nothing

-- overcomplicated/unused amount traversal stuff
--
--  Get an ordered list of 'AmountStyle's from the amounts in this
-- journal which influence canonical amount display styles. See
-- traverseJournalAmounts.
-- journalAmounts :: Journal -> [Amount]
-- journalAmounts = getConst . traverseJournalAmounts (Const . (:[]))
--
--  Apply a transformation to the journal amounts traversed by traverseJournalAmounts.
-- overJournalAmounts :: (Amount -> Amount) -> Journal -> Journal
-- overJournalAmounts f = runIdentity . traverseJournalAmounts (Identity . f)
--
--  A helper that traverses over most amounts in the journal,
-- in particular the ones which influence canonical amount display styles,
-- processing them with the given applicative function.
--
-- These include, in the following order:
--
-- * the amount in the final default commodity (D) directive
-- * amounts in market price (P) directives (in parse order)
-- * posting amounts in transactions (in parse order)
--
-- Transaction price amounts, which may be embedded in posting amounts
-- (the acost field), are left intact but not traversed/processed.
--
-- traverseJournalAmounts :: Applicative f => (Amount -> f Amount) -> Journal -> f Journal
-- traverseJournalAmounts f j =
--   recombine <$> (traverse . dcamt) f (jparsedefaultcommodity j)
--             <*> (traverse . pdamt) f (jpricedirectives j)
--             <*> (traverse . tps . traverse . pamt . amts . traverse) f (jtxns j)
--   where
--     recombine pds txns = j { jpricedirectives = pds, jtxns = txns }
--     -- a bunch of traversals
--     dcamt g pd         = (\mdc -> case mdc of Nothing -> Nothing
--                                               Just ((c,stpd{pdamount =amt}
--                          ) <$> g (pdamount pd)
--     pdamt g pd         = (\amt -> pd{pdamount =amt}) <$> g (pdamount pd)
--     tps   g t          = (\ps  -> t {tpostings=ps }) <$> g (tpostings t)
--     pamt  g p          = (\amt -> p {pamount  =amt}) <$> g (pamount p)
--     amts  g (Mixed as) = Mixed <$> g as

-- | The fully specified exact date span enclosing the dates (primary or secondary)
-- of all this journal's transactions and postings, or DateSpan Nothing Nothing
-- if there are none.
journalDateSpan :: Bool -> Journal -> DateSpan
journalDateSpan False = journalDateSpanHelper $ Just PrimaryDate
journalDateSpan True  = journalDateSpanHelper $ Just SecondaryDate

-- | The fully specified date span enclosing the dates (primary and secondary)
-- of all this journal's transactions and postings, or DateSpan Nothing Nothing
-- if there are none.
journalDateSpanBothDates :: Journal -> DateSpan
journalDateSpanBothDates = journalDateSpanHelper Nothing

-- | A helper for journalDateSpan which takes Maybe WhichDate directly. Nothing
-- uses both primary and secondary dates.
journalDateSpanHelper :: Maybe WhichDate -> Journal -> DateSpan
journalDateSpanHelper whichdate j =
    DateSpan (Exact <$> minimumMay dates) (Exact . addDays 1 <$> maximumMay dates)
  where
    dates    = pdates ++ tdates
    tdates   = concatMap gettdate ts
    pdates   = concatMap getpdate $ concatMap tpostings ts
    ts       = jtxns j
    gettdate t = case whichdate of
        Just PrimaryDate   -> [tdate t]
        Just SecondaryDate -> [fromMaybe (tdate t) $ tdate2 t]
        Nothing            -> tdate t : maybeToList (tdate2 t)
    getpdate p = case whichdate of
        Just PrimaryDate   -> maybeToList $ pdate p
        Just SecondaryDate -> maybeToList $ pdate2 p <|> pdate p
        Nothing            -> catMaybes [pdate p, pdate2 p]

-- | The earliest of this journal's transaction and posting dates, or
-- Nothing if there are none.
journalStartDate :: Bool -> Journal -> Maybe Day
journalStartDate secondary j = fromEFDay <$> b where DateSpan b _ = journalDateSpan secondary j

-- | The "exclusive end date" of this journal: the day following its latest transaction 
-- or posting date, or Nothing if there are none.
journalEndDate :: Bool -> Journal -> Maybe Day
journalEndDate secondary j = fromEFDay <$> e where DateSpan _ e = journalDateSpan secondary j

-- | The latest of this journal's transaction and posting dates, or
-- Nothing if there are none.
journalLastDay :: Bool -> Journal -> Maybe Day
journalLastDay secondary j = addDays (-1) <$> journalEndDate secondary j

-- | Apply the pivot transformation to all postings in a journal,
-- replacing their account name by their value for the given field or tag.
journalPivot :: Text -> Journal -> Journal
journalPivot fieldortagname j = j{jtxns = map (transactionPivot fieldortagname) . jtxns $ j}

-- | Replace this transaction's postings' account names with the value
-- of the given field or tag, if any.
transactionPivot :: Text -> Transaction -> Transaction
transactionPivot fieldortagname t = t{tpostings = map (postingPivot fieldortagname) . tpostings $ t}

-- | Replace this posting's account name with the value
-- of the given field or tag, if any, otherwise the empty string.
postingPivot :: Text -> Posting -> Posting
postingPivot fieldortagname p =
  p{paccount = pivotAccount fieldortagname p, poriginal = Just $ originalPosting p}

pivotAccount :: Text -> Posting -> Text
pivotAccount fieldortagname p =
  T.intercalate ":" [pivotComponent x p | x <- T.splitOn ":" fieldortagname]

-- | Get the value of the given field or tag for this posting.
-- "comm" and "cur" are accepted as synonyms meaning the commodity symbol.
-- Pivoting on an unknown field or tag, or on commodity when there are multiple commodities, returns "".
-- Pivoting on a tag when there are multiple values for that tag, returns the first value.
pivotComponent :: Text -> Posting -> Text
pivotComponent fieldortagname p
  | fieldortagname == "code",        Just t <- ptransaction p = tcode t
  | fieldortagname `elem` descnames, Just t <- ptransaction p = tdescription t
  | fieldortagname == "payee",       Just t <- ptransaction p = transactionPayee t
  | fieldortagname == "note",        Just t <- ptransaction p = transactionNote t
  | fieldortagname == "status",      Just t <- ptransaction p = T.pack . show . tstatus $ t
  | fieldortagname == "acct"        = paccount p
  | fieldortagname `elem` commnames = case map acommodity $ amounts $ pamount p of [s] -> s; _ -> unknown
  | fieldortagname == "amt"         = case amounts $ pamount p of [a] -> T.pack $ show $ aquantity a; _ -> unknown
  | fieldortagname == "cost"        = case amounts $ pamount p of [a@Amount{acost=Just _}] -> T.pack $ lstrip $ showAmountCost a; _ -> unknown
  | Just (_, tagvalue) <- postingFindTag fieldortagname p = tagvalue
  | otherwise = unknown
  where
    descnames = ["desc", "description"]   -- allow "description" for hledger <=1.30 compat
    commnames = ["cur","comm"]            -- allow either; cur is the query prefix, comm is more consistent
    unknown   = ""

postingFindTag :: TagName -> Posting -> Maybe (TagName, TagValue)
postingFindTag tagname p = find ((tagname==) . fst) $ postingAllTags p

-- | Apply some account aliases to all posting account names in the journal, as described by accountNameApplyAliases.
-- This can fail due to a bad replacement pattern in a regular expression alias.
journalApplyAliases :: [AccountAlias] -> Journal -> Either RegexError Journal
-- short circuit the common case, just in case there's a performance impact from txnTieKnot etc.
journalApplyAliases [] j = Right j
journalApplyAliases aliases j = 
  case mapM (transactionApplyAliases aliases) $ jtxns j of
    Right ts -> Right j{jtxns = ts}
    Left err -> Left err

-- -- | Build a database of market prices in effect on the given date,
-- -- from the journal's price directives.
-- journalPrices :: Day -> Journal -> Prices
-- journalPrices d = toPrices d . jpricedirectives

-- -- | Render a market price as a P directive.
-- showPriceDirectiveDirective :: PriceDirective -> String
-- showPriceDirectiveDirective pd = unwords
--     [ "P"
--     , showDate (pddate pd)
--     , T.unpack (pdcommodity pd)
--     , (showAmount . amountSetPrecision maxprecision) (pdamount pd
--     )
--     ]

-- debug helpers
-- traceAmountPrecision a = trace (show $ map (precision . acommodity) $ amounts a) a
-- tracePostingsCommodities ps = trace (show $ map ((map (precision . acommodity) . amounts) . pamount) ps) ps

-- tests
--
-- A sample journal for testing, similar to examples/sample.journal.
-- Provide an option to either use explicit amounts or missing amounts, for testing purposes.
--
-- 2008/01/01 income
--     assets:bank:checking  $1
--     income:salary
--
-- 2008/06/01 gift
--     assets:bank:checking  $1
--     income:gifts
--
-- 2008/06/02 save
--     assets:bank:saving  $1
--     assets:bank:checking
--
-- 2008/06/03 * eat & shop
--     expenses:food      $1
--     expenses:supplies  $1
--     assets:cash
--
-- 2008/10/01 take a loan
--     assets:bank:checking $1
--     liabilities:debts    $-1
--
-- 2008/12/31 * pay off
--     liabilities:debts  $1
--     assets:bank:checking

samplejournal = samplejournalMaybeExplicit True

samplejournalMaybeExplicit :: Bool -> Journal
samplejournalMaybeExplicit explicit = nulljournal
         {jtxns = [
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepospair,
             tdate=fromGregorian 2008 01 01,
             tdate2=Nothing,
             tstatus=Unmarked,
             tcode="",
             tdescription="income",
             tcomment="",
             ttags=[],
             tpostings=
                 ["assets:bank:checking" `post` usd 1
                 ,"income:salary" `post` if explicit then usd (-1) else missingamt
                 ],
             tprecedingcomment=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepospair,
             tdate=fromGregorian 2008 06 01,
             tdate2=Nothing,
             tstatus=Unmarked,
             tcode="",
             tdescription="gift",
             tcomment="",
             ttags=[],
             tpostings=
                 ["assets:bank:checking" `post` usd 1
                 ,"income:gifts" `post` if explicit then usd (-1) else missingamt
                 ],
             tprecedingcomment=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepospair,
             tdate=fromGregorian 2008 06 02,
             tdate2=Nothing,
             tstatus=Unmarked,
             tcode="",
             tdescription="save",
             tcomment="",
             ttags=[],
             tpostings=
                 ["assets:bank:saving" `post` usd 1
                 ,"assets:bank:checking" `post` if explicit then usd (-1) else missingamt
                 ],
             tprecedingcomment=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepospair,
             tdate=fromGregorian 2008 06 03,
             tdate2=Nothing,
             tstatus=Cleared,
             tcode="",
             tdescription="eat & shop",
             tcomment="",
             ttags=[],
             tpostings=["expenses:food" `post` usd 1
                       ,"expenses:supplies" `post` usd 1
                       ,"assets:cash" `post` if explicit then usd (-2) else missingamt
                       ],
             tprecedingcomment=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepospair,
             tdate=fromGregorian 2008 10 01,
             tdate2=Nothing,
             tstatus=Unmarked,
             tcode="",
             tdescription="take a loan",
             tcomment="",
             ttags=[],
             tpostings=["assets:bank:checking" `post` usd 1
                       ,"liabilities:debts" `post` usd (-1)
                       ],
             tprecedingcomment=""
           }
          ,
           txnTieKnot $ Transaction {
             tindex=0,
             tsourcepos=nullsourcepospair,
             tdate=fromGregorian 2008 12 31,
             tdate2=Nothing,
             tstatus=Unmarked,
             tcode="",
             tdescription="pay off",
             tcomment="",
             ttags=[],
             tpostings=["liabilities:debts" `post` usd 1
                       ,"assets:bank:checking" `post` if explicit then usd (-1) else missingamt
                       ],
             tprecedingcomment=""
           }
          ]
         }

tests_Journal = testGroup "Journal" [

   testCase "journalDateSpan" $
    journalDateSpan True nulljournal{
      jtxns = [nulltransaction{tdate = fromGregorian 2014 02 01
                              ,tpostings = [posting{pdate=Just (fromGregorian 2014 01 10)}]
                              }
              ,nulltransaction{tdate = fromGregorian 2014 09 01
                              ,tpostings = [posting{pdate2=Just (fromGregorian 2014 10 10)}]
                              }
              ]
      }
    @?= (DateSpan (Just $ Exact $ fromGregorian 2014 1 10) (Just $ Exact $ fromGregorian 2014 10 11))
  ]
