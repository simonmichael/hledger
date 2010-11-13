{-|

A 'Journal' is a set of 'Transaction's and related data, usually parsed
from a hledger/ledger journal file or timelog. This is the primary hledger
data object.

-}

module Hledger.Data.Journal
where
import qualified Data.Map as Map
import Data.Map (findWithDefault, (!))
import Safe (headDef)
import System.Time (ClockTime(TOD))
import Hledger.Data.Utils
import Hledger.Data.Types
import Hledger.Data.AccountName
import Hledger.Data.Amount
import Hledger.Data.Commodity (canonicaliseCommodities)
import Hledger.Data.Dates (nulldatespan)
import Hledger.Data.Transaction (journalTransactionWithDate)
import Hledger.Data.Posting
import Hledger.Data.TimeLog


instance Show Journal where
    show j = printf "Journal %s with %d transactions, %d accounts: %s"
             (journalFilePath j)
             (length (jtxns j) +
              length (jmodifiertxns j) +
              length (jperiodictxns j))
             (length accounts)
             (show accounts)
             -- ++ (show $ journalTransactions l)
             where accounts = flatten $ journalAccountNameTree j

nulljournal :: Journal
nulljournal = Journal { jmodifiertxns = []
                      , jperiodictxns = []
                      , jtxns = []
                      , open_timelog_entries = []
                      , historical_prices = []
                      , final_comment_lines = []
                      , jContext = nullctx
                      , files = []
                      , filereadtime = TOD 0 0
                      }

nullctx :: JournalContext
nullctx = Ctx { ctxYear = Nothing, ctxCommodity = Nothing, ctxAccount = [] }

nullfilterspec = FilterSpec {
     datespan=nulldatespan
    ,cleared=Nothing
    ,real=False
    ,empty=False
    ,costbasis=False
    ,acctpats=[]
    ,descpats=[]
    ,whichdate=ActualDate
    ,depth=Nothing
    }

journalFilePath :: Journal -> FilePath
journalFilePath = fst . mainfile

journalFilePaths :: Journal -> [FilePath]
journalFilePaths = map fst . files

mainfile :: Journal -> (FilePath, String)
mainfile = headDef ("", "") . files

addTransaction :: Transaction -> Journal -> Journal
addTransaction t l0 = l0 { jtxns = t : jtxns l0 }

addModifierTransaction :: ModifierTransaction -> Journal -> Journal
addModifierTransaction mt l0 = l0 { jmodifiertxns = mt : jmodifiertxns l0 }

addPeriodicTransaction :: PeriodicTransaction -> Journal -> Journal
addPeriodicTransaction pt l0 = l0 { jperiodictxns = pt : jperiodictxns l0 }

addHistoricalPrice :: HistoricalPrice -> Journal -> Journal
addHistoricalPrice h l0 = l0 { historical_prices = h : historical_prices l0 }

addTimeLogEntry :: TimeLogEntry -> Journal -> Journal
addTimeLogEntry tle l0 = l0 { open_timelog_entries = tle : open_timelog_entries l0 }

journalPostings :: Journal -> [Posting]
journalPostings = concatMap tpostings . jtxns

journalAccountNamesUsed :: Journal -> [AccountName]
journalAccountNamesUsed = accountNamesFromPostings . journalPostings

journalAccountNames :: Journal -> [AccountName]
journalAccountNames = sort . expandAccountNames . journalAccountNamesUsed

journalAccountNameTree :: Journal -> Tree AccountName
journalAccountNameTree = accountNameTreeFrom . journalAccountNames

-- Various kinds of filtering on journals. We do it differently depending
-- on the command.

-- | Keep only transactions we are interested in, as described by
-- the filter specification. May also massage the data a little.
filterJournalTransactions :: FilterSpec -> Journal -> Journal
filterJournalTransactions FilterSpec{datespan=datespan
                                    ,cleared=cleared
                                    -- ,real=real
                                    -- ,empty=empty
                                    -- ,costbasis=_
                                    ,acctpats=apats
                                    ,descpats=dpats
                                    ,whichdate=whichdate
                                    ,depth=depth
                                    } =
    filterJournalTransactionsByClearedStatus cleared .
    filterJournalPostingsByDepth depth .
    filterJournalTransactionsByAccount apats .
    filterJournalTransactionsByDescription dpats .
    filterJournalTransactionsByDate datespan .
    journalSelectingDate whichdate

-- | Keep only postings we are interested in, as described by
-- the filter specification. May also massage the data a little.
-- This can leave unbalanced transactions.
filterJournalPostings :: FilterSpec -> Journal -> Journal
filterJournalPostings FilterSpec{datespan=datespan
                                ,cleared=cleared
                                ,real=real
                                ,empty=empty
--                                ,costbasis=costbasis
                                ,acctpats=apats
                                ,descpats=dpats
                                ,whichdate=whichdate
                                ,depth=depth
                                } =
    filterJournalPostingsByRealness real .
    filterJournalPostingsByClearedStatus cleared .
    filterJournalPostingsByEmpty empty .
    filterJournalPostingsByDepth depth .
    filterJournalPostingsByAccount apats .
    filterJournalTransactionsByDescription dpats .
    filterJournalTransactionsByDate datespan .
    journalSelectingDate whichdate

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
filterJournalTransactionsByClearedStatus :: Maybe Bool -> Journal -> Journal
filterJournalTransactionsByClearedStatus Nothing j = j
filterJournalTransactionsByClearedStatus (Just val) j@Journal{jtxns=ts} = j{jtxns=filter match ts}
    where match = (==val).tstatus

-- | Keep only postings which have the requested cleared/uncleared status,
-- if there is one.
filterJournalPostingsByClearedStatus :: Maybe Bool -> Journal -> Journal
filterJournalPostingsByClearedStatus Nothing j = j
filterJournalPostingsByClearedStatus (Just c) j@Journal{jtxns=ts} = j{jtxns=map filterpostings ts}
    where filterpostings t@Transaction{tpostings=ps} = t{tpostings=filter ((==c) . postingCleared) ps}

-- | Strip out any virtual postings, if the flag is true, otherwise do
-- no filtering.
filterJournalPostingsByRealness :: Bool -> Journal -> Journal
filterJournalPostingsByRealness False l = l
filterJournalPostingsByRealness True j@Journal{jtxns=ts} = j{jtxns=map filterpostings ts}
    where filterpostings t@Transaction{tpostings=ps} = t{tpostings=filter isReal ps}

-- | Strip out any postings with zero amount, unless the flag is true.
filterJournalPostingsByEmpty :: Bool -> Journal -> Journal
filterJournalPostingsByEmpty True l = l
filterJournalPostingsByEmpty False j@Journal{jtxns=ts} = j{jtxns=map filterpostings ts}
    where filterpostings t@Transaction{tpostings=ps} = t{tpostings=filter (not . isEmptyPosting) ps}

-- | Keep only transactions which affect accounts deeper than the specified depth.
filterJournalTransactionsByDepth :: Maybe Int -> Journal -> Journal
filterJournalTransactionsByDepth Nothing j = j
filterJournalTransactionsByDepth (Just d) j@Journal{jtxns=ts} =
    j{jtxns=(filter (any ((<= d+1) . accountNameLevel . paccount) . tpostings) ts)}

-- | Strip out any postings to accounts deeper than the specified depth
-- (and any transactions which have no postings as a result).
filterJournalPostingsByDepth :: Maybe Int -> Journal -> Journal
filterJournalPostingsByDepth Nothing j = j
filterJournalPostingsByDepth (Just d) j@Journal{jtxns=ts} =
    j{jtxns=filter (not . null . tpostings) $ map filtertxns ts}
    where filtertxns t@Transaction{tpostings=ps} =
              t{tpostings=filter ((<= d) . accountNameLevel . paccount) ps}

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
      amatch pat a = containsRegex (abspat pat) a
      (negatives,positives) = partition isnegativepat apats

-- | Keep only postings which affect accounts matched by the account patterns.
-- This can leave transactions unbalanced.
filterJournalPostingsByAccount :: [String] -> Journal -> Journal
filterJournalPostingsByAccount apats j@Journal{jtxns=ts} = j{jtxns=map filterpostings ts}
    where filterpostings t@Transaction{tpostings=ps} = t{tpostings=filter (matchpats apats . paccount) ps}

-- | Convert this journal's transactions' primary date to either the
-- actual or effective date.
journalSelectingDate :: WhichDate -> Journal -> Journal
journalSelectingDate ActualDate j = j
journalSelectingDate EffectiveDate j =
    j{jtxns=map (journalTransactionWithDate EffectiveDate) $ jtxns j}

-- | Do post-parse processing on a journal, to make it ready for use.
journalFinalise :: ClockTime -> LocalTime -> FilePath -> String -> JournalContext -> Journal -> Journal
journalFinalise tclock tlocal path txt ctx j@Journal{files=fs} =
    journalCanonicaliseAmounts $
    journalApplyHistoricalPrices $
    journalCloseTimeLogEntries tlocal
    j{files=(path,txt):fs, filereadtime=tclock, jContext=ctx}

-- | Convert all the journal's amounts to their canonical display
-- settings.  Ie, all amounts in a given commodity will use (a) the
-- display settings of the first, and (b) the greatest precision, of the
-- amounts in that commodity. Prices are canonicalised as well, so consider
-- calling journalApplyHistoricalPrices before this.
journalCanonicaliseAmounts :: Journal -> Journal
journalCanonicaliseAmounts j@Journal{jtxns=ts} = j{jtxns=map fixtransaction ts}
    where
      fixtransaction t@Transaction{tpostings=ps} = t{tpostings=map fixposting ps}
      fixposting p@Posting{pamount=a} = p{pamount=fixmixedamount a}
      fixmixedamount (Mixed as) = Mixed $ map fixamount as
      fixamount a@Amount{commodity=c,price=p} = a{commodity=fixcommodity c, price=maybe Nothing (Just . fixmixedamount) p}
      fixcommodity c@Commodity{symbol=s} = findWithDefault c s canonicalcommoditymap
      canonicalcommoditymap = journalCanonicalCommodities j

-- | Apply this journal's historical price records to unpriced amounts where possible.
journalApplyHistoricalPrices :: Journal -> Journal
journalApplyHistoricalPrices j@Journal{jtxns=ts} = j{jtxns=map fixtransaction ts}
    where
      fixtransaction t@Transaction{tdate=d, tpostings=ps} = t{tpostings=map fixposting ps}
       where
        fixposting p@Posting{pamount=a} = p{pamount=fixmixedamount a}
        fixmixedamount (Mixed as) = Mixed $ map fixamount as
        fixamount = fixprice
        fixprice a@Amount{price=Just _} = a
        fixprice a@Amount{commodity=c} = a{price=journalHistoricalPriceFor j d c}

-- | Get the price for a commodity on the specified day from the price database, if known.
-- Does only one lookup step, ie will not look up the price of a price.
journalHistoricalPriceFor :: Journal -> Day -> Commodity -> Maybe MixedAmount
journalHistoricalPriceFor j d Commodity{symbol=s} = do
  let ps = reverse $ filter ((<= d).hdate) $ filter ((s==).hsymbol) $ sortBy (comparing hdate) $ historical_prices j
  case ps of (HistoricalPrice{hamount=a}:_) -> Just a
             _ -> Nothing

-- | Close any open timelog sessions in this journal using the provided current time.
journalCloseTimeLogEntries :: LocalTime -> Journal -> Journal
journalCloseTimeLogEntries now j@Journal{jtxns=ts, open_timelog_entries=es} =
  j{jtxns = ts ++ (timeLogEntriesToTransactions now es), open_timelog_entries = []}

-- | Convert all this journal's amounts to cost by applying their prices, if any.
journalConvertAmountsToCost :: Journal -> Journal
journalConvertAmountsToCost j@Journal{jtxns=ts} = j{jtxns=map fixtransaction ts}
    where
      fixtransaction t@Transaction{tpostings=ps} = t{tpostings=map fixposting ps}
      fixposting p@Posting{pamount=a} = p{pamount=fixmixedamount a}
      fixmixedamount (Mixed as) = Mixed $ map fixamount as
      fixamount = costOfAmount

-- | Get this journal's unique, display-preference-canonicalised commodities, by symbol.
journalCanonicalCommodities :: Journal -> Map.Map String Commodity
journalCanonicalCommodities j = canonicaliseCommodities $ journalAmountAndPriceCommodities j

-- | Get all this journal's amounts' commodities, in the order parsed.
journalAmountCommodities :: Journal -> [Commodity]
journalAmountCommodities = map commodity . concatMap amounts . journalAmounts

-- | Get all this journal's amount and price commodities, in the order parsed.
journalAmountAndPriceCommodities :: Journal -> [Commodity]
journalAmountAndPriceCommodities = concatMap amountCommodities . concatMap amounts . journalAmounts

-- | Get this amount's commodity and any commodities referenced in its price.
amountCommodities :: Amount -> [Commodity]
amountCommodities Amount{commodity=c,price=Nothing} = [c]
amountCommodities Amount{commodity=c,price=Just ma} = c:(concatMap amountCommodities $ amounts ma)

-- | Get all this journal's amounts, in the order parsed.
journalAmounts :: Journal -> [MixedAmount]
journalAmounts = map pamount . journalPostings

-- | The (fully specified) date span containing this journal's transactions,
-- or DateSpan Nothing Nothing if there are none.
journalDateSpan :: Journal -> DateSpan
journalDateSpan j
    | null ts = DateSpan Nothing Nothing
    | otherwise = DateSpan (Just $ tdate $ head ts) (Just $ addDays 1 $ tdate $ last ts)
    where
      ts = sortBy (comparing tdate) $ jtxns j

-- | Check if a set of hledger account/description filter patterns matches the
-- given account name or entry description.  Patterns are case-insensitive
-- regular expressions. Prefixed with not:, they become anti-patterns.
matchpats :: [String] -> String -> Bool
matchpats pats str =
    (null positives || any match positives) && (null negatives || not (any match negatives))
    where
      (negatives,positives) = partition isnegativepat pats
      match "" = True
      match pat = containsRegex (abspat pat) str

negateprefix = "not:"

isnegativepat = (negateprefix `isPrefixOf`)

abspat pat = if isnegativepat pat then drop (length negateprefix) pat else pat

-- | Calculate the account tree and all account balances from a journal's
-- postings, returning the results for efficient lookup.
journalAccountInfo :: Journal -> (Tree AccountName, Map.Map AccountName Account)
journalAccountInfo j = (ant, amap)
    where
      (ant, psof, _, inclbalof) = (groupPostings . journalPostings) j
      amap = Map.fromList [(a, acctinfo a) | a <- flatten ant]
      acctinfo a = Account a (psof a) (inclbalof a)

-- | Given a list of postings, return an account name tree and three query
-- functions that fetch postings, subaccount-excluding-balance and
-- subaccount-including-balance by account name.
groupPostings :: [Posting] -> (Tree AccountName,
                             (AccountName -> [Posting]),
                             (AccountName -> MixedAmount),
                             (AccountName -> MixedAmount))
groupPostings ps = (ant, psof, exclbalof, inclbalof)
    where
      anames = sort $ nub $ map paccount ps
      ant = accountNameTreeFrom $ expandAccountNames anames
      allanames = flatten ant
      pmap = Map.union (postingsByAccount ps) (Map.fromList [(a,[]) | a <- allanames])
      psof = (pmap !)
      balmap = Map.fromList $ flatten $ calculateBalances ant psof
      exclbalof = fst . (balmap !)
      inclbalof = snd . (balmap !)

-- | Add subaccount-excluding and subaccount-including balances to a tree
-- of account names somewhat efficiently, given a function that looks up
-- transactions by account name.
calculateBalances :: Tree AccountName -> (AccountName -> [Posting]) -> Tree (AccountName, (MixedAmount, MixedAmount))
calculateBalances ant psof = addbalances ant
    where
      addbalances (Node a subs) = Node (a,(bal,bal+subsbal)) subs'
          where
            bal         = sumPostings $ psof a
            subsbal     = sum $ map (snd . snd . root) subs'
            subs'       = map addbalances subs

-- | Convert a list of postings to a map from account name to that
-- account's postings.
postingsByAccount :: [Posting] -> Map.Map AccountName [Posting]
postingsByAccount ps = m'
    where
      sortedps = sortBy (comparing paccount) ps
      groupedps = groupBy (\p1 p2 -> paccount p1 == paccount p2) sortedps
      m' = Map.fromList [(paccount $ head g, g) | g <- groupedps]
