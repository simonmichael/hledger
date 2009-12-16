{-|

A 'Journal' is a parsed ledger file.

-}

module Ledger.Journal
where
import qualified Data.Map as Map
import Data.Map (findWithDefault, (!))
import System.Time (ClockTime(TOD))
import Ledger.Utils
import Ledger.Types
import Ledger.AccountName
import Ledger.Amount
import Ledger.LedgerTransaction (ledgerTransactionWithDate)
import Ledger.LedgerPosting
import Ledger.Posting
import Ledger.TimeLog


instance Show Journal where
    show l = printf "Journal with %d transactions, %d accounts: %s"
             (length (ledger_txns l) +
              length (modifier_txns l) +
              length (periodic_txns l))
             (length accounts)
             (show accounts)
             -- ++ (show $ journalTransactions l)
             where accounts = flatten $ journalAccountNameTree l

journalEmpty :: Journal
journalEmpty = Journal { modifier_txns = []
                           , periodic_txns = []
                           , ledger_txns = []
                           , open_timelog_entries = []
                           , historical_prices = []
                           , final_comment_lines = []
                           , filepath = ""
                           , filereadtime = TOD 0 0
                           }

addLedgerTransaction :: LedgerTransaction -> Journal -> Journal
addLedgerTransaction t l0 = l0 { ledger_txns = t : ledger_txns l0 }

addModifierTransaction :: ModifierTransaction -> Journal -> Journal
addModifierTransaction mt l0 = l0 { modifier_txns = mt : modifier_txns l0 }

addPeriodicTransaction :: PeriodicTransaction -> Journal -> Journal
addPeriodicTransaction pt l0 = l0 { periodic_txns = pt : periodic_txns l0 }

addHistoricalPrice :: HistoricalPrice -> Journal -> Journal
addHistoricalPrice h l0 = l0 { historical_prices = h : historical_prices l0 }

addTimeLogEntry :: TimeLogEntry -> Journal -> Journal
addTimeLogEntry tle l0 = l0 { open_timelog_entries = tle : open_timelog_entries l0 }

journalLedgerPostings :: Journal -> [LedgerPosting]
journalLedgerPostings = txnsof . ledger_txns
    where txnsof ts = concatMap flattenLedgerTransaction $ zip ts [1..]

journalAccountNamesUsed :: Journal -> [AccountName]
journalAccountNamesUsed = accountNamesFromLedgerPostings . journalLedgerPostings

journalAccountNames :: Journal -> [AccountName]
journalAccountNames = sort . expandAccountNames . journalAccountNamesUsed

journalAccountNameTree :: Journal -> Tree AccountName
journalAccountNameTree = accountNameTreeFrom . journalAccountNames

-- | Remove ledger transactions we are not interested in.
-- Keep only those which fall between the begin and end dates, and match
-- the description pattern, and are cleared or real if those options are active.
filterJournal :: DateSpan -> [String] -> Maybe Bool -> Bool -> Journal -> Journal
filterJournal span pats clearedonly realonly =
    filterJournalPostingsByRealness realonly .
    filterJournalPostingsByClearedStatus clearedonly .
    filterJournalLedgerTransactionsByDate span .
    filterJournalLedgerTransactionsByDescription pats

-- | Keep only ledger transactions whose description matches the description patterns.
filterJournalLedgerTransactionsByDescription :: [String] -> Journal -> Journal
filterJournalLedgerTransactionsByDescription pats (Journal ms ps ts tls hs f fp ft) =
    Journal ms ps (filter matchdesc ts) tls hs f fp ft
    where matchdesc = matchpats pats . ltdescription

-- | Keep only ledger transactions which fall between begin and end dates.
-- We include transactions on the begin date and exclude transactions on the end
-- date, like ledger.  An empty date string means no restriction.
filterJournalLedgerTransactionsByDate :: DateSpan -> Journal -> Journal
filterJournalLedgerTransactionsByDate (DateSpan begin end) (Journal ms ps ts tls hs f fp ft) =
    Journal ms ps (filter matchdate ts) tls hs f fp ft
    where
      matchdate t = maybe True (ltdate t>=) begin && maybe True (ltdate t<) end

-- | Keep only ledger transactions which have the requested
-- cleared/uncleared status, if there is one.
filterJournalPostingsByClearedStatus :: Maybe Bool -> Journal -> Journal
filterJournalPostingsByClearedStatus Nothing rl = rl
filterJournalPostingsByClearedStatus (Just val) (Journal ms ps ts tls hs f fp ft) =
    Journal ms ps (filter ((==val).ltstatus) ts) tls hs f fp ft

-- | Strip out any virtual postings, if the flag is true, otherwise do
-- no filtering.
filterJournalPostingsByRealness :: Bool -> Journal -> Journal
filterJournalPostingsByRealness False l = l
filterJournalPostingsByRealness True (Journal mts pts ts tls hs f fp ft) =
    Journal mts pts (map filtertxns ts) tls hs f fp ft
    where filtertxns t@LedgerTransaction{ltpostings=ps} = t{ltpostings=filter isReal ps}

-- | Strip out any postings to accounts deeper than the specified depth
-- (and any ledger transactions which have no postings as a result).
filterJournalPostingsByDepth :: Int -> Journal -> Journal
filterJournalPostingsByDepth depth (Journal mts pts ts tls hs f fp ft) =
    Journal mts pts (filter (not . null . ltpostings) $ map filtertxns ts) tls hs f fp ft
    where filtertxns t@LedgerTransaction{ltpostings=ps} =
              t{ltpostings=filter ((<= depth) . accountNameLevel . paccount) ps}

-- | Keep only ledger transactions which affect accounts matched by the account patterns.
filterJournalPostingsByAccount :: [String] -> Journal -> Journal
filterJournalPostingsByAccount apats (Journal ms ps ts tls hs f fp ft) =
    Journal ms ps (filter (any (matchpats apats . paccount) . ltpostings) ts) tls hs f fp ft

-- | Convert this ledger's transactions' primary date to either their
-- actual or effective date.
journalSelectingDate :: WhichDate -> Journal -> Journal
journalSelectingDate ActualDate rl = rl
journalSelectingDate EffectiveDate rl =
    rl{ledger_txns=map (ledgerTransactionWithDate EffectiveDate) $ ledger_txns rl}

-- | Give all a ledger's amounts their canonical display settings.  That
-- is, in each commodity, amounts will use the display settings of the
-- first amount detected, and the greatest precision of the amounts
-- detected.
-- Also, missing unit prices are added if known from the price history.
-- Also, amounts are converted to cost basis if that flag is active.
-- XXX refactor
canonicaliseAmounts :: Bool -> Journal -> Journal
canonicaliseAmounts costbasis rl@(Journal ms ps ts tls hs f fp ft) = Journal ms ps (map fixledgertransaction ts) tls hs f fp ft
    where
      fixledgertransaction (LedgerTransaction d ed s c de co ts pr) = LedgerTransaction d ed s c de co (map fixrawposting ts) pr
          where
            fixrawposting (Posting s ac a c t) = Posting s ac (fixmixedamount a) c t
            fixmixedamount (Mixed as) = Mixed $ map fixamount as
            fixamount = (if costbasis then costOfAmount else id) . fixprice . fixcommodity
            fixcommodity a = a{commodity=c} where c = canonicalcommoditymap ! symbol (commodity a)
            canonicalcommoditymap =
                Map.fromList [(s,firstc{precision=maxp}) | s <- commoditysymbols,
                        let cs = commoditymap ! s,
                        let firstc = head cs,
                        let maxp = maximum $ map precision cs
                       ]
            commoditymap = Map.fromList [(s,commoditieswithsymbol s) | s <- commoditysymbols]
            commoditieswithsymbol s = filter ((s==) . symbol) commodities
            commoditysymbols = nub $ map symbol commodities
            commodities = map commodity (concatMap (amounts . tamount) (journalLedgerPostings rl)
                                         ++ concatMap (amounts . hamount) (historical_prices rl))
            fixprice :: Amount -> Amount
            fixprice a@Amount{price=Just _} = a
            fixprice a@Amount{commodity=c} = a{price=journalHistoricalPriceFor rl d c}

            -- | Get the price for a commodity on the specified day from the price database, if known.
            -- Does only one lookup step, ie will not look up the price of a price.
            journalHistoricalPriceFor :: Journal -> Day -> Commodity -> Maybe MixedAmount
            journalHistoricalPriceFor rl d Commodity{symbol=s} = do
              let ps = reverse $ filter ((<= d).hdate) $ filter ((s==).hsymbol) $ sortBy (comparing hdate) $ historical_prices rl
              case ps of (HistoricalPrice{hamount=a}:_) -> Just $ canonicaliseCommodities a
                         _ -> Nothing
                  where
                    canonicaliseCommodities (Mixed as) = Mixed $ map canonicaliseCommodity as
                        where canonicaliseCommodity a@Amount{commodity=Commodity{symbol=s}} =
                                  a{commodity=findWithDefault (error "programmer error: canonicaliseCommodity failed") s canonicalcommoditymap}

-- | Get just the amounts from a ledger, in the order parsed.
journalAmounts :: Journal -> [MixedAmount]
journalAmounts = map tamount . journalLedgerPostings

-- | Get just the ammount commodities from a ledger, in the order parsed.
journalCommodities :: Journal -> [Commodity]
journalCommodities = map commodity . concatMap amounts . journalAmounts

-- | Get just the amount precisions from a ledger, in the order parsed.
journalPrecisions :: Journal -> [Int]
journalPrecisions = map precision . journalCommodities

-- | Close any open timelog sessions using the provided current time.
journalConvertTimeLog :: LocalTime -> Journal -> Journal
journalConvertTimeLog t l0 = l0 { ledger_txns = convertedTimeLog ++ ledger_txns l0
                                  , open_timelog_entries = []
                                  }
    where convertedTimeLog = entriesFromTimeLogEntries t $ open_timelog_entries l0

-- | The (fully specified) date span containing all the raw ledger's transactions,
-- or DateSpan Nothing Nothing if there are none.
journalDateSpan :: Journal -> DateSpan
journalDateSpan rl
    | null ts = DateSpan Nothing Nothing
    | otherwise = DateSpan (Just $ ltdate $ head ts) (Just $ addDays 1 $ ltdate $ last ts)
    where
      ts = sortBy (comparing ltdate) $ ledger_txns rl

-- | Check if a set of ledger account/description patterns matches the
-- given account name or entry description.  Patterns are case-insensitive
-- regular expression strings; those beginning with - are anti-patterns.
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
