{-|

A 'RawLedger' is a parsed ledger file. We call it raw to distinguish from
the cached 'Ledger'.

-}

module Ledger.RawLedger
where
import qualified Data.Map as Map
import Data.Map (findWithDefault, (!))
import System.Time (ClockTime(TOD))
import Ledger.Utils
import Ledger.Types
import Ledger.AccountName
import Ledger.Amount
import Ledger.LedgerTransaction (ledgerTransactionWithDate)
import Ledger.Transaction
import Ledger.Posting
import Ledger.TimeLog


instance Show RawLedger where
    show l = printf "RawLedger with %d transactions, %d accounts: %s"
             (length (ledger_txns l) +
              length (modifier_txns l) +
              length (periodic_txns l))
             (length accounts)
             (show accounts)
             -- ++ (show $ rawLedgerTransactions l)
             where accounts = flatten $ rawLedgerAccountNameTree l

rawLedgerEmpty :: RawLedger
rawLedgerEmpty = RawLedger { modifier_txns = []
                           , periodic_txns = []
                           , ledger_txns = []
                           , open_timelog_entries = []
                           , historical_prices = []
                           , final_comment_lines = []
                           , filepath = ""
                           , filereadtime = TOD 0 0
                           }

addLedgerTransaction :: LedgerTransaction -> RawLedger -> RawLedger
addLedgerTransaction t l0 = l0 { ledger_txns = t : ledger_txns l0 }

addModifierTransaction :: ModifierTransaction -> RawLedger -> RawLedger
addModifierTransaction mt l0 = l0 { modifier_txns = mt : modifier_txns l0 }

addPeriodicTransaction :: PeriodicTransaction -> RawLedger -> RawLedger
addPeriodicTransaction pt l0 = l0 { periodic_txns = pt : periodic_txns l0 }

addHistoricalPrice :: HistoricalPrice -> RawLedger -> RawLedger
addHistoricalPrice h l0 = l0 { historical_prices = h : historical_prices l0 }

addTimeLogEntry :: TimeLogEntry -> RawLedger -> RawLedger
addTimeLogEntry tle l0 = l0 { open_timelog_entries = tle : open_timelog_entries l0 }

rawLedgerTransactions :: RawLedger -> [Transaction]
rawLedgerTransactions = txnsof . ledger_txns
    where txnsof ts = concatMap flattenLedgerTransaction $ zip ts [1..]

rawLedgerAccountNamesUsed :: RawLedger -> [AccountName]
rawLedgerAccountNamesUsed = accountNamesFromTransactions . rawLedgerTransactions

rawLedgerAccountNames :: RawLedger -> [AccountName]
rawLedgerAccountNames = sort . expandAccountNames . rawLedgerAccountNamesUsed

rawLedgerAccountNameTree :: RawLedger -> Tree AccountName
rawLedgerAccountNameTree = accountNameTreeFrom . rawLedgerAccountNames

-- | Remove ledger transactions we are not interested in.
-- Keep only those which fall between the begin and end dates, and match
-- the description pattern, and are cleared or real if those options are active.
filterRawLedger :: DateSpan -> [String] -> Maybe Bool -> Bool -> RawLedger -> RawLedger
filterRawLedger span pats clearedonly realonly =
    filterRawLedgerPostingsByRealness realonly .
    filterRawLedgerTransactionsByClearedStatus clearedonly .
    filterRawLedgerTransactionsByDate span .
    filterRawLedgerTransactionsByDescription pats

-- | Keep only ledger transactions whose description matches the description patterns.
filterRawLedgerTransactionsByDescription :: [String] -> RawLedger -> RawLedger
filterRawLedgerTransactionsByDescription pats (RawLedger ms ps ts tls hs f fp ft) =
    RawLedger ms ps (filter matchdesc ts) tls hs f fp ft
    where matchdesc = matchpats pats . ltdescription

-- | Keep only ledger transactions which fall between begin and end dates.
-- We include transactions on the begin date and exclude transactions on the end
-- date, like ledger.  An empty date string means no restriction.
filterRawLedgerTransactionsByDate :: DateSpan -> RawLedger -> RawLedger
filterRawLedgerTransactionsByDate (DateSpan begin end) (RawLedger ms ps ts tls hs f fp ft) =
    RawLedger ms ps (filter matchdate ts) tls hs f fp ft
    where
      matchdate t = maybe True (ltdate t>=) begin && maybe True (ltdate t<) end

-- | Keep only ledger transactions which have the requested
-- cleared/uncleared status, if there is one.
filterRawLedgerTransactionsByClearedStatus :: Maybe Bool -> RawLedger -> RawLedger
filterRawLedgerTransactionsByClearedStatus Nothing rl = rl
filterRawLedgerTransactionsByClearedStatus (Just val) (RawLedger ms ps ts tls hs f fp ft) =
    RawLedger ms ps (filter ((==val).ltstatus) ts) tls hs f fp ft

-- | Strip out any virtual postings, if the flag is true, otherwise do
-- no filtering.
filterRawLedgerPostingsByRealness :: Bool -> RawLedger -> RawLedger
filterRawLedgerPostingsByRealness False l = l
filterRawLedgerPostingsByRealness True (RawLedger mts pts ts tls hs f fp ft) =
    RawLedger mts pts (map filtertxns ts) tls hs f fp ft
    where filtertxns t@LedgerTransaction{ltpostings=ps} = t{ltpostings=filter isReal ps}

-- | Strip out any postings to accounts deeper than the specified depth
-- (and any ledger transactions which have no postings as a result).
filterRawLedgerPostingsByDepth :: Int -> RawLedger -> RawLedger
filterRawLedgerPostingsByDepth depth (RawLedger mts pts ts tls hs f fp ft) =
    RawLedger mts pts (filter (not . null . ltpostings) $ map filtertxns ts) tls hs f fp ft
    where filtertxns t@LedgerTransaction{ltpostings=ps} =
              t{ltpostings=filter ((<= depth) . accountNameLevel . paccount) ps}

-- | Keep only ledger transactions which affect accounts matched by the account patterns.
filterRawLedgerTransactionsByAccount :: [String] -> RawLedger -> RawLedger
filterRawLedgerTransactionsByAccount apats (RawLedger ms ps ts tls hs f fp ft) =
    RawLedger ms ps (filter (any (matchpats apats . paccount) . ltpostings) ts) tls hs f fp ft

-- | Convert this ledger's transactions' primary date to either their
-- actual or effective date.
rawLedgerSelectingDate :: WhichDate -> RawLedger -> RawLedger
rawLedgerSelectingDate ActualDate rl = rl
rawLedgerSelectingDate EffectiveDate rl =
    rl{ledger_txns=map (ledgerTransactionWithDate EffectiveDate) $ ledger_txns rl}

-- | Give all a ledger's amounts their canonical display settings.  That
-- is, in each commodity, amounts will use the display settings of the
-- first amount detected, and the greatest precision of the amounts
-- detected.
-- Also, missing unit prices are added if known from the price history.
-- Also, amounts are converted to cost basis if that flag is active.
-- XXX refactor
canonicaliseAmounts :: Bool -> RawLedger -> RawLedger
canonicaliseAmounts costbasis rl@(RawLedger ms ps ts tls hs f fp ft) = RawLedger ms ps (map fixledgertransaction ts) tls hs f fp ft
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
            commodities = map commodity (concatMap (amounts . tamount) (rawLedgerTransactions rl)
                                         ++ concatMap (amounts . hamount) (historical_prices rl))
            fixprice :: Amount -> Amount
            fixprice a@Amount{price=Just _} = a
            fixprice a@Amount{commodity=c} = a{price=rawLedgerHistoricalPriceFor rl d c}

            -- | Get the price for a commodity on the specified day from the price database, if known.
            -- Does only one lookup step, ie will not look up the price of a price.
            rawLedgerHistoricalPriceFor :: RawLedger -> Day -> Commodity -> Maybe MixedAmount
            rawLedgerHistoricalPriceFor rl d Commodity{symbol=s} = do
              let ps = reverse $ filter ((<= d).hdate) $ filter ((s==).hsymbol) $ sortBy (comparing hdate) $ historical_prices rl
              case ps of (HistoricalPrice{hamount=a}:_) -> Just $ canonicaliseCommodities a
                         _ -> Nothing
                  where
                    canonicaliseCommodities (Mixed as) = Mixed $ map canonicaliseCommodity as
                        where canonicaliseCommodity a@Amount{commodity=Commodity{symbol=s}} =
                                  a{commodity=findWithDefault (error "programmer error: canonicaliseCommodity failed") s canonicalcommoditymap}

-- | Get just the amounts from a ledger, in the order parsed.
rawLedgerAmounts :: RawLedger -> [MixedAmount]
rawLedgerAmounts = map tamount . rawLedgerTransactions

-- | Get just the ammount commodities from a ledger, in the order parsed.
rawLedgerCommodities :: RawLedger -> [Commodity]
rawLedgerCommodities = map commodity . concatMap amounts . rawLedgerAmounts

-- | Get just the amount precisions from a ledger, in the order parsed.
rawLedgerPrecisions :: RawLedger -> [Int]
rawLedgerPrecisions = map precision . rawLedgerCommodities

-- | Close any open timelog sessions using the provided current time.
rawLedgerConvertTimeLog :: LocalTime -> RawLedger -> RawLedger
rawLedgerConvertTimeLog t l0 = l0 { ledger_txns = convertedTimeLog ++ ledger_txns l0
                                  , open_timelog_entries = []
                                  }
    where convertedTimeLog = entriesFromTimeLogEntries t $ open_timelog_entries l0

-- | The (fully specified) date span containing all the raw ledger's transactions,
-- or DateSpan Nothing Nothing if there are none.
rawLedgerDateSpan :: RawLedger -> DateSpan
rawLedgerDateSpan rl
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
