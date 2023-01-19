{-|

Various options to use when reading journal files.
Similar to CliOptions.inputflags, simplifies the journal-reading functions.

-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Read.InputOptions (
-- * Types and helpers for input options
  InputOpts(..)
, HasInputOpts(..)
, definputopts
, forecastPeriod
) where

import Control.Applicative ((<|>))
import Data.Time (Day, addDays)

import Hledger.Data.Types
import Hledger.Data.Journal (journalEndDate)
import Hledger.Data.Dates (nulldate, nulldatespan)
import Hledger.Data.Balancing (BalancingOpts(..), HasBalancingOpts(..), defbalancingopts)
import Hledger.Utils (dbg2, makeHledgerClassyLenses)

data InputOpts = InputOpts {
     -- files_             :: [FilePath]
     mformat_           :: Maybe StorageFormat  -- ^ a file/storage format to try, unless overridden
                                                --   by a filename prefix. Nothing means try all.
    ,mrules_file_       :: Maybe FilePath       -- ^ a conversion rules file to use (when reading CSV)
    ,aliases_           :: [String]             -- ^ account name aliases to apply
    ,anon_              :: Bool                 -- ^ do light anonymisation/obfuscation of the data
    ,new_               :: Bool                 -- ^ read only new transactions since this file was last read
    ,new_save_          :: Bool                 -- ^ save latest new transactions state for next time
    ,pivot_             :: String               -- ^ use the given field's value as the account name
    ,forecast_          :: Maybe DateSpan       -- ^ span in which to generate forecast transactions
    ,reportspan_        :: DateSpan             -- ^ a dirty hack keeping the query dates in InputOpts. This rightfully lives in ReportSpec, but is duplicated here.
    ,auto_              :: Bool                 -- ^ generate automatic postings when journal is parsed ?
    ,infer_equity_      :: Bool                 -- ^ infer equity conversion postings from costs ?
    ,infer_costs_       :: Bool                 -- ^ infer costs from equity conversion postings ? distinct from BalancingOpts{infer_balancing_costs_}
    ,balancingopts_     :: BalancingOpts        -- ^ options for balancing transactions
    ,strict_            :: Bool                 -- ^ do extra error checking (eg, all posted accounts are declared, no prices are inferred)
    ,_ioDay             :: Day                  -- ^ today's date, for use with forecast transactions  XXX this duplicates _rsDay, and should eventually be removed when it's not needed anymore.
 } deriving (Show)

definputopts :: InputOpts
definputopts = InputOpts
    { mformat_           = Nothing
    , mrules_file_       = Nothing
    , aliases_           = []
    , anon_              = False
    , new_               = False
    , new_save_          = True
    , pivot_             = ""
    , forecast_          = Nothing
    , reportspan_        = nulldatespan
    , auto_              = False
    , infer_equity_      = False
    , infer_costs_       = False
    , balancingopts_     = defbalancingopts
    , strict_            = False
    , _ioDay             = nulldate
    }

-- | Get the Maybe the DateSpan to generate forecast options from.
-- This begins on:
-- - the start date supplied to the `--forecast` argument, if present
-- - otherwise, the later of
--   - the report start date if specified with -b/-p/date:
--   - the day after the latest normal (non-periodic) transaction in the journal, if any
-- - otherwise today.
-- It ends on:
-- - the end date supplied to the `--forecast` argument, if present
-- - otherwise the report end date if specified with -e/-p/date:
-- - otherwise 180 days (6 months) from today.
forecastPeriod :: InputOpts -> Journal -> Maybe DateSpan
forecastPeriod iopts j = do
    DateSpan requestedStart requestedEnd <- forecast_ iopts
    let forecastStart = fromEFDay <$> requestedStart <|> max mjournalend (fromEFDay <$> reportStart) <|> Just (_ioDay iopts)
        forecastEnd   = fromEFDay <$> requestedEnd <|> fromEFDay <$> reportEnd <|> (Just $ addDays 180 $ _ioDay iopts)
        mjournalend   = dbg2 "journalEndDate" $ journalEndDate False j  -- ignore secondary dates
        DateSpan reportStart reportEnd = reportspan_ iopts
    return . dbg2 "forecastspan" $ DateSpan (Exact <$> forecastStart) (Exact <$> forecastEnd)

-- ** Lenses

makeHledgerClassyLenses ''InputOpts

instance HasBalancingOpts InputOpts where
    balancingOpts = balancingopts
