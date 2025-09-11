{-|
A partition of time into contiguous spans, for defining reporting periods.
-}
module Hledger.Data.DayPartition
( DayPartition
, boundariesToDayPartition
, boundariesToMaybeDayPartition

, lookupDayPartition
, unionDayPartitions

, dayPartitionToNonEmpty
, dayPartitionToList
, dayPartitionToPeriodData
, dayPartitionToDateSpans
, maybeDayPartitionToDateSpans
, dateSpansToDayPartition
) where

import qualified Data.IntMap.Strict as IM
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Time (Day, addDays)

import Hledger.Data.Dates
import Hledger.Data.PeriodData
import Hledger.Data.Types
import Hledger.Utils


-- | A partition of time into contiguous spans, along with a historical period
-- before any of the spans.
--
-- This is a newtype wrapper around 'PeriodData Day', where the start dates are
-- the keys and the end dates are the values. Spans are stored in inclusive format
-- [start, end]. Note that this differs from 'DateSpan' which uses [start, end)
-- format.
--
-- The constructor is not exported so that we can ensure the spans are valid
-- partitions of time.
newtype DayPartition = DayPartition { dayPartitionToPeriodData :: PeriodData Day } deriving (Eq, Ord, Show)

-- Developer's note. All constructors must guarantee that:
--   1. The value stored in pdperiods has at least one key.
--   2. The value stored in pdpre equals one day before the smallest key in pdperiods.
--   3. The value stored in each entry of pdperiods equals one day before the
--      next largest key, except for the value associated to the largest key.
isValidDayPartition :: DayPartition -> Bool
isValidDayPartition (DayPartition pd) = case ds of
    [] -> False  -- Must be at least one key in pdperiods
    xs -> and $ zipWith isContiguous ((nulldate, h) : xs) xs
  where
    (h, ds) = periodDataToList pd
    isContiguous (_, e) (s, _) = addDays 1 e == s


-- | Construct a 'DayPartition' from a non-empty list of boundary days.
boundariesToDayPartition :: NonEmpty Day -> DayPartition
boundariesToDayPartition xs =
    DayPartition $ periodDataFromList (addDays (-1) b) $ zip (b:bs) (map (addDays (-1)) bs)
  where (b:|bs) = NE.nub $ NE.sort xs

-- | Construct a 'DayPartition' from a list of boundary days, returning
-- 'Nothing' for the empty list.
boundariesToMaybeDayPartition :: [Day] -> Maybe DayPartition
boundariesToMaybeDayPartition = fmap boundariesToDayPartition . NE.nonEmpty


-- | Find the span of a 'DayPartition' which contains a given day.
lookupDayPartition :: Day -> DayPartition -> (Maybe Day, Day)
lookupDayPartition d (DayPartition xs) = lookupPeriodDataOrHistorical d xs

-- | Return the union of two 'DayPartition's if they are consistent, or 'Nothing' otherwise.
unionDayPartitions :: DayPartition -> DayPartition -> Maybe DayPartition
unionDayPartitions (DayPartition (PeriodData h as)) (DayPartition (PeriodData h' as')) =
    if equalIntersection as as' && isValidDayPartition union then Just union else Nothing
  where
    union = DayPartition . PeriodData (min h h') $ as <> as'
    equalIntersection x y = and $ IM.intersectionWith (==) x y


-- | Convert 'DayPartition' to a non-empty list of start and end dates for the periods.
--
-- Note that the end date of each period will be one day before the start date
-- of the next period.
dayPartitionToNonEmpty :: DayPartition -> NonEmpty (Day, Day)
dayPartitionToNonEmpty (DayPartition xs) = NE.fromList . snd $ periodDataToList xs  -- Constructors guarantee this is non-empty

-- | Convert 'DayPartition' to a list of start and end dates for the periods.
--
-- Note that the end date of each period will be one day before the start date
-- of the next period.
dayPartitionToList :: DayPartition -> [(Day, Day)]
dayPartitionToList = NE.toList . dayPartitionToNonEmpty

-- | Convert 'DayPartition' to a list of 'DateSpan's.
--
-- Note that the end date of each period will be equal to the start date of
-- the next period.
dayPartitionToDateSpans :: DayPartition -> [DateSpan]
dayPartitionToDateSpans = map toDateSpan . dayPartitionToList
  where
    toDateSpan (s, e) = DateSpan (toEFDay s) (toEFDay $ addDays 1 e)
    toEFDay = Just . Exact

-- Convert a periodic report 'Maybe DayPartition' to a list of 'DateSpans',
-- replacing the empty case with an appropriate placeholder.
--
-- Note that the end date of each period will be equal to the start date of
-- the next period.
maybeDayPartitionToDateSpans :: Maybe DayPartition -> [DateSpan]
maybeDayPartitionToDateSpans = maybe [DateSpan Nothing Nothing] dayPartitionToDateSpans

-- | Convert a list of 'DateSpan's to a 'DayPartition', or 'Nothing' if it is not well-formed.
--
-- Warning: This can construct ill-formed 'DayPartitions' and can raise errors.
-- It will be eliminated later.
-- PARTIAL:
dateSpansToDayPartition :: [DateSpan] -> Maybe DayPartition
-- Handle the cases of partitions which would arise from journals with no transactions
dateSpansToDayPartition []                           = Nothing
dateSpansToDayPartition [DateSpan Nothing  Nothing]  = Nothing
dateSpansToDayPartition [DateSpan Nothing  (Just _)] = Nothing
dateSpansToDayPartition [DateSpan (Just _) Nothing]  = Nothing
-- Handle properly defined reports
dateSpansToDayPartition (x:xs) = Just . DayPartition $
    periodDataFromList (addDays (-1) . fst $ boundaries x) (map boundaries (x:xs))
  where
    boundaries spn = makeJust (spanStart spn, addDays (-1) <$> spanEnd spn)
    makeJust (Just a, Just b)  = (a, b)
    makeJust ab = error' $ "dateSpansToDayPartition: expected all spans to have start and end dates, but one has " ++ show ab
