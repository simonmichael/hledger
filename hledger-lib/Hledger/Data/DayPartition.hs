{-|
A partition of time into contiguous spans, for defining reporting periods.
-}
module Hledger.Data.DayPartition
( DayPartition
-- * constructors
, boundariesToDayPartition
, boundariesToMaybeDayPartition
-- * conversions
, dayPartitionToNonEmpty
, dayPartitionToList
, dayPartitionToDateSpans
, dayPartitionToPeriodData
, maybeDayPartitionToDateSpans
-- * operations
, unionDayPartitions
, dayPartitionStartEnd
, dayPartitionFind
, splitSpan
, intervalBoundaryBefore
-- * tests
, tests_DayPartition
) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map qualified as M
import Data.Time (Day (..), addDays, addGregorianMonthsClip, addGregorianYearsClip, fromGregorian)

import Hledger.Data.Dates
import Hledger.Data.PeriodData
import Hledger.Data.Types
import Hledger.Utils


-- | A partition of time into one or more contiguous periods,
-- plus a historical period that precedes them.
-- Note 'DayPartition' does not store per-period data - only the periods' start/end dates.

-- Each period is at least one day in length.
-- The historical period is open ended, with no start date.
-- The last period has an end date, but note some queries (like 'dayPartitionFind') ignore that, acting as if the last period is open ended.
-- Only smart constructors are exported, so that a DayPartition always satisfies these invariants.
--
-- This is implemented as a newtype wrapper around 'PeriodData Day', which is a map from date to date.
-- The map's keys are the period start dates, and the values are the corresponding period end dates.
-- Note unlike 'DateSpan', which stores exclusive end dates ( @[start, end)@ ),
-- here both start and end dates are inclusive ( @[start, end]@ ).
--
newtype DayPartition = DayPartition { dayPartitionToPeriodData :: PeriodData Day } deriving (Eq, Ord, Show)


-- constructors:

-- | Construct a 'DayPartition' from a non-empty list of period boundary dates (start dates plus a final exclusive end date).
--
-- >>> boundariesToDayPartition (fromGregorian 2025 01 01 :| [fromGregorian 2025 02 01])
-- DayPartition {dayPartitionToPeriodData = PeriodData{ pdpre = 2024-12-31, pdperiods = fromList [(2025-01-01,2025-01-31)]}}
--
boundariesToDayPartition :: NonEmpty Day -> DayPartition
boundariesToDayPartition xs = DayPartition . periodDataFromList (addDays (-1) b) $ case bs of
    []  -> [(b, b)]  -- If only one boundary is supplied, it ends on the same day
    _:_ -> zip (b:bs) $ map (addDays (-1)) bs  -- Guaranteed non-empty
  where b:|bs = NE.nub $ NE.sort xs

-- | Construct a 'DayPartition' from a list of period boundary dates (start dates plus a final exclusive end date),
-- if it's a non-empty list.
boundariesToMaybeDayPartition :: [Day] -> Maybe DayPartition
boundariesToMaybeDayPartition = fmap boundariesToDayPartition . NE.nonEmpty


-- conversions:

-- | Convert 'DayPartition' to a non-empty list of period start and end dates (both inclusive).
-- Each end date will be one day before the next period's start date.
dayPartitionToNonEmpty :: DayPartition -> NonEmpty (Day, Day)
dayPartitionToNonEmpty (DayPartition xs) = NE.fromList . snd $ periodDataToList xs  -- Constructors guarantee this is non-empty

-- | Convert 'DayPartition' to a list (which will always be non-empty) of period start and end dates (both inclusive).
-- Each end date will be one day before the next period's start date.
dayPartitionToList :: DayPartition -> [(Day, Day)]
dayPartitionToList = NE.toList . dayPartitionToNonEmpty

-- | Convert 'DayPartition' to a list of 'DateSpan's.
-- Each span will end one day before the next span begins
-- (the span's exclusive end date will be equal to the next span's start date).
dayPartitionToDateSpans :: DayPartition -> [DateSpan]
dayPartitionToDateSpans = map toDateSpan . dayPartitionToList
  where
    toDateSpan (s, e) = DateSpan (toEFDay s) (toEFDay $ addDays 1 e)
    toEFDay = Just . Exact

-- Convert a 'Maybe DayPartition' to a list of one or more 'DateSpans'.
-- Each span will end one day before the next span begins
-- (the span's exclusive end date will be equal to the next span's start date).
-- If given Nothing, it returns a single open-ended span.
maybeDayPartitionToDateSpans :: Maybe DayPartition -> [DateSpan]
maybeDayPartitionToDateSpans = maybe [DateSpan Nothing Nothing] dayPartitionToDateSpans


-- operations:

-- | Check that a DayPartition has been constructed correctly,
-- with internal invariants satisfied, as well as the external ones described in 'DayPartition'.
-- Internally, all constructors must guarantee:
-- 1. The pdperiods map contains at least one key and value.
-- 2. The value stored in pdpre is one day before pdperiods' smallest key.
-- 3. Each value stored in pdperiods is one day before the next largest key,
--    (except for the value associated with the largest key).
isValidDayPartition :: DayPartition -> Bool
isValidDayPartition (DayPartition pd) = case ds of
  [] -> False
  xs -> and $ zipWith isContiguous ((nulldate, h) : xs) xs
 where
  (h, ds) = periodDataToList pd
  isContiguous (_, e) (s, _) = addDays 1 e == s

-- | Return the union of two 'DayPartition's if that is a valid 'DayPartition',
-- or 'Nothing' otherwise.
unionDayPartitions :: DayPartition -> DayPartition -> Maybe DayPartition
unionDayPartitions (DayPartition (PeriodData h as)) (DayPartition (PeriodData h' as')) =
  if equalIntersection as as' && isValidDayPartition union then Just union else Nothing
 where
  union = DayPartition . PeriodData (min h h') $ as <> as'
  equalIntersection x y = and $ M.intersectionWith (==) x y

-- | Get this DayPartition's overall start date and end date (both inclusive).
dayPartitionStartEnd :: DayPartition -> (Day, Day)
dayPartitionStartEnd (DayPartition (PeriodData _ ds)) =
  -- Guaranteed not to error because the IntMap is non-empty.
  (intToDay . fst $ M.findMin ds, snd $ M.findMax ds)

-- | Find the start and end dates of the period within a 'DayPartition' which contains a given day.
-- If the day is after the end of the last period, it is assumed to be within the last period.
-- If the day is before the start of the first period (ie, in the historical period),
-- only the historical period's end date is returned.
dayPartitionFind :: Day -> DayPartition -> (Maybe Day, Day)
dayPartitionFind d (DayPartition xs) = lookupPeriodDataOrHistorical d xs

-- | Split a 'DateSpan' into a 'DayPartition' consisting of consecutive exact
-- spans of the specified Interval, or `Nothing` if the span is invalid.
-- If no interval is specified, the original span is returned.
-- If the original span is the null date span, ie unbounded, `Nothing` is returned.
-- If the original span is empty, eg if the end date is <= the start date, `Nothing` is returned.
--
-- ==== Date adjustment
-- Some intervals respect the "adjust" flag (years, quarters, months, weeks, every Nth weekday
-- of month seem to be the ones that need it). This will move the start date earlier, if needed,
-- to the previous natural interval boundary (first of year, first of quarter, first of month,
-- monday, previous Nth weekday of month). Related: #1982 #2218
--
-- The end date is always moved later if needed to the next natural interval boundary,
-- so that the last period is the same length as the others.
--
-- ==== Examples
-- >>> let t i y1 m1 d1 y2 m2 d2 = fmap dayPartitionToNonEmpty . splitSpan True i $ DateSpan (Just $ Flex $ fromGregorian y1 m1 d1) (Just $ Flex $ fromGregorian y2 m2 d2)
-- >>> t NoInterval 2008 01 01 2009 01 01
-- Just ((2008-01-01,2008-12-31) :| [])
-- >>> t (Quarters 1) 2008 01 01 2009 01 01
-- Just ((2008-01-01,2008-03-31) :| [(2008-04-01,2008-06-30),(2008-07-01,2008-09-30),(2008-10-01,2008-12-31)])
-- >>> splitSpan True (Quarters 1) nulldatespan
-- Nothing
-- >>> t (Days 1) 2008 01 01 2008 01 01  -- an empty datespan
-- Nothing
-- >>> t (Quarters 1) 2008 01 01 2008 01 01
-- Nothing
-- >>> t (Months 1) 2008 01 01 2008 04 01
-- Just ((2008-01-01,2008-01-31) :| [(2008-02-01,2008-02-29),(2008-03-01,2008-03-31)])
-- >>> t (Months 2) 2008 01 01 2008 04 01
-- Just ((2008-01-01,2008-02-29) :| [(2008-03-01,2008-04-30)])
-- >>> t (Weeks 1) 2008 01 01 2008 01 15
-- Just ((2007-12-31,2008-01-06) :| [(2008-01-07,2008-01-13),(2008-01-14,2008-01-20)])
-- >>> t (Weeks 2) 2008 01 01 2008 01 15
-- Just ((2007-12-31,2008-01-13) :| [(2008-01-14,2008-01-27)])
-- >>> t (MonthDay 2) 2008 01 01 2008 04 01
-- Just ((2008-01-02,2008-02-01) :| [(2008-02-02,2008-03-01),(2008-03-02,2008-04-01)])
-- >>> t (NthWeekdayOfMonth 2 4) 2011 01 01 2011 02 15
-- Just ((2010-12-09,2011-01-12) :| [(2011-01-13,2011-02-09),(2011-02-10,2011-03-09)])
-- >>> t (DaysOfWeek [2]) 2011 01 01 2011 01 15
-- Just ((2010-12-28,2011-01-03) :| [(2011-01-04,2011-01-10),(2011-01-11,2011-01-17)])
-- >>> t (MonthAndDay 11 29) 2012 10 01 2013 10 15
-- Just ((2012-11-29,2013-11-28) :| [])
splitSpan :: Bool -> Interval -> DateSpan -> Maybe DayPartition
splitSpan _      _                        (DateSpan Nothing Nothing) = Nothing
splitSpan _      _                        ds | isEmptySpan ds = Nothing
splitSpan _      NoInterval               (DateSpan (Just s) (Just e)) = Just $ boundariesToDayPartition (fromEFDay s :| [fromEFDay e])
splitSpan _      NoInterval               _  = Nothing
splitSpan _      (Days n)                 ds = splitspan id addDays n ds
splitSpan adjust (Weeks n)                ds = splitspan (if adjust then startofweek    else id) addDays                 (7*n) ds
splitSpan adjust (Months n)               ds = splitspan (if adjust then startofmonth   else id) addGregorianMonthsClip  n     ds
splitSpan adjust (Quarters n)             ds = splitspan (if adjust then startofquarter else id) addGregorianMonthsClip  (3*n) ds
splitSpan adjust (Years n)                ds = splitspan (if adjust then startofyear    else id) addGregorianYearsClip   n     ds
splitSpan adjust (NthWeekdayOfMonth n wd) ds = splitspan (startWeekdayOfMonth n wd)              advancemonths           1     ds
  where
    startWeekdayOfMonth = if adjust then prevNthWeekdayOfMonth else nextNthWeekdayOfMonth
    advancemonths 0 = id
    advancemonths m = advanceToNthWeekday n wd . startofmonth . addGregorianMonthsClip m
splitSpan _      (MonthDay dom)           ds = splitspan (nextnthdayofmonth dom) (addGregorianMonthsToMonthday dom) 1 ds
splitSpan _      (MonthAndDay m d)        ds = splitspan (nextmonthandday m d)   addGregorianYearsClip              1 ds
splitSpan _      (DaysOfWeek [])          _  = Nothing
splitSpan _      (DaysOfWeek days@(n:_))  ds = do
    (s, e) <- dateSpanSplitLimits (nthdayofweekcontaining n) nextday ds
    let -- can't show this when debugging, it'll hang:
        bdrys = concatMap (\d -> map (addDays d) starts) [0,7..]
        -- The first representative of each weekday
        starts = map (\d -> addDays (toInteger $ d - n) $ nthdayofweekcontaining n s) days
    spansFromBoundaries e bdrys

-- | Fill in missing start/end dates for calculating 'splitSpan'.
dateSpanSplitLimits :: (Day -> Day) -> (Day -> Day) -> DateSpan -> Maybe (Day, Day)
dateSpanSplitLimits _     _    (DateSpan Nothing   Nothing) = Nothing
dateSpanSplitLimits _     _    ds | isEmptySpan ds          = Nothing
dateSpanSplitLimits start _    (DateSpan (Just s) (Just e)) = Just (start $ fromEFDay s, fromEFDay e)
dateSpanSplitLimits start next (DateSpan (Just s) Nothing)  = Just (start $ fromEFDay s, next $ start $ fromEFDay s)
dateSpanSplitLimits start next (DateSpan Nothing  (Just e)) = Just (start $ fromEFDay e, next $ start $ fromEFDay e)

-- Split the given span into exact spans using the provided helper functions:
--
-- 1. The start function is used to adjust the provided span's start date to get the first sub-span's start date.
--
-- 2. The next function is used to calculate subsequent sub-spans' start dates, possibly with stride increased by a multiplier.
--    It should handle spans of varying length, eg when splitting on "every 31st of month",
--    it adjusts to 28/29/30 in short months but returns to 31 in the long months.
splitspan :: (Day -> Day) -> (Integer -> Day -> Day) -> Int -> DateSpan -> Maybe DayPartition
splitspan start next mult ds = do
    (s, e) <- dateSpanSplitLimits start (next (toInteger mult)) ds
    let bdrys = mapM (next . toInteger) [0,mult..] $ start s
    spansFromBoundaries e bdrys

-- | Construct a list of exact 'DateSpan's from a list of boundaries, which fit within a given range.
spansFromBoundaries :: Day -> [Day] -> Maybe DayPartition
spansFromBoundaries _ []     = Nothing
spansFromBoundaries e (x:_)  | x >= e = Nothing
spansFromBoundaries e (x:xs) = Just . boundariesToDayPartition $ takeUntilFailsNE (<e) (x:|xs)

-- | Get the natural start for the given interval that falls on or before the given day,
-- when applicable. Works for Weeks, Months, Quarters, Years, eg.
intervalBoundaryBefore :: Interval -> Day -> Day
intervalBoundaryBefore i d =
  case dayPartitionToNonEmpty <$> splitSpan True i (DateSpan (Just $ Exact d) (Just . Exact $ addDays 1 d)) of
    Just ((start, _) :| _ ) -> start
    _ -> d

intToDay = ModifiedJulianDay . toInteger


-- tests:

tests_DayPartition =
  testGroup "splitSpan" [
      testCase "weekday" $ do
        fmap dayPartitionToNonEmpty (splitSpan False (DaysOfWeek [1..5]) (DateSpan (Just $ Exact $ fromGregorian 2021 07 01) (Just $ Exact $ fromGregorian 2021 07 08)))
          @?= Just ( (fromGregorian 2021 06 28, fromGregorian 2021 06 28) :|
                   [ (fromGregorian 2021 06 29, fromGregorian 2021 06 29)
                   , (fromGregorian 2021 06 30, fromGregorian 2021 06 30)
                   , (fromGregorian 2021 07 01, fromGregorian 2021 07 01)
                   , (fromGregorian 2021 07 02, fromGregorian 2021 07 04)
                   -- next week
                   , (fromGregorian 2021 07 05, fromGregorian 2021 07 05)
                   , (fromGregorian 2021 07 06, fromGregorian 2021 07 06)
                   , (fromGregorian 2021 07 07, fromGregorian 2021 07 07)
                   ])

        fmap dayPartitionToNonEmpty (splitSpan False (DaysOfWeek [1, 5]) (DateSpan (Just $ Exact $ fromGregorian 2021 07 01) (Just $ Exact $ fromGregorian 2021 07 08)))
          @?= Just ( (fromGregorian 2021 06 28, fromGregorian 2021 07 01) :|
                   [ (fromGregorian 2021 07 02, fromGregorian 2021 07 04)
                   -- next week
                   , (fromGregorian 2021 07 05, fromGregorian 2021 07 08)
                   ])

    , testCase "match dayOfWeek" $ do
        let dayofweek n = splitspan (nthdayofweekcontaining n) (\w -> (if w == 0 then id else applyN (n-1) nextday . applyN (fromInteger w) nextweek)) 1
            matchdow ds day = splitSpan False (DaysOfWeek [day]) ds @?= dayofweek day ds
            ys2021 = fromGregorian 2021 01 01
            ye2021 = fromGregorian 2021 12 31
            ys2022 = fromGregorian 2022 01 01
        mapM_ (matchdow (DateSpan (Just $ Exact ys2021) (Just $ Exact ye2021))) [1..7]
        mapM_ (matchdow (DateSpan (Just $ Exact ys2021) (Just $ Exact ys2022))) [1..7]
        mapM_ (matchdow (DateSpan (Just $ Exact ye2021) (Just $ Exact ys2022))) [1..7]

        mapM_ (matchdow (DateSpan (Just $ Exact ye2021) Nothing)) [1..7]
        mapM_ (matchdow (DateSpan (Just $ Exact ys2022) Nothing)) [1..7]

        mapM_ (matchdow (DateSpan Nothing (Just $ Exact ye2021))) [1..7]
        mapM_ (matchdow (DateSpan Nothing (Just $ Exact ys2022))) [1..7]

    ]
