{- | UIState operations. -}

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Hledger.UI.UIState
where

import Brick.Widgets.Edit
import Data.List ((\\), foldl', sort)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Max(..))
import qualified Data.Text as T
import Data.Text.Zipper (gotoEOL)
import Data.Time.Calendar (Day)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.UI.UITypes
import Hledger.UI.UIOptions

-- | Toggle between showing only unmarked items or all items.
toggleUnmarked :: UIState -> UIState
toggleUnmarked ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=reportSpecToggleStatus Unmarked copts rspec}}}

-- | Toggle between showing only pending items or all items.
togglePending :: UIState -> UIState
togglePending ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=reportSpecToggleStatus Pending copts rspec}}}

-- | Toggle between showing only cleared items or all items.
toggleCleared :: UIState -> UIState
toggleCleared ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=reportSpecToggleStatus Cleared copts rspec}}}

-- TODO testing different status toggle styles

-- | Generate zero or more indicators of the status filters currently active,
-- which will be shown comma-separated as part of the indicators list.
uiShowStatus :: CliOpts -> [Status] -> [String]
uiShowStatus copts ss =
  case style of
    -- in style 2, instead of "Y, Z" show "not X"
    Just 2 | length ss == numstatuses-1
      -> map (("not "++). showstatus) $ sort $ complement ss  -- should be just one
    _ -> map showstatus $ sort ss
  where
    numstatuses = length [minBound..maxBound::Status]
    style = maybeposintopt "status-toggles" $ rawopts_ copts
    showstatus Cleared  = "cleared"
    showstatus Pending  = "pending"
    showstatus Unmarked = "unmarked"

reportSpecToggleStatus :: Status -> CliOpts -> ReportSpec -> ReportSpec
reportSpecToggleStatus s _copts =
    either (error "reportSpecToggleStatus: changing status should not have caused this error") id  -- PARTIAL:
    . updateReportSpecWith (reportOptsToggleStatus1 s)

-- various toggle behaviours:

-- 1 UPC toggles only X/all
reportOptsToggleStatus1 s ropts@ReportOpts{statuses_=ss}
  | ss == [s]  = ropts{statuses_=[]}
  | otherwise = ropts{statuses_=[s]}

-- 2 UPC cycles X/not-X/all
-- repeatedly pressing X cycles:
-- [] U [u]
-- [u] U [pc]
-- [pc] U []
-- pressing Y after first or second step starts new cycle:
-- [u] P [p]
-- [pc] P [p]
-- reportOptsToggleStatus2 s ropts@ReportOpts{statuses_=ss}
--   | ss == [s]            = ropts{statuses_=complement [s]}
--   | ss == complement [s] = ropts{statuses_=[]}
--   | otherwise            = ropts{statuses_=[s]}  -- XXX assume only three values

-- 3 UPC toggles each X
-- reportOptsToggleStatus3 s ropts@ReportOpts{statuses_=ss}
--   | s `elem` ss = ropts{statuses_=filter (/= s) ss}
--   | otherwise   = ropts{statuses_=simplifyStatuses (s:ss)}

-- 4 upc sets X, UPC sets not-X
--reportOptsToggleStatus4 s ropts@ReportOpts{statuses_=ss}
--  | s `elem` ss = ropts{statuses_=filter (/= s) ss}
--  | otherwise   = ropts{statuses_=simplifyStatuses (s:ss)}

-- 5 upc toggles X, UPC toggles not-X
--reportOptsToggleStatus5 s ropts@ReportOpts{statuses_=ss}
--  | s `elem` ss = ropts{statuses_=filter (/= s) ss}
--  | otherwise   = ropts{statuses_=simplifyStatuses (s:ss)}

-- | Given a list of unique enum values, list the other possible values of that enum.
complement :: (Bounded a, Enum a, Eq a) => [a] -> [a]
complement = ([minBound..maxBound] \\)

--

-- | Toggle between showing all and showing only nonempty (more precisely, nonzero) items.
toggleEmpty :: UIState -> UIState
toggleEmpty ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=toggleEmpty ropts}}}}
  where
    toggleEmpty ropts = ropts{empty_=not $ empty_ ropts}

-- | Show primary amounts, not cost or value.
clearCostValue :: UIState -> UIState
clearCostValue ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{value_ = plog "clearing value mode" Nothing}}}}}

-- | Toggle between showing the primary amounts or costs.
toggleCost :: UIState -> UIState
toggleCost ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
    ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{cost_ = toggle $ cost_ ropts}}}}}
  where
    toggle Cost   = NoCost
    toggle NoCost = Cost

-- | Toggle between showing primary amounts or default valuation.
toggleValue :: UIState -> UIState
toggleValue ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{
    value_ = valuationToggleValue $ value_ ropts}}}}}

-- | Basic toggling of -V, for hledger-ui.
valuationToggleValue :: Maybe ValuationType -> Maybe ValuationType
valuationToggleValue (Just (AtEnd _)) = Nothing
valuationToggleValue _                = Just $ AtEnd Nothing

-- | Set hierarchic account tree mode.
setTree :: UIState -> UIState
setTree ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{accountlistmode_=ALTree}}}}}

-- | Set flat account list mode.
setList :: UIState -> UIState
setList ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{accountlistmode_=ALFlat}}}}}

-- | Toggle between flat and tree mode. If current mode is unspecified/default, assume it's flat.
toggleTree :: UIState -> UIState
toggleTree ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=toggleTreeMode ropts}}}}
  where
    toggleTreeMode ropts
      | accountlistmode_ ropts == ALTree = ropts{accountlistmode_=ALFlat}
      | otherwise                        = ropts{accountlistmode_=ALTree}

-- | Toggle between historical balances and period balances.
toggleHistorical :: UIState -> UIState
toggleHistorical ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{balancetype_=b}}}}}
  where
    b | balancetype_ ropts == HistoricalBalance = PeriodChange
      | otherwise                               = HistoricalBalance

-- | Toggle hledger-ui's "forecast/future mode". When this mode is enabled,
-- hledger-shows regular transactions which have future dates, and
-- "forecast" transactions generated by periodic transaction rules
-- (which are usually but not necessarily future-dated).
-- In normal mode, both of these are hidden.
toggleForecast :: Day -> UIState -> UIState
toggleForecast d ui@UIState{aopts=UIOpts{cliopts_=copts@CliOpts{reportspec_=ReportSpec{rsOpts=ropts}}}} =
  uiSetForecast ui $
    case forecast_ ropts of
      Just _  -> Nothing
      Nothing -> Just $ fromMaybe nulldatespan $ forecastPeriodFromRawOpts d $ rawopts_ copts

-- | Helper: set forecast mode (with the given forecast period) on or off in the UI state.
uiSetForecast :: UIState -> Maybe DateSpan -> UIState
uiSetForecast
  ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}}
  mforecast =
  -- we assume forecast mode has no effect on ReportSpec's derived fields
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{forecast_=mforecast}}}}}

-- | Toggle between showing all and showing only real (non-virtual) items.
toggleReal :: UIState -> UIState
toggleReal ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec}}} =
    ui{aopts=uopts{cliopts_=copts{reportspec_=update rspec}}}
  where
    update = either (error "toggleReal: updating Real should not result in an error") id  -- PARTIAL:
           . updateReportSpecWith (\ropts -> ropts{real_=not $ real_ ropts})

-- | Toggle the ignoring of balance assertions.
toggleIgnoreBalanceAssertions :: UIState -> UIState
toggleIgnoreBalanceAssertions ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{inputopts_=iopts@InputOpts{balancingopts_=bopts}}}} =
  ui{aopts=uopts{cliopts_=copts{inputopts_=iopts{balancingopts_=bopts{ignore_assertions_=not $ ignore_assertions_ bopts}}}}}

-- | Step through larger report periods, up to all.
growReportPeriod :: Day -> UIState -> UIState
growReportPeriod _d = updateReportPeriod periodGrow

-- | Step through smaller report periods, down to a day.
shrinkReportPeriod :: Day -> UIState -> UIState
shrinkReportPeriod d = updateReportPeriod (periodShrink d)

-- | Step the report start/end dates to the next period of same duration,
-- remaining inside the given enclosing span.
nextReportPeriod :: DateSpan -> UIState -> UIState
nextReportPeriod enclosingspan = updateReportPeriod (periodNextIn enclosingspan)

-- | Step the report start/end dates to the next period of same duration,
-- remaining inside the given enclosing span.
previousReportPeriod :: DateSpan -> UIState -> UIState
previousReportPeriod enclosingspan = updateReportPeriod (periodPreviousIn enclosingspan)

-- | If a standard report period is set, step it forward/backward if needed so that
-- it encloses the given date.
moveReportPeriodToDate :: Day -> UIState -> UIState
moveReportPeriodToDate d = updateReportPeriod (periodMoveTo d)

-- | Get the report period.
reportPeriod :: UIState -> Period
reportPeriod = period_ . rsOpts . reportspec_ . cliopts_ . aopts

-- | Set the report period.
setReportPeriod :: Period -> UIState -> UIState
setReportPeriod p = updateReportPeriod (const p)

-- | Clear any report period limits.
resetReportPeriod :: UIState -> UIState
resetReportPeriod = setReportPeriod PeriodAll

-- | Update report period by a applying a function.
updateReportPeriod :: (Period -> Period) -> UIState -> UIState
updateReportPeriod updatePeriod ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec}}} =
    ui{aopts=uopts{cliopts_=copts{reportspec_=update rspec}}}
  where
    update = either (error "updateReportPeriod: updating period should not result in an error") id  -- PARTIAL:
           . updateReportSpecWith (\ropts -> ropts{period_=updatePeriod $ period_ ropts})

-- | Apply a new filter query.
setFilter :: String -> UIState -> UIState
setFilter s ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec}}} =
    ui{aopts=uopts{cliopts_=copts{reportspec_=update rspec}}}
  where
    update = either (const rspec) id . updateReportSpecWith (\ropts -> ropts{querystring_=querystring})  -- XXX silently ignores an error
    querystring = words'' prefixes $ T.pack s

-- | Reset some filters & toggles.
resetFilter :: UIState -> UIState
resetFilter ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{
     rsQuery=Any
    ,rsQueryOpts=[]
    ,rsOpts=ropts{
       empty_=True
      ,statuses_=[]
      ,real_=False
      ,querystring_=[]
      --,period_=PeriodAll
    }}}}}

-- | Reset all options state to exactly what it was at startup
-- (preserving any command-line options/arguments).
resetOpts :: UIState -> UIState
resetOpts ui@UIState{astartupopts} = ui{aopts=astartupopts}

resetDepth :: UIState -> UIState
resetDepth = updateReportDepth (const Nothing)

-- | Get the maximum account depth in the current journal.
maxDepth :: UIState -> Int
maxDepth UIState{ajournal=j} = getMax . foldMap (Max . accountNameLevel) $ journalAccountNamesDeclaredOrImplied j

-- | Decrement the current depth limit towards 0. If there was no depth limit,
-- set it to one less than the maximum account depth.
decDepth :: UIState -> UIState
decDepth ui = updateReportDepth dec ui
  where
    dec (Just d) = Just $ max 0 (d-1)
    dec Nothing  = Just $ maxDepth ui - 1

-- | Increment the current depth limit. If this makes it equal to the
-- the maximum account depth, remove the depth limit.
incDepth :: UIState -> UIState
incDepth = updateReportDepth (fmap succ)

-- | Set the current depth limit to the specified depth, or remove the depth limit.
-- Also remove the depth limit if the specified depth is greater than the current
-- maximum account depth. If the specified depth is negative, reset the depth limit
-- to whatever was specified at uiartup.
setDepth :: Maybe Int -> UIState -> UIState
setDepth mdepth = updateReportDepth (const mdepth)

getDepth :: UIState -> Maybe Int
getDepth = depth_ . rsOpts . reportspec_ . cliopts_ . aopts

-- | Update report depth by a applying a function. If asked to set a depth less
-- than zero, it will leave it unchanged.
updateReportDepth :: (Maybe Int -> Maybe Int) -> UIState -> UIState
updateReportDepth updateDepth ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec}}} =
    ui{aopts=uopts{cliopts_=copts{reportspec_=update rspec}}}
  where
    update = either (error "updateReportDepth: updating depth should not result in an error") id  -- PARTIAL:
           . updateReportSpecWith (\ropts -> ropts{depth_=updateDepth (depth_ ropts) >>= clipDepth ropts})
    clipDepth ropts d | d < 0            = depth_ ropts
                      | d >= maxDepth ui = Nothing
                      | otherwise        = Just d

-- | Open the minibuffer, setting its content to the current query with the cursor at the end.
showMinibuffer :: UIState -> UIState
showMinibuffer ui = setMode (Minibuffer e) ui
  where
    e = applyEdit gotoEOL $ editor MinibufferEditor (Just 1) oldq
    oldq = T.unpack . T.unwords . map textQuoteIfNeeded
         . querystring_ . rsOpts . reportspec_ . cliopts_ $ aopts ui

-- | Close the minibuffer, discarding any edit in progress.
closeMinibuffer :: UIState -> UIState
closeMinibuffer = setMode Normal

setMode :: Mode -> UIState -> UIState
setMode m ui = ui{aMode=m}

-- | Regenerate the content for the current and previous screens, from a new journal and current date.
regenerateScreens :: Journal -> Day -> UIState -> UIState
regenerateScreens j d ui@UIState{aScreen=s,aPrevScreens=ss} =
  -- XXX clumsy due to entanglement of UIState and Screen.
  -- sInit operates only on an appstate's current screen, so
  -- remove all the screens from the appstate and then add them back
  -- one at a time, regenerating as we go.
  let
    first:rest = reverse $ s:ss :: [Screen]
    ui0 = ui{ajournal=j, aScreen=first, aPrevScreens=[]} :: UIState

    ui1 = (sInit first) d False ui0 :: UIState
    ui2 = foldl' (\ui s -> (sInit s) d False $ pushScreen s ui) ui1 rest :: UIState
  in
    ui2

pushScreen :: Screen -> UIState -> UIState
pushScreen scr ui = ui{aPrevScreens=(aScreen ui:aPrevScreens ui)
                      ,aScreen=scr
                      }

popScreen :: UIState -> UIState
popScreen ui@UIState{aPrevScreens=s:ss} = ui{aScreen=s, aPrevScreens=ss}
popScreen ui = ui

resetScreens :: Day -> UIState -> UIState
resetScreens d ui@UIState{aScreen=s,aPrevScreens=ss} =
  (sInit topscreen) d True $
  resetOpts $
  closeMinibuffer ui{aScreen=topscreen, aPrevScreens=[]}
  where
    topscreen = case ss of _:_ -> last ss
                           []  -> s

-- | Enter a new screen, saving the old screen & state in the
-- navigation history and initialising the new screen's state.
screenEnter :: Day -> Screen -> UIState -> UIState
screenEnter d scr ui = (sInit scr) d True $
                       pushScreen scr
                       ui

