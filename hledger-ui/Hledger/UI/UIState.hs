{- | UIState operations. -}

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hledger.UI.UIState
where

import Brick.Widgets.Edit
import Data.List ((\\), foldl', sort)
import Data.Maybe (fromMaybe)
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
  ui{aopts=uopts{cliopts_=copts{reportspec_=reportSpecToggleStatusSomehow Unmarked copts rspec}}}

-- | Toggle between showing only pending items or all items.
togglePending :: UIState -> UIState
togglePending ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=reportSpecToggleStatusSomehow Pending copts rspec}}}

-- | Toggle between showing only cleared items or all items.
toggleCleared :: UIState -> UIState
toggleCleared ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=reportSpecToggleStatusSomehow Cleared copts rspec}}}

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

reportSpecToggleStatusSomehow :: Status -> CliOpts -> ReportSpec -> ReportSpec
reportSpecToggleStatusSomehow s copts rspec = rspec{rsOpts=ropts}
  where
    ropts = case maybeposintopt "status-toggles" $ rawopts_ copts of
      Just 2 -> reportOptsToggleStatus2 s ropts
      Just 3 -> reportOptsToggleStatus3 s ropts
--      Just 4 -> reportOptsToggleStatus4 s ropts
--      Just 5 -> reportOptsToggleStatus5 s ropts
      _      -> reportOptsToggleStatus1 s ropts

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
reportOptsToggleStatus2 s ropts@ReportOpts{statuses_=ss}
  | ss == [s]            = ropts{statuses_=complement [s]}
  | ss == complement [s] = ropts{statuses_=[]}
  | otherwise            = ropts{statuses_=[s]}  -- XXX assume only three values

-- 3 UPC toggles each X
reportOptsToggleStatus3 s ropts@ReportOpts{statuses_=ss}
  | s `elem` ss = ropts{statuses_=filter (/= s) ss}
  | otherwise   = ropts{statuses_=simplifyStatuses (s:ss)}

-- 4 upc sets X, UPC sets not-X
--reportOptsToggleStatus4 s ropts@ReportOpts{statuses_=ss}
--  | s `elem` ss = ropts{statuses_=filter (/= s) ss}
--  | otherwise   = ropts{statuses_=simplifyStatuses (s:ss)}
--
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
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{value_ = valuationToggleCost $ value_ ropts}}}}}

-- | Toggle between showing primary amounts or default valuation.
toggleValue :: UIState -> UIState
toggleValue ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{
    value_ = plog "toggling value mode to" $ valuationToggleValue $ value_ ropts}}}}}

-- | Basic toggling of -B/cost, for hledger-ui.
valuationToggleCost :: Maybe ValuationType -> Maybe ValuationType
valuationToggleCost (Just (AtCost _)) = Nothing
valuationToggleCost _                 = Just $ AtCost Nothing

-- | Basic toggling of -V, for hledger-ui.
valuationToggleValue :: Maybe ValuationType -> Maybe ValuationType
valuationToggleValue (Just (AtDefault _)) = Nothing
valuationToggleValue _                    = Just $ AtDefault Nothing

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

-- | Toggle hledger-ui's "forecast mode". In forecast mode, periodic
-- transactions (generated by periodic rules) are enabled (as with
-- hledger --forecast), and also future transactions in general
-- (including non-periodic ones) are displayed. In normal mode, all
-- future transactions (periodic or not) are suppressed (unlike
-- command-line hledger).
--
-- After toggling this, we do a full reload of the journal from disk
-- to make it take effect; currently that's done in the callers (cf
-- AccountsScreen, RegisterScreen) where it's easier. This is
-- overkill, probably we should just hide/show the periodic
-- transactions with a query for their special tag.
--
toggleForecast :: Day -> UIState -> UIState
toggleForecast d ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts'}}
  where
    copts' = copts{reportspec_=rspec{rsOpts=ropts{forecast_=forecast'}}}
    forecast' =
      case forecast_ ropts of
        Just _  -> Nothing
        Nothing -> Just $ fromMaybe nulldatespan $ forecastPeriodFromRawOpts d $ rawopts_ copts

-- | Toggle between showing all and showing only real (non-virtual) items.
toggleReal :: UIState -> UIState
toggleReal ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=toggleReal ropts}}}}
  where
    toggleReal ropts = ropts{real_=not $ real_ ropts}

-- | Toggle the ignoring of balance assertions.
toggleIgnoreBalanceAssertions :: UIState -> UIState
toggleIgnoreBalanceAssertions ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{inputopts_=iopts}}} =
  ui{aopts=uopts{cliopts_=copts{inputopts_=iopts{ignore_assertions_=not $ ignore_assertions_ iopts}}}}

-- | Step through larger report periods, up to all.
growReportPeriod :: Day -> UIState -> UIState
growReportPeriod _d ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{period_=periodGrow $ period_ ropts}}}}}

-- | Step through smaller report periods, down to a day.
shrinkReportPeriod :: Day -> UIState -> UIState
shrinkReportPeriod d ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{period_=periodShrink d $ period_ ropts}}}}}

-- | Step the report start/end dates to the next period of same duration,
-- remaining inside the given enclosing span.
nextReportPeriod :: DateSpan -> UIState -> UIState
nextReportPeriod enclosingspan ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts@ReportOpts{period_=p}}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{period_=periodNextIn enclosingspan p}}}}}

-- | Step the report start/end dates to the next period of same duration,
-- remaining inside the given enclosing span.
previousReportPeriod :: DateSpan -> UIState -> UIState
previousReportPeriod enclosingspan ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts@ReportOpts{period_=p}}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{period_=periodPreviousIn enclosingspan p}}}}}

-- | If a standard report period is set, step it forward/backward if needed so that
-- it encloses the given date.
moveReportPeriodToDate :: Day -> UIState -> UIState
moveReportPeriodToDate d ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts@ReportOpts{period_=p}}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{period_=periodMoveTo d p}}}}}

-- | Get the report period.
reportPeriod :: UIState -> Period
reportPeriod UIState{aopts=UIOpts{cliopts_=CliOpts{reportspec_=ReportSpec{rsOpts=ReportOpts{period_=p}}}}} =
  p

-- | Set the report period.
setReportPeriod :: Period -> UIState -> UIState
setReportPeriod p ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{period_=p}}}}}

-- | Clear any report period limits.
resetReportPeriod :: UIState -> UIState
resetReportPeriod = setReportPeriod PeriodAll

-- | Apply a new filter query.
setFilter :: String -> UIState -> UIState
setFilter s ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
    ui{aopts=uopts{cliopts_=copts{reportspec_=newrspec}}}
  where
    newrspec = either (const rspec) id $ reportOptsToSpec (rsToday rspec) ropts{querystring_=querystring}
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
resetDepth ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}} =
  ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{depth_=Nothing}}}}}

-- | Get the maximum account depth in the current journal.
maxDepth :: UIState -> Int
maxDepth UIState{ajournal=j} = maximum $ map accountNameLevel $ journalAccountNames j

-- | Decrement the current depth limit towards 0. If there was no depth limit,
-- set it to one less than the maximum account depth.
decDepth :: UIState -> UIState
decDepth ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts@ReportOpts{..}}}}}
  = ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{depth_=dec depth_}}}}}
  where
    dec (Just d) = Just $ max 0 (d-1)
    dec Nothing  = Just $ maxDepth ui - 1

-- | Increment the current depth limit. If this makes it equal to the
-- the maximum account depth, remove the depth limit.
incDepth :: UIState -> UIState
incDepth ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts@ReportOpts{..}}}}}
  = ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{depth_=inc depth_}}}}}
  where
    inc (Just d) | d < (maxDepth ui - 1) = Just $ d+1
    inc _ = Nothing

-- | Set the current depth limit to the specified depth, or remove the depth limit.
-- Also remove the depth limit if the specified depth is greater than the current
-- maximum account depth. If the specified depth is negative, reset the depth limit
-- to whatever was specified at uiartup.
setDepth :: Maybe Int -> UIState -> UIState
setDepth mdepth ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}}
  = ui{aopts=uopts{cliopts_=copts{reportspec_=rspec{rsOpts=ropts{depth_=mdepth'}}}}}
  where
    mdepth' = case mdepth of
                Nothing                   -> Nothing
                Just d | d < 0            -> depth_ ropts
                       | d >= maxDepth ui -> Nothing
                       | otherwise        -> mdepth

getDepth :: UIState -> Maybe Int
getDepth UIState{aopts=UIOpts{cliopts_=CliOpts{reportspec_=rspec}}} = depth_ $ rsOpts rspec

-- | Open the minibuffer, setting its content to the current query with the cursor at the end.
showMinibuffer :: UIState -> UIState
showMinibuffer ui = setMode (Minibuffer e) ui
  where
    e = applyEdit gotoEOL $ editor MinibufferEditor (Just 1) oldq
    oldq = unwords . map (quoteIfNeeded . T.unpack)
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

