{- | UIState operations. -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hledger.UI.UIState
where

import Brick
import Brick.Widgets.Edit
import Data.List
import Data.Text.Zipper (gotoEOL)
import Data.Time.Calendar (Day)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.UI.UITypes
import Hledger.UI.UIOptions

-- | Toggle between showing only cleared items or all items.
toggleCleared :: UIState -> UIState
toggleCleared ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=toggleCleared ropts}}}
  where
    toggleCleared ropts@ReportOpts{clearedstatus_=Just ClearedFilter} = ropts{clearedstatus_=Nothing}
    toggleCleared ropts = ropts{clearedstatus_=Just ClearedFilter}

-- | Toggle between showing only pending items or all items.
togglePending :: UIState -> UIState
togglePending ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=togglePending ropts}}}
  where
    togglePending ropts@ReportOpts{clearedstatus_=Just PendingFilter} = ropts{clearedstatus_=Nothing}
    togglePending ropts = ropts{clearedstatus_=Just PendingFilter}

-- | Toggle between showing only uncleared items or all items.
toggleUncleared :: UIState -> UIState
toggleUncleared ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=toggleUncleared ropts}}}
  where
    toggleUncleared ropts@ReportOpts{clearedstatus_=Just UnclearedFilter} = ropts{clearedstatus_=Nothing}
    toggleUncleared ropts = ropts{clearedstatus_=Just UnclearedFilter}

-- | Toggle between showing only non-pending items or all items.
toggleNotPending :: UIState -> UIState
toggleNotPending ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=toggleNotPending ropts}}}
  where
    toggleNotPending ropts@ReportOpts{clearedstatus_=Just NotPendingFilter} = ropts{clearedstatus_=Nothing}
    toggleNotPending ropts = ropts{clearedstatus_=Just NotPendingFilter}

-- | Toggle between showing all and showing only nonempty (more precisely, nonzero) items.
toggleEmpty :: UIState -> UIState
toggleEmpty ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=toggleEmpty ropts}}}
  where
    toggleEmpty ropts = ropts{empty_=not $ empty_ ropts}

-- | Toggle between flat and tree mode. If in the third "default" mode, go to flat mode.
toggleFlat :: UIState -> UIState
toggleFlat ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=toggleFlatMode ropts}}}
  where
    toggleFlatMode ropts@ReportOpts{accountlistmode_=ALFlat} = ropts{accountlistmode_=ALTree}
    toggleFlatMode ropts = ropts{accountlistmode_=ALFlat}

-- | Toggle between historical balances and period balances.
toggleHistorical :: UIState -> UIState
toggleHistorical ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=ropts{balancetype_=b}}}}
  where
    b | balancetype_ ropts == HistoricalBalance = PeriodChange
      | otherwise                               = HistoricalBalance

-- | Toggle between showing all and showing only real (non-virtual) items.
toggleReal :: UIState -> UIState
toggleReal ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=toggleReal ropts}}}
  where
    toggleReal ropts = ropts{real_=not $ real_ ropts}

-- | Toggle the ignoring of balance assertions.
toggleIgnoreBalanceAssertions :: UIState -> UIState
toggleIgnoreBalanceAssertions ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{}}} =
  ui{aopts=uopts{cliopts_=copts{ignore_assertions_=not $ ignore_assertions_ copts}}}

-- | Step through larger report periods, up to all.
growReportPeriod :: Day -> UIState -> UIState
growReportPeriod _d ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=ropts{period_=periodGrow $ period_ ropts}}}}

-- | Step through smaller report periods, down to a day.
shrinkReportPeriod :: Day -> UIState -> UIState
shrinkReportPeriod d ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=ropts{period_=periodShrink d $ period_ ropts}}}}

-- | Step the report start/end dates to the next period of same duration,
-- remaining inside the given enclosing span.
nextReportPeriod :: DateSpan -> UIState -> UIState
nextReportPeriod enclosingspan ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts@ReportOpts{period_=p}}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=ropts{period_=periodNextIn enclosingspan p}}}}

-- | Step the report start/end dates to the next period of same duration,
-- remaining inside the given enclosing span.
previousReportPeriod :: DateSpan -> UIState -> UIState
previousReportPeriod enclosingspan ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts@ReportOpts{period_=p}}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=ropts{period_=periodPreviousIn enclosingspan p}}}}

-- | If a standard report period is set, step it forward/backward if needed so that
-- it encloses the given date.
moveReportPeriodToDate :: Day -> UIState -> UIState
moveReportPeriodToDate d ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts@ReportOpts{period_=p}}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=ropts{period_=periodMoveTo d p}}}}

-- | Get the report period.
reportPeriod :: UIState -> Period
reportPeriod UIState{aopts=UIOpts{cliopts_=CliOpts{reportopts_=ReportOpts{period_=p}}}} =
  p

-- | Set the report period.
setReportPeriod :: Period -> UIState -> UIState
setReportPeriod p ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=ropts{period_=p}}}}

-- | Apply a new filter query.
setFilter :: String -> UIState -> UIState
setFilter s ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=ropts{query_=s}}}}

-- | Clear all filters/flags.
resetFilter :: UIState -> UIState
resetFilter ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=ropts{
     accountlistmode_=ALTree
    ,empty_=True
    ,clearedstatus_=Nothing
    ,real_=False
    ,query_=""
    --,period_=PeriodAll
    }}}}

resetDepth :: UIState -> UIState
resetDepth ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  ui{aopts=uopts{cliopts_=copts{reportopts_=ropts{depth_=Nothing}}}}

-- | Get the maximum account depth in the current journal.
maxDepth :: UIState -> Int
maxDepth UIState{ajournal=j} = maximum $ map accountNameLevel $ journalAccountNames j

-- | Decrement the current depth limit towards 0. If there was no depth limit,
-- set it to one less than the maximum account depth.
decDepth :: UIState -> UIState
decDepth ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts@ReportOpts{..}}}}
  = ui{aopts=uopts{cliopts_=copts{reportopts_=ropts{depth_=dec depth_}}}}
  where
    dec (Just d) = Just $ max 0 (d-1)
    dec Nothing  = Just $ maxDepth ui - 1

-- | Increment the current depth limit. If this makes it equal to the
-- the maximum account depth, remove the depth limit.
incDepth :: UIState -> UIState
incDepth ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts@ReportOpts{..}}}}
  = ui{aopts=uopts{cliopts_=copts{reportopts_=ropts{depth_=inc depth_}}}}
  where
    inc (Just d) | d < (maxDepth ui - 1) = Just $ d+1
    inc _ = Nothing

-- | Set the current depth limit to the specified depth, or remove the depth limit.
-- Also remove the depth limit if the specified depth is greater than the current
-- maximum account depth. If the specified depth is negative, reset the depth limit
-- to whatever was specified at uiartup.
setDepth :: Maybe Int -> UIState -> UIState
setDepth mdepth ui@UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}}
  = ui{aopts=uopts{cliopts_=copts{reportopts_=ropts{depth_=mdepth'}}}}
  where
    mdepth' = case mdepth of
                Nothing                   -> Nothing
                Just d | d < 0            -> depth_ ropts
                       | d >= maxDepth ui -> Nothing
                       | otherwise        -> mdepth

getDepth :: UIState -> Maybe Int
getDepth UIState{aopts=UIOpts{cliopts_=CliOpts{reportopts_=ropts}}} = depth_ ropts

-- | Open the minibuffer, setting its content to the current query with the cursor at the end.
showMinibuffer :: UIState -> UIState
showMinibuffer ui = setMode (Minibuffer e) ui
  where
    e = applyEdit gotoEOL $ editor MinibufferEditor (str . unlines) (Just 1) oldq
    oldq = query_ $ reportopts_ $ cliopts_ $ aopts ui

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
  (sInit topscreen) d True $ resetDepth $ resetFilter $ closeMinibuffer ui{aScreen=topscreen, aPrevScreens=[]}
  where
    topscreen = case ss of _:_ -> last ss
                           []  -> s

-- | Enter a new screen, saving the old screen & state in the
-- navigation history and initialising the new screen's state.
screenEnter :: Day -> Screen -> UIState -> UIState
screenEnter d scr ui = (sInit scr) d True $
                       pushScreen scr
                       ui

