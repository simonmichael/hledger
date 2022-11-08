-- The income statement accounts screen, like the accounts screen but restricted to income statement accounts.

module Hledger.UI.IncomestatementScreen
 (isNew
 ,isUpdate
 ,isDraw
 ,isHandle
 )
where

import Brick

import Hledger
import Hledger.Cli hiding (mode, progname, prognameandversion)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIUtils
import Hledger.UI.UIScreens
import Hledger.UI.AccountsScreen (asHandle, asDrawHelper)


isDraw :: UIState -> [Widget Name]
isDraw ui = dbgui "isDraw" $ asDrawHelper ui ropts' scrname
  where
    scrname = "income statement changes"
    ropts' = (_rsReportOpts $ reportspec_ $ uoCliOpts $ aopts ui){balanceaccum_=PerPeriod}

isHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
isHandle = asHandle . dbgui "isHandle"
