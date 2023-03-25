-- The cash accounts screen, like the accounts screen but restricted to cash accounts.

module Hledger.UI.CashScreen
 (csNew
 ,csUpdate
 ,csDraw
 ,csHandle
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


csDraw :: UIState -> [Widget Name]
csDraw ui = dbgui "csDraw" $ asDrawHelper ui ropts' scrname
  where
    scrname = "cash balances"
    ropts' = (_rsReportOpts $ reportspec_ $ uoCliOpts $ aopts ui){balanceaccum_=Historical}

csHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
csHandle = asHandle . dbgui "csHandle"
