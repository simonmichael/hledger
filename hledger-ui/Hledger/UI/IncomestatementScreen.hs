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
isDraw ui = dlogUiTrace "isDraw" $ asDrawHelper ui ropts' scrname showbalchgkey
  where
    scrname = "income statement"
    ropts' = (_rsReportOpts $ reportspec_ $ uoCliOpts $ aopts ui){balanceaccum_=PerPeriod}
    showbalchgkey = False

isHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
isHandle = asHandle . dlogUiTrace "isHandle"
