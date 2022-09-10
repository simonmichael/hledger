-- The balance sheet screen, like the accounts screen but restricted to balance sheet accounts.

module Hledger.UI.BalancesheetScreen
 (bsNew
 ,bsUpdate
 ,bsDraw
 ,bsHandle
 )
where

import Brick hiding (bsDraw)

import Hledger
import Hledger.Cli hiding (mode, progname, prognameandversion)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIUtils
import Hledger.UI.UIScreens
import Hledger.UI.AccountsScreen (asHandle, asDrawHelper)


bsDraw :: UIState -> [Widget Name]
bsDraw ui = dlogUiTrace "bsDraw" $ asDrawHelper ui ropts' scrname showbalchgkey
  where
    scrname = "balance sheet"
    ropts' = (_rsReportOpts $ reportspec_ $ uoCliOpts $ aopts ui){balanceaccum_=Historical}
    showbalchgkey = False

bsHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
bsHandle = asHandle . dlogUiTrace "bsHandle"
