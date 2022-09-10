-- The balance sheet screen, like the accounts screen but restricted to balance sheet accounts.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hledger.UI.BalancesheetScreen
 (bsNew
 ,bsUpdate
 ,bsDraw
 ,bsHandle
 )
where

import Brick hiding (bsDraw)
import Brick.Widgets.List
import Lens.Micro.Platform

import Hledger
import Hledger.Cli hiding (mode, progname, prognameandversion)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIUtils
import Hledger.UI.UIScreens
import Hledger.UI.AccountsScreen (asDrawHelper, handleHelpMode, handleMinibufferMode, asHandleNormalMode)


bsDraw :: UIState -> [Widget Name]
bsDraw ui = dlogUiTrace "bsDraw" $ asDrawHelper ui ropts' scrname showbalchgkey
  where
    scrname = "balance sheet"
    ropts' = (_rsReportOpts $ reportspec_ $ uoCliOpts $ aopts ui){balanceaccum_=Historical}
    showbalchgkey = False

bsHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
bsHandle ev = do
  ui0 <- get'
  dlogUiTraceM "bsHandle"
  case ui0 of
    ui1@UIState{aMode=mode, aScreen=BS sst} -> case mode of
      Normal          -> asHandleNormalMode ui scr ev
      Minibuffer _ ed -> handleMinibufferMode ui ed ev
      Help            -> handleHelpMode ui ev
      where
        scr = BS
        -- save the currently selected account, in case we leave this screen and lose the selection
        selacct = case listSelectedElement $ _assList sst of
                    Just (_, AccountsScreenItem{..}) -> asItemAccountName
                    Nothing -> sst ^. assSelectedAccount
        ui = ui1{aScreen=scr sst{_assSelectedAccount=selacct}}
    _ -> dlogUiTraceM "bsHandle" >> errorWrongScreenType "event handler"
