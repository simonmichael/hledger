{- |
Overview:
hledger-ui's AppState holds the active screen and any previously visited screens.
Screens have their own render state, render function, event handler,
and app state update function (which can update the whole AppState).
A brick App delegates event-handling and rendering to our AppState's active screen.

@
Brick.defaultMain brickapp st
  where
    brickapp :: App (AppState) V.Event
    brickapp = App {
        appLiftVtyEvent = id
      , appStartEvent   = return
      , appAttrMap      = const theme
      , appChooseCursor = showFirstCursor
      , appHandleEvent  = \st ev -> sHandleFn (aScreen st) st ev
      , appDraw         = \st    -> sDrawFn   (aScreen st) st
      }
    st :: AppState
    st = (sInitFn scr) d
         AppState{
            aopts=uopts'
           ,ajournal=j
           ,aScreen=scr
           ,aPrevScreens=prevscrs
           ,aMinibuffer=Nothing
           }
@
-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.UI.UITypes where

import Data.Monoid
import Data.Time.Calendar (Day)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Widgets.List
import Brick.Widgets.Edit (Editor)
import qualified Data.Vector as V
import Lens.Micro
import Lens.Micro.TH
import Text.Show.Functions ()
  -- import the Show instance for functions. Warning, this also re-exports it

import Hledger
import Hledger.UI.UIOptions

-- | hledger-ui's application state. This holds one or more stateful screens.
data AppState = AppState {
   aopts        :: UIOpts       -- ^ the command-line options and query arguments currently in effect
  ,ajournal     :: Journal      -- ^ the journal being viewed
  ,aScreen      :: Screen       -- ^ the currently active screen
  ,aPrevScreens :: [Screen]     -- ^ previously visited screens, most recent first
  ,aMinibuffer  :: Maybe Editor -- ^ a compact editor used for data entry, when active
  } deriving (Show)

-- | Types of screen available within hledger-ui. Each has its own
-- specific state type, and generic initialisation, event handling
-- and rendering functions.
--
-- Screen types are pattern-matched by their constructor and their
-- state field, which must have a unique name. This type causes
-- partial functions, so take care.
data Screen =
    AccountsScreen {
       _asState   :: AccountsScreenState
      ,sInitFn   :: Day -> Bool -> AppState -> AppState            -- ^ function to generate the screen's state on entry or change
      ,sDrawFn   :: AppState -> [Widget]                           -- ^ brick renderer for this screen
      ,sHandleFn :: AppState -> Vty.Event -> EventM (Next AppState)  -- ^ brick event handler for this screen
    }
  | RegisterScreen {
       rsState   :: RegisterScreenState
      ,sInitFn   :: Day -> Bool -> AppState -> AppState
      ,sDrawFn   :: AppState -> [Widget]
      ,sHandleFn :: AppState -> Vty.Event -> EventM (Next AppState)
    }
  | TransactionScreen {
       tsState   :: TransactionScreenState
      ,sInitFn   :: Day -> Bool -> AppState -> AppState
      ,sDrawFn   :: AppState -> [Widget]
      ,sHandleFn :: AppState -> Vty.Event -> EventM (Next AppState)
    }
  | ErrorScreen {
       esState   :: ErrorScreenState
      ,sInitFn   :: Day -> Bool -> AppState -> AppState
      ,sDrawFn   :: AppState -> [Widget]
      ,sHandleFn :: AppState -> Vty.Event -> EventM (Next AppState)
    }
  deriving (Show)

instance Show (List a) where show _ = "<List>"
instance Show Editor   where show _ = "<Editor>"

instance Monoid (List a)
  where
    mempty      = list "" V.empty 1
    mappend a b = a & listElementsL .~ (a^.listElementsL <> b^.listElementsL)

-- | Render state for this type of screen.
data AccountsScreenState = AccountsScreenState {
   _asItems           :: List AccountsScreenItem  -- ^ list of account names & balances
  ,_asSelectedAccount :: AccountName              -- ^ full name of the currently selected account (or "")
  } deriving (Show)

-- | An item in the accounts screen's list of accounts and balances.
data AccountsScreenItem = AccountsScreenItem {
   asItemIndentLevel        :: Int          -- ^ indent level
  ,asItemAccountName        :: AccountName  -- ^ full account name
  ,asItemDisplayAccountName :: AccountName  -- ^ full or short account name to display
  ,asItemRenderedAmounts    :: [String]     -- ^ rendered amounts
  }

-- | Render state for this type of screen.
data RegisterScreenState = RegisterScreenState {
   rsItems           :: List RegisterScreenItem  -- ^ list of transactions affecting this account
  ,rsSelectedAccount :: AccountName              -- ^ full name of the account we are showing a register for
  } deriving (Show)

-- | An item in the register screen's list of transactions in the current account.
data RegisterScreenItem = RegisterScreenItem {
   rsItemDate           :: String           -- ^ date
  ,rsItemDescription    :: String           -- ^ description
  ,rsItemOtherAccounts  :: String           -- ^ other accounts
  ,rsItemChangeAmount   :: String           -- ^ the change to the current account from this transaction
  ,rsItemBalanceAmount  :: String           -- ^ the balance or running total after this transaction
  ,rsItemTransaction    :: Transaction      -- ^ the full transaction
  }

-- | Render state for this type of screen.
data TransactionScreenState = TransactionScreenState {
   tsTransaction     :: NumberedTransaction    -- ^ the transaction we are currently viewing, and its position in the list
  ,tsTransactions    :: [NumberedTransaction]  -- ^ the list of transactions we can step through
  ,tsSelectedAccount :: AccountName            -- ^ the account whose register we entered this screen from
  } deriving (Show)

type NumberedTransaction = (Integer, Transaction)

-- | Render state for this type of screen.
data ErrorScreenState = ErrorScreenState {
                           esError :: String  -- ^ error message to show
  } deriving (Show)

-- makeLenses ''AccountsScreenState
concat <$> mapM makeLenses [
   ''AccountsScreenState
--   ,''RegisterScreenState
--   ,''TransactionScreenState
--   ,''ErrorScreenState
  ,''Screen
  ]

