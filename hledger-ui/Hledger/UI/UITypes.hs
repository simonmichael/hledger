{- |
Overview:
hledger-ui's UIState holds the currently active screen and any previously visited
screens (and their states).
The brick App delegates all event-handling and rendering
to the UIState's active screen.
Screens have their own screen state, render function, event handler, and app state
update function, so they have full control.

@
Brick.defaultMain brickapp st
  where
    brickapp :: App (UIState) V.Event
    brickapp = App {
        appLiftVtyEvent = id
      , appStartEvent   = return
      , appAttrMap      = const theme
      , appChooseCursor = showFirstCursor
      , appHandleEvent  = \st ev -> sHandle (aScreen st) st ev
      , appDraw         = \st    -> sDraw   (aScreen st) st
      }
    st :: UIState
    st = (sInit s) d
         UIState{
            aopts=uopts'
           ,ajournal=j
           ,aScreen=s
           ,aPrevScreens=prevscrs
           ,aMinibuffer=Nothing
           }
@
-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Hledger.UI.UITypes where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Brick
import Brick.Widgets.List (List)
import Brick.Widgets.Edit (Editor)
import Lens.Micro.Platform
import Text.Show.Functions ()
  -- import the Show instance for functions. Warning, this also re-exports it

import Hledger
import Hledger.Cli (HasCliOpts(..))
import Hledger.UI.UIOptions

-- | hledger-ui's application state. This holds one or more stateful screens.
-- As you navigate through screens, the old ones are saved in a stack.
-- The app can be in one of several modes: normal screen operation,
-- showing a help dialog, entering data in the minibuffer etc.
data UIState = UIState {
   astartupopts :: UIOpts    -- ^ the command-line options and query arguments specified at startup
  ,aopts        :: UIOpts    -- ^ the command-line options and query arguments currently in effect
  ,ajournal     :: Journal   -- ^ the journal being viewed
  ,aPrevScreens :: [Screen]  -- ^ previously visited screens, most recent first
  ,aScreen      :: Screen    -- ^ the currently active screen
  ,aMode        :: Mode      -- ^ the currently active mode
  } deriving (Show)

-- | The mode modifies the screen's rendering and event handling.
-- It resets to Normal when entering a new screen.
data Mode =
    Normal
  | Help
  | Minibuffer Text (Editor String Name)
  deriving (Show,Eq)

-- Ignore the editor when comparing Modes.
instance Eq (Editor l n) where _ == _ = True

-- Unique names required for widgets, viewports, cursor locations etc.
data Name =
    HelpDialog
  | MinibufferEditor
  | AccountsViewport
  | AccountsList
  | RegisterViewport
  | RegisterList
  | TransactionEditor
  deriving (Ord, Show, Eq)

data AppEvent =
    FileChange          -- one of the Journal's files has been added/modified/removed
  | DateChange Day Day  -- the current date has changed since last checked (with the old and new values)
  deriving (Eq, Show)

-- | hledger-ui screen types & instances.
-- Each screen type has generically named initialisation, draw, and event handling functions,
-- and zero or more uniquely named screen state fields, which hold the data for a particular
-- instance of this screen. Note the latter create partial functions, which means that some invalid
-- cases need to be handled, and also that their lenses are traversals, not single-value getters.
data Screen =
    AccountsScreen {
       sInit   :: Day -> Bool -> UIState -> UIState              -- ^ function to initialise or update this screen's state
      ,sDraw   :: UIState -> [Widget Name]                             -- ^ brick renderer for this screen
      ,sHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()  -- ^ brick event handler for this screen
      -- state fields.These ones have lenses:
      ,_asList            :: List Name AccountsScreenItem  -- ^ list widget showing account names & balances
      ,_asSelectedAccount :: AccountName              -- ^ a backup of the account name from the list widget's selected item (or "")
    }
  | RegisterScreen {
       sInit   :: Day -> Bool -> UIState -> UIState
      ,sDraw   :: UIState -> [Widget Name]
      ,sHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
      --
      ,rsList    :: List Name RegisterScreenItem      -- ^ list widget showing transactions affecting this account
      ,rsAccount :: AccountName                       -- ^ the account this register is for
      ,rsForceInclusive :: Bool                       -- ^ should this register always include subaccount transactions,
                                                      --   even when in flat mode ? (ie because entered from a
                                                      --   depth-clipped accounts screen item)
    }
  | TransactionScreen {
       sInit   :: Day -> Bool -> UIState -> UIState
      ,sDraw   :: UIState -> [Widget Name]
      ,sHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
      --
      ,tsTransaction  :: NumberedTransaction          -- ^ the transaction we are currently viewing, and its position in the list
      ,tsTransactions :: [NumberedTransaction]        -- ^ list of transactions we can step through
      ,tsAccount      :: AccountName                  -- ^ the account whose register we entered this screen from
    }
  | ErrorScreen {
       sInit   :: Day -> Bool -> UIState -> UIState
      ,sDraw   :: UIState -> [Widget Name]
      ,sHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
      --
      ,esError :: String                              -- ^ error message to show
    }
  deriving (Show)

-- | Error message to use in case statements adapting to the different Screen shapes.
errorWrongScreenType = error' "handler called with wrong screen type, should not happen"

-- | An item in the accounts screen's list of accounts and balances.
data AccountsScreenItem = AccountsScreenItem {
   asItemIndentLevel        :: Int                -- ^ indent level
  ,asItemAccountName        :: AccountName        -- ^ full account name
  ,asItemDisplayAccountName :: AccountName        -- ^ full or short account name to display
  ,asItemMixedAmount        :: Maybe MixedAmount  -- ^ mixed amount to display
  } deriving (Show)

-- | An item in the register screen's list of transactions in the current account.
data RegisterScreenItem = RegisterScreenItem {
   rsItemDate           :: Text         -- ^ date
  ,rsItemStatus         :: Status       -- ^ transaction status
  ,rsItemDescription    :: Text         -- ^ description
  ,rsItemOtherAccounts  :: Text         -- ^ other accounts
  ,rsItemChangeAmount   :: WideBuilder  -- ^ the change to the current account from this transaction
  ,rsItemBalanceAmount  :: WideBuilder  -- ^ the balance or running total after this transaction
  ,rsItemTransaction    :: Transaction  -- ^ the full transaction
  }
  deriving (Show)

type NumberedTransaction = (Integer, Transaction)

-- dummy monoid instance needed make lenses work with List fields not common across constructors
--instance Monoid (List n a)
--  where
--    mempty        = list "" V.empty 1  -- XXX problem in 0.7, every list requires a unique Name
--    mappend l1 l2 = l1 & listElementsL .~ (l1^.listElementsL <> l2^.listElementsL)

makeLenses ''Screen

uioptslens f ui = (\x -> ui{aopts=x}) <$> f (aopts ui)

instance HasCliOpts UIState where
    cliOpts = uioptslens.cliOpts

instance HasInputOpts UIState where
    inputOpts = uioptslens.inputOpts

instance HasBalancingOpts UIState where
    balancingOpts = uioptslens.balancingOpts

instance HasReportSpec UIState where
    reportSpec = uioptslens.reportSpec

instance HasReportOptsNoUpdate UIState where
    reportOptsNoUpdate = uioptslens.reportOptsNoUpdate

instance HasReportOpts UIState where
    reportOpts = uioptslens.reportOpts
