-- The error screen, showing a current error condition (such as a parse error after reloading the journal)

{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}

module Hledger.UI.ErrorScreen
 (errorScreen
 ,stReloadJournalIfChanged
 )
where

-- import Lens.Micro.Platform ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
-- import Data.Maybe
import Data.Time.Calendar (Day)
import Graphics.Vty as Vty
import Brick
-- import Brick.Widgets.List
-- import Brick.Widgets.Border
-- import Brick.Widgets.Border.Style
-- import Brick.Widgets.Center
-- import Text.Printf

-- import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.UIOptions
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIUtils

errorScreen :: Screen
errorScreen = ErrorScreen{
   sInit    = esInit
  ,sDraw    = esDraw
  ,sHandle  = esHandle
  ,esError  = ""
  }

esInit :: Day -> Bool -> AppState -> AppState
esInit _ _ st@AppState{aScreen=ErrorScreen{}} = st
esInit _ _ _ = error "init function called with wrong screen type, should not happen"

esDraw :: AppState -> [Widget]
esDraw AppState{ -- aopts=_uopts@UIOpts{cliopts_=_copts@CliOpts{reportopts_=_ropts@ReportOpts{query_=querystr}}},
                             aScreen=ErrorScreen{..}} = [ui]
  where
    toplabel = withAttr ("border" <> "bold") (str "Oops. Please fix this problem then press g to reload")
            -- <+> str " transactions"
            -- <+> borderQueryStr querystr -- no, account transactions report shows all transactions in the acct ?
            -- <+> str " and subs"
    --         <+> str " ("
    --         <+> cur
    --         <+> str "/"
    --         <+> total
    --         <+> str ")"
    -- cur = str $ case l^.listSelectedL of
    --              Nothing -> "-"
    --              Just i -> show (i + 1)
    -- total = str $ show $ length displayitems
    -- displayitems = V.toList $ l^.listElementsL
    bottomlabel = borderKeysStr [
       -- ("up/down/pgup/pgdown/home/end", "move")
       ("g", "reload")
      -- ,("left", "return to accounts")
      ]


    -- query = query_ $ reportopts_ $ cliopts_ opts

    ui = Widget Greedy Greedy $ do

      -- calculate column widths, based on current available width
      -- c <- getContext
      -- let
      --   totalwidth = c^.availWidthL
      --                - 2 -- XXX due to margin ? shouldn't be necessary (cf UIUtils)

      render $ defaultLayout toplabel bottomlabel $ withAttr "error" $ str $ esError

esDraw _ = error "draw function called with wrong screen type, should not happen"

-- drawErrorItem :: (Int,Int,Int,Int,Int) -> Bool -> (String,String,String,String,String) -> Widget
-- drawErrorItem (datewidth,descwidth,acctswidth,changewidth,balwidth) selected (date,desc,accts,change,bal) =
--   Widget Greedy Fixed $ do
--     render $
--       str (fitString (Just datewidth) (Just datewidth) True True date) <+>
--       str "  " <+>
--       str (fitString (Just descwidth) (Just descwidth) True True desc) <+>
--       str "  " <+>
--       str (fitString (Just acctswidth) (Just acctswidth) True True accts) <+>
--       str "   " <+>
--       withAttr changeattr (str (fitString (Just changewidth) (Just changewidth) True False change)) <+>
--       str "   " <+>
--       withAttr balattr (str (fitString (Just balwidth) (Just balwidth) True False bal))
--   where
--     changeattr | '-' `elem` change = sel $ "list" <> "amount" <> "decrease"
--                | otherwise         = sel $ "list" <> "amount" <> "increase"
--     balattr    | '-' `elem` bal    = sel $ "list" <> "balance" <> "negative"
--                | otherwise         = sel $ "list" <> "balance" <> "positive"
--     sel | selected  = (<> "selected")
--         | otherwise = id

esHandle :: AppState -> Vty.Event -> EventM (Next AppState)
esHandle st@AppState{
   aScreen=s@ErrorScreen{}
  ,aopts=UIOpts{cliopts_=copts}
  ,ajournal=j
  } e = do
  d <- liftIO getCurrentDay
  case e of
    Vty.EvKey (Vty.KChar 'q') [] -> halt st
    Vty.EvKey Vty.KEsc   [] -> continue $ resetScreens d st

    Vty.EvKey (Vty.KChar 'g') [] -> do
      (ej, _) <- liftIO $ journalReloadIfChanged copts d j
      case ej of
        Left err -> continue st{aScreen=s{esError=err}} -- show latest parse error
        Right j' -> continue $ regenerateScreens j' d $ popScreen st  -- return to previous screen, and reload it

    -- Vty.EvKey (Vty.KLeft) []     -> continue $ popScreen st
    -- Vty.EvKey (Vty.KRight) []    -> error (show curItem) where curItem = listSelectedElement is
    -- fall through to the list's event handler (handles [pg]up/down)
    _                       -> do continue st
                                 -- is' <- handleEvent ev is
                                 -- continue st{aScreen=s{rsState=is'}}
                                 -- continue =<< handleEventLensed st someLens e
esHandle _ _ = error "event handler called with wrong screen type, should not happen"

-- If journal file(s) have changed, reload the journal and regenerate all screens.
-- This is here so it can reference the error screen.
stReloadJournalIfChanged :: CliOpts -> Day -> Journal -> AppState -> IO AppState
stReloadJournalIfChanged copts d j st = do
  (ej, _) <- journalReloadIfChanged copts d j
  return $ case ej of
    Right j' -> regenerateScreens j' d st
    Left err -> screenEnter d errorScreen{esError=err} st

