-- | Constructors and updaters for all hledger-ui screens.
--
-- Constructors (*New) create and initialise a new screen with valid state,
-- based on the provided options, reporting date, journal, and screen-specific parameters.
--
-- Updaters (*Update) recalculate an existing screen's state, 
-- based on new options, reporting date, journal, and the old screen state.
--
-- These are gathered in this low-level module so that any screen's handler 
-- can create or regenerate all other screens.
-- Drawing and event-handling code is elsewhere, in per-screen modules.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.UI.UIScreens
(screenUpdate
,esNew
,esUpdate
,msNew
,msUpdate
,asNew
,asUpdate
,bsNew
,bsUpdate
,isNew
,isUpdate
,rsNew
,rsUpdate
,tsNew
,tsUpdate
)
where

import Brick.Widgets.List (listMoveTo, listSelectedElement, list)
import Data.List
import Data.Maybe
import Data.Time.Calendar (Day, diffDays)
import Safe
import qualified Data.Vector as V

import Hledger.Cli hiding (mode, progname,prognameandversion)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIUtils
import Data.Function ((&))


-- | Regenerate the content of any screen from new options, reporting date and journal.
screenUpdate :: UIOpts -> Day -> Journal -> Screen -> Screen
screenUpdate opts d j = \case
  MS sst -> MS $ msUpdate sst  -- opts d j ass
  AS sst -> AS $ asUpdate opts d j sst
  BS sst -> BS $ bsUpdate opts d j sst
  IS sst -> IS $ isUpdate opts d j sst
  RS sst -> RS $ rsUpdate opts d j sst
  TS sst -> TS $ tsUpdate sst
  ES sst -> ES $ esUpdate sst

-- | Construct an error screen.
-- Screen-specific arguments: the error message to show.
esNew :: String -> Screen
esNew msg =
  dlogUiTrace "esNew" $
  ES ESS {
    _essError = msg
    ,_essUnused = ()
    }

-- | Update an error screen. Currently a no-op since error screen
-- depends only on its screen-specific state.
esUpdate :: ErrorScreenState -> ErrorScreenState
esUpdate = dlogUiTrace "esUpdate`"

-- | Construct a menu screen.
-- Screen-specific arguments: none.
msNew :: Screen
msNew =
  dlogUiTrace "msNew" $
  MS MSS {
     _mssList            = list MenuList (V.fromList [
       MenuScreenItem "All accounts" Accounts
      ,MenuScreenItem "Balance sheet accounts (assets, liabilities, equity)" Balancesheet
      ,MenuScreenItem "Income statement accounts (revenues, expenses)" Incomestatement
      ]) 1
    ,_mssUnused = ()
    }

-- | Update a menu screen. Currently a no-op since menu screen
-- has unchanging content.
msUpdate :: MenuScreenState -> MenuScreenState
msUpdate = dlogUiTrace "msUpdate"

nullass macct = ASS {
   _assSelectedAccount = fromMaybe "" macct
  ,_assList            = list AccountsList (V.fromList []) 1
  }

-- | Construct an accounts screen listing the appropriate set of accounts,
-- with the appropriate one selected.
-- Screen-specific arguments: the account to select if any.
asNew :: UIOpts -> Day -> Journal -> Maybe AccountName -> Screen
asNew uopts d j macct = dlogUiTrace "asNew" $ AS $ asUpdate uopts d j $ nullass macct

-- | Update an accounts screen's state from these options, reporting date, and journal.
asUpdate :: UIOpts -> Day -> Journal -> AccountsScreenState -> AccountsScreenState
asUpdate uopts d = dlogUiTrace "asUpdate" .
  asUpdateHelper rspec d copts roptsmod extraquery
  where
    UIOpts{uoCliOpts=copts@CliOpts{reportspec_=rspec}} = uopts
    roptsmod       = id
    extraquery     = Any

-- | Update an accounts-like screen's state from this report spec, reporting date,
-- cli options, report options modifier, extra query, and journal.
asUpdateHelper :: ReportSpec -> Day -> CliOpts -> (ReportOpts -> ReportOpts) -> Query -> Journal -> AccountsScreenState -> AccountsScreenState
asUpdateHelper rspec0 d copts roptsModify extraquery j ass = dlogUiTrace "asUpdateHelper"
  ass{_assList=l}
  where
    ropts = roptsModify $ _rsReportOpts rspec0
    rspec =
      updateReportSpec
        ropts
        rspec0{_rsDay=d}  -- update to the current date, might have changed since program start
      & either (error "asUpdateHelper: adjusting the query, should not have failed") id -- PARTIAL:
      & reportSpecSetFutureAndForecast (forecast_ $ inputopts_ copts)  -- include/exclude future & forecast transactions
      & reportSpecAddQuery extraquery  -- add any extra restrictions

    -- decide which account is selected:
    -- if selectfirst is true, the first account;
    -- otherwise, the previously selected account if possible;
    -- otherwise, the first account with the same prefix (eg first leaf account when entering flat mode);
    -- otherwise, the alphabetically preceding account.
    l =
      listMoveTo selidx $
      list AccountsList (V.fromList $ displayitems ++ blankitems) 1
      where
        selidx = headDef 0 $ catMaybes [
                  elemIndex a as
                  ,findIndex (a `isAccountNamePrefixOf`) as
                  ,Just $ max 0 (length (filter (< a) as) - 1)
                  ]
                  where
                    a = _assSelectedAccount ass
                    as = map asItemAccountName displayitems

        displayitems = map displayitem items
          where
            -- run the report
            (items, _) = balanceReport rspec j

            -- pre-render a list item
            displayitem (fullacct, shortacct, indent, bal) =
              AccountsScreenItem{asItemIndentLevel        = indent
                                ,asItemAccountName        = fullacct
                                ,asItemDisplayAccountName = replaceHiddenAccountsNameWith "All" $ if tree_ ropts then shortacct else fullacct
                                ,asItemMixedAmount        = Just bal
                                }

        -- blanks added for scrolling control, cf RegisterScreen.
        -- XXX Ugly. Changing to 0 helps when debugging.
        blankitems = replicate uiNumBlankItems
          AccountsScreenItem{asItemIndentLevel        = 0
                            ,asItemAccountName        = ""
                            ,asItemDisplayAccountName = ""
                            ,asItemMixedAmount        = Nothing
                            }

-- | Construct a balance sheet screen listing the appropriate set of accounts,
-- with the appropriate one selected.
-- Screen-specific arguments: the account to select if any.
bsNew :: UIOpts -> Day -> Journal -> Maybe AccountName -> Screen
bsNew uopts d j macct = dlogUiTrace "bsNew" $ BS $ bsUpdate uopts d j $ nullass macct

-- | Update a balance sheet screen's state from these options, reporting date, and journal.
bsUpdate :: UIOpts -> Day -> Journal -> AccountsScreenState -> AccountsScreenState
bsUpdate uopts d = dlogUiTrace "bsUpdate" .
  asUpdateHelper rspec d copts roptsmod extraquery
  where
    UIOpts{uoCliOpts=copts@CliOpts{reportspec_=rspec}} = uopts
    roptsmod ropts = ropts{balanceaccum_=Historical}  -- always show historical end balances
    extraquery     = Type [Asset,Liability,Equity]    -- restrict to balance sheet accounts

-- | Construct an income statement screen listing the appropriate set of accounts,
-- with the appropriate one selected.
-- Screen-specific arguments: the account to select if any.
isNew :: UIOpts -> Day -> Journal -> Maybe AccountName -> Screen
isNew uopts d j macct = dlogUiTrace "isNew" $ IS $ isUpdate uopts d j $ nullass macct

-- | Update an income statement screen's state from these options, reporting date, and journal.
isUpdate :: UIOpts -> Day -> Journal -> AccountsScreenState -> AccountsScreenState
isUpdate uopts d = dlogUiTrace "isUpdate" .
  asUpdateHelper rspec d copts roptsmod extraquery
  where
    UIOpts{uoCliOpts=copts@CliOpts{reportspec_=rspec}} = uopts
    roptsmod ropts = ropts{balanceaccum_=PerPeriod}  -- always show historical end balances
    extraquery     = Type [Revenue, Expense]         -- restrict to income statement accounts

-- | Construct a register screen listing the appropriate set of transactions,
-- with the appropriate one selected.
-- Screen-specific arguments: the account whose register this is,
-- whether to force inclusive balances.
rsNew :: UIOpts -> Day -> Journal -> AccountName -> Bool -> Screen
rsNew uopts d j acct forceinclusive =  -- XXX forcedefaultselection - whether to force selecting the last transaction.
  dlogUiTrace "rsNew" $
  RS $
  rsUpdate uopts d j $
  RSS {
     _rssAccount        = replaceHiddenAccountsNameWith "*" acct
    ,_rssForceInclusive = forceinclusive
    ,_rssList           = list RegisterList (V.fromList []) 1
    }

-- | Update a register screen from these options, reporting date, and journal.
rsUpdate :: UIOpts -> Day -> Journal -> RegisterScreenState -> RegisterScreenState
rsUpdate uopts d j rss@RSS{_rssAccount, _rssForceInclusive, _rssList=oldlist} =
  dlogUiTrace "rsUpdate"
  rss{_rssList=l'}
  where
    UIOpts{uoCliOpts=copts@CliOpts{reportspec_=rspec@ReportSpec{_rsReportOpts=ropts}}} = uopts
    -- gather arguments and queries
    -- XXX temp
    inclusive = tree_ ropts || _rssForceInclusive
    thisacctq = Acct $ mkregex _rssAccount
      where
        mkregex = if inclusive then accountNameToAccountRegex else accountNameToAccountOnlyRegex

    -- adjust the report options and report spec, carefully as usual to avoid screwups (#1523)
    ropts' = ropts {
        -- ignore any depth limit, as in postingsReport; allows register's total to match accounts screen
        depth_=Nothing
        -- do not strip prices so we can toggle costs within the ui
      , show_costs_=True
      -- XXX aregister also has this, needed ?
        -- always show historical balance
      -- , balanceaccum_= Historical
      }
    rspec' =
      updateReportSpec ropts' rspec{_rsDay=d}
      & either (error "rsUpdate: adjusting the query for register, should not have failed") id -- PARTIAL:
      & reportSpecSetFutureAndForecast (forecast_ $ inputopts_ copts)

    -- gather transactions to display
    items = accountTransactionsReport rspec' j thisacctq
    items' =
      (if empty_ ropts then id else filter (not . mixedAmountLooksZero . fifth6)) $  -- without --empty, exclude no-change txns
      reverse  -- most recent last
      items

    -- pre-render the list items, helps calculate column widths
    displayitems = map displayitem items'
      where
        displayitem (t, _, _issplit, otheracctsstr, change, bal) =
          RegisterScreenItem{rsItemDate          = showDate $ transactionRegisterDate wd (_rsQuery rspec') thisacctq t
                            ,rsItemStatus        = tstatus t
                            ,rsItemDescription   = tdescription t
                            ,rsItemOtherAccounts = otheracctsstr
                                                    -- _   -> "<split>"  -- should do this if accounts field width < 30
                            ,rsItemChangeAmount  = showamt change
                            ,rsItemBalanceAmount = showamt bal
                            ,rsItemTransaction   = t
                            }
            where
              showamt = showMixedAmountB oneLine{displayMaxWidth=Just 3}
              wd = whichDate ropts'

    -- blank items are added to allow more control of scroll position; we won't allow movement over these.
    -- XXX Ugly. Changing to 0 helps when debugging.
    blankitems = replicate uiNumBlankItems
          RegisterScreenItem{rsItemDate          = ""
                            ,rsItemStatus        = Unmarked
                            ,rsItemDescription   = ""
                            ,rsItemOtherAccounts = ""
                            ,rsItemChangeAmount  = mempty
                            ,rsItemBalanceAmount = mempty
                            ,rsItemTransaction   = nulltransaction
                            }

    -- build the new list widget
    l = list RegisterList (V.fromList $ displayitems ++ blankitems) 1

    -- ensure the appropriate list item is selected:
    -- if forcedefaultselection is true, the last (latest) transaction;  XXX still needed ?
    -- otherwise, the previously selected transaction if possible;
    -- otherwise, the transaction nearest in date to it;
    -- or if there's several with the same date, the nearest in journal order;
    -- otherwise, the last (latest) transaction.
    l' = listMoveTo newselidx l
      where
        endidx = max 0 $ length displayitems - 1
        newselidx =
          -- case (forcedefaultselection, listSelectedElement _rssList) of
          --   (True, _)    -> endidx
          --   (_, Nothing) -> endidx
          --   (_, Just (_, RegisterScreenItem{rsItemTransaction=Transaction{tindex=prevselidx, tdate=prevseld}})) ->
          --     headDef endidx $ catMaybes [
          --       findIndex ((==prevselidx) . tindex . rsItemTransaction) displayitems
          --       ,findIndex ((==nearestidbydatethenid) . Just . tindex . rsItemTransaction) displayitems
          --       ]
          --     where
          --       nearestidbydatethenid = third3 <$> (headMay $ sort
          --         [(abs $ diffDays (tdate t) prevseld, abs (tindex t - prevselidx), tindex t) | t <- ts])
          --       ts = map rsItemTransaction displayitems
          case listSelectedElement oldlist of
            Nothing -> endidx
            Just (_, RegisterScreenItem{rsItemTransaction=Transaction{tindex=prevselidx, tdate=prevseld}}) ->
              headDef endidx $ catMaybes [
                findIndex ((==prevselidx) . tindex . rsItemTransaction) displayitems
                ,findIndex ((==nearestidbydatethenid) . Just . tindex . rsItemTransaction) displayitems
                ]
              where
                nearestidbydatethenid = third3 <$> (headMay $ sort
                  [(abs $ diffDays (tdate t) prevseld, abs (tindex t - prevselidx), tindex t) | t <- ts])
                ts = map rsItemTransaction displayitems

-- | Construct a transaction screen showing one of a given list of transactions,
-- with the ability to step back and forth through the list.
-- Screen-specific arguments: the account whose transactions are being shown,
-- the list of showable transactions, the currently shown transaction.
tsNew :: AccountName -> [NumberedTransaction] -> NumberedTransaction -> Screen
tsNew acct nts nt =
  dlogUiTrace "tsNew" $
  TS TSS{
     _tssAccount      = acct
    ,_tssTransactions = nts
    ,_tssTransaction  = nt
    }

-- | Update a transaction screen. Currently a no-op since transaction screen
-- depends only on its screen-specific state.
tsUpdate :: TransactionScreenState -> TransactionScreenState
tsUpdate = dlogUiTrace "tsUpdate"

