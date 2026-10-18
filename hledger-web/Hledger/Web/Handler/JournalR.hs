-- | /journal handlers.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Handler.JournalR where

import Data.Text qualified as T
import Hledger.Utils.I18n (tr, trf)
import Hledger
import Hledger.Cli.CliOptions
import Hledger.Web.Import
import Hledger.Web.Paging
import Hledger.Web.WebOptions
import Hledger.Web.Widget.AddForm (addModal)
import Hledger.Web.ReportPage (amountModeOf, deepestDepth, intervalLinks, intervalPeriodExpr, listWord, parseCalc, parseListMode, parseValue, removeAttributes, setAmountMode, valueParams)
import Hledger.Web.Widget.Common
            (accountQuery, amountLinks, calcLinks, columnsLinks, commodityLinks,
             controlsHint, controlsToggle,
             depthLinks, inertControls, listModeLinks,
             mixedAmountAsHtml, percentLinks, pricesLinks, realLinks, sortLinks,
             statusLinks, typeLinks,
             transactionFragment, replaceInacct, journalDayQuery, zeroBalanceLinks)

-- | The formatted journal view, with sidebar.
getJournalR :: Handler Html
getJournalR = do
  checkServerSideUiEnabled
  VD{perms, j, q, opts, qparam, qopts, today, trs, hideEmpty, controlsOpen, commoditiesOpen} <- getViewData
  require ViewPermission
  mmode <- either (const Nothing) id . parseValue <$> lookupGetParam "value"
  mlist <- either (const Nothing) id . parseListMode <$> lookupGetParam "list"
  inferOn <- (== Just "1") <$> lookupGetParam "infer"
  mperiod <- lookupGetParam "period"
  totalOn <- (== Just "1") <$> lookupGetParam "total"
  avgOn   <- (== Just "1") <$> lookupGetParam "avg"
  sortAmt <- (== Just "amount") <$> lookupGetParam "sort"
  pctOn   <- (== Just "1") <$> lookupGetParam "pct"
  mcalc   <- lookupGetParam "calc"
  date2On <- (== Just "1") <$> lookupGetParam "date2"
  pagereq <- pageRequest
  let title = case inAccount qopts of
        Nothing         -> tr trs "General Journal"
        Just (a, True)  -> trf trs "Transactions in {account}" [("account", a)]
        Just (a, False) -> trf trs "Transactions in {account} (excluding subaccounts)" [("account", a)]
      title' = if q /= Any then trf trs "{title}, filtered" [("title", title)] else title
      -- An account's register, opened on the page holding this transaction.
      acctlink a t = (RegisterR, ("q", replaceInacct qparam $ accountQuery a) : ("txn", T.pack $ show $ tindex t) : carried)
      q' = filterQuery (not . queryIsDepth) q
      rspec = (reportspec_ $ cliopts_ opts){_rsQuery = q'}
      -- The matching transactions, newest first; this page shows one page of them.
      alltxns = reverse $
        styleAmounts (journalCommodityStylesWith HardRounding j) $
        entriesReport rspec j
      (page, items) = pageOf pagereq tindex alltxns
      -- The years the search matches in, ignoring any date term in it.
      years = map tdate $
        maybe alltxns (\dq -> entriesReport rspec{_rsQuery = filterQuery (not . queryIsDepth) dq} j) $
        datelessQuery today qparam
      transactionFrag = transactionFragment j
      -- Links staying on this page keep the states that shape the
      -- sidebar: zero balances, amounts, the accounts mode, and
      -- inferred prices. The journal's entries always show as written.
      roptsStartup = _rsReportOpts $ reportspec_ $ cliopts_ opts
      dfltHidden = empty_ roptsStartup
      emptyParams = [("empty", if hideEmpty then "0" else "1") | hideEmpty /= dfltHidden]
      valParams = valueParams roptsStartup mmode
      elist = fromMaybe (accountlistmode_ roptsStartup) mlist
      listParams = [("list", listWord elist) | elist /= accountlistmode_ roptsStartup]
      inferParams = [("infer", "1") | inferOn]
      -- The report-shaping state shapes nothing here, but it is passed
      -- along and shown dimmed below, so the startup options' url
      -- survives this landing page and the url's state is never
      -- invisible.
      stickyParams =
        [("period", p) | Just p <- [mperiod], not (T.null p)] ++
        [("total", "1") | totalOn] ++
        [("avg", "1") | avgOn] ++
        [("sort", "amount") | sortAmt] ++
        [("pct", "1") | pctOn] ++
        [("calc", c) | Just c <- [mcalc], not (T.null c)] ++
        [("date2", "1") | date2On]
      carried = dbg1 "journal carried params" $ stickyParams ++ valParams ++ listParams ++ inferParams ++ emptyParams
      qps = [("q", qparam) | not (T.null qparam)]
      -- An entry's date link: the journal on that date, same modes.
      dayParams t = ("q", journalDayQuery qparam (tdate t)) : carried
      -- the rows of links above the entries: the filters (status,
      -- realness, type, tag, commodity, and depth), and the sidebar's
      -- accounts and amounts modes
      statusRow = statusLinks trs JournalR carried qparam
      realRow = realLinks trs JournalR carried qparam
      typeRow = typeLinks trs JournalR carried qparam
      commodityRow = commodityLinks trs JournalR carried qparam commoditiesOpen j
      -- TRANSLATORS: the hint below the account type and commodity filter rows.
      hintRow = controlsHint $ tr trs "Shift-click selects several types or commodities."
      -- The depth links clip the sidebar's accounts, or in tree mode
      -- fold them, and carry to the reports.
      deepest = deepestDepth j
      depthRow = depthLinks trs JournalR carried qparam deepest
      -- The amounts mode converts the sidebar's amounts, and tree mode
      -- adds its fold carets; the journal's entries always show as written.
      listRow = listModeLinks trs JournalR (qps ++ removeAttributes ["list"] carried) (accountlistmode_ roptsStartup) elist
      amountsRow = amountLinks trs JournalR (qps ++ removeAttributes ["value"] carried)
        (amountModeOf roptsStartup) (amountModeOf $ maybe id setAmountMode mmode roptsStartup)
      pricesRow = pricesLinks trs JournalR (qps ++ removeAttributes ["infer"] carried) inferOn
      zeroRow = zeroBalanceLinks trs JournalR (qps ++ removeAttributes ["empty"] carried) dfltHidden hideEmpty
      -- The report-shaping controls, operative on the report pages,
      -- shown dimmed with the url's state marked.
      (pivl, pspan) = case mperiod of
        Just p | not (T.null p), Right r <- parsePeriodExpr today p -> r
        _ -> (NoInterval, nulldatespan)
      rpInterval = fromMaybe pivl $ intervalFromQueryOpts qopts
      calcCur = either (const CalcChange) id $ parseCalc mcalc
      -- The years row; its links rewrite the date narrowing themselves,
      -- so they reduce the period parameter to its interval.
      yearsKept = [("period", w) | Just w <- [intervalPeriodExpr pivl]] ++ removeAttributes ["period"] carried
      yearsNav = yearsRow trs JournalR qparam today pspan yearsKept years
      intervalRow = inertControls trs $
        intervalLinks trs JournalR (removeAttributes ["period"] carried) qparam pspan rpInterval
      inertRows = inertControls trs $ mconcat
        [ columnsLinks trs JournalR (qps ++ removeAttributes ["total", "avg"] carried) totalOn avgOn
        , sortLinks trs JournalR (qps ++ removeAttributes ["sort"] carried) sortAmt
        , percentLinks trs JournalR (qps ++ removeAttributes ["pct"] carried) pctOn
        , calcLinks trs JournalR (qps ++ removeAttributes ["calc"] carried) calcCur
        ]
      -- All of them behind the controls disclosure, closed by default,
      -- in the same order as on the report pages.
      controlsRows = controlsToggle trs controlsOpen $ mconcat $
        [intervalRow, yearsNav, statusRow, realRow, typeRow, commodityRow, hintRow] ++ [depthRow | deepest > 1] ++
        [listRow, amountsRow, pricesRow, zeroRow, inertRows]

  defaultLayout $ do
    -- TRANSLATORS: the browser tab title of this page.
    setTitleI (HMsg "journal - hledger-web")
    $(widgetFile "journal")
