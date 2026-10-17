{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hledger.Web.Widget.Common
  ( accountQuery
  , accountOnlyQuery
  , balanceReportAsHtml
  , controlsHint
  , controlsToggle
  , inertControls
  , linksRow
  , linksRowClass
  , pricesLinks
  , commodityLinks
  , depthLinks
  , amountLinks
  , zeroBalanceLinks
  , statusLinks
  , realLinks
  , listModeLinks
  , columnsLinks
  , sortLinks
  , percentLinks
  , calcLinks
  , costsLinks
  , lotsLinks
  , closedLinks
  , holdingsValueLinks
  , typeLinks
  , AmountMode(..)
  , accumWord
  , listWord
  , valueWord
  , searchTerms
  , helplink
  , mixedAmountAsHtml
  , mixedAmountAsHtmlWith
  , mixedAmountAsHtmlElided
  , fromFormSuccess
  , writeJournalTextIfValidAndChanged
  , journalFile404
  , transactionFragment
  , removeDates
  , removeInacct
  , replaceInacct
  , journalDayQuery
  ) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, mapExceptT)
import Data.Char (isDigit)
import Data.Foldable (find, for_)
import Data.List (elemIndex)
import Data.Set qualified as Set
import Data.Maybe (isNothing, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day)
import Safe (lastMay, readMay)
import System.FilePath (takeFileName)
import Text.Blaze ((!), (!?), textValue)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Internal (preEscapedString)
import Text.Hamlet (hamletFile)
import Text.Printf (printf)
import Yesod

import Hledger.Utils.I18n (Translations, tr, trc, trf)
import Hledger
import Hledger.Cli.Anchor qualified as Anchor
import Hledger.Cli.Utils (writeFileWithBackupIfChanged)
import Hledger.Web.Settings (manualurl)
import Hledger.Query qualified as Query


journalFile404 :: FilePath -> Journal -> HandlerFor m (FilePath, Text)
journalFile404 f j =
  case find ((== f) . fst) (jfiles j) of
    Just (_, txt) -> pure (takeFileName f, txt)
    Nothing -> notFound

fromFormSuccess :: Applicative m => m a -> FormResult a -> m a
fromFormSuccess h FormMissing = h
fromFormSuccess h (FormFailure _) = h
fromFormSuccess _ (FormSuccess a) = pure a

-- | A helper for postEditR/postUploadR: check that the given text
-- parses as a Journal, and if so, write it to the given file, if the
-- text has changed. Or, return any error message encountered.
--
-- As a convenience for data received from web forms, which does not
-- have normalised line endings, line endings will be normalised (to \n)
-- before parsing.
--
-- The file will be written (if changed) with the current system's native
-- line endings (see writeFileWithBackupIfChanged).
--
writeJournalTextIfValidAndChanged :: MonadHandler m => FilePath -> Text -> ExceptT String m ()
writeJournalTextIfValidAndChanged f t = mapExceptT liftIO $ do
  -- Ensure unix line endings, since both readJournal (cf
  -- formatdirectivep, #1194) writeFileWithBackupIfChanged require them.
  -- XXX klunky. Any equivalent of "hSetNewlineMode h universalNewlineMode" for form posts ?
  let t' = T.replace "\r" "" t
  j <- readJournal definputopts (Just f) =<< liftIO (textToHandle t')
  _ <- liftIO $ j `seq` writeFileWithBackupIfChanged f t'  -- Only write backup if the journal didn't error
  return ()

-- | Link to a topic in the manual.
helplink :: Text -> Text -> HtmlUrl r
helplink topic label _ = H.a ! A.href u ! A.target "hledgerhelp" $ toHtml label
  where u = textValue $ manualurl <> if T.null topic then "" else T.cons '#' topic

-- | Render a "BalanceReport" as the sidebar: the journal and report
-- links (each report's route, label, and title, with the parameters
-- their links carry), then the accounts. The current page's row is marked.
balanceReportAsHtml ::
  Eq r => (r, r) -> r -> [(r, Text, Text)] -> [(Text, Text)] -> Bool -> Text -> Text -> Translations -> Journal -> Text -> [QueryOpt] -> BalanceReport -> HtmlUrl r
balanceReportAsHtml (journalR, registerR) here reports reportParams hideEmpty totlabel tottitle trs j qparam qopts (items, total) =
  $(hamletFile "templates/balance-report.hamlet")
  where
    l = ledgerFromJournal Any j
    indent a = preEscapedString $ concat $ replicate (2 + 2 * a) "&nbsp;"
    hasSubAccounts acct = maybe True (not . null . asubs) $ ledgerAccount l acct
    isInterestingAccount acct = maybe False isInteresting $ ledgerAccount l acct
      where isInteresting a = not (all (mixedAmountLooksZero . bdexcludingsubs) . pdperiods $ adata a) || any isInteresting (asubs a)
    matchesAcctSelector acct = Just True == ((`matchesAccount` acct) <$> inAccountQuery qopts)
    -- The journal and register links keep the states those pages read:
    -- the amounts mode, the accounts mode, inferred prices, zero
    -- balances, and lot detail; each link rewrites the search itself.
    kept = [p | p@(k, _) <- reportParams, k `elem` ["value", "list", "infer", "empty", "lots"]]
    acctlink acct = (registerR, ("q", replaceInacct qparam $ accountQuery acct) : kept)
    acctonlylink acct = (registerR, ("q", replaceInacct qparam $ accountOnlyQuery acct) : kept)
    -- The journal, and the register of everything, for the sidebar's
    -- search minus any account term; the search form's clear button is
    -- the way out of a search.
    journallink = (journalR, [("q", t) | let t = T.unwords $ removeInacct qparam, not (T.null t)] ++ kept)
    totallink = (registerR, [("q", t) | let t = T.unwords $ removeInacct qparam, not (T.null t)] ++ kept)

-- | A row of links above a report: a label, then each link's label,
-- title, target, and whether it is the one being shown.
-- Each label and title is a whole phrase, not a word slotted into a
-- sentence: an adjective that fits one language's sentence does not fit
-- another's, so a translation cannot be assembled from parts.
linksRow :: Text -> [(Text, Text, (r, [(Text, Text)]), Bool)] -> HtmlUrl r
linksRow = linksRowClass ""

-- | 'linksRow' with an extra class on the row: a hook for hledger.js's
-- keyboard shortcuts.
linksRowClass :: Text -> Text -> [(Text, Text, (r, [(Text, Text)]), Bool)] -> HtmlUrl r
linksRowClass cls rowlabel items renderUrl =
    H.p ! A.class_ (H.toValue $ T.unwords $ "report-links" : [cls | not (T.null cls)]) $ do
      H.span ! A.class_ "row-label" $ H.toHtml rowlabel
      for_ items $ \(label, title, link, current) ->
        H.a !? (current, A.class_ "current")
            ! A.href (H.toValue $ uncurry renderUrl link)
            ! A.title (H.toValue title) $
          H.toHtml label

-- | A page's rows of report controls behind a disclosure, closed by
-- default so the page stays uncluttered. Opening it is remembered per
-- browser in the showcontrols cookie (hledger.js), which the server
-- reads (App.showControls), so each page is born the way the last one
-- was left and nothing moves after paint.
controlsToggle :: Translations -> Bool -> HtmlUrl r -> HtmlUrl r
controlsToggle trs open rows =
  -- TRANSLATORS: the disclosure hiding each page's rows of report controls: its text, and its tooltip.
  [hamlet|
<details .report-controls :open:open>
  <summary title=#{tr trs "Show the options that reshape this view"}>#{tr trs "Options"}
  ^{rows}
|]

-- | Rows of controls that take effect on other pages, their labels
-- muted: they show and preserve the url's report state here too, so it
-- is never invisible, and never lost by following a link.
inertControls :: Translations -> HtmlUrl r -> HtmlUrl r
inertControls trs rows =
  -- TRANSLATORS: the tooltip on the muted control rows of the journal and register pages.
  [hamlet|
<div .controls-inert title=#{tr trs "These controls take effect on the report pages"}>
  ^{rows}
|]

-- | A short usage hint below a row of controls, in muted small text.
-- The text arrives already translated, so that the literal at the call
-- site is what the extraction tool collects.
controlsHint :: Text -> HtmlUrl r
controlsHint t _ = H.p ! A.class_ "controls-hint" $ H.toHtml t

-- | Links choosing where market prices come from: declared ones only,
-- or inferred from transaction costs too, keeping the given parameters;
-- the source being used is marked.
pricesLinks :: Translations -> r -> [(Text, Text)] -> Bool -> HtmlUrl r
pricesLinks trs route kept inferOn =
  -- TRANSLATORS: the label before a page's market price source links, and the links' text and tooltips.
  linksRow (tr trs "Prices:")
    [ (tr trs "Declared", tr trs "Value amounts using declared market prices (P directives) only", (route, kept), not inferOn)
    , (tr trs "Inferred too", tr trs "Infer market prices from transaction costs too (--infer-market-prices)", (route, kept ++ [("infer", "1")]), inferOn)
    ]

-- | The accum parameter value naming an accumulation mode.
accumWord :: BalanceAccumulation -> Text
accumWord = \case
  PerPeriod  -> "change"
  Historical -> "historical"
  Cumulative -> "cumulative"

-- | The list parameter value naming an accounts mode.
listWord :: AccountListMode -> Text
listWord ALTree = "tree"
listWord ALFlat = "flat"

-- | How a page shows amounts: in the commodities they were recorded in,
-- converted to cost (as with -B), or converted with one of the market
-- valuations (as with -V, -X, and --value).
data AmountMode = AsRecorded | AtCost | AtVal ValuationType
  deriving (Eq, Show)

-- | The value parameter naming an amount mode.
valueWord :: AmountMode -> Text
valueWord = \case
  AsRecorded -> "none"
  AtCost     -> "cost"
  AtVal vt   -> case vt of
    AtEnd  mc   -> "end"  <> comm mc
    AtThen mc   -> "then" <> comm mc
    AtNow  mc   -> "now"  <> comm mc
    AtDate d mc -> showDate d <> comm mc
  where comm = maybe "" ("," <>)

-- | Links to the same report page limited to each account depth, from 1
-- up to (but not including) the given deepest one, after an All link
-- that removes the limit. Each replaces the search's depth limit and
-- keeps its other terms and the given parameters; the depth being shown
-- is marked. (A depth:0 term, a single total, can be typed in the search.)
-- The digit keys set a depth through the All link's url (hledger.js).
depthLinks :: Translations -> r -> [(Text, Text)] -> Text -> Int -> HtmlUrl r
depthLinks trs route kept qparam deepest =
  -- TRANSLATORS: the label before a report's depth links, and the links' text and tooltips.
  linksRowClass "depth" (tr trs "Depth:") $
    (trc trs "depth" "All", tr trs "Show accounts at every depth", link Nothing, maybe True (>= deepest) current)
    : [ (T.pack (show n), depthTitle n, link (Just n), current == Just n)
      | n <- [1 .. deepest - 1] ]
  where
    current = searchDepth qparam
    -- TRANSLATORS: the tooltips of a report's depth links.
    depthTitle n = trf trs "Show accounts down to depth {depth}" [("depth", T.pack (show n))]
    link mn =
      (route, [("q", t) | let t = T.unwords $ removeDepth qparam ++ ["depth:" <> T.pack (show (n :: Int)) | Just n <- [mn]], not (T.null t)] ++ kept)

-- | Links to the same report page showing amounts as recorded, at cost,
-- or at market value, keeping the given parameters; the mode being
-- shown, if it is one of these, is marked. The page's default mode, the
-- startup one, needs no parameter. The V key toggles between the first
-- link (as recorded) and the last (market value), and B the second, at
-- cost (hledger.js).
amountLinks :: Translations -> r -> [(Text, Text)] -> Maybe AmountMode -> Maybe AmountMode -> HtmlUrl r
amountLinks trs route kept startup current =
  -- TRANSLATORS: the label before a report's amount conversion links, and the links' text and tooltips.
  linksRowClass "amounts" (tr trs "Amounts:")
    [ (label, title, (route, kept ++ [("value", valueWord m) | Just m /= startup]), Just m == current)
    | (label, title, m) <-
        [ (tr trs "As recorded",     tr trs "Show amounts in the commodities they were recorded in", AsRecorded)
        , (tr trs "At cost",         tr trs "Show amounts converted to their cost, as recorded in transactions (-B)", AtCost)
        , (tr trs "At market value", tr trs "Show amounts converted to their market value at the end of each period, using market prices (-V)", AtVal (AtEnd Nothing))
        ]
    ]

-- | Links that show or hide accounts with zero balances: each rewrites
-- the empty parameter (1 shows them, 0 hides them, the direction of
-- the command line's -E), keeping the given parameters; the state
-- being shown is marked. The page's startup default needs no
-- parameter. The e key follows the other link (hledger.js).
zeroBalanceLinks :: Translations -> r -> [(Text, Text)] -> Bool -> Bool -> HtmlUrl r
zeroBalanceLinks trs route kept dfltHidden hidden =
  -- TRANSLATORS: the label before the links that show or hide zero balances, and the links' text and tooltips.
  [hamlet|
$newline never
<p .report-links .zero-balances>
  <span .row-label>#{tr trs "Zero balances:"}
  <a href=@?{link True} :hidden:.current title=#{tr trs "Hide accounts whose balances are zero"}>#{tr trs "Hidden"}
  <a href=@?{link False} :not hidden:.current title=#{tr trs "Show accounts whose balances are zero"}>#{tr trs "Shown"}
|]
  where link h = (route, kept ++ [("empty", if h then "0" else "1") | h /= dfltHidden])

-- | A row of links that each rewrite one kind of search term: each link
-- replaces the terms the row owns (those the predicate matches) with its
-- own term, or removes them, keeping the search's other terms and the
-- given parameters. The link whose term the search has is marked, or the
-- removing link when it has none.
queryTermRow :: r -> [(Text, Text)] -> Text -> (Text -> Bool) -> Text -> Text -> [(Text, Text, Maybe Text)] -> HtmlUrl r
queryTermRow route kept qparam owns cls rowlabel items =
  linksRowClass cls rowlabel [ (label, title, link mterm, current mterm) | (label, title, mterm) <- items ]
  where
    terms = searchTerms qparam
    rest = map quoteIfSpaced $ filter (not . owns) terms
    current mterm = filter owns terms == maybeToList mterm
    link mterm =
      (route, [("q", t) | let t = T.unwords $ rest ++ map quoteIfSpaced (maybeToList mterm), not (T.null t)] ++ kept)

-- | Links filtering by transaction status, as hledger-ui's U, P, and C
-- keys do: each rewrites the status: term in the search. The same keys
-- follow them, by their order here (hledger.js).
statusLinks :: Translations -> r -> [(Text, Text)] -> Text -> HtmlUrl r
statusLinks trs route kept qparam =
  -- TRANSLATORS: the label before a report's status filter links, and the links' text and tooltips.
  queryTermRow route kept qparam (T.isPrefixOf "status:") "status" (tr trs "Status:")
    [ (trc trs "status" "All", tr trs "Show transactions of any status", Nothing)
    , (tr trs "Unmarked", tr trs "Show only unmarked transactions (status:)",     Just "status:")
    , (tr trs "Pending",  tr trs "Show only pending transactions (status:!)",    Just "status:!")
    , (tr trs "Cleared",  tr trs "Show only cleared transactions (status:*)",    Just "status:*")
    ]

-- | Links filtering to real or virtual postings: each rewrites the
-- real: term in the search. The R key toggles Real, the second link
-- (hledger.js).
realLinks :: Translations -> r -> [(Text, Text)] -> Text -> HtmlUrl r
realLinks trs route kept qparam =
  -- TRANSLATORS: the label before a report's real/virtual filter links, and the links' text and tooltips.
  queryTermRow route kept qparam (T.isPrefixOf "real:") "postings" (tr trs "Postings:")
    [ (trc trs "postings" "All", tr trs "Show real and virtual postings", Nothing)
    , (tr trs "Real",    tr trs "Show only real postings, as -R does (real:1)", Just "real:1")
    , (tr trs "Virtual", tr trs "Show only virtual postings, the parenthesized ones (real:0)", Just "real:0")
    ]

-- | Links showing the report's accounts as a flat list or a tree,
-- keeping the given parameters; the mode being shown is marked, and
-- the page's default mode, the startup one, needs no parameter. The t
-- key toggles them (hledger.js).
listModeLinks :: Translations -> r -> [(Text, Text)] -> AccountListMode -> AccountListMode -> HtmlUrl r
listModeLinks trs route kept dflt mode =
  -- TRANSLATORS: the label before a report's list/tree links, and the links' text and tooltips.
  linksRowClass "accounts" (tr trs "Accounts:")
    [ (trc trs "accounts" "List", tr trs "Show accounts as a flat list (--flat)", link ALFlat, mode == ALFlat)
    , (trc trs "accounts" "Tree", tr trs "Show accounts as a tree (--tree)", link ALTree, mode == ALTree)
    ]
  where link m = (route, kept ++ [("list", listWord m) | m /= dflt])

-- | Toggles adding a row-total and a row-average column to a
-- multi-period report, as -T and -A do; the columns shown are marked.
-- The T and A keys follow these links, by their order here (hledger.js).
columnsLinks :: Translations -> r -> [(Text, Text)] -> Bool -> Bool -> HtmlUrl r
columnsLinks trs route kept total avg =
  -- TRANSLATORS: the label before a report's extra-column toggles, and the toggles' text and tooltips.
  linksRowClass "columns" (tr trs "Extra columns:")
    [ (trc trs "column" "Total", tr trs "Add a column totaling each row (-T)",
        (route, kept ++ [("avg", "1") | avg] ++ [("total", "1") | not total]), total)
    , (trc trs "column" "Average", tr trs "Add a column averaging each row (-A)",
        (route, kept ++ [("total", "1") | total] ++ [("avg", "1") | not avg]), avg)
    ]

-- | Links sorting the report's rows by account name or by amount,
-- keeping the given parameters; the order being shown is marked.
-- The S key toggles By amount, the second link (hledger.js).
sortLinks :: Translations -> r -> [(Text, Text)] -> Bool -> HtmlUrl r
sortLinks trs route kept byAmount =
  -- TRANSLATORS: the label before a report's sort links, and the links' text and tooltips.
  linksRowClass "sort" (tr trs "Sort:")
    [ (tr trs "By account", tr trs "Sort rows by account name", (route, kept), not byAmount)
    , (tr trs "By amount", tr trs "Sort rows by their amounts, largest first (-S)", (route, kept ++ [("sort", "amount")]), byAmount)
    ]

-- | Links showing the report's numbers as amounts or as percentages of
-- their column's total, keeping the given parameters.
percentLinks :: Translations -> r -> [(Text, Text)] -> Bool -> HtmlUrl r
percentLinks trs route kept pct =
  -- TRANSLATORS: the label before a report's amounts/percentages links, and the links' text and tooltips.
  linksRow (tr trs "Numbers:")
    [ (trc trs "numbers" "Amounts", tr trs "Show the amounts", (route, kept), not pct)
    , (tr trs "Percentages", tr trs "Show each amount as a percentage of its column's total (-%)", (route, kept ++ [("pct", "1")]), pct)
    ]

-- | Links choosing what the report's cells calculate, as the balance
-- commands' --valuechange, --gain, and --count flags do.
calcLinks :: Translations -> r -> [(Text, Text)] -> BalanceCalculation -> HtmlUrl r
calcLinks trs route kept calc =
  -- TRANSLATORS: the label before a report's calculation links, and the links' text and tooltips.
  linksRow (tr trs "Calculate:")
    [ (trc trs "calculation" "Sums", tr trs "Show sums of postings", (route, kept), calc == CalcChange)
    , (tr trs "Value change", tr trs "Show how period-end market values changed (--valuechange)", link "valuechange", calc == CalcValueChange)
    , (tr trs "Unrealized gain", tr trs "Show unrealized capital gains: market value minus cost (--gain)", link "gain", calc == CalcGain)
    , (tr trs "Posting counts", tr trs "Count the postings instead of summing them (--count)", link "count", calc == CalcPostingsCount)
    ]
  where link w = (route, kept ++ [("calc", w)])

-- | Links showing or hiding the costs recorded with a register's
-- amounts (10 AAPL @ $95), keeping the given parameters.
costsLinks :: Translations -> r -> [(Text, Text)] -> Bool -> HtmlUrl r
costsLinks trs route kept on =
  -- TRANSLATORS: the label before the register's cost links, and the links' text and tooltips.
  linksRow (tr trs "Costs:")
    [ (trc trs "costs" "Hidden", tr trs "Show amounts without their costs", (route, kept), not on)
    , (trc trs "costs" "Shown", tr trs "Show amounts with the costs recorded in the journal", (route, kept ++ [("costs", "1")]), on)
    ]

-- | Links that show or hide lot detail: each holding's individual lots,
-- shown as lot subaccounts (the lots parameter; 1 shows them).
lotsLinks :: Translations -> r -> [(Text, Text)] -> Bool -> HtmlUrl r
lotsLinks trs route kept on =
  -- TRANSLATORS: the label before a report's lot detail links, and the links' text and tooltips.
  linksRow (tr trs "Lots:")
    [ (trc trs "lots" "Hidden", tr trs "Aggregate each account's lots", (route, kept), not on)
    , (trc trs "lots" "Shown", tr trs "Show each lot separately (--lots)", (route, kept ++ [("lots", "1")]), on)
    ]

-- | Links that show or hide fully closed positions: commodities no
-- longer held, whose realized gains still count in the totals (the
-- empty parameter, as for zero balances; 1 shows them).
closedLinks :: Translations -> r -> [(Text, Text)] -> Bool -> Bool -> HtmlUrl r
closedLinks trs route kept dfltHidden hidden =
  -- TRANSLATORS: the label before the holdings page's closed-position links, and the links' text and tooltips.
  linksRow (tr trs "Closed positions:")
    [ (trc trs "closed" "Hidden", tr trs "Show only open positions", link True, hidden)
    , (trc trs "closed" "Shown", tr trs "Also show fully disposed positions and their realized gains (-E)", link False, not hidden)
    ]
  where link h = (route, kept ++ [("empty", if h then "0" else "1") | h /= dfltHidden])

-- | Links choosing how the holdings page values holdings: each lot in
-- its own cost commodity (the default), or all in the market valuation
-- commodity, as -V does.
holdingsValueLinks :: Translations -> r -> [(Text, Text)] -> Bool -> HtmlUrl r
holdingsValueLinks trs route kept valued =
  -- TRANSLATORS: the label before the holdings page's valuation links, and the links' text and tooltips.
  linksRow (tr trs "Values:")
    [ (tr trs "Per cost commodity", tr trs "Value each holding in its own cost commodity", (route, kept), not valued)
    , (tr trs "At market value", tr trs "Convert values and costs to the market valuation commodity (-V)", (route, kept ++ [("value", "end")]), valued)
    ]

-- | Links filtering to accounts of one or more types: a link rewrites
-- the type: term in the search to its own type, and shift-clicking adds
-- or removes a type (hledger.js), the selection carried as one
-- type:AL.. term, whose letters mark their links. Not for the statement
-- pages, which choose their own types.
typeLinks :: Translations -> r -> [(Text, Text)] -> Text -> HtmlUrl r
typeLinks trs route kept qparam renderUrl =
  H.div ! A.class_ "types" $ do
    -- TRANSLATORS: the label before the account type filter links, and the links' text and tooltips.
    linksRow (tr trs "Type:")
      ( (trc trs "account type" "All", tr trs "Show accounts of every type", link Nothing, null selected)
      : [ (label, title, link (Just letter), letter `elem` selected)
        | (label, title, letter) <- types ]) renderUrl
  where
    types =
      [ (tr trs "Asset",      tr trs "Show only asset accounts (type:A)",      'A')
      , (tr trs "Liability",  tr trs "Show only liability accounts (type:L)",  'L')
      , (tr trs "Equity",     tr trs "Show only equity accounts (type:E)",     'E')
      , (tr trs "Revenue",    tr trs "Show only revenue accounts (type:R)",    'R')
      , (tr trs "Expense",    tr trs "Show only expense accounts (type:X)",    'X')
      , (tr trs "Cash",       tr trs "Show only cash accounts (type:C)",       'C')
      , (tr trs "Conversion", tr trs "Show only conversion accounts (type:V)", 'V')
      ]
    terms = searchTerms qparam
    rest = map quoteIfSpaced $ filter (not . T.isPrefixOf "type:") terms
    -- the letters of the search's type: terms
    selected = [c | t <- terms, Just ls <- [T.stripPrefix "type:" t], c <- T.unpack (T.toUpper ls)]
    link mletter =
      (route, [("q", t) | let t = T.unwords $ rest ++ ["type:" <> T.singleton l | Just l <- [mletter]], not (T.null t)] ++ kept)

-- | Links filtering to one or more of the journal's commodities: a link
-- rewrites the cur: term in the search to its own commodity, and
-- shift-clicking adds or removes one (hledger.js), the selection
-- carried as a single cur:A|B term, whose alternatives mark their
-- links. The symbols are regex-escaped, since cur: matches whole
-- symbols as regular expressions. Each link carries its pattern, and
-- the row the search's other terms, as data attributes, so the script
-- composes selections without parsing the search: any symbol works,
-- spaces included, a composed term containing one being quoted whole.
-- The given pre-translated hint renders below the row, and fewer than
-- two commodities render nothing, hint included.
-- The collapsed list's open state is remembered per browser in the
-- showcommodities cookie (hledger.js), rendered here server side like
-- the controls disclosure's.
commodityLinks :: Translations -> r -> [(Text, Text)] -> Text -> Bool -> Text -> Journal -> HtmlUrl r
commodityLinks trs route kept qparam open hint j renderUrl
  | length syms < 2 = mempty
  -- A few commodities are a row like any other; many collapse to a
  -- line showing the selection, the full list opening on demand.
  | length syms <= 6 =
      (H.div ! A.class_ "commodities" ! H.dataAttribute "rest" (H.toValue restText) $
        links True)
      <> hinted
  | otherwise =
      (H.div ! A.class_ "commodities" ! H.dataAttribute "rest" (H.toValue restText) $
        H.details !? (open, A.open "open") $ do
          H.summary ! A.title (H.toValue $ tr trs "List all the commodities") $ do
            -- TRANSLATORS: the label before a page's commodity filter links, and the links' text and tooltips.
            H.span ! A.class_ "row-label" $ H.toHtml $ tr trs "Commodity:"
            H.span ! A.class_ "sel" $ H.toHtml selectionText
          links False)
      <> hinted
  where
    hinted = controlsHint hint renderUrl
    links withLabel =
      H.p ! A.class_ "report-links" $ do
        H.span ! A.class_ "row-label" $
          if withLabel then H.toHtml $ tr trs "Commodity:" else mempty
        H.a !? (null selected, A.class_ "current")
            ! A.href (H.toValue $ uncurry renderUrl alllink)
            ! A.title (H.toValue $ tr trs "Show amounts in every commodity") $
          H.toHtml $ trc trs "commodity" "All"
        for_ syms $ \sym -> do
          let pat = regexEscape sym
          H.a !? (pat `elem` selected, A.class_ "current")
              ! A.href (H.toValue $ uncurry renderUrl $ symlink pat)
              ! A.title (H.toValue $ trf trs "Show only amounts in {commodity}" [("commodity", sym)])
              ! H.dataAttribute "cur" (H.toValue pat) $
            H.toHtml sym
    chosen = [sym | sym <- syms, regexEscape sym `elem` selected]
    selectionText
      | null chosen = trc trs "commodity" "All"
      | otherwise = T.intercalate ", " chosen
    syms = filter (not . T.null) $ Set.toAscList $ journalCommodities j
    terms = searchTerms qparam
    rest = map quoteIfSpaced $ filter (not . T.isPrefixOf "cur:") terms
    restText = T.unwords rest
    -- the alternatives of the search's cur: term(s), marking their links
    selected = concatMap (splitAlternatives . T.drop 4) $ filter (T.isPrefixOf "cur:") terms
    symlink pat = (route, ("q", T.unwords $ rest ++ [quoteIfSpaced $ "cur:" <> pat]) : kept)
    alllink = (route, [("q", restText) | not (T.null restText)] ++ kept)
    -- Split a cur: pattern on its unescaped |s, regex alternation.
    splitAlternatives = map T.pack . go . T.unpack
      where
        go s = case break (== '|') s of
          (a, '|':b)
            | odd (length (takeWhile (== '\\') (reverse a))) ->
                case go b of (c:cs) -> (a ++ "|" ++ c) : cs; [] -> [a ++ "|"]
            | otherwise -> a : go b
          (a, _) -> [a]

accountQuery :: AccountName -> Text
accountQuery = ("inacct:" <>) . quoteForQuery

accountOnlyQuery :: AccountName -> Text
accountOnlyQuery = ("inacctonly:" <>) . quoteForQuery

mixedAmountAsHtml :: MixedAmount -> HtmlUrl a
mixedAmountAsHtml = mixedAmountAsHtmlWith noCostFmt{displayZeroCommodity=True}

-- | Like 'mixedAmountAsHtml' with a chosen display format, for a page
-- showing amounts with their costs.
mixedAmountAsHtmlWith :: AmountFormat -> MixedAmount -> HtmlUrl a
mixedAmountAsHtmlWith fmt b _ =
  for_ (lines (showMixedAmountWith fmt b)) $ \t -> do
    H.span ! A.class_ c $ toHtml t
    H.br
  where
    c = case isNegativeMixedAmount b of
      Just True -> "negative amount"
      _ -> "positive amount"

-- | Like 'mixedAmountAsHtml', but showing at most the given number of
-- commodities, then a line saying how many more there are: for the
-- sidebar, where an account holding many commodities would otherwise
-- be a wall of lines. Searching for a commodity (cur:) shows it alone.
mixedAmountAsHtmlElided :: Translations -> Int -> MixedAmount -> HtmlUrl a
mixedAmountAsHtmlElided trs n b _ = do
    for_ (take n ls) $ \t -> do
      H.span ! A.class_ c $ toHtml t
      H.br
    when (length ls > n) $ do
      -- TRANSLATORS: the sidebar's note for a balance with more commodities than it shows.
      H.span ! A.class_ "more-commodities" $ toHtml $
        trf trs "… and {n} more" [("n", T.pack $ show $ length ls - n)]
      H.br
  where
    ls = lines (showMixedAmountWith noCostFmt{displayZeroCommodity=True} b)
    c = case isNegativeMixedAmount b of
      Just True -> "negative amount"
      _ -> "positive amount"

-- Make a slug to uniquely identify this transaction
-- in hyperlinks (as far as possible).
transactionFragment :: Journal -> Transaction -> String
transactionFragment j Transaction{tindex, tsourcepos} = 
  printf "transaction-%d-%d" tfileindex tindex
  where
    -- the numeric index of this txn's file within all the journal files,
    -- or 0 if this txn has no known file (eg a forecasted txn)
    tfileindex = maybe 0 (+1) $ elemIndex (sourceName $ fst tsourcepos) (journalFilePaths j)

-- | The search's terms without its date terms, each quoted if it needs to be.
removeDates :: Text -> [Text]
removeDates = map quoteIfSpaced . Anchor.removeDates . searchTerms

-- | The search's terms without those naming an account, each quoted if it needs to be.
removeInacct :: Text -> [Text]
removeInacct = map quoteIfSpaced . Anchor.removeInacct . searchTerms

-- | The search's terms without its depth:N terms, each quoted if it needs
-- to be. Its depth:REGEX=N terms, which limit some accounts, stay.
removeDepth :: Text -> [Text]
removeDepth = map quoteIfSpaced . filter (isNothing . flatDepth) . searchTerms

-- | The search's depth limit, from its last depth:N term.
searchDepth :: Text -> Maybe Int
searchDepth = lastMay . mapMaybe flatDepth . searchTerms

-- | The depth a depth:N term limits all accounts to.
flatDepth :: Text -> Maybe Int
flatDepth t = do
  n <- T.stripPrefix "depth:" t
  if not (T.null n) && T.all isDigit n then readMay (T.unpack n) else Nothing

-- | The search's terms; none for an empty search.
searchTerms :: Text -> [Text]
searchTerms = filter (not . T.null) . Query.words'' queryprefixes

-- | The search for the journal page narrowed to a day: the day's date
-- term in place of any date terms, and without any account term, which
-- the journal page does not use.
journalDayQuery :: Text -> Day -> Text
journalDayQuery qparam d =
  T.unwords $ ("date:" <> showDate d) : removeDates (T.unwords $ removeInacct qparam)

replaceInacct :: Text -> Text -> Text
replaceInacct q acct = T.unwords $ acct : removeInacct q
