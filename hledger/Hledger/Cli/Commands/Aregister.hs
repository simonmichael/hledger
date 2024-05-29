{-|

The @aregister@ command lists a single account's transactions,
like the account register in hledger-ui and hledger-web,
and unlike the register command which lists postings across multiple accounts.

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Aregister (
  aregistermode
 ,aregister
 -- ,showPostingWithBalanceForVty
 ,tests_Aregister
) where

import Data.Default (def)
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Lucid as L hiding (value_)
import System.Console.CmdArgs.Explicit (flagNone, flagReq)

import Hledger
import Hledger.Read.CsvUtils (CSV, CsvRecord, printCSV, printTSV)
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils
import Text.Tabular.AsciiWide hiding (render)

aregistermode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Aregister.txt")
  ([
   flagNone ["txn-dates"] (setboolopt "txn-dates") 
     "filter strictly by transaction date, not posting date. Warning: this can show a wrong running balance."
   ,flagNone ["no-elide"] (setboolopt "no-elide") "don't show only 2 commodities per amount"
  --  flagNone ["cumulative"] (setboolopt "cumulative")
  --    "show running total from report start date (default)"
  -- ,flagNone ["historical","H"] (setboolopt "historical")
  --    "show historical running total/balance (includes postings before report start date)\n "
  -- ,flagNone ["average","A"] (setboolopt "average")
  --    "show running average of posting amounts instead of total (implies --empty)"
  -- ,flagNone ["related","r"] (setboolopt "related") "show postings' siblings instead"
  -- ,flagNone ["invert"] (setboolopt "invert") "display all amounts with reversed sign"
  ,flagReq  ["width","w"] (\s opts -> Right $ setopt "width" s opts) "N"
     ("set output width (default: " ++
#ifdef mingw32_HOST_OS
      show defaultWidth
#else
      "terminal width"
#endif
      ++ " or $COLUMNS). -wN,M sets description width as well."
     )
  ,flagNone ["align-all"] (setboolopt "align-all") "guarantee alignment across all lines (slower)"
  ,outputFormatFlag ["txt","html","csv","tsv","json"]
  ,outputFileFlag
  ])
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "ACCTPAT [QUERY]")

-- based on Hledger.UI.RegisterScreen:

-- | Print an account register report for a specified account.
aregister :: CliOpts -> Journal -> IO ()
aregister opts@CliOpts{rawopts_=rawopts,reportspec_=rspec} j = do
  -- the first argument specifies the account, any remaining arguments are a filter query
  let help = "aregister needs an ACCTPAT argument to select an account"
  (apat,querystr) <- case listofstringopt "args" rawopts of
      []     -> error' $ help <> ".\nPlease provide an account name or a (case-insensitive, infix, regexp) pattern."
      (a:as) -> return (a, map T.pack as)
  let
    -- keep synced with accounts --find
    acct = fromMaybe (error' $ help <> ",\nbut " ++ show apat++" did not match any account.")   -- PARTIAL:
           . firstMatch $ journalAccountNamesDeclaredOrImplied j
    firstMatch = case toRegexCI $ T.pack apat of
        Right re -> find (regexMatchText re)
        Left  _  -> const Nothing
    -- gather report options
    inclusive = True  -- tree_ ropts
    thisacctq = Acct $ (if inclusive then accountNameToAccountRegex else accountNameToAccountOnlyRegex) acct
    ropts' = (_rsReportOpts rspec) {
        -- ignore any depth limit, as in postingsReport; allows register's total to match balance reports (cf #1468)
        depth_=Nothing
        -- always show historical balance
      , balanceaccum_= Historical
      , querystring_ = querystr
      }
    wd = whichDate ropts'
  -- and regenerate the ReportSpec, making sure to use the above
  rspec' <- either fail return $ updateReportSpec ropts' rspec
  let
    -- run the report
    -- TODO: need to also pass the queries so we can choose which date to render - move them into the report ?
    items = accountTransactionsReport rspec' j thisacctq
    items' =
      styleAmounts (journalCommodityStyles j) $
      (if empty_ ropts' then id else filter (not . mixedAmountLooksZero . fifth6)) $
      reverse items
    -- select renderer
    render | fmt=="txt"  = accountTransactionsReportAsText opts (_rsQuery rspec') thisacctq
           | fmt=="html" = accountTransactionsReportAsHTML opts (_rsQuery rspec') thisacctq
           | fmt=="csv"  = printCSV . accountTransactionsReportAsCsv wd (_rsQuery rspec') thisacctq
           | fmt=="tsv"  = printTSV . accountTransactionsReportAsCsv wd (_rsQuery rspec') thisacctq
           | fmt=="json" = toJsonText
           | otherwise   = error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
      where
        fmt = outputFormatFromOpts opts

  writeOutputLazyText opts $ render items'

accountTransactionsReportAsCsv :: WhichDate -> Query -> Query -> AccountTransactionsReport -> CSV
accountTransactionsReportAsCsv wd reportq thisacctq is =
  ["txnidx","date","code","description","otheraccounts","change","balance"]
  : map (accountTransactionsReportItemAsCsvRecord wd reportq thisacctq) is

accountTransactionsReportItemAsCsvRecord :: WhichDate -> Query -> Query -> AccountTransactionsReportItem -> CsvRecord
accountTransactionsReportItemAsCsvRecord
  wd reportq thisacctq
  (t@Transaction{tindex,tcode,tdescription}, _, _issplit, otheracctsstr, change, balance)
  = [idx,date,tcode,tdescription,otheracctsstr,amt,bal]
  where
    idx  = T.pack $ show tindex
    date = showDate $ transactionRegisterDate wd reportq thisacctq t
    amt  = wbToText $ showMixedAmountB machineFmt change
    bal  = wbToText $ showMixedAmountB machineFmt balance

-- | Render a register report as a HTML snippet.
accountTransactionsReportAsHTML :: CliOpts -> Query -> Query -> AccountTransactionsReport -> TL.Text
accountTransactionsReportAsHTML copts reportq thisacctq items =
    L.renderText $ L.table_ (do L.thead_ (L.tr_ (do L.th_ "date"
                                                    L.th_ "description"
                                                    L.th_ "otheraccounts"
                                                    L.th_ "change"
                                                    L.th_ "balance"))
                                L.tbody_ (mconcat (map (htmlRow copts reportq thisacctq) items)))

-- | Render one account register report line item as a HTML table row snippet.
htmlRow :: CliOpts -> Query -> Query -> AccountTransactionsReportItem -> L.Html ()
htmlRow CliOpts{reportspec_=ReportSpec{_rsReportOpts=ropts}} reportq thisacctq
    (t@Transaction{tdescription}, _, _issplit, otheracctsstr, amt, bal) =
    L.tr_ (do (L.td_ . toHtml . show . transactionRegisterDate (whichDate ropts) reportq thisacctq) t
              (L.td_ . toHtml) tdescription
              (L.td_ . toHtml) otheracctsstr
              -- piggy back on the oneLineNoCostFmt display style for now.
              (L.td_ . toHtml . wbUnpack . showMixedAmountB oneLineNoCostFmt) amt
              (L.td_ . toHtml . wbUnpack . showMixedAmountB oneLineNoCostFmt) bal)

-- | Render a register report as plain text suitable for console output.
accountTransactionsReportAsText :: CliOpts -> Query -> Query -> AccountTransactionsReport -> TL.Text
accountTransactionsReportAsText copts reportq thisacctq items = TB.toLazyText $
    title <> TB.singleton '\n' <>
    postingsOrTransactionsReportAsText alignAll copts itemAsText itemamt itembal items
  where
    alignAll = boolopt "align-all" $ rawopts_ copts
    itemAsText = accountTransactionsReportItemAsText copts reportq thisacctq
    itemamt (_,_,_,_,a,_) = a
    itembal (_,_,_,_,_,a) = a

    -- show a title indicating which account was picked, which can be confusing otherwise
    title = maybe mempty (\s -> foldMap TB.fromText ["Transactions in ", s, " and subaccounts", qmsg, ":"]) macct
      where
        -- XXX temporary hack ? recover the account name from the query
        macct = case filterQuery queryIsAcct thisacctq of
                  Acct r -> Just . T.drop 1 . T.dropEnd 5 $ reString r  -- Acct "^JS:expenses(:|$)"
                  _      -> Nothing  -- shouldn't happen
        -- show a hint in the title when results are restricted by an extra query (other than depth or date or date2)
        qmsg = if hasextraquery then " (matching query)" else ""
          where
            hasextraquery =
              length (querystring_ $ _rsReportOpts $ reportspec_ copts) > 1
              && not (queryIsNull $ filterQuery (not.(\q->queryIsDepth q || queryIsDateOrDate2 q)) reportq)

-- | Render one account register report line item as plain text. Layout is like so:
-- @
-- <---------------- width (specified, terminal width, or 80) -------------------->
-- date (10)  description           other accounts       change (12)   balance (12)
-- DDDDDDDDDD dddddddddddddddddddd  aaaaaaaaaaaaaaaaaaa  AAAAAAAAAAAA  AAAAAAAAAAAA
-- @
-- If description's width is specified, account will use the remaining space.
-- Otherwise, description and account divide up the space equally.
--
-- Returns a string which can be multi-line, eg if the running balance
-- has multiple commodities.
--
accountTransactionsReportItemAsText :: CliOpts -> Query -> Query -> Int -> Int
                                    -> (AccountTransactionsReportItem, [WideBuilder], [WideBuilder])
                                    -> TB.Builder
accountTransactionsReportItemAsText
  copts@CliOpts{reportspec_=ReportSpec{_rsReportOpts=ropts}}
  reportq thisacctq preferredamtwidth preferredbalwidth
  ((t@Transaction{tdescription}, _, _issplit, otheracctsstr, _, _), amt, bal) =
    -- Transaction -- the transaction, unmodified
    -- Transaction -- the transaction, as seen from the current account
    -- Bool        -- is this a split (more than one posting to other accounts) ?
    -- String      -- a display string describing the other account(s), if any
    -- MixedAmount -- the amount posted to the current account(s) (or total amount posted)
    -- MixedAmount -- the register's running total or the current account(s)'s historical balance, after this transaction
    table <> TB.singleton '\n'
  where
    table = renderRowB def{tableBorders=False, borderSpaces=False} . Group NoLine $ map Header
      [ textCell TopLeft $ fitText (Just datewidth) (Just datewidth) True True date
      , spacerCell
      , textCell TopLeft $ fitText (Just descwidth) (Just descwidth) True True tdescription
      , spacerCell2
      , textCell TopLeft $ fitText (Just acctwidth) (Just acctwidth) True True accts
      , spacerCell2
      , Cell TopRight $ map (pad amtwidth) amt
      , spacerCell2
      , Cell BottomRight $ map (pad balwidth) bal
      ]
    spacerCell  = Cell BottomLeft [WideBuilder (TB.singleton ' ') 1]
    spacerCell2 = Cell BottomLeft [WideBuilder (TB.fromString "  ") 2]
    pad fullwidth amt1 = WideBuilder (TB.fromText $ T.replicate w " ") w <> amt1
      where w = fullwidth - wbWidth amt1
    -- calculate widths
    (totalwidth,mdescwidth) = registerWidthsFromOpts copts
    (datewidth, date) = (10, showDate $ transactionRegisterDate wd reportq thisacctq t)
      where wd = whichDate ropts
    (amtwidth, balwidth)
      | shortfall <= 0 = (preferredamtwidth, preferredbalwidth)
      | otherwise      = (adjustedamtwidth, adjustedbalwidth)
      where
        mincolwidth = 2 -- columns always show at least an ellipsis
        maxamtswidth = max 0 (totalwidth - (datewidth + 1 + mincolwidth + 2 + mincolwidth + 2 + 2))
        shortfall = (preferredamtwidth + preferredbalwidth) - maxamtswidth
        amtwidthproportion = fromIntegral preferredamtwidth / fromIntegral (preferredamtwidth + preferredbalwidth)
        adjustedamtwidth = round $ amtwidthproportion * fromIntegral maxamtswidth
        adjustedbalwidth = maxamtswidth - adjustedamtwidth

    remaining = totalwidth - (datewidth + 1 + 2 + amtwidth + 2 + balwidth)
    (descwidth, acctwidth) = (w, remaining - 2 - w)
      where w = fromMaybe ((remaining - 2) `div` 2) mdescwidth

    -- gather content
    accts = -- T.unpack $ elideAccountName acctwidth $ T.pack
            otheracctsstr

-- tests

tests_Aregister = testGroup "Aregister" [

 ]
