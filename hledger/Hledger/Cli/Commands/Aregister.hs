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

import Data.List (find, intersperse)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Time (addDays)
import System.Console.CmdArgs.Explicit (flagNone, flagReq)
import Hledger.Read.CsvReader (CSV, CsvRecord, printCSV)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils

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
  ,outputFormatFlag ["txt","csv","json"]
  ,outputFileFlag
  ])
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "ACCTPAT [QUERY]")

-- based on Hledger.UI.RegisterScreen:

-- | Print an account register report for a specified account.
aregister :: CliOpts -> Journal -> IO ()
aregister opts@CliOpts{rawopts_=rawopts,reportspec_=rspec} j = do
  d <- getCurrentDay
  -- the first argument specifies the account, any remaining arguments are a filter query
  (apat,querystring) <- case listofstringopt "args" rawopts of
      []     -> fail "aregister needs an account, please provide an account name or pattern"
      (a:as) -> return (a, map T.pack as)
  argsquery <- either fail (return . fst) $ parseQueryList d querystring
  let
    acct = fromMaybe (error' $ show apat++" did not match any account")   -- PARTIAL:
           . firstMatch $ journalAccountNamesDeclaredOrImplied j
    firstMatch = case toRegexCI $ T.pack apat of
        Right re -> find (regexMatchText re)
        Left  _  -> const Nothing
    -- gather report options
    inclusive = True  -- tree_ ropts
    thisacctq = Acct $ (if inclusive then accountNameToAccountRegex else accountNameToAccountOnlyRegex) acct
    ropts' = (rsOpts rspec) {
        -- ignore any depth limit, as in postingsReport; allows register's total to match balance reports (cf #1468)
        depth_=Nothing
        -- always show historical balance
      , balancetype_= HistoricalBalance
      }
    -- and regenerate the ReportSpec, making sure to use the above
    rspec' = rspec{ rsQuery=simplifyQuery $ And [queryFromFlags ropts', argsquery]
                  , rsOpts=ropts'
                  }
    reportq = And [rsQuery rspec', excludeforecastq (isJust $ forecast_ ropts')]
      where
        -- As in RegisterScreen, why ? XXX
        -- Except in forecast mode, exclude future/forecast transactions.
        excludeforecastq True = Any
        excludeforecastq False =  -- not:date:tomorrow- not:tag:generated-transaction
          And [
             Not (Date $ DateSpan (Just $ addDays 1 d) Nothing)
            ,Not generatedTransactionTag
          ]
    -- run the report
    -- TODO: need to also pass the queries so we can choose which date to render - move them into the report ?
    items = accountTransactionsReport rspec' j reportq thisacctq
    items' = (if empty_ ropts' then id else filter (not . mixedAmountLooksZero . fifth6)) $
             reverse items
    -- select renderer
    render | fmt=="txt"  = accountTransactionsReportAsText opts reportq thisacctq
           | fmt=="csv"  = printCSV . accountTransactionsReportAsCsv reportq thisacctq
           | fmt=="json" = toJsonText
           | otherwise   = error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
      where
        fmt = outputFormatFromOpts opts

  writeOutputLazyText opts $ render items'

accountTransactionsReportAsCsv :: Query -> Query -> AccountTransactionsReport -> CSV
accountTransactionsReportAsCsv reportq thisacctq is =
  ["txnidx","date","code","description","otheraccounts","change","balance"]
  : map (accountTransactionsReportItemAsCsvRecord reportq thisacctq) is

accountTransactionsReportItemAsCsvRecord :: Query -> Query -> AccountTransactionsReportItem -> CsvRecord
accountTransactionsReportItemAsCsvRecord
  reportq thisacctq
  (t@Transaction{tindex,tcode,tdescription}, _, _issplit, otheracctsstr, change, balance)
  = [idx,date,tcode,tdescription,otheracctsstr,amt,bal]
  where
    idx  = T.pack $ show tindex
    date = showDate $ transactionRegisterDate reportq thisacctq t
    amt  = wbToText $ showMixedAmountB oneLine change
    bal  = wbToText $ showMixedAmountB oneLine balance

-- | Render a register report as plain text suitable for console output.
accountTransactionsReportAsText :: CliOpts -> Query -> Query -> AccountTransactionsReport -> TL.Text
accountTransactionsReportAsText copts reportq thisacctq items
  = TB.toLazyText . unlinesB $
    title :
    map (accountTransactionsReportItemAsText copts reportq thisacctq amtwidth balwidth) items
  where
    amtwidth = maximumStrict $ 12 : map (wbWidth . showamt . itemamt) items
    balwidth = maximumStrict $ 12 : map (wbWidth . showamt . itembal) items
    showamt = showMixedAmountB oneLine{displayMinWidth=Just 12, displayMaxWidth=mmax}  -- color_
      where mmax = if no_elide_ . rsOpts . reportspec_ $ copts then Nothing else Just 32
    itemamt (_,_,_,_,a,_) = a
    itembal (_,_,_,_,_,a) = a
    -- show a title indicating which account was picked, which can be confusing otherwise
    title = maybe mempty (\s -> foldMap TB.fromText ["Transactions in ", s, " and subaccounts:"]) macct
      where
        -- XXX temporary hack ? recover the account name from the query
        macct = case filterQuery queryIsAcct thisacctq of
                  Acct r -> Just . T.drop 1 . T.dropEnd 5 $ reString r  -- Acct "^JS:expenses(:|$)"
                  _      -> Nothing  -- shouldn't happen

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
accountTransactionsReportItemAsText :: CliOpts -> Query -> Query -> Int -> Int -> AccountTransactionsReportItem -> TB.Builder
accountTransactionsReportItemAsText
  copts@CliOpts{reportspec_=ReportSpec{rsOpts=ReportOpts{color_}}}
  reportq thisacctq preferredamtwidth preferredbalwidth
  (t@Transaction{tdescription}, _, _issplit, otheracctsstr, change, balance) =
    -- Transaction -- the transaction, unmodified
    -- Transaction -- the transaction, as seen from the current account
    -- Bool        -- is this a split (more than one posting to other accounts) ?
    -- String      -- a display string describing the other account(s), if any
    -- MixedAmount -- the amount posted to the current account(s) (or total amount posted)
    -- MixedAmount -- the register's running total or the current account(s)'s historical balance, after this transaction
    foldMap TB.fromText . concat . intersperse (["\n"]) $
      [ fitText (Just datewidth) (Just datewidth) True True date
      , " "
      , fitText (Just descwidth) (Just descwidth) True True tdescription
      , "  "
      , fitText (Just acctwidth) (Just acctwidth) True True accts
      , "  "
      , amtfirstline
      , "  "
      , balfirstline
      ]
      :
      [ [ spacer, a, "  ", b ] | (a,b) <- zip amtrest balrest ]
  where
    -- calculate widths
    (totalwidth,mdescwidth) = registerWidthsFromOpts copts
    (datewidth, date) = (10, showDate $ transactionRegisterDate reportq thisacctq t)
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
    amt = TL.toStrict . TB.toLazyText . wbBuilder $ showamt amtwidth change
    bal = TL.toStrict . TB.toLazyText . wbBuilder $ showamt balwidth balance
    showamt w = showMixedAmountB noPrice{displayColour=color_, displayMinWidth=Just w, displayMaxWidth=Just w}
    -- alternate behaviour, show null amounts as 0 instead of blank
    -- amt = if null amt' then "0" else amt'
    -- bal = if null bal' then "0" else bal'
    (amtlines, ballines) = (T.lines amt, T.lines bal)
    (amtlen, ballen) = (length amtlines, length ballines)
    numlines = max 1 (max amtlen ballen)
    (amtfirstline:amtrest) = take numlines $ amtlines ++ repeat "" -- posting amount is top-aligned
    (balfirstline:balrest) = take numlines $ replicate (numlines - ballen) "" ++ ballines -- balance amount is bottom-aligned
    spacer = T.replicate (totalwidth - (amtwidth + 2 + balwidth)) " "

-- tests

tests_Aregister = tests "Aregister" [

 ]
