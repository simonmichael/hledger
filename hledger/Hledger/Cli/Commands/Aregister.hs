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

import Data.Aeson (toJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.List
import Data.Maybe
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (addDays)
import Safe (headDef)
import System.Console.CmdArgs.Explicit
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
  --  flagNone ["cumulative"] (setboolopt "change")
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
      (a:as) -> return (a, T.pack . unwords $ map quoteIfNeeded as)
  argsquery <- either fail (return . fst) $ parseQuery d querystring
  let
    acct = headDef (error' $ show apat++" did not match any account")   -- PARTIAL:
           . filterAccts $ journalAccountNames j
    filterAccts = case toRegexCI apat of
        Right re -> filter (regexMatch re . T.unpack)
        Left  _  -> const []
    -- gather report options
    inclusive = True  -- tree_ ropts
    thisacctq = Acct $ (if inclusive then accountNameToAccountRegex else accountNameToAccountOnlyRegex) acct
    rspec' = rspec{ rsQuery=simplifyQuery $ And [queryFromFlags ropts, argsquery]
                  , rsOpts=ropts'
                  }
    ropts' = ropts
      { -- remove a depth limit for reportq, as in RegisterScreen, I forget why XXX
        depth_=Nothing
        -- always show historical balance
      , balancetype_= HistoricalBalance
      }
    ropts = rsOpts rspec
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
    (balancelabel,items) = accountTransactionsReport rspec' j reportq thisacctq
    items' = (if empty_ ropts then id else filter (not . mixedAmountLooksZero . fifth6)) $
             reverse items
    -- select renderer
    render | fmt=="json" = (++"\n") . T.unpack . TL.toStrict . encodeToLazyText . toJSON
           | fmt=="csv"  = (++"\n") . printCSV . accountTransactionsReportAsCsv reportq thisacctq
           | fmt=="txt"  = accountTransactionsReportAsText opts reportq thisacctq
           | otherwise   = const $ error' $ unsupportedOutputFormatError fmt  -- PARTIAL:
      where
        fmt = outputFormatFromOpts opts

  writeOutput opts $ render (balancelabel,items')

accountTransactionsReportAsCsv :: Query -> Query -> AccountTransactionsReport -> CSV
accountTransactionsReportAsCsv reportq thisacctq (_,is) =
  ["txnidx","date","code","description","otheraccounts","change","balance"]
  : map (accountTransactionsReportItemAsCsvRecord reportq thisacctq) is

accountTransactionsReportItemAsCsvRecord :: Query -> Query -> AccountTransactionsReportItem -> CsvRecord
accountTransactionsReportItemAsCsvRecord
  reportq thisacctq
  (t@Transaction{tindex,tcode,tdescription}, _, _issplit, otheracctsstr, change, balance)
  = [idx,date,code,desc,otheracctsstr,amt,bal]
  where
    idx  = show tindex
    date = showDate $ transactionRegisterDate reportq thisacctq t
    code = T.unpack tcode
    desc = T.unpack tdescription
    amt  = showMixedAmountOneLineWithoutPrice False change
    bal  = showMixedAmountOneLineWithoutPrice False balance

-- | Render a register report as plain text suitable for console output.
accountTransactionsReportAsText :: CliOpts -> Query -> Query -> AccountTransactionsReport -> String
accountTransactionsReportAsText
  copts@CliOpts{reportspec_=ReportSpec{rsOpts=ReportOpts{no_elide_}}} reportq thisacctq (_balancelabel,items)
  = unlines $ title :
    map (accountTransactionsReportItemAsText copts reportq thisacctq amtwidth balwidth) items
  where
    amtwidth = maximumStrict $ 12 : map (snd . showamt . itemamt) items
    balwidth = maximumStrict $ 12 : map (snd . showamt . itembal) items
    showamt = showMixedOneLine showAmountWithoutPrice (Just 12) mmax False  -- color_
      where mmax = if no_elide_ then Nothing else Just 22
    itemamt (_,_,_,_,a,_) = a
    itembal (_,_,_,_,_,a) = a
    -- show a title indicating which account was picked, which can be confusing otherwise
    title = T.unpack $ maybe "" (("Transactions in "<>).(<>" and subaccounts:")) macct
      where
        -- XXX temporary hack ? recover the account name from the query
        macct = case filterQuery queryIsAcct thisacctq of
                  Acct r -> Just . T.drop 1 . T.dropEnd 5 . T.pack $ reString r  -- Acct "^JS:expenses(:|$)"
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
accountTransactionsReportItemAsText :: CliOpts -> Query -> Query -> Int -> Int -> AccountTransactionsReportItem -> String
accountTransactionsReportItemAsText
  copts@CliOpts{reportspec_=ReportSpec{rsOpts=ReportOpts{color_}}}
  reportq thisacctq preferredamtwidth preferredbalwidth
  (t@Transaction{tdescription}, _, _issplit, otheracctsstr, change, balance)
    -- Transaction -- the transaction, unmodified
    -- Transaction -- the transaction, as seen from the current account
    -- Bool        -- is this a split (more than one posting to other accounts) ?
    -- String      -- a display string describing the other account(s), if any
    -- MixedAmount -- the amount posted to the current account(s) (or total amount posted)
    -- MixedAmount -- the register's running total or the current account(s)'s historical balance, after this transaction

  = intercalate "\n" $
    concat [fitString (Just datewidth) (Just datewidth) True True date
           ," "
           ,fitString (Just descwidth) (Just descwidth) True True desc
           ,"  "
           ,fitString (Just acctwidth) (Just acctwidth) True True accts
           ,"  "
           ,amtfirstline
           ,"  "
           ,balfirstline
           ]
    :
    [concat [spacer
            ,a
            ,"  "
            ,b
            ]
     | (a,b) <- zip amtrest balrest
     ]
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
        where
          w = fromMaybe ((remaining - 2) `div` 2) mdescwidth

      -- gather content
      desc = T.unpack tdescription
      accts = -- T.unpack $ elideAccountName acctwidth $ T.pack
              otheracctsstr
      amt = fst $ showMixed showAmountWithoutPrice (Just amtwidth) (Just balwidth) color_ change
      bal = fst $ showMixed showAmountWithoutPrice (Just balwidth) (Just balwidth) color_ balance
      -- alternate behaviour, show null amounts as 0 instead of blank
      -- amt = if null amt' then "0" else amt'
      -- bal = if null bal' then "0" else bal'
      (amtlines, ballines) = (lines amt, lines bal)
      (amtlen, ballen) = (length amtlines, length ballines)
      numlines = max 1 (max amtlen ballen)
      (amtfirstline:amtrest) = take numlines $ amtlines ++ repeat (replicate amtwidth ' ') -- posting amount is top-aligned
      (balfirstline:balrest) = take numlines $ replicate (numlines - ballen) (replicate balwidth ' ') ++ ballines -- balance amount is bottom-aligned
      spacer = replicate (totalwidth - (amtwidth + 2 + balwidth)) ' '

-- tests

tests_Aregister = tests "Aregister" [

 ]
