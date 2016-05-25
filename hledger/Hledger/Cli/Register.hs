{-|

A ledger-compatible @register@ command.

-}

{-# LANGUAGE CPP, OverloadedStrings #-}

module Hledger.Cli.Register (
  registermode
 ,register
 ,postingsReportAsText
 ,postingsReportItemAsText
 -- ,showPostingWithBalanceForVty
 ,tests_Hledger_Cli_Register
) where

import Data.List
import Data.Maybe
-- import Data.Text (Text)
import qualified Data.Text as T
import System.Console.CmdArgs.Explicit
import Text.CSV
import Test.HUnit

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils


registermode = (defCommandMode $ ["register"] ++ aliases) {
  modeHelp = "show postings and running total" `withAliases` aliases
 ,modeGroupFlags = Group {
     groupUnnamed = [
      flagNone ["historical","H"] (\opts -> setboolopt "historical" opts) "include prior postings in the running total"
     ,flagNone ["average","A"] (\opts -> setboolopt "average" opts) "show a running average instead of the running total (implies --empty)"
     ,flagNone ["related","r"] (\opts -> setboolopt "related" opts) "show postings' siblings instead"
     ,flagReq  ["width","w"] (\s opts -> Right $ setopt "width" s opts) "N"
      (unlines
       ["set output width (default:"
#ifdef mingw32_HOST_OS
       ,(show defaultWidth)
#else
       ,"terminal width"
#endif
       ,"or COLUMNS. -wN,M sets description width as well)"
       ])
    ]
     ++ outputflags
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }
  where aliases = ["reg"]

-- | Print a (posting) register report.
register :: CliOpts -> Journal -> IO ()
register opts@CliOpts{reportopts_=ropts} j = do
  d <- getCurrentDay
  let fmt = outputFormatFromOpts opts
      render | fmt=="csv" = const ((++"\n") . printCSV . postingsReportAsCsv)
             | otherwise  = postingsReportAsText
  writeOutput opts $ render opts $ postingsReport ropts (queryFromOpts d ropts) j

postingsReportAsCsv :: PostingsReport -> CSV
postingsReportAsCsv (_,is) =
  ["date","description","account","amount","running total or balance"]
  :
  map postingsReportItemAsCsvRecord is

postingsReportItemAsCsvRecord :: PostingsReportItem -> Record
postingsReportItemAsCsvRecord (_, _, _, p, b) = [date,desc,acct,amt,bal]
  where
    date = showDate $ postingDate p -- XXX csv should show date2 with --date2
    desc = T.unpack $ maybe "" tdescription $ ptransaction p
    acct = bracket $ T.unpack $ paccount p
      where
        bracket = case ptype p of
                             BalancedVirtualPosting -> (\s -> "["++s++"]")
                             VirtualPosting -> (\s -> "("++s++")")
                             _ -> id
    amt = showMixedAmountOneLineWithoutPrice $ pamount p
    bal = showMixedAmountOneLineWithoutPrice b

-- | Render a register report as plain text suitable for console output.
postingsReportAsText :: CliOpts -> PostingsReport -> String
postingsReportAsText opts (_,items) = unlines $ map (postingsReportItemAsText opts amtwidth balwidth) items
  where
    amtwidth = maximum $ 12 : map (strWidth . showMixedAmount . itemamt) items
    balwidth = maximum $ 12 : map (strWidth . showMixedAmount . itembal) items
    itemamt (_,_,_,Posting{pamount=a},_) = a
    itembal (_,_,_,_,a) = a

tests_postingsReportAsText = [
  "postingsReportAsText" ~: do
  -- "unicode in register layout" ~: do
    j <- readJournal'
      "2009/01/01 * медвежья шкура\n  расходы:покупки  100\n  актив:наличные\n"
    let opts = defreportopts
    (postingsReportAsText defcliopts $ postingsReport opts (queryFromOpts (parsedate "2008/11/26") opts) j) `is` unlines
      ["2009/01/01 медвежья шкура       расходы:покупки                100           100"
      ,"                                актив:наличные                -100             0"]
 ]

-- | Render one register report line item as plain text. Layout is like so:
-- @
-- <---------------- width (specified, terminal width, or 80) -------------------->
-- date (10)  description           account              amount (12)   balance (12)
-- DDDDDDDDDD dddddddddddddddddddd  aaaaaaaaaaaaaaaaaaa  AAAAAAAAAAAA  AAAAAAAAAAAA
-- @
-- If description's width is specified, account will use the remaining space.
-- Otherwise, description and account divide up the space equally.
--
-- With a reporting interval, the layout is like so:
-- @
-- <---------------- width (specified, terminal width, or 80) -------------------->
-- date (21)              account                        amount (12)   balance (12)
-- DDDDDDDDDDDDDDDDDDDDD  aaaaaaaaaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAAA  AAAAAAAAAAAA
-- @
--
-- date and description are shown for the first posting of a transaction only.
--
-- Returns a string which can be multi-line, eg if the running balance
-- has multiple commodities. Does not yet support formatting control
-- like balance reports.
--
postingsReportItemAsText :: CliOpts -> Int -> Int -> PostingsReportItem -> String
postingsReportItemAsText opts preferredamtwidth preferredbalwidth (mdate, menddate, mdesc, p, b) =
  -- use elide*Width to be wide-char-aware
  -- trace (show (totalwidth, datewidth, descwidth, acctwidth, amtwidth, balwidth)) $
  intercalate "\n" $
    [concat [fitString (Just datewidth) (Just datewidth) True True date
            ," "
            ,fitString (Just descwidth) (Just descwidth) True True desc
            ,"  "
            ,fitString (Just acctwidth) (Just acctwidth) True True acct
            ,"  "
            ,fitString (Just amtwidth) (Just amtwidth) True False amtfirstline
            ,"  "
            ,fitString (Just balwidth) (Just balwidth) True False balfirstline
            ]]
    ++
    [concat [spacer
            ,fitString (Just amtwidth) (Just amtwidth) True False a
            ,"  "
            ,fitString (Just balwidth) (Just balwidth) True False b
            ]
     | (a,b) <- zip amtrest balrest
     ]
    where
      -- calculate widths
      (totalwidth,mdescwidth) = registerWidthsFromOpts opts
      (datewidth, date) = case (mdate,menddate) of
                            (Just _, Just _)   -> (21, showDateSpan (DateSpan mdate menddate))
                            (Nothing, Just _)  -> (21, "")
                            (Just d, Nothing)  -> (10, showDate d)
                            _                  -> (10, "")
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
      (descwidth, acctwidth)
        | hasinterval = (0, remaining - 2)
        | otherwise   = (w, remaining - 2 - w)
        where
            hasinterval = isJust menddate
            w = fromMaybe ((remaining - 2) `div` 2) mdescwidth

      -- gather content
      desc = fromMaybe "" mdesc
      acct = parenthesise $ T.unpack $ elideAccountName awidth $ paccount p
         where
          (parenthesise, awidth) =
            case ptype p of
              BalancedVirtualPosting -> (\s -> "["++s++"]", acctwidth-2)
              VirtualPosting         -> (\s -> "("++s++")", acctwidth-2)
              _                      -> (id,acctwidth)
      amt = showMixedAmountWithoutPrice $ pamount p
      bal = showMixedAmountWithoutPrice b
      -- alternate behaviour, show null amounts as 0 instead of blank
      -- amt = if null amt' then "0" else amt'
      -- bal = if null bal' then "0" else bal'
      (amtlines, ballines) = (lines amt, lines bal)
      (amtlen, ballen) = (length amtlines, length ballines)
      numlines = max 1 (max amtlen ballen)
      (amtfirstline:amtrest) = take numlines $ amtlines ++ repeat "" -- posting amount is top-aligned
      (balfirstline:balrest) = take numlines $ replicate (numlines - ballen) "" ++ ballines -- balance amount is bottom-aligned
      spacer = replicate (totalwidth - (amtwidth + 2 + balwidth)) ' '

tests_Hledger_Cli_Register :: Test
tests_Hledger_Cli_Register = TestList
  tests_postingsReportAsText
