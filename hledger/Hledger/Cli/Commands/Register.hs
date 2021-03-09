{-|

A ledger-compatible @register@ command.

-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hledger.Cli.Commands.Register (
  registermode
 ,register
 ,postingsReportAsText
 ,postingsReportItemAsText
 -- ,showPostingWithBalanceForVty
 ,tests_Register
) where

import Data.Default (def)
import Data.Maybe (fromMaybe, isJust)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif
-- import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import System.Console.CmdArgs.Explicit (flagNone, flagReq)
import Hledger.Read.CsvReader (CSV, CsvRecord, printCSV)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils
import Text.Tabular (Header(..), Properties(..))
import Text.Tabular.AsciiWide

registermode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Register.txt")
  ([flagNone ["cumulative"] (setboolopt "cumulative")
     "show running total from report start date (default)"
  ,flagNone ["historical","H"] (setboolopt "historical")
     "show historical running total/balance (includes postings before report start date)\n "
  ,flagNone ["average","A"] (setboolopt "average")
     "show running average of posting amounts instead of total (implies --empty)"
  ,flagNone ["related","r"] (setboolopt "related") "show postings' siblings instead"
  ,flagNone ["invert"] (setboolopt "invert") "display all amounts with reversed sign"
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
  ([], Just $ argsFlag "[QUERY]")

-- | Print a (posting) register report.
register :: CliOpts -> Journal -> IO ()
register opts@CliOpts{reportspec_=rspec} j =
    writeOutputLazyText opts . render $ postingsReport rspec j
  where
    fmt = outputFormatFromOpts opts
    render | fmt=="txt"  = postingsReportAsText opts
           | fmt=="csv"  = printCSV . postingsReportAsCsv
           | fmt=="json" = toJsonText
           | otherwise   = error' $ unsupportedOutputFormatError fmt  -- PARTIAL:

postingsReportAsCsv :: PostingsReport -> CSV
postingsReportAsCsv is =
  ["txnidx","date","code","description","account","amount","total"]
  :
  map postingsReportItemAsCsvRecord is

postingsReportItemAsCsvRecord :: PostingsReportItem -> CsvRecord
postingsReportItemAsCsvRecord (_, _, _, p, b) = [idx,date,code,desc,acct,amt,bal]
  where
    idx  = T.pack . show . maybe 0 tindex $ ptransaction p
    date = showDate $ postingDate p -- XXX csv should show date2 with --date2
    code = maybe "" tcode $ ptransaction p
    desc = maybe "" tdescription $ ptransaction p
    acct = bracket $ paccount p
      where
        bracket = case ptype p of
                             BalancedVirtualPosting -> wrap "[" "]"
                             VirtualPosting -> wrap "(" ")"
                             _ -> id
    amt = wbToText . showMixedAmountB oneLine $ pamount p
    bal = wbToText $ showMixedAmountB oneLine b

-- | Render a register report as plain text suitable for console output.
postingsReportAsText :: CliOpts -> PostingsReport -> TL.Text
postingsReportAsText opts items =
    TB.toLazyText . unlinesB $
      map (postingsReportItemAsText opts amtwidth balwidth) items
  where
    amtwidth = maximumStrict $ map (wbWidth . showAmt . itemamt) items
    balwidth = maximumStrict $ map (wbWidth . showAmt . itembal) items
    itemamt (_,_,_,Posting{pamount=a},_) = a
    itembal (_,_,_,_,a) = a
    showAmt = showMixedAmountB noColour{displayMinWidth=Just 12}

-- | Render one register report line item as plain text. Layout is like so:
-- @
-- <---------------- width (specified, terminal width, or 80) -------------------->
-- date (10)  description           account              amount (12)   balance (12)
-- DDDDDDDDDD dddddddddddddddddddd  aaaaaaaaaaaaaaaaaaa  AAAAAAAAAAAA  AAAAAAAAAAAA
-- @
-- If description's width is specified, account will use the remaining space.
-- Otherwise, description and account divide up the space equally.
--
-- With a report interval, the layout is like so:
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
postingsReportItemAsText :: CliOpts -> Int -> Int -> PostingsReportItem -> TB.Builder
postingsReportItemAsText opts preferredamtwidth preferredbalwidth (mdate, menddate, mdesc, p, b) =
    render
      [ textCell TopLeft $ fitText (Just datewidth) (Just datewidth) True True date
      , spacerCell
      , textCell TopLeft $ fitText (Just descwidth) (Just descwidth) True True desc
      , spacerCell2
      , textCell TopLeft $ fitText (Just acctwidth) (Just acctwidth) True True acct
      , spacerCell2
      , Cell TopRight amt
      , spacerCell2
      , Cell BottomRight bal
      ]
    where
      render = renderRowB def{tableBorders=False, borderSpaces=False} . Group NoLine . map Header
      spacerCell  = Cell BottomLeft [WideBuilder (TB.singleton ' ') 1]
      spacerCell2 = Cell BottomLeft [WideBuilder (TB.fromString "  ") 2]
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
      acct = parenthesise . elideAccountName awidth $ paccount p
         where
          (parenthesise, awidth) =
            case ptype p of
              BalancedVirtualPosting -> (\s -> wrap "[" "]" s, acctwidth-2)
              VirtualPosting         -> (\s -> wrap "(" ")" s, acctwidth-2)
              _                      -> (id,acctwidth)
          wrap a b x = a <> x <> b
      amt = showAmountsLinesB (dopts amtwidth) . (\x -> if null x then [nullamt] else x) . amounts $ pamount p
      bal = showAmountsLinesB (dopts balwidth) $ amounts b
      dopts w = noPrice{displayColour=color_ . rsOpts $ reportspec_ opts, displayMinWidth=Just w, displayMaxWidth=Just w}

-- tests

tests_Register = tests "Register" [

   tests "postingsReportAsText" [
    test "unicode in register layout" $ do
      j <- readJournal' "2009/01/01 * медвежья шкура\n  расходы:покупки  100\n  актив:наличные\n"
      let rspec = defreportspec
      (TL.unpack . postingsReportAsText defcliopts $ postingsReport rspec j)
        @?=
        unlines
        ["2009-01-01 медвежья шкура       расходы:покупки                100           100"
        ,"                                актив:наличные                -100             0"]
   ]

 ]
