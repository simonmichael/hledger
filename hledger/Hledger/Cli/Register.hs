{-| 

A ledger-compatible @register@ command.

-}

module Hledger.Cli.Register (
  registermode
 ,register
 ,postingsReportAsText
 -- ,showPostingWithBalanceForVty
 ,tests_Hledger_Cli_Register
) where

import Data.List
import Data.Maybe
import System.Console.CmdArgs.Explicit
import Test.HUnit
import Text.Printf

import Hledger
import Prelude hiding (putStr)
import Hledger.Utils.UTF8IOCompat (putStr)
import Hledger.Cli.Options


registermode = (defCommandMode $ ["register"] ++ aliases) {
  modeHelp = "show postings and running total" `withAliases` aliases
 ,modeGroupFlags = Group {
     groupUnnamed = [
      flagNone ["historical","H"] (\opts -> setboolopt "historical" opts) "include prior postings in the running total"
     ,flagNone ["average","A"] (\opts -> setboolopt "average" opts) "show a running average instead of the running total (implies --empty)"
     ,flagNone ["related","r"] (\opts -> setboolopt "related" opts) "show postings' siblings instead"
     ,flagReq  ["width","w"] (\s opts -> Right $ setopt "width" s opts) "N" "set output width (default: 80)"
     ]
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }
  where aliases = ["reg"]

-- | Print a (posting) register report.
register :: CliOpts -> Journal -> IO ()
register opts@CliOpts{reportopts_=ropts} j = do
  d <- getCurrentDay
  putStr $ postingsReportAsText opts $ postingsReport ropts (queryFromOpts d ropts) j

-- | Render a register report as plain text suitable for console output.
postingsReportAsText :: CliOpts -> PostingsReport -> String
postingsReportAsText opts = unlines . map (postingsReportItemAsText opts) . snd

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
-- <----------------------------- width (default: 80) ---------------------------->
-- date (10)  description (50%)     account (50%)         amount (12)  balance (12)
-- DDDDDDDDDD dddddddddddddddddddd  aaaaaaaaaaaaaaaaaaa  AAAAAAAAAAAA  AAAAAAAAAAAA
--
-- date and description are shown for the first posting of a transaction only.
-- @
postingsReportItemAsText :: CliOpts -> PostingsReportItem -> String
postingsReportItemAsText opts (mdate, menddate, mdesc, p, b) =
  intercalate "\n" $
    [printf ("%-"++datew++"s %-"++descw++"s  %-"++acctw++"s  %"++amtw++"s  %"++balw++"s")
            date desc acct amtfirstline balfirstline]
    ++
    [printf (spacer ++ "%"++amtw++"s  %"++balw++"s") a b | (a,b) <- zip amtrest balrest ]
    
    where
      totalwidth = case widthFromOpts opts of
           Left _                       -> defaultWidth -- shouldn't happen
           Right (TotalWidth (Width w)) -> w
           Right (TotalWidth Auto)      -> defaultWidth -- XXX
           Right (FieldWidths _)        -> defaultWidth -- XXX
      amtwidth = 12
      balwidth = 12
      (datewidth, date) = case (mdate,menddate) of
                            (Just _, Just _)   -> (21, showDateSpan (DateSpan mdate menddate))
                            (Nothing, Just _)  -> (21, "")
                            (Just d, Nothing)  -> (10, showDate d)
                            _                  -> (10, "")
      remaining = totalwidth - (datewidth + 1 + 2 + amtwidth + 2 + balwidth)
      (descwidth, acctwidth) | isJust menddate = (0, remaining-2)
                             | even remaining  = (r2, r2)
                             | otherwise       = (r2, r2+1)
        where
          r2 = (remaining-2) `div` 2
      [datew,descw,acctw,amtw,balw] = map show [datewidth,descwidth,acctwidth,amtwidth,balwidth]



      desc = maybe "" (take descwidth . elideRight descwidth) mdesc
      acct = parenthesise $ elideAccountName awidth $ paccount p
         where
          (parenthesise, awidth) = case ptype p of
                               BalancedVirtualPosting -> (\s -> "["++s++"]", acctwidth-2)
                               VirtualPosting         -> (\s -> "("++s++")", acctwidth-2)
                               _                      -> (id,acctwidth)
      amt = showMixedAmountWithoutPrice $ pamount p
      bal = showMixedAmountWithoutPrice b
      (amtlines, ballines) = (lines amt, lines bal)
      (amtlen, ballen) = (length amtlines, length ballines)
      numlines = max amtlen ballen
      (amtfirstline:amtrest) = take numlines $ amtlines ++ repeat "" -- posting amount is top-aligned
      (balfirstline:balrest) = take numlines $ replicate (numlines - ballen) "" ++ ballines -- balance amount is bottom-aligned
      spacer = replicate (totalwidth - (amtwidth + 2 + balwidth)) ' '

-- XXX
-- showPostingWithBalanceForVty showtxninfo p b = postingsReportItemAsText defreportopts $ mkpostingsReportItem showtxninfo p b

tests_Hledger_Cli_Register :: Test
tests_Hledger_Cli_Register = TestList
  tests_postingsReportAsText
