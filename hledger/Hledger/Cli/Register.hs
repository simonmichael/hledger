{-| 

A ledger-compatible @register@ command.

-}

module Hledger.Cli.Register (
  register
 ,postingsReportAsText
 -- ,showPostingWithBalanceForVty
 ,tests_Hledger_Cli_Register
) where

import Data.List
import Data.Maybe
import Test.HUnit
import Text.Printf

import Hledger
import Prelude hiding (putStr)
import Hledger.Utils.UTF8IOCompat (putStr)
import Hledger.Cli.Options


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
postingsReportItemAsText opts (mdate, mdesc, p, b) =
  concatTopPadded [date, " ", desc, "  ", acct, "  ", amt, "  ", bal]
    where
      totalwidth = case widthFromOpts opts of
           Left _                       -> defaultWidth -- shouldn't happen
           Right (TotalWidth (Width w)) -> w
           Right (TotalWidth Auto)      -> defaultWidth -- XXX
           Right (FieldWidths _)        -> defaultWidth -- XXX
      datewidth = 10
      amtwidth = 12
      balwidth = 12
      remaining = totalwidth - (datewidth + 1 + 2 + amtwidth + 2 + balwidth)
      (descwidth, acctwidth) | even r    = (r', r')
                             | otherwise = (r', r'+1)
        where r = remaining - 2
              r' = r `div` 2
      date = maybe (replicate datewidth ' ') (printf ("%-"++show datewidth++"s") . showDate) mdate
      desc = maybe (replicate descwidth ' ') (printf ("%-"++show descwidth++"s") . take descwidth . elideRight descwidth) mdesc
      acct = printf ("%-"++(show acctwidth)++"s") a
        where
          a = bracket $ elideAccountName awidth $ paccount p
          (bracket, awidth) = case ptype p of
                               BalancedVirtualPosting -> (\s -> "["++s++"]", acctwidth-2)
                               VirtualPosting -> (\s -> "("++s++")", acctwidth-2)
                               _ -> (id,acctwidth)
      amt = padleft amtwidth $ showMixedAmountWithoutPrice $ pamount p
      bal = padleft balwidth $ showMixedAmountWithoutPrice b

-- XXX
-- showPostingWithBalanceForVty showtxninfo p b = postingsReportItemAsText defreportopts $ mkpostingsReportItem showtxninfo p b

tests_Hledger_Cli_Register :: Test
tests_Hledger_Cli_Register = TestList
  tests_postingsReportAsText
