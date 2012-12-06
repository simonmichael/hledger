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
register CliOpts{reportopts_=ropts} j = do
  d <- getCurrentDay
  putStr $ postingsReportAsText ropts $ postingsReport ropts (queryFromOpts d ropts) j

-- | Render a register report as plain text suitable for console output.
postingsReportAsText :: ReportOpts -> PostingsReport -> String
postingsReportAsText opts = unlines . map (postingsReportItemAsText opts) . snd

tests_postingsReportAsText = [
  "postingsReportAsText" ~: do
  -- "unicode in register layout" ~: do
    j <- readJournal'
      "2009/01/01 * медвежья шкура\n  расходы:покупки  100\n  актив:наличные\n"
    let opts = defreportopts
    (postingsReportAsText opts $ postingsReport opts (queryFromOpts (parsedate "2008/11/26") opts) j) `is` unlines
      ["2009/01/01 медвежья шкура       расходы:покупки                 100          100"
      ,"                                актив:наличные                 -100            0"]
 ]

-- | Render one register report line item as plain text. Eg:
-- @
-- date (10)  description (20)     account (22)            amount (11)  balance (12)
-- DDDDDDDDDD dddddddddddddddddddd aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
-- ^ displayed for first postings^
--   only, otherwise blank
-- @
postingsReportItemAsText :: ReportOpts -> PostingsReportItem -> String
postingsReportItemAsText _ (dd, p, b) = concatTopPadded [datedesc, pstr, " ", bal]
    where
      datedesc = case dd of Nothing -> replicate datedescwidth ' '
                            Just (da, de) -> printf "%s %s " date desc
                                where
                                  date = showDate da
                                  desc = printf ("%-"++(show descwidth)++"s") $ elideRight descwidth de :: String
          where
            descwidth = datedescwidth - datewidth - 2
            datedescwidth = 32
            datewidth = 10
      pstr = showPostingForRegister p
      bal = padleft 12 (showMixedAmountWithoutPrice b)

-- XXX
-- showPostingWithBalanceForVty showtxninfo p b = postingsReportItemAsText defreportopts $ mkpostingsReportItem showtxninfo p b

tests_Hledger_Cli_Register :: Test
tests_Hledger_Cli_Register = TestList
  tests_postingsReportAsText
