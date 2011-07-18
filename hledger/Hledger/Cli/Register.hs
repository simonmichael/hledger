{-# LANGUAGE CPP #-}
{-| 

A ledger-compatible @register@ command.

-}

module Hledger.Cli.Register (
  register
 ,postingRegisterReportAsText
 ,showPostingWithBalanceForVty
 ,tests_Hledger_Cli_Register
) where

import Data.List
import Data.Maybe
import Test.HUnit
import Text.Printf

import Hledger
import Prelude hiding (putStr)
import Hledger.Utils.UTF8 (putStr)
import Hledger.Cli.Options
import Hledger.Cli.Reports


-- | Print a (posting) register report.
register :: [Opt] -> [String] -> Journal -> IO ()
register opts args j = do
  d <- getCurrentDay
  putStr $ postingRegisterReportAsText opts $ postingRegisterReport opts (optsToFilterSpec opts args d) j

-- | Render a register report as plain text suitable for console output.
postingRegisterReportAsText :: [Opt] -> PostingRegisterReport -> String
postingRegisterReportAsText opts = unlines . map (postingRegisterReportItemAsText opts) . snd

-- | Render one register report line item as plain text. Eg:
-- @
-- date (10)  description (20)     account (22)            amount (11)  balance (12)
-- DDDDDDDDDD dddddddddddddddddddd aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
-- ^ displayed for first postings^
--   only, otherwise blank
-- @
postingRegisterReportItemAsText :: [Opt] -> PostingRegisterReportItem -> String
postingRegisterReportItemAsText _ (dd, p, b) = concatTopPadded [datedesc, pstr, " ", bal]
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
      bal = padleft 12 (showMixedAmountOrZeroWithoutPrice b)

-- XXX
showPostingWithBalanceForVty showtxninfo p b = postingRegisterReportItemAsText [] $ mkpostingRegisterItem showtxninfo p b

tests_Hledger_Cli_Register :: Test
tests_Hledger_Cli_Register = TestList
 [

 ]
