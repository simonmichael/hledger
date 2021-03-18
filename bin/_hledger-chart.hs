#!/usr/bin/env stack
{- stack runghc --verbosity info --resolver lts-9.3
   --package hledger
   --package Chart
   --package Chart-diagrams
   --package cmdargs
   --package colour
   --package data-default
   --package here
   --package safe
   --package text
-}
{- stackage nightly:
   --package SVGFonts
   --package diagrams-core
   --package diagrams-lib
   --package diagrams-postscript
   --package diagrams-svg
   --package svg-builder
   --package cereal-vector
   --package dual-tree
   --package diagrams-solve
   --package statestack
-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Data.Colour
import Data.Colour.Names hiding (red,green)
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL (hsl)
import Data.Colour.SRGB.Linear (rgb)
import Data.Default
import Data.List
import Data.Maybe
import Data.Ord
import Data.String.Here
import qualified Data.Text as T
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Safe
import System.Console.CmdArgs.Explicit
import System.Exit

import Hledger.Cli hiding (num,green,is,balance)

defchartoutput   = "hledger.svg"
defchartitems    = 10
defchartsize     = "600x400"

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  [here| chart
Generate a pie chart for the top account balances with the same sign,
in SVG format.

Based on the old hledger-chart package, this is not yet useful.
It's supposed to show only balances of one sign, but this might be broken.
  |]
  [flagReq ["chart-output","o"]  (\s opts -> Right $ setopt "chart-output" s opts) "IMGFILE" ("output filename (default: "++defchartoutput++")")
  ,flagReq ["chart-items"]  (\s opts -> Right $ setopt "chart-items" s opts) "N" ("number of accounts to show (default: "++show defchartitems++")")
  ,flagReq ["chart-size"]  (\s opts -> Right $ setopt "chart-size" s opts) "WIDTHxHEIGHT" ("image size (default: "++defchartsize++")")
  ]  [generalflagsgroup1]
  []
  ([], Just $ argsFlag "[QUERY]")
------------------------------------------------------------------------------

data ChartOpts = ChartOpts {
     chart_output_ :: FilePath
    ,chart_items_ :: Int
    ,chart_size_ :: String
    ,cliopts_   :: CliOpts
 } deriving (Show)

defchartopts = ChartOpts
    def
    def
    def
    defcliopts

getHledgerChartOpts :: IO ChartOpts
getHledgerChartOpts = do
  cliopts <- getHledgerCliOpts cmdmode
  return defchartopts {
              chart_output_ = fromMaybe defchartoutput $ maybestringopt "debug-chart" $ rawopts_ cliopts
             ,chart_items_ = fromMaybe defchartitems $ maybeintopt "debug-items" $ rawopts_ cliopts
             ,chart_size_ = fromMaybe defchartsize $ maybestringopt "debug-size" $ rawopts_ cliopts
             ,cliopts_   = cliopts
             }

main :: IO ()
main = do
  chopts <- getHledgerChartOpts
  d <- getCurrentDay
  j <- defaultJournal
  let ropts = (reportopts_ $ cliopts_ chopts)
  let balreport = balanceReportFromMultiBalanceReport ropts (queryFromOpts d ropts) j
  let go -- | "--help" `elem` (rawopts_ $ cliopts_ chopts)    = putStr (showModeHelp chartmode) >> exitSuccess
         -- | "--version" `elem` (rawopts_ $ cliopts_ chopts) = putStrLn progversion >> exitSuccess
         | otherwise                                       = withJournalAndChartOptsDo chopts (writeChart balreport)
  go

-- copied from hledger-web
withJournalAndChartOptsDo :: ChartOpts -> (ChartOpts -> Journal -> IO ()) -> IO ()
withJournalAndChartOptsDo opts cmd = do
  f <- head `fmap` journalFilePathFromOpts (cliopts_ opts)
  readJournalFile Nothing Nothing True f >>=
   either error' (cmd opts . journalApplyAliases (aliasesFromOpts $ cliopts_ opts))

-- | Generate an image with the pie chart and write it to a file
writeChart :: BalanceReport -> ChartOpts -> Journal -> IO ()
writeChart balreport opts j = do
--   d <- getCurrentDay
  if null $ jtxns j
   then putStrLn "This journal has no transactions, can't make a chart." >> exitFailure
   else do
     let chart = genPie opts balreport
     let fileoptions = def -- FileOptions (fromIntegral w, fromIntegral h) SVG loadSansSerifFonts
     renderableToFile fileoptions filename (toRenderable chart)
     return ()
      where
        filename = chart_output_ opts
--         (w,h) = parseSize $ chart_size_ opts
--         ropts = reportopts_ $ cliopts_ opts

-- | Parse image size from a command-line option
-- parseSize :: String -> (Int,Int)
-- parseSize str = (read w, read h)
--     where
--     x = fromMaybe (error' "Size should be in WIDTHxHEIGHT format") $ findIndex (=='x') str
--     (w,_:h) = splitAt x str

-- | Generate pie chart
genPie :: ChartOpts -> BalanceReport -> PieLayout
genPie opts (items, _total) = def { _pie_background = solidFillStyle $ opaque $ white
                                 , _pie_plot = pie_chart }
    where
      pie_chart = def { _pie_data = map (uncurry accountPieItem) chartitems
                      , _pie_start_angle = (-90)
                      , _pie_colors = mkColours hue
                      , _pie_label_style = def{_font_size=12}
                      }
      chartitems = dbg1 "chart" $ top num samesignitems :: [(AccountName, Double)]
      (samesignitems, sign) = sameSignNonZero items
      top n t = topn ++ [other]
          where
            (topn,rest) = splitAt n $ reverse $ sortBy (comparing snd) t
            other = ("other", sum $ map snd rest)
      num = chart_items_ opts
      hue = if sign > 0 then red else green where (red, green) = (0, 110)
--       copts = cliopts_ opts
--       ropts = reportopts_ copts

-- | Select the nonzero items with same sign as the first, and make
-- them positive. Also return a 1 or -1 corresponding to the original sign.
sameSignNonZero :: [BalanceReportItem] -> ([(AccountName, Double)], Int)
sameSignNonZero is
 | null nzs = ([], 1)
 | otherwise = (map pos $ filter (test.fourth4) nzs, sign)
 where
   nzs = filter ((/=0).fourth4) is
   pos (acct,_,_,as) = (acct, abs $ read $ show $ maybe 0 aquantity $ headMay $ amounts as)
   sign = if fourth4 (head nzs) >= 0 then 1 else (-1)
   test = if sign > 0 then (>0) else (<0)

-- | Convert all quantities of MixedAccount to a single commodity
-- amountValue :: MixedAmount -> Double
-- amountValue = quantity . mixedAmountWithCommodity unknown

-- | Generate a tree of account names together with their balances.
--   The balance of account is decremented by the balance of its subaccounts
--   which are drawn on the chart.
-- balances :: Tree Account -> Tree (AccountName, Double)
-- balances (Node rootAcc subAccs) = Node newroot newsubs
--     where
--       newroot = (aname rootAcc,
--                  amountValue $
--                  aibalance rootAcc - (sum . map (aibalance . root)) subAccs)
--       newsubs = map balances subAccs

-- | Build a single pie chart item
accountPieItem :: AccountName -> Double -> PieItem
accountPieItem accname balance = PieItem (T.unpack accname) offset balance where offset = 0

-- | Generate an infinite color list suitable for charts.
mkColours :: Double -> [AlphaColour Double]
mkColours hue = cycle $ [opaque $ rgbToColour $ hsl h s l | (h,s,l) <- liftM3 (,,)
                         [hue] [0.7] [0.1,0.2..0.7] ]

rgbToColour :: (Fractional a) => RGB a -> Colour a
rgbToColour (RGB r g b) = rgb r g b
