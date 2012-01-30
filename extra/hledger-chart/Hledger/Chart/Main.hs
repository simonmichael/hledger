{-|
hledger-chart - a hledger add-on providing rudimentary pie chart generation.
Copyright (c) 2007-2011 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.
-}

module Hledger.Chart.Main
where
import Control.Monad
import Data.Colour
import Data.Colour.Names
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL (hsl)
import Data.Colour.SRGB.Linear (rgb)
import Data.List
import Data.Maybe
import Data.Ord
import Data.Tree
import Graphics.Rendering.Chart
import System.Exit
import Text.Printf

import Hledger
import Hledger.Cli hiding (progname,progversion)
import Prelude hiding (putStrLn)
import Hledger.Utils.UTF8 (putStrLn)

import Hledger.Chart.Options

main :: IO ()
main = do
  opts <- getHledgerChartOpts
  when (debug_ $ cliopts_ opts) $ printf "%s\n" progversion >> printf "opts: %s\n" (show opts)
  runWith opts

runWith :: ChartOpts -> IO ()
runWith opts = run opts
    where
      run opts
          | "help" `in_` (rawopts_ $ cliopts_ opts)            = putStr (showModeHelp chartmode) >> exitSuccess
          | "version" `in_` (rawopts_ $ cliopts_ opts)         = putStrLn progversion >> exitSuccess
          | "binary-filename" `in_` (rawopts_ $ cliopts_ opts) = putStrLn (binaryfilename progname)
          | otherwise                                          = withJournalDo' opts chart

withJournalDo' :: ChartOpts -> (ChartOpts -> Journal -> IO ()) -> IO ()
withJournalDo' opts cmd = do
  journalFilePathFromOpts (cliopts_ opts) >>= readJournalFile Nothing >>=
    either error' (cmd opts . journalApplyAliases (aliasesFromOpts $ cliopts_ opts))

-- | Generate an image with the pie chart and write it to a file
chart :: ChartOpts -> Journal -> IO ()
chart opts j = do
  d <- getCurrentDay
  if null $ jtxns j
   then putStrLn "This journal has no transactions, can't make a chart." >> exitFailure
   else do
     let chart = genPie opts (optsToFilterSpec ropts d) j
     renderableToPNGFile (toRenderable chart) w h filename
     return ()
      where
        filename = chart_output_ opts
        (w,h) = parseSize $ chart_size_ opts
        ropts = reportopts_ $ cliopts_ opts

-- | Parse image size from a command-line option
parseSize :: String -> (Int,Int)
parseSize str = (read w, read h)
    where
    x = fromMaybe (error' "Size should be in WIDTHxHEIGHT format") $ findIndex (=='x') str
    (w,_:h) = splitAt x str

-- | Generate pie chart
genPie :: ChartOpts -> FilterSpec -> Journal -> PieLayout
genPie opts filterspec j = defaultPieLayout { pie_background_ = solidFillStyle $ opaque $ white
                                            , pie_plot_ = pie_chart }
    where
      pie_chart = defaultPieChart { pie_data_ = map (uncurry accountPieItem) chartitems
                                  , pie_start_angle_ = (-90)
                                  , pie_colors_ = mkColours hue
                                  , pie_label_style_ = defaultFontStyle{font_size_=12}
                                  }
      chartitems = debug "chart" $ top num samesignitems
      (samesignitems, sign) = sameSignNonZero rawitems
      rawitems = debug "raw" $ flatten $ balances $
                 ledgerAccountTree (fromMaybe 99999 $ depth_ ropts) $ journalToLedger filterspec j
      top n t = topn ++ [other]
          where
            (topn,rest) = splitAt n $ reverse $ sortBy (comparing snd) t
            other = ("other", sum $ map snd rest)
      num = chart_items_ opts
      hue = if sign > 0 then red else green where (red, green) = (0, 110)
      debug s = if debug_ copts then ltrace s else id
      copts = cliopts_ opts
      ropts = reportopts_ copts

-- | Select the nonzero items with same sign as the first, and make
-- them positive. Also return a 1 or -1 corresponding to the original sign.
sameSignNonZero :: [(AccountName, Double)] -> ([(AccountName, Double)], Int)
sameSignNonZero is | null nzs = ([], 1)
                   | otherwise = (map pos $ filter (test.snd) nzs, sign)
                   where
                     nzs = filter ((/=0).snd) is
                     pos (a,b) = (a, abs b)
                     sign = if snd (head nzs) >= 0 then 1 else (-1)
                     test = if sign > 0 then (>0) else (<0)

-- | Convert all quantities of MixedAccount to a single commodity
amountValue :: MixedAmount -> Double
amountValue = quantity . mixedAmountWithCommodity unknown

-- | Generate a tree of account names together with their balances.
--   The balance of account is decremented by the balance of its subaccounts
--   which are drawn on the chart.
balances :: Tree Account -> Tree (AccountName, Double)
balances (Node rootAcc subAccs) = Node newroot newsubs
    where
      newroot = (aname rootAcc,
                 amountValue $
                 abalance rootAcc - (sum . map (abalance . root)) subAccs)
      newsubs = map balances subAccs

-- | Build a single pie chart item
accountPieItem :: AccountName -> Double -> PieItem
accountPieItem accname balance = PieItem accname offset balance where offset = 0

-- | Generate an infinite color list suitable for charts.
mkColours :: Double -> [AlphaColour Double]
mkColours hue = cycle $ [opaque $ rgbToColour $ hsl h s l | (h,s,l) <- liftM3 (,,)
                         [hue] [0.7] [0.1,0.2..0.7] ]

rgbToColour :: (Fractional a) => RGB a -> Colour a
rgbToColour (RGB r g b) = rgb r g b
