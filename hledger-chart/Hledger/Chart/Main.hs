{-# LANGUAGE CPP #-}
{-|
hledger-web - a hledger add-on providing rudimentary pie chart generation.
Copyright (c) 2007-2011 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.
-}

module Hledger.Chart.Main
where
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL (hsl)
import Data.Colour.SRGB.Linear (rgb)
import Data.List
import Safe (readDef)
import System.Console.GetOpt
import System.Exit (exitFailure)

import Hledger.Cli.Options
import Hledger.Cli.Utils (withJournalDo)
import Hledger.Cli.Version (progversionstr, binaryfilename)
import Hledger.Data
import Prelude hiding (putStr, putStrLn)
import Hledger.Utils.UTF8 (putStr, putStrLn)


progname_chart = progname_cli ++ "-chart"

defchartoutput   = "hledger.png"
defchartitems    = 10
defchartsize     = "600x400"

options_chart :: [OptDescr Opt]
options_chart = [
  Option "o" ["output"] (ReqArg ChartOutput "FILE")       ("output filename (default: "++defchartoutput++")")
 ,Option ""  ["items"]  (ReqArg ChartItems "N")           ("number of accounts to show (default: "++show defchartitems++")")
 ,Option ""  ["size"]   (ReqArg ChartSize "WIDTHxHEIGHT") ("image size (default: "++defchartsize++")")
 ]

usage_preamble_chart =
  "Usage: hledger-chart [OPTIONS] [PATTERNS]\n" ++
  "\n" ++
  "Reads your ~/.journal file, or another specified by $LEDGER or -f, and\n" ++
  "generates simple pie chart images.\n" ++
  "\n"

usage_options_chart = usageInfo "hledger-chart options:" options_chart ++ "\n"

usage_chart = concat [
             usage_preamble_chart
            ,usage_options_chart
            ,usage_options_cli
            ,usage_postscript_cli
            ]

main :: IO ()
main = do
  (opts, args) <- parseArgumentsWith $ options_cli++options_chart
  run opts args
    where
      run opts args
       | Help `elem` opts             = putStr usage_chart
       | Version `elem` opts          = putStrLn $ progversionstr progname_chart
       | BinaryFilename `elem` opts   = putStrLn $ binaryfilename progname_chart
       | otherwise                    = withJournalDo opts args "chart" chart

-- | Generate an image with the pie chart and write it to a file
chart :: [Opt] -> [String] -> Journal -> IO ()
chart opts args j = do
  d <- getCurrentDay
  if null $ jtxns j
   then putStrLn "This journal has no transactions, can't make a chart." >> exitFailure
   else do
     let chart = genPie opts (optsToFilterSpec opts args d) j
     renderableToPNGFile (toRenderable chart) w h filename
     return ()
      where
        filename = getOption opts ChartOutput defchartoutput
        (w,h) = parseSize $ getOption opts ChartSize defchartsize

-- | Extract string option value from a list of options or use the default
getOption :: [Opt] -> (String->Opt) -> String -> String
getOption opts opt def = 
    case reverse $ optValuesForConstructor opt opts of
        [] -> def
        x:_ -> x

-- | Parse image size from a command-line option
parseSize :: String -> (Int,Int)
parseSize str = (read w, read h)
    where
    x = fromMaybe (error' "Size should be in WIDTHxHEIGHT format") $ findIndex (=='x') str
    (w,_:h) = splitAt x str

-- | Generate pie chart
genPie :: [Opt] -> FilterSpec -> Journal -> PieLayout
genPie opts filterspec j = defaultPieLayout { pie_background_ = solidFillStyle $ opaque $ white
                                            , pie_plot_ = pie_chart }
    where
      pie_chart = defaultPieChart { pie_data_ = map (uncurry accountPieItem) chartitems'
                                  , pie_start_angle_ = (-90)
                                  , pie_colors_ = mkColours hue
                                  , pie_label_style_ = defaultFontStyle{font_size_=12}
                                  }
      chartitems' = debug "chart" $ top num samesignitems
      (samesignitems, sign) = sameSignNonZero rawitems
      rawitems = debug "raw" $ flatten $ balances $
                 ledgerAccountTree (fromMaybe 99999 $ depthFromOpts opts) $ journalToLedger filterspec j
      top n t = topn ++ [other]
          where
            (topn,rest) = splitAt n $ reverse $ sortBy (comparing snd) t
            other = ("other", sum $ map snd rest)
      num = readDef (fromIntegral defchartitems) (getOption opts ChartItems (show defchartitems))
      hue = if sign > 0 then red else green where (red, green) = (0, 110)
      debug s = if Debug `elem` opts then ltrace s else id

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
amountValue = quantity . convertMixedAmountToSimilarCommodity unknown

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
