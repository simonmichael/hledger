{-|

-}

module Hledger.Chart.Options
where
import Data.Maybe
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit

import Hledger.Cli hiding (progname,progversion)
import qualified Hledger.Cli (progname)

progname = Hledger.Cli.progname ++ "-chart"
progversion = progversionstr progname

defchartoutput   = "hledger.png"
defchartitems    = 10
defchartsize     = "600x400"

chartflags = [
  flagReq ["chart-output","o"]  (\s opts -> Right $ setopt "chart-output" s opts) "IMGFILE" ("output filename (default: "++defchartoutput++")")
 ,flagReq ["chart-items"]  (\s opts -> Right $ setopt "chart-items" s opts) "N" ("number of accounts to show (default: "++show defchartitems++")")
 ,flagReq ["chart-size"]  (\s opts -> Right $ setopt "chart-size" s opts) "WIDTHxHEIGHT" ("image size (default: "++defchartsize++")")
 ]
 
chartmode =  (mode "hledger-chart" [("command","chart")]
            "generate a pie chart image for the top account balances (of one sign only)"
            commandargsflag (chartflags++generalflags1)){
             modeHelpSuffix=[
                  -- "Reads your ~/.hledger.journal file, or another specified by $LEDGER_FILE or -f, and starts the full-window curses ui."
                 ]
           }

-- hledger-chart options, used in hledger-chart and above
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
    def

-- instance Default CliOpts where def = defcliopts

toChartOpts :: RawOpts -> IO ChartOpts
toChartOpts rawopts = do
  cliopts <- toCliOpts rawopts
  return defchartopts {
              chart_output_ = fromMaybe defchartoutput $ maybestringopt "debug-chart" rawopts
             ,chart_items_ = fromMaybe defchartitems $ maybeintopt "debug-items" rawopts
             ,chart_size_ = fromMaybe defchartsize $ maybestringopt "debug-size" rawopts
             ,cliopts_   = cliopts
             }

checkChartOpts :: ChartOpts -> IO ChartOpts
checkChartOpts opts = do
  checkCliOpts $ cliopts_ opts
  return opts

getHledgerChartOpts :: IO ChartOpts
getHledgerChartOpts = processArgs chartmode >>= return . decodeRawOpts >>= toChartOpts >>= checkChartOpts

