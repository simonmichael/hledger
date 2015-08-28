{-# LANGUAGE TemplateHaskell #-}
{-|

-}

module Hledger.Chart.ChartOptions
where
import Data.Maybe
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit

import Hledger.Cli hiding (progname)
--import qualified Hledger.Cli (progname)

progname    = "hledger-chart"
progversion = progname ++ " dev"

defchartoutput   = "hledger.png"
defchartitems    = 10
defchartsize     = "600x400"

chartmode = (defCommandMode ["hledger-chart"]) {
   modeArgs = ([], Just $ argsFlag "[PATTERNS] --add-posting \"ACCT  AMTEXPR\" ...")
  ,modeHelp = "generate a pie chart image for the top account balances (of one sign only)"
  ,modeHelpSuffix=[]
  ,modeGroupFlags = Group {
     groupNamed = [generalflagsgroup1]
    ,groupUnnamed = [
         flagReq ["chart-output","o"]  (\s opts -> Right $ setopt "chart-output" s opts) "IMGFILE" ("output filename (default: "++defchartoutput++")")
        ,flagReq ["chart-items"]  (\s opts -> Right $ setopt "chart-items" s opts) "N" ("number of accounts to show (default: "++show defchartitems++")")
        ,flagReq ["chart-size"]  (\s opts -> Right $ setopt "chart-size" s opts) "WIDTHxHEIGHT" ("image size (default: "++defchartsize++")")
        ]
    ,groupHidden = []
    }
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
  cliopts <- rawOptsToCliOpts rawopts
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

