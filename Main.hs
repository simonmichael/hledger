module Main ( main ) where

import Hledger

import Control.Exception ( bracket )
import Control.Monad
import Distribution.Text ( display )
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Data.Time.Calendar
import Text.Printf
import Data.List
import Data.Ord
import Statistics.Math.RootFinding


import Paths_hledger_irr ( version )

data Options = Options
  { optShowVersion  :: Bool
  , optShowHelp     :: Bool
  , optCashFlow     :: Bool
  , optInput        :: FilePath
  , optInvAcc       :: String
  , optInterestAcc  :: String
  , optBegin        :: Maybe String
  , optEnd          :: Maybe String
  , optInterval     :: Maybe Interval
  }

defaultOptions :: Options
defaultOptions = Options
  { optShowVersion  = False
  , optShowHelp     = False
  , optCashFlow     = False
  , optInput        = "-"
  , optInvAcc       = ""
  , optInterestAcc  = ""
  , optBegin        = Nothing
  , optEnd          = Nothing
  , optInterval     = Nothing
  }

options :: [OptDescr (Options -> Options)]
options =
 [ Option "h" ["help"]
              (NoArg (\o -> o { optShowHelp = True }))
              "print this message and exit"
 , Option "V" ["version"]
              (NoArg (\o -> o { optShowVersion = True }))
              "show version number and exit"
 , Option "c" ["cashflow"]
              (NoArg (\o -> o { optCashFlow = True }))
              "also show all revant transactions"
 , Option "f" ["file"]
              (ReqArg (\f o -> o { optInput = f }) "FILE")
              "input ledger file (pass '-' for stdin)"
 , Option "i" ["investment-account"]
              (ReqArg (\a o -> o { optInvAcc = a }) "ACCOUNT")
              "investment account"
 , Option "t" ["interest-account"]
              (ReqArg (\a o -> o { optInterestAcc = a }) "ACCOUNT")
              "interest/gain/fees/losses account"
 , Option "b" ["begin"]
              (ReqArg (\d o -> o { optBegin = Just d }) "DATE")
              "calculate interest from this date"
 , Option "e" ["end"]
              (ReqArg (\d o -> o { optEnd = Just d }) "DATE")
              "calculate interest until this date"
 , Option "D" ["daily"]
              (NoArg (\o -> o { optInterval = Just (Days 1) }))
              "calculate intereste for each day"
 , Option "W" ["weekly"]
              (NoArg (\o -> o { optInterval = Just (Weeks 1) }))
              "calculate intereste for each week"
 , Option "M" ["monthly"]
              (NoArg (\o -> o { optInterval = Just (Months 1) }))
              "calculate intereste for each month"
 , Option "Y" ["yearly"]
              (NoArg (\o -> o { optInterval = Just (Years 1) }))
              "calculate intereste for each year"
 ]

usageMessage :: String
usageMessage = usageInfo header options
  where header = "Usage: hledger-irr [OPTION...]"

commandLineError :: String -> IO a
commandLineError err = do hPutStrLn stderr (err ++ usageMessage)
                          exitFailure

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> commandLineError (concat errs)

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() -> do
  (opts, args) <- getArgs >>= parseOpts
  when (optShowVersion opts) (putStrLn (display version) >> exitSuccess)
  when (optShowHelp opts) (putStr usageMessage >> exitSuccess)
  when (null (optInvAcc opts)) (commandLineError "required --investment-account option is missing\n")
  when (null (optInterestAcc opts)) (commandLineError "required --interest-account option is missing\n")
  when (length args > 0) (commandLineError "no command line arguments allowed")
  jnl' <- readJournalFile Nothing Nothing (optInput opts) >>= either fail return

  let ts = jtxns $ filterJournalTransactions (Acct (optInvAcc opts)) jnl'
  when (null ts) $ do
    putStrLn "No relevant transactions found. Did you mis-spell your accounts?"
    exitFailure

  thisDay <- getCurrentDay
  let firstDay = minimum $ map transactionEffectiveDate ts
  let lastDay = maximum $ map transactionEffectiveDate ts
  let existingSpan = DateSpan (Just firstDay) (Just lastDay)

  let begin = maybe firstDay (fixSmartDateStr' thisDay) (optBegin opts)
  let end =   maybe thisDay  (fixSmartDateStr' thisDay) (optEnd opts)
  let wholeSpan = DateSpan (Just begin) (Just end)

  let spans = case optInterval opts of
        Nothing -> [wholeSpan]
        Just interval ->
            splitSpan interval $
            spanIntersect existingSpan wholeSpan

  forM_ spans $ \(DateSpan (Just ibegin) (Just iend)) -> do
      let preQuery = And [ Acct (optInvAcc opts),
                           EDate (openClosedSpan Nothing (Just ibegin))]
          pre_amount = negate $ unMix $ accountAmount preQuery ts
      let prefix = (ibegin, pre_amount)

      let eQuery = And [Acct (optInvAcc opts), EDate (openClosedSpan Nothing (Just iend))]
      let final = unMix $ accountAmount eQuery ts
      let postfix = (iend, final)

      let cfQuery = And [ Not (Or [Acct (optInvAcc opts), Acct (optInterestAcc opts)]), 
                          EDate (openClosedSpan (Just ibegin) (Just iend)) ] 
      let cf = calculateCashFlow cfQuery ts

      let totalCF = sortBy (comparing fst) $ filter ((/=0) . aquantity . snd) $ prefix : cf ++ [postfix]

      when (optCashFlow opts) $ do
          mapM_ (putStrLn . showCashFlowEntry) totalCF

      -- 0% is always a solution, so require at least something here
      putStr $ printf "%s - %s: " (showDate ibegin) (showDate iend)
      case ridders 0.00001 (0.000001,1000) (aquantity . interestSum iend totalCF) of
        Root rate -> putStrLn (printf "%0.2f%%" ((rate-1)*100))
        _ -> putStrLn "Error: Failed to find solution."

openClosedSpan :: Maybe Day -> Maybe Day -> DateSpan
openClosedSpan md1 md2 = DateSpan (fmap (addDays 1) md1) (fmap (addDays 1) md2)


-- Bad hack – what to do?
unMix :: MixedAmount -> Amount
unMix = sum . amounts


showCashFlowEntry :: (Day, Amount) -> String
showCashFlowEntry (d, a) = showDate d ++ ": " ++ showAmount a

type CashFlow = [(Day, Amount)]


-- | Divide an amount's quantity by a constant.
multiplyAmount :: Amount -> Double -> Amount
multiplyAmount a@Amount{aquantity=q} d = a{aquantity=q*d}


interestSum :: Day -> CashFlow -> Double -> Amount
interestSum referenceDay cf rate = sum $ map go cf
    where go (t,m) = multiplyAmount m (rate ** (fromIntegral (referenceDay `diffDays` t) / 365))


calculateCashFlow :: Query -> [Transaction] -> CashFlow
calculateCashFlow query = map go
    where
    go t = (transactionEffectiveDate t, amt)
        where
        amt = sum $
              map (unMix . pamount) $ filter (matchesPosting query) $
              realPostings t

accountAmount :: Query -> [Transaction] -> MixedAmount
accountAmount query = sumPostings . filter (matchesPosting query) . concatMap realPostings 

-- | Convert a smart date string to a day using
-- the provided reference date, or raise an error.
fixSmartDateStr' :: Day -> String -> Day
fixSmartDateStr' d s = either
                       (\e->error' $ printf "could not parse date %s %s" (show s) (show e))
                       id
                       $ fixSmartDateStrEither' d s
