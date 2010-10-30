{-# LANGUAGE CPP #-}
{-| 

A history-aware add command to help with data entry.

-}

module Hledger.Cli.Commands.Add
where
import Hledger.Data
import Hledger.Read.Journal (someamount)
import Hledger.Cli.Options
import Hledger.Cli.Commands.Register (registerReport, registerReportAsText)
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding (putStr, putStrLn, getLine, appendFile)
import System.IO.UTF8
import System.IO ( stderr )
#else
import System.IO ( stderr, hPutStrLn, hPutStr )
#endif
import System.IO.Error
import Text.ParserCombinators.Parsec
import Hledger.Cli.Utils (readJournalWithOpts)
import qualified Data.Foldable as Foldable (find)
import System.Console.Haskeline (
            InputT, runInputT, defaultSettings, setComplete, getInputLine)
import Control.Monad.Trans (liftIO)
import System.Console.Haskeline.Completion
import qualified Data.Set as Set

-- | Read transactions from the terminal, prompting for each field,
-- and append them to the journal file. If the journal came from stdin, this
-- command has no effect.
add :: [Opt] -> [String] -> Journal -> IO ()
add opts args j
    | f == "-" = return ()
    | otherwise = do
  hPutStrLn stderr $
    "Enter one or more transactions, which will be added to your journal file.\n"
    ++"To complete a transaction, enter . as account name. To quit, press control-c."
  today <- getCurrentDay
  runInteraction j (getAndAddTransactions j opts args today)
        `catch` (\e -> unless (isEOFError e) $ ioError e)
      where f = journalFilePath j

-- | Read a number of transactions from the command line, prompting,
-- validating, displaying and appending them to the journal file, until
-- end of input (then raise an EOF exception). Any command-line arguments
-- are used as the first transaction's description.
getAndAddTransactions :: Journal -> [Opt] -> [String] -> Day -> InputT IO ()
getAndAddTransactions j opts args defaultDate = do
  (t, d) <- getTransaction j opts args defaultDate
  j <- liftIO $ journalAddTransaction j opts t
  getAndAddTransactions j opts args d

-- | Read a transaction from the command line, with history-aware prompting.
getTransaction :: Journal -> [Opt] -> [String] -> Day
                    -> InputT IO (Transaction,Day)
getTransaction j opts args defaultDate = do
  today <- liftIO getCurrentDay
  datestr <- askFor "date" 
            (Just $ showDate defaultDate)
            (Just $ \s -> null s || 
             isRight (parse (smartdate >> many spacenonewline >> eof) "" $ lowercase s))
  description <- askFor "description" (Just "") Nothing
  let historymatches = transactionsSimilarTo j args description
      bestmatch | null historymatches = Nothing
                | otherwise = Just $ snd $ head historymatches
      bestmatchpostings = maybe Nothing (Just . tpostings) bestmatch
      date = fixSmartDate today $ fromparse $ (parse smartdate "" . lowercase) datestr
      accept x = x == "." || (not . null) x &&
        if NoNewAccts `elem` opts
            then isJust $ Foldable.find (== x) ant
            else True
        where (ant,_,_,_) = groupPostings $ journalPostings j
      getpostingsandvalidate = do
        ps <- getPostings accept bestmatchpostings []
        let t = nulltransaction{tdate=date
                               ,tstatus=False
                               ,tdescription=description
                               ,tpostings=ps
                               }
            retry msg = do
              liftIO $ hPutStrLn stderr $ "\n" ++ msg ++ "please re-enter."
              getpostingsandvalidate
        either retry (return . flip (,) date) $ balanceTransaction t
  unless (null historymatches) 
       (liftIO $ do
         hPutStrLn stderr "Similar transactions found, using the first for defaults:\n"
         hPutStr stderr $ concatMap (\(n,t) -> printf "[%3d%%] %s" (round $ n*100 :: Int) (show t)) $ take 3 historymatches)
  getpostingsandvalidate

-- | Read postings from the command line until . is entered, using the
-- provided historical postings, if any, to guess defaults.
getPostings :: (AccountName -> Bool) -> Maybe [Posting] -> [Posting] -> InputT IO [Posting]
getPostings accept historicalps enteredps = do
  account <- askFor (printf "account %d" n) defaultaccount (Just accept)
  if account=="."
    then return enteredps
    else do
      amountstr <- askFor (printf "amount  %d" n) defaultamount validateamount
      let amount = fromparse $ parse (someamount <|> return missingamt) "" amountstr
      let p = nullposting{paccount=stripbrackets account,
                          pamount=amount,
                          ptype=postingtype account}
      getPostings accept historicalps $ enteredps ++ [p]
    where
      n = length enteredps + 1
      enteredrealps = filter isReal enteredps
      bestmatch | isNothing historicalps = Nothing
                | n <= length ps = Just $ ps !! (n-1)
                | otherwise = Nothing
                where Just ps = historicalps
      defaultaccount = maybe Nothing (Just . showacctname) bestmatch
      showacctname p = showAccountName Nothing (ptype p) $ paccount p
      defaultamount = maybe balancingamount (Just . show . pamount) bestmatch
          where balancingamount = Just $ show $ negate $ sumMixedAmountsPreservingHighestPrecision $ map pamount enteredrealps
      postingtype ('[':_) = BalancedVirtualPosting
      postingtype ('(':_) = VirtualPosting
      postingtype _ = RegularPosting
      stripbrackets = dropWhile (`elem` "([") . reverse . dropWhile (`elem` "])") . reverse
      validateamount = Just $ \s -> (null s && not (null enteredrealps))
                                   || isRight (parse (someamount>>many spacenonewline>>eof) "" s)

-- | Prompt for and read a string value, optionally with a default value
-- and a validator. A validator causes the prompt to repeat until the
-- input is valid. May also raise an EOF exception if control-d is pressed.
askFor :: String -> Maybe String -> Maybe (String -> Bool) -> InputT IO String
askFor prompt def validator = do
  l <- fmap (maybe "" id)
            $ getInputLine $ prompt ++ maybe "" showdef def ++ ": "
  let input = if null l then fromMaybe l def else l
  case validator of
    Just valid -> if valid input
                   then return input
                   else askFor prompt def validator
    Nothing -> return input
    where showdef s = " [" ++ s ++ "]"

-- | Append this transaction to the journal's file, and to the journal's
-- transaction list.
journalAddTransaction :: Journal -> [Opt] -> Transaction -> IO Journal
journalAddTransaction j@Journal{jtxns=ts} opts t = do
  let f = journalFilePath j
  appendToJournalFile f $ showTransaction t
  when (Debug `elem` opts) $ do
    putStrLn $ printf "\nAdded transaction to %s:" f
    putStrLn =<< registerFromString (show t)
  return j{jtxns=ts++[t]}

-- | Append data to a journal file; or if the file is "-", dump it to stdout.
appendToJournalFile :: FilePath -> String -> IO ()
appendToJournalFile f s =
    if f == "-"
    then putStr $ sep ++ s
    else appendFile f $ sep++s
    where 
      sep = "\n\n"
      -- sep | null $ strip t = ""
      --     | otherwise = replicate (2 - min 2 (length lastnls)) '\n'
      --     where lastnls = takeWhile (=='\n') $ reverse t

-- | Convert a string of journal data into a register report.
registerFromString :: String -> IO String
registerFromString s = do
  now <- getCurrentLocalTime
  l <- readJournalWithOpts [] s
  return $ registerReportAsText opts $ registerReport opts (optsToFilterSpec opts [] now) l
    where opts = [Empty]

-- | Return a similarity measure, from 0 to 1, for two strings.
-- This is Simon White's letter pairs algorithm from
-- http://www.catalysoft.com/articles/StrikeAMatch.html
-- with a modification for short strings.
compareStrings :: String -> String -> Double
compareStrings "" "" = 1
compareStrings (_:[]) "" = 0
compareStrings "" (_:[]) = 0
compareStrings (a:[]) (b:[]) = if toUpper a == toUpper b then 1 else 0
compareStrings s1 s2 = 2.0 * fromIntegral i / fromIntegral u
    where
      i = length $ intersect pairs1 pairs2
      u = length pairs1 + length pairs2
      pairs1 = wordLetterPairs $ uppercase s1
      pairs2 = wordLetterPairs $ uppercase s2
wordLetterPairs = concatMap letterPairs . words
letterPairs (a:b:rest) = [a,b] : letterPairs (b:rest)
letterPairs _ = []

compareDescriptions :: [Char] -> [Char] -> Double
compareDescriptions s t = compareStrings s' t'
    where s' = simplify s
          t' = simplify t
          simplify = filter (not . (`elem` "0123456789"))

transactionsSimilarTo :: Journal -> [String] -> String -> [(Double,Transaction)]
transactionsSimilarTo j apats s =
    sortBy compareRelevanceAndRecency
               $ filter ((> threshold).fst)
               [(compareDescriptions s $ tdescription t, t) | t <- ts]
    where
      compareRelevanceAndRecency (n1,t1) (n2,t2) = compare (n2,tdate t2) (n1,tdate t1)
      ts = jtxns $ filterJournalTransactionsByAccount apats j
      threshold = 0

runInteraction :: Journal -> InputT IO a -> IO a
runInteraction j m = do
    let cc = completionCache j
    runInputT (setComplete (accountCompletion cc) defaultSettings) m

-- A precomputed list of all accounts previously entered into the journal.
type CompletionCache = [AccountName]

completionCache :: Journal -> CompletionCache
completionCache j = -- Only keep unique account names.
                    Set.toList $ Set.fromList
                        [paccount p | t <- jtxns j, p <- tpostings t]

accountCompletion :: CompletionCache -> CompletionFunc IO
accountCompletion cc = completeWord Nothing
                        "" -- don't break words on whitespace, since account names
                           -- can contain spaces.
                        $ \s -> return $ map simpleCompletion
                                        $ filter (s `isPrefixOf`) cc
