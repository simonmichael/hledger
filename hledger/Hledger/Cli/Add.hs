{-# LANGUAGE CPP #-}
{-| 

A history-aware add command to help with data entry.

Note: this might not be sensible, but add has some aspirations of being
both user-fiendly and pipeable/scriptable and for this reason
informational messages are mostly written to stderr rather than stdout.

-}

module Hledger.Cli.Add
where
import Hledger.Data
import Hledger.Read.JournalReader (someamount)
import Hledger.Cli.Options
import Hledger.Cli.Register (registerReport, registerReportAsText)
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
import Safe (headMay)
import Control.Exception (throw)

{- | Information used as the basis for suggested account names, amounts,
     etc in add prompt
-}
data PostingState = PostingState {
      psJournal :: Journal,
      psAccept  :: AccountName -> Bool,
      psSuggestHistoricalAmount :: Bool,
      psHistory :: Maybe [Posting]}

-- | Read transactions from the terminal, prompting for each field,
-- and append them to the journal file. If the journal came from stdin, this
-- command has no effect.
add :: [Opt] -> [String] -> Journal -> IO ()
add opts args j
    | f == "-" = return ()
    | otherwise = do
  hPutStrLn stderr $
    "Enter one or more transactions, which will be added to your journal file.\n"
    ++"To complete a transaction, enter . when prompted for an account.\n"
    ++"To quit, press control-d or control-c."
  today <- getCurrentDay
  getAndAddTransactions j opts args today
        `catch` (\e -> unless (isEOFError e) $ ioError e)
      where f = journalFilePath j

-- | Read a number of transactions from the command line, prompting,
-- validating, displaying and appending them to the journal file, until
-- end of input (then raise an EOF exception). Any command-line arguments
-- are used as the first transaction's description.
getAndAddTransactions :: Journal -> [Opt] -> [String] -> Day -> IO ()
getAndAddTransactions j opts args defaultDate = do
  (t, d) <- getTransaction j opts args defaultDate
  j <- journalAddTransaction j opts t
  getAndAddTransactions j opts args d

-- | Read a transaction from the command line, with history-aware prompting.
getTransaction :: Journal -> [Opt] -> [String] -> Day
                    -> IO (Transaction,Day)
getTransaction j opts args defaultDate = do
  today <- getCurrentDay
  datestr <- runInteractionDefault $ askFor "date" 
            (Just $ showDate defaultDate)
            (Just $ \s -> null s || 
             isRight (parse (smartdate >> many spacenonewline >> eof) "" $ lowercase s))
  description <- runInteractionDefault $ askFor "description" (Just "") Nothing
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
        ps <- getPostings (PostingState j accept True bestmatchpostings) []
        let t = nulltransaction{tdate=date
                               ,tstatus=False
                               ,tdescription=description
                               ,tpostings=ps
                               }
            retry msg = do
              liftIO $ hPutStrLn stderr $ "\n" ++ msg ++ "please re-enter."
              getpostingsandvalidate
        either retry (return . flip (,) date) $ balanceTransaction Nothing t -- imprecise balancing
  unless (null historymatches) 
       (liftIO $ do
         hPutStrLn stderr "Similar transactions found, using the first for defaults:\n"
         hPutStr stderr $ concatMap (\(n,t) -> printf "[%3d%%] %s" (round $ n*100 :: Int) (show t)) $ take 3 historymatches)
  getpostingsandvalidate

-- fragile
-- | Read postings from the command line until . is entered, using any
-- provided historical postings and the journal context to guess defaults.
getPostings :: PostingState -> [Posting] -> IO [Posting]
getPostings st enteredps = do
  let bestmatch | isNothing historicalps = Nothing
                | n <= length ps = Just $ ps !! (n-1)
                | otherwise = Nothing
                where Just ps = historicalps
      defaultaccount = maybe Nothing (Just . showacctname) bestmatch
  account <- runInteraction j $ askFor (printf "account %d" n) defaultaccount (Just accept)
  if account=="."
    then return enteredps
    else do
      let defaultacctused = Just account == defaultaccount
          historicalps' = if defaultacctused then historicalps else Nothing
          bestmatch' | isNothing historicalps' = Nothing
                     | n <= length ps = Just $ ps !! (n-1)
                     | otherwise = Nothing
                     where Just ps = historicalps'
          defaultamountstr | isJust bestmatch' && suggesthistorical = Just historicalamountstr
                           | n > 1             = Just balancingamountstr
                           | otherwise         = Nothing
              where
                -- force a decimal point in the output in case there's a
                -- digit group separator that would be mistaken for one
                historicalamountstr = showMixedAmountWithPrecision maxprecisionwithpoint $ pamount $ fromJust bestmatch'
                balancingamountstr  = showMixedAmountWithPrecision maxprecisionwithpoint $ negate $ sumMixedAmountsPreservingHighestPrecision $ map pamount enteredrealps
      amountstr <- runInteractionDefault $ askFor (printf "amount  %d" n) defaultamountstr validateamount
      let amount  = fromparse $ runParser (someamount <|> return missingamt) ctx     "" amountstr
          amount' = fromparse $ runParser (someamount <|> return missingamt) nullctx "" amountstr
          defaultamtused = Just (showMixedAmount amount) == defaultamountstr
          commodityadded | c == cwithnodef = Nothing
                         | otherwise       = c
              where c          = maybemixedamountcommodity amount
                    cwithnodef = maybemixedamountcommodity amount'
                    maybemixedamountcommodity = maybe Nothing (Just . commodity) . headMay . amounts
          p = nullposting{paccount=stripbrackets account,
                          pamount=amount,
                          ptype=postingtype account}
          st' = if defaultamtused then st
                   else st{psHistory = historicalps',
                           psSuggestHistoricalAmount = False}
      when (isJust commodityadded) $
           liftIO $ hPutStrLn stderr $ printf "using default commodity (%s)" (symbol $ fromJust commodityadded)
      getPostings st' (enteredps ++ [p])
    where
      j = psJournal st
      historicalps = psHistory st
      ctx = jContext j
      accept = psAccept st
      suggesthistorical = psSuggestHistoricalAmount st
      n = length enteredps + 1
      enteredrealps = filter isReal enteredps
      showacctname p = showAccountName Nothing (ptype p) $ paccount p
      postingtype ('[':_) = BalancedVirtualPosting
      postingtype ('(':_) = VirtualPosting
      postingtype _ = RegularPosting
      stripbrackets = dropWhile (`elem` "([") . reverse . dropWhile (`elem` "])") . reverse
      validateamount = Just $ \s -> (null s && not (null enteredrealps))
                                   || isRight (runParser (someamount>>many spacenonewline>>eof) ctx "" s)

-- | Prompt for and read a string value, optionally with a default value
-- and a validator. A validator causes the prompt to repeat until the
-- input is valid. May also raise an EOF exception if control-d is pressed.
askFor :: String -> Maybe String -> Maybe (String -> Bool) -> InputT IO String
askFor prompt def validator = do
  l <- fmap (maybe eofErr id)
            $ getInputLine $ prompt ++ maybe "" showdef def ++ ": "
  let input = if null l then fromMaybe l def else l
  case validator of
    Just valid -> if valid input
                   then return input
                   else askFor prompt def validator
    Nothing -> return input
    where
        showdef s = " [" ++ s ++ "]"
        eofErr = throw $ mkIOError eofErrorType "end of input" Nothing Nothing

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

runInteractionDefault :: InputT IO a -> IO a
runInteractionDefault m = do
    runInputT (setComplete noCompletion defaultSettings) m

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
