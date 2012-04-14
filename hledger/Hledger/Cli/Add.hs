{-| 

A history-aware add command to help with data entry.

Note: this might not be sensible, but add has some aspirations of being
both user-friendly and pipeable/scriptable and for this reason
informational messages are mostly written to stderr rather than stdout.

-}

module Hledger.Cli.Add
where
import Control.Exception (throw)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Char (toUpper)
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Safe (headMay)
import System.Console.Haskeline (InputT, runInputT, defaultSettings, setComplete, getInputLine)
import System.Console.Haskeline.Completion
import System.IO ( stderr, hPutStrLn, hPutStr )
import System.IO.Error
import Text.ParserCombinators.Parsec
import Text.Printf
import qualified Data.Foldable as Foldable (find)
import qualified Data.Set as Set

import Hledger
import Prelude hiding (putStr, putStrLn, appendFile)
import Hledger.Utils.UTF8IOCompat (putStr, putStrLn, appendFile)
import Hledger.Cli.Options
import Hledger.Cli.Register (postingsReportAsText)


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
add :: CliOpts -> Journal -> IO ()
add opts j
    | f == "-" = return ()
    | otherwise = do
  hPrintf stderr "Adding transactions to journal file \"%s\".\n" f
  hPutStrLn stderr $
    "To complete a transaction, enter . (period) at an account prompt.\n"
    ++"To stop adding transactions, enter . at a date prompt, or control-d/control-c."
  today <- getCurrentDay
  getAndAddTransactions j opts today
        `catch` (\e -> unless (isEOFError e) $ ioError e)
      where f = journalFilePath j

-- | Read a number of transactions from the command line, prompting,
-- validating, displaying and appending them to the journal file, until
-- end of input (then raise an EOF exception). Any command-line arguments
-- are used as the first transaction's description.
getAndAddTransactions :: Journal -> CliOpts -> Day -> IO ()
getAndAddTransactions j opts defaultDate = do
  (t, d) <- getTransaction j opts defaultDate
  j <- journalAddTransaction j opts t
  getAndAddTransactions j opts d

-- | Read a transaction from the command line, with history-aware prompting.
getTransaction :: Journal -> CliOpts -> Day
                    -> IO (Transaction,Day)
getTransaction j opts defaultDate = do
  today <- getCurrentDay
  datestr <- runInteractionDefault $ askFor "date, or . to end"
            (Just $ showDate defaultDate)
            (Just $ \s -> null s
                         || s == "."
                         || isRight (parse (smartdate >> many spacenonewline >> eof) "" $ lowercase s))
  when (datestr == ".") $ ioError $ mkIOError eofErrorType "" Nothing Nothing
  description <- runInteractionDefault $ askFor "description" (Just "") Nothing
  let historymatches = transactionsSimilarTo j (patterns_ $ reportopts_ opts) description
      bestmatch | null historymatches = Nothing
                | otherwise = Just $ snd $ head historymatches
      bestmatchpostings = maybe Nothing (Just . tpostings) bestmatch
      date = fixSmartDate today $ fromparse $ (parse smartdate "" . lowercase) datestr
      accept x = x == "." || (not . null) x &&
        if no_new_accounts_ opts
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
      ordot | null enteredps || length enteredrealps == 1 = ""
            | otherwise = ", or . to record"
  account <- runInteraction j $ askFor (printf "account %d%s" n ordot) defaultaccount (Just accept)
  if account=="."
    then
     if null enteredps
      then do hPutStrLn stderr $ "\nPlease enter some postings first."
              getPostings st enteredps
      else return enteredps
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
                historicalamountstr = showMixedAmountWithPrecision p $ pamount $ fromJust bestmatch'
                balancingamountstr  = showMixedAmountWithPrecision p $ negate $ sum $ map pamount enteredrealps
                -- what should this be ?
                -- 1 maxprecision (show all decimal places or none) ?
                -- 2 maxprecisionwithpoint (show all decimal places or .0 - avoids some but not all confusion with thousands separators) ?
                -- 3 canonical precision for this commodity in the journal ?
                -- 4 maximum precision entered so far in this transaction ?
                -- 5 3 or 4, whichever would show the most decimal places ?
                -- I think 1 or 4, whichever would show the most decimal places
                p = maxprecisionwithpoint
      amountstr <- runInteractionDefault $ askFor (printf "amount  %d" n) defaultamountstr validateamount
      let a  = fromparse $ runParser (amount <|> return missingamt) ctx     "" amountstr
          a' = fromparse $ runParser (amount <|> return missingamt) nullctx "" amountstr
          defaultamtused = Just (showMixedAmount a) == defaultamountstr
          commodityadded | c == cwithnodef = Nothing
                         | otherwise       = c
              where c          = maybemixedamountcommodity a
                    cwithnodef = maybemixedamountcommodity a'
                    maybemixedamountcommodity = maybe Nothing (Just . commodity) . headMay . amounts
          p = nullposting{paccount=stripbrackets account,
                          pamount=a,
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
                                   || isRight (runParser (amount>>many spacenonewline>>eof) ctx "" s)

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
journalAddTransaction :: Journal -> CliOpts -> Transaction -> IO Journal
journalAddTransaction j@Journal{jtxns=ts} opts t = do
  let f = journalFilePath j
  appendToJournalFileOrStdout f $ showTransaction t
  when (debug_ opts) $ do
    putStrLn $ printf "\nAdded transaction to %s:" f
    putStrLn =<< registerFromString (show t)
  return j{jtxns=ts++[t]}

-- | Append a string, typically one or more transactions, to a journal
-- file, or if the file is "-", dump it to stdout.  Tries to avoid
-- excess whitespace.
appendToJournalFileOrStdout :: FilePath -> String -> IO ()
appendToJournalFileOrStdout f s
  | f == "-"  = putStr s'
  | otherwise = appendFile f s'
  where s' = "\n" ++ ensureOneNewlineTerminated s

-- | Replace a string's 0 or more terminating newlines with exactly one.
ensureOneNewlineTerminated :: String -> String
ensureOneNewlineTerminated = (++"\n") . reverse . dropWhile (=='\n') . reverse

-- | Convert a string of journal data into a register report.
registerFromString :: String -> IO String
registerFromString s = do
  d <- getCurrentDay
  j <- readJournal' s
  return $ postingsReportAsText opts $ postingsReport opts (filterSpecFromOpts opts d) j
      where opts = defreportopts{empty_=True}

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
