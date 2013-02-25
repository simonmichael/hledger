{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
{-| 

A history-aware add command to help with data entry.

Note: this might not be sensible, but add has some aspirations of being
both user-friendly and pipeable/scriptable and for this reason
informational messages are mostly written to stderr rather than stdout.

-}

module Hledger.Cli.Add
where
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Char (toUpper)
import Data.List
import Data.Maybe
import Data.Typeable (Typeable)
import Safe (headDef, tailDef, headMay)
import System.Console.Haskeline (InputT, runInputT, defaultSettings, setComplete, getInputLine)
import System.Console.Haskeline.Completion
import System.IO ( stderr, hPutStrLn )
import System.IO.Error
import Text.ParserCombinators.Parsec
import Text.Printf
import qualified Data.Set as Set

import Hledger
import Prelude hiding (putStr, putStrLn, appendFile)
import Hledger.Utils.UTF8IOCompat (putStr, putStrLn, appendFile)
import Hledger.Cli.Options
import Hledger.Cli.Register (postingsReportAsText)


-- | Read multiple transactions from the console, prompting for each
-- field, and append them to the journal file.  If the journal came
-- from stdin, this command has no effect.
add :: CliOpts -> Journal -> IO ()
add opts j
    | f == "-" = return ()
    | otherwise = do
  hPutStrLn stderr $ unlines [
     "Adding transactions to journal file "++f
    ,"Provide field values at the prompts, or press enter to accept defaults."
    ,"Use readline keys to edit, use tab key to complete account names."
    ,"A code (in parentheses) may be entered following transaction dates."
    ,"A comment may be entered following descriptions or amounts."
    ,"If you make a mistake, enter < at any prompt to restart the transaction."
    ,"To record a transaction, enter . when prompted."
    ,"To quit, press control-d or control-c."
    ]
  today <- showDate `fmap` getCurrentDay
  let args                = words' $ query_ $ reportopts_ opts
      (defdate, moredefs) = headTailDef today args
  getAndAddTransactionsLoop j opts defdate moredefs
        `E.catch` (\e -> unless (isEOFError e) $ ioError e)
      where f = journalFilePath j

-- | Loop reading transactions from the console, prompting for,
-- validating, displaying and appending each one to the journal file,
-- until end of input or ctrl-c (then raise an EOF exception).
-- If provided, command-line arguments are used as defaults for the
-- first transaction; otherwise defaults come from the most similar
-- recent transaction in the journal.
getAndAddTransactionsLoop :: Journal -> CliOpts -> String -> [String] -> IO ()
getAndAddTransactionsLoop j opts defdate moredefs = do
  t <- getTransaction j opts defdate moredefs
  j <- journalAddTransaction j opts t
  hPrintf stderr "\nRecorded transaction:\n%s" (show t)
  let defdate' = showDate $ tdate t
  getAndAddTransactionsLoop j opts defdate' []

-- | Read a single transaction from the console, with history-aware prompting.
-- A default date, and zero or more defaults for subsequent fields, are provided.
getTransaction :: Journal -> CliOpts -> String -> [String] -> IO Transaction
getTransaction j opts defdate moredefs = do
  let dateandcodep = do {d <- smartdate; c <- optionMaybe code; many spacenonewline; eof; return (d, fromMaybe "" c)}
  datecodestr <- runInteractionDefault $ askFor "date"
            (Just defdate)
            (Just $ \s -> null s
                         || s == "."
                         || isRight (parseWithCtx nullctx dateandcodep $ lowercase s))
  when (datecodestr == ".") $ ioError $ mkIOError eofErrorType "" Nothing Nothing
  let (sdate,code) = fromparse $ parseWithCtx nullctx dateandcodep datecodestr
      datestr = showDate $ fixSmartDate (parsedate defdate) sdate

  let (defdesc, moredefs') = headTailDef "" moredefs
  desc <- runInteractionDefault $ askFor "description" (Just defdesc) Nothing
  let restart = do hPrintf stderr "\nRestarting this transaction..\n\n"
                   getTransaction j opts defdate moredefs'
  if desc == "<"
   then restart
   else do
    let (description,comment) = (strip a, strip $ dropWhile (==';') b) where (a,b) = break (==';') desc
    mt <- getPostingsForTransactionWithHistory j opts datestr code description comment moredefs'
    case mt of
      Nothing -> restart
      Just t  -> return t

data RestartEntryException = RestartEntryException deriving (Typeable,Show)
instance Exception RestartEntryException

-- | Information used as the basis for suggested account names, amounts etc. in add prompt.
data PostingsState = PostingsState {
   psJournal                 :: Journal
  ,psAccept                  :: AccountName -> Bool
  ,psSuggestHistoricalAmount :: Bool
  ,psHistory                 :: Maybe [Posting]
  }

-- | Loop reading postings from the console, until a valid balanced
-- set of postings has been entered, then return the final transaction,
-- or nothing indicating that the user wants to restart entering this transaction.
getPostingsForTransactionWithHistory :: Journal -> CliOpts -> String -> String -> String -> String -> [String] -> IO (Maybe Transaction)
getPostingsForTransactionWithHistory j opts datestr code description comment defargs = do
  today <- getCurrentDay
  let historymatches = transactionsSimilarTo j (queryFromOpts today $ reportopts_ opts) description
      bestmatch | not (null defargs) || null historymatches = Nothing
                | otherwise = Just $ snd $ head historymatches
      bestmatchpostings = maybe Nothing (Just . tpostings) bestmatch
      date = fixSmartDate today $ fromparse $ (parse smartdate "" . lowercase) datestr
      accept x = x == "." || (not . null) x &&
        if no_new_accounts_ opts
            then x `elem` existingaccts
            else True
      existingaccts = journalAccountNames j
      getvalidpostings = do
        ps <- getPostingsLoop (PostingsState j accept True bestmatchpostings) [] defargs
        let t = nulltransaction{tdate=date
                               ,tstatus=False
                               ,tcode=code
                               ,tdescription=description
                               ,tcomment=comment
                               ,tpostings=ps
                               }
        either retry (return . Just) $ balanceTransaction Nothing t -- imprecise balancing
          where
            retry msg = liftIO (hPutStrLn stderr $ "\n" ++ (capitalize msg) ++ "please re-enter.") >> getvalidpostings

  when (isJust bestmatch) $ liftIO $ hPrintf stderr "\nUsing this similar transaction for defaults:\n%s" (show $ fromJust bestmatch)
  getvalidpostings `E.catch` \(_::RestartEntryException) -> return Nothing

-- | Read postings from the command line until . is entered, generating
-- useful defaults based on historical context and postings entered so far.
getPostingsLoop :: PostingsState -> [Posting] -> [String] -> IO [Posting]
getPostingsLoop st enteredps defargs = do
  let bestmatch | isNothing historicalps = Nothing
                | n <= length ps = Just $ ps !! (n-1)
                | otherwise = Nothing
                where Just ps = historicalps
      bestmatchacct = maybe Nothing (Just . showacctname) bestmatch
      defacct  = maybe bestmatchacct Just $ headMay defargs
      defargs' = tailDef [] defargs
      ordot | null enteredps || length enteredrealps == 1 = "" :: String
            | otherwise = " (or . to record)"
  account <- runInteraction j $ askFor (printf "account %d%s" n ordot) defacct (Just accept)
  when (account=="<") $ throwIO RestartEntryException
  if account=="."
    then
     if null enteredps
      then do hPutStrLn stderr $ "\nPlease enter some postings first."
              getPostingsLoop st enteredps defargs
      else return enteredps
    else do
      let defacctused = Just account == defacct
          historicalps' = if defacctused then historicalps else Nothing
          bestmatch' | isNothing historicalps' = Nothing
                     | n <= length ps = Just $ ps !! (n-1)
                     | otherwise = Nothing
                     where Just ps = historicalps'
          defamountstr | isJust commandlineamt                  = commandlineamt
                       | isJust bestmatch' && suggesthistorical = Just historicalamountstr
                       | n > 1                                  = Just balancingamountstr
                       | otherwise                              = Nothing
              where
                commandlineamt      = headMay defargs'
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
          defargs'' = tailDef [] defargs'
      amt <- runInteractionDefault $ askFor (printf "amount  %d" n) defamountstr validateamount
      when (amt=="<") $ throwIO RestartEntryException
      let (amountstr,comment) = (strip a, strip $ dropWhile (==';') b) where (a,b) = break (==';') amt
      let a  = fromparse $ runParser (amountp <|> return missingamt) ctx     "" amountstr
          a' = fromparse $ runParser (amountp <|> return missingamt) nullctx "" amountstr
          wasdefamtused = Just (showAmount a) == defamountstr
          defcommodityadded | acommodity a == acommodity a' = Nothing
                                | otherwise                     = Just $ acommodity a
          p = nullposting{paccount=stripbrackets account
                         ,pamount=mixed a
                         ,pcomment=comment
                         ,ptype=postingtype account
                         }
          st' = if wasdefamtused
                 then st
                 else st{psHistory=historicalps', psSuggestHistoricalAmount=False}
      when (isJust defcommodityadded) $
           liftIO $ hPutStrLn stderr $ printf "using default commodity (%s)" (fromJust defcommodityadded)
      getPostingsLoop st' (enteredps ++ [p]) defargs''
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
      validateamount = Just $ \s -> (null s && not (null enteredrealps))
                                    || s == "<"
                                    || (isRight (runParser (amountp >> many spacenonewline >> optional (char ';' >> many anyChar) >> eof) ctx "" s)
                                        && s /= ".")

-- | Prompt for and read a string value, optionally with a default value
-- and a validator. A validator causes the prompt to repeat until the
-- input is valid. May also raise an EOF exception if control-d or control-c is pressed.
askFor :: String -> Maybe String -> Maybe (String -> Bool) -> InputT IO String
askFor prompt def validator = do
  l <- fmap (maybe eofErr id)
            $ getInputLine $ prompt ++ " ? " ++ maybe "" showdef def ++ ": "
  let input = if null l then fromMaybe l def else l
  case validator of
    Just valid -> if valid input
                   then return input
                   else askFor prompt def validator
    Nothing -> return input
    where
        showdef "" = ""
        showdef s = "[" ++ s ++ "]"
        eofErr = E.throw $ mkIOError eofErrorType "end of input" Nothing Nothing

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
  return $ postingsReportAsText opts $ postingsReport ropts (queryFromOpts d ropts) j
      where
        ropts = defreportopts{empty_=True}
        opts = defcliopts{reportopts_=ropts}

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

transactionsSimilarTo :: Journal -> Query -> String -> [(Double,Transaction)]
transactionsSimilarTo j q s =
    sortBy compareRelevanceAndRecency
               $ filter ((> threshold).fst)
               [(compareDescriptions s $ tdescription t, t) | t <- ts]
    where
      compareRelevanceAndRecency (n1,t1) (n2,t2) = compare (n2,tdate t2) (n1,tdate t1)
      ts = filter (q `matchesTransaction`) $ jtxns j
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

capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = toUpper c : cs

headTailDef :: a -> [a] -> (a,[a])
headTailDef defhead as = (headDef defhead as, tailDef [] as)

