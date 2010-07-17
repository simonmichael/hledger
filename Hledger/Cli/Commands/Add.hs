{-# LANGUAGE CPP #-}
{-| 

A history-aware add command to help with data entry.

-}

module Hledger.Cli.Commands.Add
where
import Hledger.Data
import Hledger.Read.Journal (someamount)
import Hledger.Cli.Options
import Hledger.Cli.Commands.Register (showRegisterReport)
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding (putStr, putStrLn, getLine, appendFile)
import System.IO.UTF8
import System.IO ( stderr, hFlush )
#else
import System.IO ( stderr, hFlush, hPutStrLn, hPutStr )
#endif
import System.IO.Error
import Text.ParserCombinators.Parsec
import Hledger.Cli.Utils (readJournalWithOpts)
import qualified Data.Foldable as Foldable (find)

-- | Read transactions from the terminal, prompting for each field,
-- and append them to the journal file. If the journal came from stdin, this
-- command has no effect.
add :: [Opt] -> [String] -> Journal -> IO ()
add opts args j
    | filepath j == "-" = return ()
    | otherwise = do
  hPutStrLn stderr $
    "Enter one or more transactions, which will be added to your journal file.\n"
    ++"To complete a transaction, enter . as account name. To quit, press control-c."
  today <- getCurrentDay
  getAndAddTransactions j opts args today `catch` (\e -> unless (isEOFError e) $ ioError e)

-- | Read a number of transactions from the command line, prompting,
-- validating, displaying and appending them to the journal file, until
-- end of input (then raise an EOF exception). Any command-line arguments
-- are used as the first transaction's description.
getAndAddTransactions :: Journal -> [Opt] -> [String] -> Day -> IO ()
getAndAddTransactions j opts args defaultDate = do
  (t, d) <- getTransaction j opts args defaultDate
  j <- journalAddTransaction j t
  getAndAddTransactions j opts args d

-- | Read a transaction from the command line, with history-aware prompting.
getTransaction :: Journal -> [Opt] -> [String] -> Day -> IO (Transaction,Day)
getTransaction j opts args defaultDate = do
  today <- getCurrentDay
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
              hPutStrLn stderr $ "\n" ++ msg ++ "please re-enter."
              getpostingsandvalidate
        either retry (return . flip (,) date) $ balanceTransaction t
  unless (null historymatches) 
       (do
         hPutStrLn stderr "Similar transactions found, using the first for defaults:\n"
         hPutStr stderr $ concatMap (\(n,t) -> printf "[%3d%%] %s" (round $ n*100 :: Int) (show t)) $ take 3 historymatches)
  getpostingsandvalidate

-- | Read postings from the command line until . is entered, using the
-- provided historical postings, if any, to guess defaults.
getPostings :: (AccountName -> Bool) -> Maybe [Posting] -> [Posting] -> IO [Posting]
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
askFor :: String -> Maybe String -> Maybe (String -> Bool) -> IO String
askFor prompt def validator = do
  hPutStr stderr $ prompt ++ maybe "" showdef def ++ ": "
  hFlush stderr
  l <- getLine
  let input = if null l then fromMaybe l def else l
  case validator of
    Just valid -> if valid input
                   then return input
                   else askFor prompt def validator
    Nothing -> return input
    where showdef s = " [" ++ s ++ "]"

-- | Append this transaction to the journal's file. Also, to the journal's
-- transaction list, but we don't bother updating the other fields - this
-- is enough to include new transactions in the history matching.
journalAddTransaction :: Journal -> Transaction -> IO Journal
journalAddTransaction j@Journal{jtxns=ts} t = do
  appendToJournalFile j $ showTransaction t
  putStrLn $ printf "\nAdded transaction to %s:" (filepath j)
  putStrLn =<< registerFromString (show t)
  return j{jtxns=ts++[t]}

-- | Append data to the journal's file, ensuring proper separation from
-- any existing data; or if the file is "-", dump it to stdout.
appendToJournalFile :: Journal -> String -> IO ()
appendToJournalFile Journal{filepath=f, jtext=t} s =
    if f == "-"
    then putStr $ sep ++ s
    else appendFile f $ sep++s
    where 
      -- XXX we are looking at the original raw text from when the journal
      -- was first read, but that's good enough for now
      sep | null $ strip t = ""
          | otherwise = replicate (2 - min 2 (length lastnls)) '\n'
          where lastnls = takeWhile (=='\n') $ reverse t

-- | Convert a string of journal data into a register report.
registerFromString :: String -> IO String
registerFromString s = do
  now <- getCurrentLocalTime
  l <- readJournalWithOpts [] s
  return $ showRegisterReport opts (optsToFilterSpec opts [] now) l
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

