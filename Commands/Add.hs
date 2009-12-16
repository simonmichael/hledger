{-| 

A history-aware add command to help with data entry.

-}

module Commands.Add
where
import Prelude hiding (putStr, putStrLn, getLine, appendFile)
import Ledger
import Options
import Commands.Register (showRegisterReport)
import System.IO.UTF8
import System.IO (stderr, hFlush)
import System.IO.Error
import Text.ParserCombinators.Parsec
import Utils (ledgerFromStringWithOpts)


-- | Read ledger transactions from the terminal, prompting for each field,
-- and append them to the ledger file. If the ledger came from stdin, this
-- command has no effect.
add :: [Opt] -> [String] -> Ledger -> IO ()
add _ args l
    | filepath (journal l) == "-" = return ()
    | otherwise = do
  hPutStrLn stderr
    "Enter one or more transactions, which will be added to your ledger file.\n\
    \To complete a transaction, enter . as account name. To quit, enter control-d."
  getAndAddTransactions l args `catch` (\e -> unless (isEOFError e) $ ioError e)

-- | Read a number of ledger transactions from the command line,
-- prompting, validating, displaying and appending them to the ledger
-- file, until end of input (then raise an EOF exception). Any
-- command-line arguments are used as the first transaction's description.
getAndAddTransactions :: Ledger -> [String] -> IO ()
getAndAddTransactions l args = do
  l <- getTransaction l args >>= addTransaction l
  getAndAddTransactions l []

-- | Read a transaction from the command line, with history-aware prompting.
getTransaction :: Ledger -> [String] -> IO LedgerTransaction
getTransaction l args = do
  today <- getCurrentDay
  datestr <- askFor "date" 
            (Just $ showDate today)
            (Just $ \s -> null s || 
             isRight (parse (smartdate >> many spacenonewline >> eof) "" $ lowercase s))
  description <- if null args 
                  then askFor "description" Nothing (Just $ not . null) 
                  else do
                         let description = unwords args
                         hPutStrLn stderr $ "description: " ++ description
                         return description
  let historymatches = transactionsSimilarTo l description
      bestmatch | null historymatches = Nothing
                | otherwise = Just $ snd $ head historymatches
      bestmatchpostings = maybe Nothing (Just . ltpostings) bestmatch
      date = fixSmartDate today $ fromparse $ (parse smartdate "" . lowercase) datestr
      getpostingsandvalidate = do
        ps <- getPostings bestmatchpostings []
        let t = nullledgertxn{ltdate=date
                             ,ltstatus=False
                             ,ltdescription=description
                             ,ltpostings=ps
                             }
            retry = do
              hPutStrLn stderr $ "\n" ++ nonzerobalanceerror ++ ". Re-enter:"
              getpostingsandvalidate
        either (const retry) return $ balanceLedgerTransaction t
  unless (null historymatches) 
       (do
         hPutStrLn stderr "Similar transactions found, using the first for defaults:\n"
         hPutStr stderr $ concatMap (\(n,t) -> printf "[%3d%%] %s" (round $ n*100 :: Int) (show t)) $ take 3 historymatches)
  getpostingsandvalidate

-- | Read postings from the command line until . is entered, using the
-- provided historical postings, if any, to guess defaults.
getPostings :: Maybe [Posting] -> [Posting] -> IO [Posting]
getPostings historicalps enteredps = do
  account <- askFor (printf "account %d" n) defaultaccount (Just $ not . null)
  if account=="."
    then return enteredps
    else do
      amountstr <- askFor (printf "amount  %d" n) defaultamount validateamount
      let amount = fromparse $ parse (someamount <|> return missingamt) "" amountstr
      let p = nullrawposting{paccount=stripbrackets account,
                             pamount=amount,
                             ptype=postingtype account}
      getPostings historicalps $ enteredps ++ [p]
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
          where balancingamount = Just $ show $ negate $ sum $ map pamount enteredrealps
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

-- | Append this transaction to the ledger's file. Also, to the ledger's
-- transaction list, but we don't bother updating the other fields - this
-- is enough to include new transactions in the history matching.
addTransaction :: Ledger -> LedgerTransaction -> IO Ledger
addTransaction l t = do
  appendToLedgerFile l $ show t
  putStrLn $ printf "\nAdded transaction to %s:" (filepath $ journal l)
  putStrLn =<< registerFromString (show t)
  return l{journal=rl{ledger_txns=ts}}
      where rl = journal l
            ts = ledger_txns rl ++ [t]

-- | Append data to the ledger's file, ensuring proper separation from any
-- existing data; or if the file is "-", dump it to stdout.
appendToLedgerFile :: Ledger -> String -> IO ()
appendToLedgerFile l s = 
    if f == "-"
    then putStr $ sep ++ s
    else appendFile f $ sep++s
    where 
      f = filepath $ journal l
      -- we keep looking at the original raw text from when the ledger
      -- was first read, but that's good enough for now
      t = journaltext l
      sep | null $ strip t = ""
          | otherwise = replicate (2 - min 2 (length lastnls)) '\n'
          where lastnls = takeWhile (=='\n') $ reverse t

-- | Convert a string of ledger data into a register report.
registerFromString :: String -> IO String
registerFromString s = do
  now <- getCurrentLocalTime
  l <- ledgerFromStringWithOpts [] [] now s
  return $ showRegisterReport [Empty] [] l

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

compareLedgerDescriptions s t = compareStrings s' t'
    where s' = simplify s
          t' = simplify t
          simplify = filter (not . (`elem` "0123456789"))

transactionsSimilarTo :: Ledger -> String -> [(Double,LedgerTransaction)]
transactionsSimilarTo l s =
    sortBy compareRelevanceAndRecency
               $ filter ((> threshold).fst)
               [(compareLedgerDescriptions s $ ltdescription t, t) | t <- ts]
    where
      compareRelevanceAndRecency (n1,t1) (n2,t2) = compare (n2,ltdate t2) (n1,ltdate t1)
      ts = ledger_txns $ journal l
      threshold = 0

