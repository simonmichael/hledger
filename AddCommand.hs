{-| 

An add command to help with data entry.

-}

module AddCommand
where
-- import Data.List.Utils (replace)
import Prelude hiding (putStr, putStrLn, getLine, appendFile)
import Ledger
import Options
import RegisterCommand (showRegisterReport)
import System.IO.UTF8
import System.IO (stderr, hFlush)
import System.IO.Error
import Text.ParserCombinators.Parsec
import Utils (ledgerFromStringWithOpts)


-- | Read ledger transactions from the command line, prompting for each
-- field, and append them to the ledger file. If the ledger came from
-- stdin, this command has no effect.
add :: [Opt] -> [String] -> Ledger -> IO ()
add opts args l
    | filepath (rawledger l) == "-" = return ()
    | otherwise = do
  hPutStrLn stderr ("Enter one or more transactions, which will be added to your ledger file.\n\
                    \A blank account or amount ends a transaction, control-d to finish.")
  ts <- getAndAddTransactions l args
  hPutStrLn stderr $ printf "\nAdded %d transactions to %s" (length ts) (filepath $ rawledger l)

-- | Read a number of ledger transactions from the command line,
-- prompting, validating, displaying and appending them to the ledger
-- file, until EOF.
getAndAddTransactions :: Ledger -> [String] -> IO [LedgerTransaction]
getAndAddTransactions l args = (do
  t <- getTransaction l args >>= addTransaction l
  liftM (t:) (getAndAddTransactions l args)
 ) `catch` (\e -> if isEOFError e then return [] else ioError e)

-- | Read a transaction from the command line.
getTransaction :: Ledger -> [String] -> IO LedgerTransaction
getTransaction l args = do
  today <- getCurrentDay
  datestr <- askFor "date" (Just $ showDate today)
            (Just $ \s -> null s || 
             (isRight $ parse (smartdate >> many spacenonewline >> eof) "" $ lowercase s))
  let date = fixSmartDate today $ fromparse $ (parse smartdate "" . lowercase) datestr
  description <- if null args 
                then askFor "description" Nothing (Just $ not . null) 
                else (do
                       hPutStrLn stderr $ "description: " ++ unwords args
                       return $ unwords args)
  let historymatches = transactionsSimilarTo l description
  when (not $ null historymatches) (do
                     hPutStrLn stderr "Similar past transactions:"
                     hPutStr stderr $ concatMap (\(n,t) -> printf "[%3d%%] %s" (round $ n*100 :: Int) (show t)) $ take 3 historymatches)
  let bestmatch | null historymatches = Nothing
                | otherwise = Just $ snd $ head $ historymatches
  let bestmatchpostings = maybe Nothing (Just . ltpostings) bestmatch
  let getpostingsandvalidate = do
                     ps <- getPostings bestmatchpostings []
                     let t = nullledgertxn{ltdate=date
                                          ,ltstatus=False
                                          ,ltdescription=description
                                          ,ltpostings=ps
                                          }
                     either (const retry) (return) $ balanceLedgerTransaction t
      retry = do
        hPutStrLn stderr $ nonzerobalanceerror ++ ". Re-enter:"
        getpostingsandvalidate
  getpostingsandvalidate

-- | Read two or more postings from the command line.
getPostings :: Maybe [Posting] -> [Posting] -> IO [Posting]
getPostings bestmatchps enteredps = do
  account <- askFor (printf "account %d" n) defaultaccount validateaccount
  if null account
    then return enteredps
    else do
      amountstr <- askFor (printf "amount  %d" n) defaultamount validateamount
      let amount = fromparse $ parse (someamount <|> return missingamt) "" amountstr
      let p = nullrawposting{paccount=account,pamount=amount}
      if amount == missingamt
        then return $ enteredps ++ [p]
        else getPostings bestmatchps $ enteredps ++ [p]
    where
      n = length enteredps + 1
      bestmatch | isNothing bestmatchps = Nothing
                | n <= length ps = Just $ ps !! (n-1)
                | otherwise = Nothing
                where Just ps = bestmatchps
      defaultaccount = maybe Nothing (Just . paccount) bestmatch
      validateaccount = Just $ \s -> not $ null s && (length enteredps < 2)
      defaultamount = maybe Nothing (Just . show . pamount) bestmatch
      validateamount = Just $ \s -> 
                       (null s && (not $ null enteredps)) ||
                       (isRight $ parse (someamount>>many spacenonewline>>eof) "" s)


-- | Prompt and read a string value, possibly with a default and a validator.
-- A validator will cause the prompt to repeat until the input is valid.
askFor :: String -> Maybe String -> Maybe (String -> Bool) -> IO String
askFor prompt def validator = do
  hPutStr stderr $ prompt ++ (maybe "" showdef def) ++ ": "
  hFlush stderr
  l <- getLine
  let input = if null l then fromMaybe l def else l
  case validator of
    Just valid -> if valid input then return input else askFor prompt def validator
    Nothing -> return input
    where showdef s = " [" ++ s ++ "]"

-- | Append this transaction to the ledger's file.
addTransaction :: Ledger -> LedgerTransaction -> IO LedgerTransaction
addTransaction l t = do
  putStrLn =<< registerFromString (show t)
  appendToLedgerFile l $ show t
  return t

-- | Append data to the ledger's file, ensuring proper separation from any
-- existing data; or if the file is "-", dump it to stdout.
appendToLedgerFile :: Ledger -> String -> IO ()
appendToLedgerFile l s = 
    if f == "-"
    then putStr $ sep ++ s
    else appendFile f $ sep++s
    where 
      f = filepath $ rawledger l
      t = rawledgertext l
      sep | null $ strip t = ""
          | otherwise = replicate (2 - min 2 (length lastnls)) '\n'
          where lastnls = takeWhile (=='\n') $ reverse t

-- | Convert a string of ledger data into a register report.
registerFromString :: String -> IO String
registerFromString s = do
  now <- getCurrentLocalTime
  l <- ledgerFromStringWithOpts [] [] now s
  return $ showRegisterReport [] [] l

-- | Simon White's letter pairs algorithm from
-- http://www.catalysoft.com/articles/StrikeAMatch.html
compareStrings s t = 2.0 * (fromIntegral i) / (fromIntegral u)
    where
      pairs1 = wordLetterPairs $ uppercase s
      pairs2 = wordLetterPairs $ uppercase t
      u = length pairs1 + length pairs2
      i = length $ intersect pairs1 pairs2
wordLetterPairs = concatMap letterPairs . words
letterPairs (a:b:rest) = [a,b]:(letterPairs (b:rest))
letterPairs _ = []

compareLedgerDescriptions s t = compareStrings s' t'
    where s' = simplify s
          t' = simplify t
          simplify = filter (not . (`elem` "0123456789"))

transactionsSimilarTo :: Ledger -> String -> [(Float,LedgerTransaction)]
transactionsSimilarTo l s =
    sortBy compareRelevanceAndRecency
               $ filter ((/=0).fst)
               $ [(compareLedgerDescriptions s $ ltdescription t, t) | t <- ts]
               -- $ [(compareLedgerDescriptions s $ (strace $ unwords $ [ltdescription t] ++ (map (replace ":" " " . paccount) $ ltpostings t)), t) | t <- ts]
    where
      compareRelevanceAndRecency (n1,t1) (n2,t2) = compare (n2,ltdate t2) (n1,ltdate t1)
      ts = ledger_txns $ rawledger l

{- doctests

@
$ echo "2009/13/1"|hledger -f /dev/null add 2>&1|tail -1|sed -e's/\[[^]]*\]//g' # a bad date is not accepted
date : date : 
@

@
$ echo|hledger -f /dev/null add 2>&1|tail -1|sed -e's/\[[^]]*\]//g' # a blank date is ok
date : description: 
@

@
$ printf "\n\n"|hledger -f /dev/null add 2>&1|tail -1|sed -e's/\[[^]]*\]//g' # a blank description should fail
date : description: description: 
@

-}
