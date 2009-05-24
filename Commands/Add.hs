{-| 

An add command to help with data entry.

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
add opts args l
    | filepath (rawledger l) == "-" = return ()
    | otherwise = do
  hPutStrLn stderr
    "Enter one or more transactions, which will be added to your ledger file.\n\
    \To complete a transaction, enter . as account name.\n\
    \To finish input, enter control-d (discards any transaction in progress)."
  getAndAddTransactions l args
  return ()

-- | Read a number of ledger transactions from the command line,
-- prompting, validating, displaying and appending them to the ledger
-- file, until end of input. Any command-line arguments are used as the
-- first transaction's description.
getAndAddTransactions :: Ledger -> [String] -> IO [LedgerTransaction]
getAndAddTransactions l args = do
  -- for now, thread the eoi flag throughout rather than muck about with monads
  (t, eoi) <- getTransaction l args
  l <- if isJust t then addTransaction l (fromJust t) else return l
  if eoi then return $ maybe [] (:[]) t
         else liftM (fromJust t:) (getAndAddTransactions l [])

-- | Get a transaction from the command line, if possible, and a flag
-- indicating end of input.
getTransaction :: Ledger -> [String] -> IO (Maybe LedgerTransaction, Bool)
getTransaction l args = do
  today <- getCurrentDay
  (datestr, eoi) <- askFor "date" 
                        (Just $ showDate today)
                        (Just $ \s -> null s || 
                          (isRight $ parse (smartdate >> many spacenonewline >> eof) "" $ lowercase s))
  if eoi
    then return (Nothing, True)
    else do
      (description, eoi) <- if null args 
                           then askFor "description" Nothing (Just $ not . null) 
                           else (do
                                  let description = unwords args
                                  hPutStrLn stderr $ "description: " ++ description
                                  return (description, False))
      if eoi
        then return (Nothing, True)
        else do
          let historymatches = transactionsSimilarTo l description
          when (not $ null historymatches) (do
                             hPutStrLn stderr "Similar past transactions found:"
                             hPutStr stderr $ concatMap (\(n,t) -> printf "[%3d%%] %s" (round $ n*100 :: Int) (show t)) $ take 3 historymatches)
          let bestmatch | null historymatches = Nothing
                        | otherwise = Just $ snd $ head $ historymatches
              bestmatchpostings = maybe Nothing (Just . ltpostings) bestmatch
              date = fixSmartDate today $ fromparse $ (parse smartdate "" . lowercase) datestr
              getpostingsandvalidate = do
                             (ps, eoi) <- getPostings bestmatchpostings []
                             let t = nullledgertxn{ltdate=date
                                                  ,ltstatus=False
                                                  ,ltdescription=description
                                                  ,ltpostings=ps
                                                  }
                             if eoi && null ps
                               then return (Nothing, eoi)
                               else either (const retry) (return . flip (,) eoi . Just) $ balanceLedgerTransaction t
              retry = do
                hPutStrLn stderr $ "\n" ++ nonzerobalanceerror ++ ". Re-enter:"
                getpostingsandvalidate
          getpostingsandvalidate

-- | Get two or more postings from the command line, if possible, and a
-- flag indicating end of input.
getPostings :: Maybe [Posting] -> [Posting] -> IO ([Posting], Bool)
getPostings bestmatchps enteredps = do
  (account, eoi) <- askFor (printf "account %d" n) defaultaccount validateaccount
  if account=="." || eoi
    then return (enteredps, eoi)
    else do
      (amountstr, eoi) <- askFor (printf "amount  %d" n) defaultamount validateamount
      let amount = fromparse $ parse (someamount <|> return missingamt) "" amountstr
      let p = nullrawposting{paccount=stripbrackets account,pamount=amount,ptype=postingaccounttype account}
      if eoi
        then if null enteredps
               then return ([], True)
               else return (enteredps ++ [p], True)
        else if amount == missingamt
               then return $ (enteredps ++ [p], eoi)
               else getPostings bestmatchps $ enteredps ++ [p]
    where
      n = length enteredps + 1
      realn = length enteredrealps + 1
      bestmatch | isNothing bestmatchps = Nothing
                | n <= length ps = Just $ ps !! (n-1)
                | otherwise = Nothing
                where Just ps = bestmatchps
      defaultaccount = maybe Nothing (Just . showacctname) bestmatch
      showacctname p = showAccountName Nothing (ptype p) $ paccount p
      validateaccount = Just $ \s -> not $ null s
      defaultamount = maybe balancingamount (Just . show . pamount) bestmatch
          where balancingamount = Just $ show $ negate $ sum $ map pamount enteredrealps
      enteredrealps = filter isReal enteredps
      postingaccounttype ('[':_) = BalancedVirtualPosting
      postingaccounttype ('(':_) = VirtualPosting
      postingaccounttype _ = RegularPosting
      stripbrackets = dropWhile (`elem` "([") . reverse . dropWhile (`elem` "])") . reverse
      validateamount = Just $ \s -> 
                       (null s && (not $ null enteredps)) ||
                       (isRight $ parse (someamount>>many spacenonewline>>eof) "" s)


-- | Prompt for and read a string value and a flag indicating whether
-- input has ended (control-d was pressed), optionally with a default
-- value and a validator.  A validator will cause the prompt to repeat
-- until the input is valid (unless the input is just ctrl-d).
askFor :: String -> Maybe String -> Maybe (String -> Bool) -> IO (String, Bool)
askFor prompt def validator = do
  hPutStr stderr $ prompt ++ (maybe "" showdef def) ++ ": "
  hFlush stderr
  -- ugly
  l <- getLine `catch` (\e -> if isEOFError e then return "*EOF*" else ioError e)
  let (l', eoi) = case l of "*EOF*" -> ("", True)
                            _       -> (l, False)
  let input = if null l' then fromMaybe l' def else l'
  case validator of
    Just valid -> if valid input || (null input && eoi)
                   then return (input, eoi) 
                   else askFor prompt def validator
    Nothing -> return (input, eoi)
    where showdef s = " [" ++ s ++ "]"

-- | Append this transaction to the ledger's file. Also, to the ledger's
-- transaction list, but we don't bother updating the other fields - this
-- is enough to include new transactions in the history matching.
addTransaction :: Ledger -> LedgerTransaction -> IO Ledger
addTransaction l t = do
  appendToLedgerFile l $ show t
  putStrLn $ printf "\nAdded transaction to %s:" (filepath $ rawledger l)
  putStrLn =<< registerFromString (show t)
  return l{rawledger=rl{ledger_txns=ts}}
      where rl = rawledger l
            ts = ledger_txns rl ++ [t]

-- | Append data to the ledger's file, ensuring proper separation from any
-- existing data; or if the file is "-", dump it to stdout.
appendToLedgerFile :: Ledger -> String -> IO ()
appendToLedgerFile l s = 
    if f == "-"
    then putStr $ sep ++ s
    else appendFile f $ sep++s
    where 
      f = filepath $ rawledger l
      -- we keep looking at the original raw text from when the ledger
      -- was first read, but that's good enough for now
      t = rawledgertext l
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
compareStrings :: String -> String -> Float
compareStrings "" "" = 1
compareStrings (a:[]) "" = 0
compareStrings "" (b:[]) = 0
compareStrings (a:[]) (b:[]) = if toUpper a == toUpper b then 1 else 0
compareStrings s1 s2 = 2.0 * (fromIntegral i) / (fromIntegral u)
    where
      i = length $ intersect pairs1 pairs2
      u = length pairs1 + length pairs2
      pairs1 = wordLetterPairs $ uppercase s1
      pairs2 = wordLetterPairs $ uppercase s2
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
               $ filter ((> threshold).fst)
               $ [(compareLedgerDescriptions s $ ltdescription t, t) | t <- ts]
    where
      compareRelevanceAndRecency (n1,t1) (n2,t2) = compare (n2,ltdate t2) (n1,ltdate t1)
      ts = ledger_txns $ rawledger l
      threshold = 0

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
