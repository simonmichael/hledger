{-| 

A simple add command to help with data entry.

-}

module AddCommand
where
import Ledger
import Options
import RegisterCommand (showRegisterReport)
import System.IO
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
  hPutStrLn stderr ("Please enter one or more transactions, which will be added to your ledger file.\n\
                    \A blank account or amount ends a transaction, control-d to finish.")
  ts <- getAndAddTransactions l
  putStrLn $ printf "\nAdded %d transactions to %s ." (length ts) (filepath $ rawledger l)

-- | Read a number of ledger transactions from the command line,
-- prompting, validating, displaying and appending them to the ledger file.
getAndAddTransactions :: Ledger -> IO [LedgerTransaction]
getAndAddTransactions l = (do
  today <- getCurrentDay
  date <- liftM (fixSmartDate today . fromparse . parse smartdate "" . lowercase)
         $ askFor "date" (Just $ showDate today)
  description <- askFor "description" Nothing
  ps <- getPostings []
  let t = nullledgertxn{ltdate=date
                       ,ltstatus=False
                       ,ltdescription=description
                       ,ltpostings=ps
                       }
  registerFromString (show t) >>= putStrLn
  appendToLedgerFile l $ show t
  liftM (t:) (getAndAddTransactions l)
                          ) `catch` (\e -> if isEOFError e then return [] else ioError e)

-- | Read one or more postings interactively.
getPostings :: [Posting] -> IO [Posting]
getPostings prevps = do
  account <- askFor "account" Nothing
  if null account
    then return prevps
    else do
      amount <- liftM (fromparse . parse (someamount <|> return missingamt) "")
               $ askFor "amount" Nothing
      let p = nullrawposting{paccount=account,pamount=amount}
      if amount == missingamt
        then return $ prevps ++ [p]
        else getPostings $ prevps ++ [p]

-- | Prompt and read a string value, possibly with a default.
askFor :: String -> Maybe String -> IO String
askFor prompt def = do
  hPutStr stderr $ prompt ++ (maybe "" showdef def) ++ ": "
  hFlush stderr
  l <- getLine
  return $ if null l then fromMaybe l def else l
    where showdef s = " [" ++ s ++ "]"

-- | Append a string of transactions to the ledger's file, ensuring proper
-- separation from the existing data; or if the file is "-", print them
-- to stdout.
appendToLedgerFile :: Ledger -> String -> IO ()
appendToLedgerFile l s = 
    if f == "-"
    then putStr $ sep ++ s
    else appendFile f $ sep++s
    where 
      f = filepath $ rawledger l
      t = rawledgertext l
      sep = replicate (2 - min 2 (length nls)) '\n' where nls = takeWhile (=='\n') $ reverse t

-- | Convert a string of ledger data into a register report.
registerFromString :: String -> IO String
registerFromString s = do
  now <- getCurrentLocalTime
  l <- ledgerFromStringWithOpts [] [] now s
  return $ showRegisterReport [] [] l

