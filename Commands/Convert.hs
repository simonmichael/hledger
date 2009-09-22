{-|

Convert account data in CSV format (eg downloaded from a bank) to ledger
format, and print it on stdout.

Usage: hledger convert CSVFILE ACCOUNTNAME RULESFILE

ACCOUNTNAME is the base account to use for transactions.  RULESFILE
provides some rules to help convert the data. It should contain paragraphs
separated by one blank line.  The first paragraph is a single line of five
comma-separated numbers, which are the csv field positions corresponding
to the ledger transaction's date, status, code, description, and amount.
All other paragraphs specify one or more regular expressions, followed by
the ledger account to use when a transaction's description matches any of
them. A regexp may optionally have a replacement pattern specified after =.
Here's an example rules file:

> 0,2,3,4,1
>
> ATM DEPOSIT
> assets:bank:checking
>
> (TO|FROM) SAVINGS
> assets:bank:savings
>
> ITUNES
> BLKBSTR=BLOCKBUSTER
> expenses:entertainment

Roadmap: 
Support for other formats will be added. To update a ledger file, pipe the
output into the import command. The rules will move to a hledger config
file. When no rule matches, accounts will be guessed based on similarity
to descriptions in the current ledger, with interactive prompting and
optional rule saving.

-}

module Commands.Convert where
import Data.List.Split (splitOn)
import Options -- (Opt,Debug)
import Ledger.Types (Ledger,AccountName)
import Ledger.Utils (strip)
import System.IO (stderr, hPutStrLn)
import Text.CSV (parseCSVFromFile)
import Text.Printf (printf)
import Text.RegexPR (matchRegexPR)
import Data.Maybe
import Ledger.Dates (firstJust, showDate)
import Locale (defaultTimeLocale)
import Data.Time.Format (parseTime)
import Control.Monad (when)


convert :: [Opt] -> [String] -> Ledger -> IO ()
convert opts args _ = do
  when (length args /= 3) (error "please specify a csv file, base account, and import rules file.")
  let [csvfile,baseacct,rulesfile] = args
  rulesstr <- readFile rulesfile
  (fieldpositions,rules) <- parseRules rulesstr
  parse <- parseCSVFromFile csvfile
  let records = case parse of
                  Left e -> error $ show e
                  Right rs -> reverse rs
  mapM_ (print_ledger_txn (Debug `elem` opts) (baseacct,fieldpositions,rules)) records


type Rule = (
   [(String, Maybe String)] -- list of patterns and optional replacements
  ,AccountName              -- account name to use for a matched transaction
  )

parseRules :: String -> IO ([Int],[Rule])
parseRules s = do
  let ls = map strip $ lines s
  let paras = splitOn [""] ls
  let fieldpositions = map read $ splitOn "," $ head $ head paras
  let rules = [(map parsePatRepl $ init ls, last ls) | ls <- tail paras]
  return (fieldpositions,rules)

parsePatRepl :: String -> (String, Maybe String)
parsePatRepl l = case splitOn "=" l of
                   (p:r:_) -> (p, Just r)
                   _       -> (l, Nothing)

print_ledger_txn :: Bool -> (String,[Int],[Rule]) -> [String] -> IO ()
print_ledger_txn debug (baseacct,fieldpositions,rules) record@(_:_:_:_:_:[]) = do
  let [date,_,number,description,amount] = map (record !!) fieldpositions
      amount' = strnegate amount where strnegate ('-':s) = s
                                       strnegate s = '-':s
      unknownacct | (read amount' :: Double) < 0 = "income:unknown"
                  | otherwise = "expenses:unknown"
      (acct,desc) = choose_acct_desc rules (unknownacct,description)
  when (debug) $ hPutStrLn stderr $ printf "using %s for %s" desc description
  putStrLn $ printf "%s%s %s" (fixdate date) (if not (null number) then printf " (%s)" number else "") desc
  putStrLn $ printf "    %-30s  %15s" acct (printf "$%s" amount' :: String)
  putStrLn $ printf "    %s\n" baseacct
print_ledger_txn True _ record = do
  hPutStrLn stderr $ printf "ignoring %s" $ show record
print_ledger_txn _ _ _ = return ()

choose_acct_desc :: [Rule] -> (String,String) -> (String,String)
choose_acct_desc rules (acct,desc) | null matchingrules = (acct,desc)
                                   | otherwise = (a,d)
    where
      matchingrules = filter ismatch rules :: [Rule]
          where ismatch = any (isJust . flip matchregex desc . fst) . fst
      (prs,a) = head matchingrules
      mrs = filter (isJust . fst) $ map (\(p,r) -> (matchregex p desc, r)) prs
      (m,repl) = head mrs
      matched = fst $ fst $ fromJust m
      d = fromMaybe matched repl

matchregex = matchRegexPR . ("(?i)" ++)

fixdate :: String -> String
fixdate s = maybe "0000/00/00" showDate $ 
              firstJust
              [parseTime defaultTimeLocale "%Y/%m/%d" s
              ,parseTime defaultTimeLocale "%Y-%m-%d" s
              ,parseTime defaultTimeLocale "%m/%d/%Y" s
              ,parseTime defaultTimeLocale "%m-%d-%Y" s
              ]

