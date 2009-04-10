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

module ConvertCommand where
import Data.Maybe (isJust)
import Data.List.Split (splitOn)
import Options -- (Opt,Debug)
import Ledger.Types (Ledger)
import Ledger.Utils (strip)
import System (getArgs)
import System.IO (stderr, hPutStrLn)
import Text.CSV (parseCSVFromFile, Record)
import Text.Printf (printf)
import Text.Regex.PCRE ((=~))
import Data.Maybe
import Ledger.Dates (firstJust, showDate)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (parseTime)
import Control.Monad (when)


convert :: [Opt] -> [String] -> Ledger -> IO ()
convert opts args l = do
  when (length args /= 3) (error "please specify a csv file, base account, and import rules file.")
  let [csvfile,baseacct,rulesfile] = args
  rulesstr <- readFile rulesfile
  (fieldpositions,rules) <- parseRules rulesstr
  parse <- parseCSVFromFile csvfile
  let records = case parse of
                  Left e -> error $ show e
                  Right rs -> reverse rs
  mapM_ (print_ledger_txn (Debug `elem` opts) (baseacct,fieldpositions,rules)) records


type Rule = (String             -- account name to use
            ,[[String]])        -- list of pattern+replacements. The second replacement item may or may not be present.

parseRules :: String -> IO ([Int],[Rule])
parseRules s = do
  let ls = map strip $ lines s
  let paras = splitOn [""] ls
  let fieldpositions = map read $ splitOn "," $ head $ head paras
  let rules = [(last p,map (splitOn "=") $ init p) | p <- tail paras]
  return (fieldpositions,rules)

print_ledger_txn debug (baseacct,fieldpositions,rules) record@(a:b:c:d:e) = do
  let [date,cleared,number,description,amount] = map (record !!) fieldpositions
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

choose_acct_desc rules (acct,desc) | null matches = (acct,desc)
                                   | otherwise = (a,d)
    where
      matches = filter (any (desc =~) . map head . snd) rules
      (a,pats) = head matches :: Rule
      ((before,match,after,groups),repl) = head $ filter isMatch $ map (\(pat:repl) -> (desc=~pat,repl)) pats
      d = head $ repl ++ [match]  -- show the replacement text if any, or the matched text

isMatch :: ((String, String, String, [String]),[String]) -> Bool
isMatch ((_,m,_,_),_) = not $ null m

fixdate :: String -> String
fixdate s = maybe "0000/00/00" showDate $ 
              firstJust
              [parseTime defaultTimeLocale "%Y/%m/%d" s
              ,parseTime defaultTimeLocale "%Y-%m-%d" s
              ,parseTime defaultTimeLocale "%m/%d/%Y" s
              ,parseTime defaultTimeLocale "%m-%d-%Y" s
              ]
