{-|

Convert account data in CSV format (eg downloaded from a bank) to ledger
format, and print it on stdout. See the manual for more details.

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
import Safe (readMay, readDef)


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

parseRules :: String -> IO ([Maybe Int],[Rule])
parseRules s = do
  let ls = map strip $ lines s
  let paras = splitOn [""] ls
  let fieldpositions = map readMay $ splitOn "," $ head $ head paras
  let rules = [(map parsePatRepl $ init ls, last ls) | ls <- tail paras]
  return (fieldpositions,rules)

parsePatRepl :: String -> (String, Maybe String)
parsePatRepl l = case splitOn "=" l of
                   (p:r:_) -> (p, Just r)
                   _       -> (l, Nothing)

print_ledger_txn :: Bool -> (String,[Maybe Int],[Rule]) -> [String] -> IO ()
print_ledger_txn _ (_,[],_) _ = return ()
print_ledger_txn _ (('#':_),_,_) _ = return ()
print_ledger_txn debug (baseacct,fieldpositions,rules) csvrecord
    | length csvrecord < maximum (map (fromMaybe 0) fieldpositions) + 1 = return ()
    | otherwise =
 do
  when debug $ hPutStrLn stderr $ show csvrecord
  let date = maybe "" (csvrecord !!) (fieldpositions !! 0)
      number = maybe "" (csvrecord !!) (fieldpositions !! 1)
      description = maybe "" (csvrecord !!) (fieldpositions !! 2)
      amount = maybe "" (csvrecord !!) (fieldpositions !! 3)
      amount' = strnegate amount where strnegate ('-':s) = s
                                       strnegate s = '-':s
      unknownacct | (readDef 0 amount' :: Double) < 0 = "income:unknown"
                  | otherwise = "expenses:unknown"
      (acct,desc) = choose_acct_desc rules (unknownacct,description)
  when debug $ hPutStrLn stderr $ printf "using %s for %s" desc description
  printf "%s%s %s\n" (fixdate date) (if not (null number) then printf " (%s)" number else "") desc
  printf "    %-30s  %15s\n" acct (printf "$%s" amount' :: String)
  printf "    %s\n\n" baseacct

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
              [parseTime defaultTimeLocale "%Y/%m/%e" s
               -- can't parse a month without leading 0, try adding onee
              ,parseTime defaultTimeLocale "%Y/%m/%e" (take 5 s ++ "0" ++ drop 5 s)
              ,parseTime defaultTimeLocale "%Y-%m-%e" s
              ,parseTime defaultTimeLocale "%Y-%m-%e" (take 5 s ++ "0" ++ drop 5 s)
              ,parseTime defaultTimeLocale "%m/%e/%Y" s
              ,parseTime defaultTimeLocale "%m/%e/%Y" ('0':s)
              ,parseTime defaultTimeLocale "%m-%e-%Y" s
              ,parseTime defaultTimeLocale "%m-%e-%Y" ('0':s)
              ]

