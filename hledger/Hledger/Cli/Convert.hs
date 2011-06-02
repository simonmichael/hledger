{-|
Convert account data in CSV format (eg downloaded from a bank) to journal
format, and print it on stdout. See the manual for more details.
-}

module Hledger.Cli.Convert where
import Control.Monad (when, guard, liftM)
import Data.Maybe
import Data.Time.Format (parseTime)
import Safe
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, replaceExtension)
import System.IO (stderr)
import System.Locale (defaultTimeLocale)
import Test.HUnit
import Text.CSV (parseCSV, parseCSVFromFile, printCSV, CSV)
import Text.ParserCombinators.Parsec
import Text.Printf (hPrintf)

import Prelude hiding (getContents)
import Hledger.Utils.UTF8 (getContents)
import Hledger
import Hledger.Cli.Format
import qualified Hledger.Cli.Format as Format
import Hledger.Cli.Version
import Hledger.Cli.Options

{- |
A set of data definitions and account-matching patterns sufficient to
convert a particular CSV data file into meaningful journal transactions. See above.
-}
data CsvRules = CsvRules {
      dateField :: Maybe FieldPosition,
      dateFormat :: Maybe String,
      statusField :: Maybe FieldPosition,
      codeField :: Maybe FieldPosition,
      descriptionField :: [FormatString],
      amountField :: Maybe FieldPosition,
      inField :: Maybe FieldPosition,
      outField :: Maybe FieldPosition,
      currencyField :: Maybe FieldPosition,
      baseCurrency :: Maybe String,
      accountField :: Maybe FieldPosition,
      account2Field :: Maybe FieldPosition,
      effectiveDateField :: Maybe FieldPosition,
      baseAccount :: AccountName,
      accountRules :: [AccountRule]
} deriving (Show, Eq)

nullrules = CsvRules {
      dateField=Nothing,
      dateFormat=Nothing,
      statusField=Nothing,
      codeField=Nothing,
      descriptionField=[],
      amountField=Nothing,
      inField=Nothing,
      outField=Nothing,
      currencyField=Nothing,
      baseCurrency=Nothing,
      accountField=Nothing,
      account2Field=Nothing,
      effectiveDateField=Nothing,
      baseAccount="unknown",
      accountRules=[]
}

type FieldPosition = Int

type AccountRule = (
   [(String, Maybe String)] -- list of regex match patterns with optional replacements
  ,AccountName              -- account name to use for a transaction matching this rule
  )

type CsvRecord = [String]


-- | Read the CSV file named as an argument and print equivalent journal transactions,
-- using/creating a .rules file.
convert :: CliOpts -> IO ()
convert opts = do
  let csvfile = case headDef "" $ patterns_ $ reportopts_ opts of
                  "" -> "-"
                  s -> s
      usingStdin = csvfile == "-"
      rulesFileSpecified = isJust $ rules_file_ opts
      rulesfile = rulesFileFor opts csvfile
  when (usingStdin && (not rulesFileSpecified)) $ error' "please specify a files file when converting stdin"
  csvparse <- parseCsv csvfile
  let records = case csvparse of
                  Left e -> error' $ show e
                  Right rs -> reverse $ filter (/= [""]) rs
  exists <- doesFileExist rulesfile
  if (not exists) then do
                  hPrintf stderr "creating conversion rules file %s, edit this file for better results\n" rulesfile
                  writeFile rulesfile initialRulesFileContent
   else
      hPrintf stderr "using conversion rules file %s\n" rulesfile
  rules <- liftM (either (error'.show) id) $ parseCsvRulesFile rulesfile
  let invalid = validateRules rules
  when (debug_ opts) $ hPrintf stderr "rules: %s\n" (show rules)
  when (isJust invalid) $ error (fromJust invalid)
  let requiredfields = max 2 (maxFieldIndex rules + 1)
      badrecords = take 1 $ filter ((< requiredfields).length) records
  if null badrecords
   then mapM_ (printTxn (debug_ opts) rules) records
   else do
     hPrintf stderr (unlines [
                      "Warning, at least one CSV record does not contain a field referenced by the"
                     ,"conversion rules file, or has less than two fields. Are you converting a"
                     ,"valid CSV file ? First bad record:\n%s"
                     ]) (show $ head badrecords)
     exitFailure

parseCsv :: FilePath -> IO (Either ParseError CSV)
parseCsv path =
  case path of
    "-" -> liftM (parseCSV "(stdin)") getContents
    p   -> parseCSVFromFile p

-- | The highest (0-based) field index referenced in the field
-- definitions, or -1 if no fields are defined.
maxFieldIndex :: CsvRules -> Int
maxFieldIndex r = maximumDef (-1) $ catMaybes [
                   dateField r
                  ,statusField r
                  ,codeField r
                  ,amountField r
                  ,inField r
                  ,outField r
                  ,currencyField r
                  ,accountField r
                  ,account2Field r
                  ,effectiveDateField r
                  ]

rulesFileFor :: CliOpts -> FilePath -> FilePath
rulesFileFor CliOpts{rules_file_=Just f} _ = f
rulesFileFor CliOpts{rules_file_=Nothing} csvfile = replaceExtension csvfile ".rules"

initialRulesFileContent :: String
initialRulesFileContent =
    "# csv conversion rules file generated by "++(progversionstr progname)++"\n" ++
    "# Add rules to this file for more accurate conversion, see\n"++
    "# http://hledger.org/MANUAL.html#convert\n" ++
    "\n" ++
    "base-account assets:bank:checking\n" ++
    "date-field 0\n" ++
    "description-field 4\n" ++
    "amount-field 1\n" ++
    "currency $\n" ++
    "\n" ++
    "# account-assigning rules\n" ++
    "\n" ++
    "SPECTRUM\n" ++
    "expenses:health:gym\n" ++
    "\n" ++
    "ITUNES\n" ++
    "BLKBSTR=BLOCKBUSTER\n" ++
    "expenses:entertainment\n" ++
    "\n" ++
    "(TO|FROM) SAVINGS\n" ++
    "assets:bank:savings\n"

validateRules :: CsvRules -> Maybe String
validateRules rules = let
    hasAccount = isJust $ accountField rules
    hasIn = isJust $ inField rules
    hasOut = isJust $ outField rules
  in case (hasAccount, hasIn, hasOut) of
    (True, True, _) -> Just "Don't specify in-field when specifying amount-field"
    (True, _, True) -> Just "Don't specify out-field when specifying amount-field"
    (_, False, True) -> Just "Please specify in-field when specifying out-field"
    (_, True, False) -> Just "Please specify out-field when specifying in-field"
    (False, False, False) -> Just "Please specify either amount-field, or in-field and out-field"
    _ -> Nothing

-- rules file parser

parseCsvRulesFile :: FilePath -> IO (Either ParseError CsvRules)
parseCsvRulesFile f = do
  s <- readFile f
  return $ parseCsvRules f s

parseCsvRules :: FilePath -> String -> Either ParseError CsvRules
parseCsvRules rulesfile s = runParser csvrulesfile nullrules{baseAccount=takeBaseName rulesfile} rulesfile s

csvrulesfile :: GenParser Char CsvRules CsvRules
csvrulesfile = do
  many blankorcommentline
  many definitions
  r <- getState
  ars <- many accountrule
  many blankorcommentline
  eof
  return r{accountRules=ars}

definitions :: GenParser Char CsvRules ()
definitions = do
  choice' [
    datefield
   ,dateformat
   ,statusfield
   ,codefield
   ,descriptionfield
   ,amountfield
   ,infield
   ,outfield
   ,currencyfield
   ,accountfield
   ,account2field
   ,effectivedatefield
   ,basecurrency
   ,baseaccount
   ,commentline
   ] <?> "definition"
  return ()

datefield = do
  string "date-field"
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{dateField=readMay v})

effectivedatefield = do
  string "effective-date-field"
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{effectiveDateField=readMay v})

dateformat = do
  string "date-format"
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{dateFormat=Just v})

codefield = do
  string "code-field"
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{codeField=readMay v})

statusfield = do
  string "status-field"
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{statusField=readMay v})

descriptionFieldValue :: GenParser Char st [FormatString]
descriptionFieldValue = do
--      try (fieldNo <* spacenonewline)
      try fieldNo
  <|> formatStrings
  where
    fieldNo = many1 digit >>= \x -> return [FormatField False Nothing Nothing $ FieldNo $ read x]

descriptionfield = do
  string "description-field"
  many1 spacenonewline
  formatS <- descriptionFieldValue
  restofline
  updateState (\x -> x{descriptionField=formatS})

amountfield = do
  string "amount-field"
  many1 spacenonewline
  v <- restofline
  x <- updateState (\r -> r{amountField=readMay v})
  return x

infield = do
  string "in-field"
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{inField=readMay v})

outfield = do
  string "out-field"
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{outField=readMay v})

currencyfield = do
  string "currency-field"
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{currencyField=readMay v})

accountfield = do
  string "account-field"
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{accountField=readMay v})

account2field = do
  string "account2-field"
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{account2Field=readMay v})

basecurrency = do
  string "currency"
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{baseCurrency=Just v})

baseaccount = do
  string "base-account"
  many1 spacenonewline
  v <- ledgeraccountname
  optional newline
  updateState (\r -> r{baseAccount=v})

accountrule :: GenParser Char CsvRules AccountRule
accountrule = do
  many blankorcommentline
  pats <- many1 matchreplacepattern
  guard $ length pats >= 2
  let pats' = init pats
      acct = either (fail.show) id $ runParser ledgeraccountname () "" $ fst $ last pats
  many blankorcommentline
  return (pats',acct)
 <?> "account rule"

blanklines = many1 blankline

blankline = many spacenonewline >> newline >> return () <?> "blank line"

commentchar = oneOf ";#"

commentline = many spacenonewline >> commentchar >> restofline >> return () <?> "comment line"

blankorcommentline = choice' [blankline, commentline]

matchreplacepattern = do
  notFollowedBy commentchar
  matchpat <- many1 (noneOf "=\n")
  replpat <- optionMaybe $ do {char '='; many $ noneOf "\n"}
  newline
  return (matchpat,replpat)

printTxn :: Bool -> CsvRules -> CsvRecord -> IO ()
printTxn debug rules rec = do
  when debug $ hPrintf stderr "record: %s" (printCSV [rec])
  putStr $ show $ transactionFromCsvRecord rules rec

-- csv record conversion
formatD :: CsvRecord -> Bool -> Maybe Int -> Maybe Int -> Field -> String
formatD record leftJustified min max f = case f of 
  FieldNo n       -> maybe "" show $ atMay record n
  -- Some of these might in theory in read from fields
  Format.Account  -> ""
  DepthSpacer     -> ""
  Total           -> ""
  DefaultDate     -> ""
  Description     -> ""
 where
   show = formatValue leftJustified min max

formatDescription :: CsvRecord -> [FormatString] -> String
formatDescription _ [] = ""
formatDescription record (f:fs) = s ++ (formatDescription record fs)
  where s = case f of
                FormatLiteral l -> l
                FormatField leftJustified min max field  -> formatD record leftJustified min max field

transactionFromCsvRecord :: CsvRules -> CsvRecord -> Transaction
transactionFromCsvRecord rules fields =
  let 
      date = parsedate $ normaliseDate (dateFormat rules) $ maybe "1900/1/1" (atDef "" fields) (dateField rules)
      effectivedate = do idx <- effectiveDateField rules
                         return $ parsedate $ normaliseDate (dateFormat rules) $ (atDef "" fields) idx
      status = maybe False (null . strip . (atDef "" fields)) (statusField rules)
      code = maybe "" (atDef "" fields) (codeField rules)
      desc = formatDescription fields (descriptionField rules)
      comment = ""
      precomment = ""
      baseacc = maybe (baseAccount rules) (atDef "" fields) (accountField rules)
      amountstr = getAmount rules fields
      amountstr' = strnegate amountstr where strnegate ('-':s) = s
                                             strnegate s = '-':s
      currency = maybe (fromMaybe "" $ baseCurrency rules) (atDef "" fields) (currencyField rules)
      amountstr'' = currency ++ amountstr'
      amountparse = runParser someamount nullctx "" amountstr''
      amount = either (const nullmixedamt) id amountparse
      -- Using costOfMixedAmount here to allow complex costs like "10 GBP @@ 15 USD".
      -- Aim is to have "10 GBP @@ 15 USD" applied to account "acct", but have "-15USD" applied to "baseacct"
      baseamount = costOfMixedAmount amount
      unknownacct | (readDef 0 amountstr' :: Double) < 0 = "income:unknown"
                  | otherwise = "expenses:unknown"
      (acct',newdesc) = identify (accountRules rules) unknownacct desc
      acct = maybe acct' (atDef "" fields) (account2Field rules)
      t = Transaction {
              tdate=date,
              teffectivedate=effectivedate,
              tstatus=status,
              tcode=code,
              tdescription=newdesc,
              tcomment=comment,
              tpreceding_comment_lines=precomment,
              tmetadata=[],
              tpostings=[
                   Posting {
                     pstatus=False,
                     paccount=acct,
                     pamount=amount,
                     pcomment="",
                     ptype=RegularPosting,
                     pmetadata=[],
                     ptransaction=Just t
                   },
                   Posting {
                     pstatus=False,
                     paccount=baseacc,
                     pamount=(-baseamount),
                     pcomment="",
                     ptype=RegularPosting,
                     pmetadata=[],
                     ptransaction=Just t
                   }
                  ]
            }
  in t

-- | Convert some date string with unknown format to YYYY/MM/DD.
normaliseDate :: Maybe String -- ^ User-supplied date format: this should be tried in preference to all others
              -> String -> String
normaliseDate mb_user_format s = maybe "0000/00/00" showDate $
              firstJust $
              (maybe id (\user_format -> (parseTime defaultTimeLocale user_format s :)) mb_user_format) $
              [parseTime defaultTimeLocale "%Y/%m/%e" s
               -- can't parse a month without leading 0, try adding one
              ,parseTime defaultTimeLocale "%Y/%m/%e" (take 5 s ++ "0" ++ drop 5 s)
              ,parseTime defaultTimeLocale "%Y-%m-%e" s
              ,parseTime defaultTimeLocale "%Y-%m-%e" (take 5 s ++ "0" ++ drop 5 s)
              ,parseTime defaultTimeLocale "%m/%e/%Y" s
              ,parseTime defaultTimeLocale "%m/%e/%Y" ('0':s)
              ,parseTime defaultTimeLocale "%m-%e-%Y" s
              ,parseTime defaultTimeLocale "%m-%e-%Y" ('0':s)
              ]

-- | Apply account matching rules to a transaction description to obtain
-- the most appropriate account and a new description.
identify :: [AccountRule] -> String -> String -> (String,String)
identify rules defacct desc | null matchingrules = (defacct,desc)
                            | otherwise = (acct,newdesc)
    where
      matchingrules = filter ismatch rules :: [AccountRule]
          where ismatch = any ((`regexMatchesCI` desc) . fst) . fst
      (prs,acct) = head matchingrules
      p_ms_r = filter (\(_,m,_) -> m) $ map (\(p,r) -> (p, p `regexMatchesCI` desc, r)) prs
      (p,_,r) = head p_ms_r
      newdesc = case r of Just repl -> regexReplaceCI p repl desc
                          Nothing   -> desc

caseinsensitive = ("(?i)"++)

getAmount :: CsvRules -> CsvRecord -> String
getAmount rules fields = case (accountField rules) of
  Just f  -> maybe "" (atDef "" fields) $ Just f
  Nothing ->
    case (c, d) of
      (x, "") -> x
      ("", x) -> "-"++x
      _ -> ""
    where
      c = maybe "" (atDef "" fields) (inField rules)
      d = maybe "" (atDef "" fields) (outField rules)

tests_Hledger_Cli_Convert = TestList (test_parser ++ test_description_parsing)

test_description_parsing = [
      "description-field 1" ~: assertParseDescription "description-field 1\n" [FormatField False Nothing Nothing (FieldNo 1)]
    , "description-field 1 " ~: assertParseDescription "description-field 1 \n" [FormatField False Nothing Nothing (FieldNo 1)]
    , "description-field %(1)" ~: assertParseDescription "description-field %(1)\n" [FormatField False Nothing Nothing (FieldNo 1)]
    , "description-field %(1)/$(2)" ~: assertParseDescription "description-field %(1)/%(2)\n" [
          FormatField False Nothing Nothing (FieldNo 1)
        , FormatLiteral "/"
        , FormatField False Nothing Nothing (FieldNo 2)
        ]
    ]
  where
    assertParseDescription string expected = do assertParseEqual (parseDescription string) (nullrules {descriptionField = expected})
    parseDescription :: String -> Either ParseError CsvRules
    parseDescription x = runParser descriptionfieldWrapper nullrules "(unknown)" x
    descriptionfieldWrapper :: GenParser Char CsvRules CsvRules
    descriptionfieldWrapper = do
      descriptionfield
      r <- getState
      return r

test_parser =  [

   "convert rules parsing: empty file" ~: do
     -- let assertMixedAmountParse parseresult mixedamount =
     --         (either (const "parse error") showMixedAmountDebug parseresult) ~?= (showMixedAmountDebug mixedamount)
    assertParseEqual (parseCsvRules "unknown" "") nullrules

  ,"convert rules parsing: accountrule" ~: do
     assertParseEqual (parseWithCtx nullrules accountrule "A\na\n") -- leading blank line required
                 ([("A",Nothing)], "a")

  ,"convert rules parsing: trailing comments" ~: do
     assertParse (parseWithCtx nullrules csvrulesfile "A\na\n# \n#\n")

  ,"convert rules parsing: trailing blank lines" ~: do
     assertParse (parseWithCtx nullrules csvrulesfile "A\na\n\n  \n")

  -- not supported
  -- ,"convert rules parsing: no final newline" ~: do
  --    assertParse (parseWithCtx nullrules csvrulesfile "A\na")
  --    assertParse (parseWithCtx nullrules csvrulesfile "A\na\n# \n#")
  --    assertParse (parseWithCtx nullrules csvrulesfile "A\na\n\n  ")

                 -- (nullrules{
                 --   -- dateField=Maybe FieldPosition,
                 --   -- statusField=Maybe FieldPosition,
                 --   -- codeField=Maybe FieldPosition,
                 --   -- descriptionField=Maybe FieldPosition,
                 --   -- amountField=Maybe FieldPosition,
                 --   -- currencyField=Maybe FieldPosition,
                 --   -- baseCurrency=Maybe String,
                 --   -- baseAccount=AccountName,
                 --   accountRules=[
                 --        ([("A",Nothing)], "a")
                 --       ]
                 --  })

  ]
