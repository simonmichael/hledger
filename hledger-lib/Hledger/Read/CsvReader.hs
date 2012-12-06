{-|

A reader for the CSV data format. Uses an extra rules file
(<http://hledger.org/MANUAL.html#rules-file-directives>) to help interpret
the data. Example:

@
\"2012\/3\/22\",\"something\",\"10.00\"
\"2012\/3\/23\",\"another\",\"5.50\"
@

and rules file:

@
date-field 0
description-field 1
amount-field 2
base-account assets:bank:checking

SAVINGS
assets:bank:savings
@

-}

module Hledger.Read.CsvReader (
  -- * Reader
  reader,
  -- * Misc.
  CsvRecord,
  nullrules,
  rulesFileFor,
  parseCsvRulesFile,
  transactionFromCsvRecord,
  -- * Tests
  tests_Hledger_Read_CsvReader
)
where
import Control.Exception hiding (try)
import Control.Monad
import Control.Monad.Error
-- import Test.HUnit
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time.Format (parseTime)
import Safe
import System.Directory (doesFileExist)
import System.FilePath
import System.IO (stderr)
import System.Locale (defaultTimeLocale)
import Test.HUnit
import Text.CSV (parseCSV, CSV)
import Text.ParserCombinators.Parsec  hiding (parse)
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos
import Text.Printf (hPrintf)

import Hledger.Data
import Prelude hiding (getContents)
import Hledger.Utils.UTF8IOCompat (getContents)
import Hledger.Utils
import Hledger.Data.FormatStrings as FormatStrings
import Hledger.Read.JournalReader (accountname, amountp)


reader :: Reader
reader = Reader format detect parse

format :: String
format = "csv"

-- | Does the given file path and data look like CSV ?
detect :: FilePath -> String -> Bool
detect f _ = takeExtension f == '.':format

-- | Parse and post-process a "Journal" from CSV data, or give an error.
-- XXX currently ignores the string and reads from the file path
parse :: Maybe FilePath -> FilePath -> String -> ErrorT String IO Journal
parse rulesfile f s = -- trace ("running "++format++" reader") $
 do
  r <- liftIO $ readJournalFromCsv rulesfile f s
  case r of Left e -> throwError e
            Right j -> return j

nullrules = CsvRules {
      dateField=Nothing,
      dateFormat=Nothing,
      statusField=Nothing,
      codeField=Nothing,
      descriptionField=[],
      amountField=Nothing,
      amountInField=Nothing,
      amountOutField=Nothing,
      currencyField=Nothing,
      baseCurrency=Nothing,
      accountField=Nothing,
      account2Field=Nothing,
      effectiveDateField=Nothing,
      baseAccount="unknown",
      accountRules=[],
      skipLines=0
}

type CsvRecord = [String]


-- | Read a Journal from the given CSV data (and filename, used for error
-- messages), or return an error. Proceed as follows:
-- @
-- 1. parse the CSV data
-- 2. identify the name of a file specifying conversion rules: either use
-- the name provided, derive it from the CSV filename, or raise an error
-- if the CSV filename is -.
-- 3. auto-create the rules file with default rules if it doesn't exist
-- 4. parse the rules file
-- 5. convert the CSV records to a journal using the rules
-- @
readJournalFromCsv :: Maybe FilePath -> FilePath -> String -> IO (Either String Journal)
readJournalFromCsv Nothing "-" _ = return $ Left "please use --rules-file when converting stdin"
readJournalFromCsv mrulesfile csvfile csvdata =
 handle (\e -> return $ Left $ show (e :: IOException)) $ do
  csvparse <- parseCsv csvfile csvdata
  let rs = case csvparse of
                  Left e -> throw $ userError $ show e
                  Right rs -> filter (/= [""]) rs
      badrecords = take 1 $ filter ((< 2).length) rs
      records = case badrecords of
                 []    -> rs
                 (_:_) -> throw $ userError $ "Parse error: at least one CSV record has less than two fields:\n"++(show $ head badrecords)

  let rulesfile = fromMaybe (rulesFileFor csvfile) mrulesfile
  created <- records `seq` ensureRulesFileExists rulesfile
  if created
   then hPrintf stderr "creating default conversion rules file %s, edit this file for better results\n" rulesfile
   else hPrintf stderr "using conversion rules file %s\n" rulesfile

  rules <- liftM (either (throw.userError.show) id) $ parseCsvRulesFile rulesfile

  let requiredfields = (maxFieldIndex rules + 1)
      realrecords = drop (skipLines rules) records
      badrecords = take 1 $ filter ((< requiredfields).length) realrecords
  return $ case badrecords of
            []    -> Right nulljournal{jtxns=sortBy (comparing tdate) $ map (transactionFromCsvRecord rules) realrecords}
            (_:_) -> Left $ "Parse error: at least one CSV record does not contain a field referenced by the conversion rules file:\n"++(show $ head badrecords)

-- | Ensure there is a conversion rules file at the given path, creating a
-- default one if needed and returning True in this case.
ensureRulesFileExists :: FilePath -> IO Bool
ensureRulesFileExists f = do
  exists <- doesFileExist f
  if exists
   then return False
   else do
     -- note Hledger.Utils.UTF8.* do no line ending conversion on windows,
     -- we currently require unix line endings on all platforms.
     writeFile f newRulesFileContent
     return True

parseCsv :: FilePath -> String -> IO (Either ParseError CSV)
parseCsv path csvdata =
  case path of
    "-" -> liftM (parseCSV "(stdin)") getContents
    _   -> return $ parseCSV path csvdata

-- | The highest (0-based) field index referenced in the field
-- definitions, or -1 if no fields are defined.
maxFieldIndex :: CsvRules -> Int
maxFieldIndex r = maximumDef (-1) $ catMaybes [
                   dateField r
                  ,statusField r
                  ,codeField r
                  ,amountField r
                  ,amountInField r
                  ,amountOutField r
                  ,currencyField r
                  ,accountField r
                  ,account2Field r
                  ,effectiveDateField r
                  ]

-- rulesFileFor :: CliOpts -> FilePath -> FilePath
-- rulesFileFor CliOpts{rules_file_=Just f} _ = f
-- rulesFileFor CliOpts{rules_file_=Nothing} csvfile = replaceExtension csvfile ".rules"
rulesFileFor :: FilePath -> FilePath
rulesFileFor = (++ ".rules")

newRulesFileContent :: String
newRulesFileContent = let prognameandversion = "hledger" in
    "# csv conversion rules file generated by " ++ prognameandversion ++ "\n" ++
    "# Add rules to this file for more accurate conversion, see\n"++
    "# http://hledger.org/MANUAL.html#convert\n" ++
    "\n" ++
    "skip-lines 0\n" ++
    "base-account assets:bank:checking\n" ++
    "date-field 0\n" ++
    "description-field 4\n" ++
    "amount-field 1\n" ++
    "base-currency $\n" ++
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

-- rules file parser

parseCsvRulesFile :: FilePath -> IO (Either ParseError CsvRules)
parseCsvRulesFile f = do
  s <- readFile f
  let rules = parseCsvRules f s
  return $ case rules of
             Left e -> Left e
             Right r -> case validateRules r of
                          Left e -> Left $ toParseError e
                          Right r -> Right r
  where
    toParseError s = newErrorMessage (Message s) (initialPos "")

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
   ,amountinfield
   ,amountoutfield
   ,currencyfield
   ,accountfield
   ,account2field
   ,effectivedatefield
   ,basecurrency
   ,baseaccount
   ,skiplines
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

amountinfield = do
  choice [string "amount-in-field", string "in-field"]
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{amountInField=readMay v})

amountoutfield = do
  choice [string "amount-out-field", string "out-field"]
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{amountOutField=readMay v})

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
  choice [string "base-currency", string "currency"]
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{baseCurrency=Just v})

baseaccount = do
  string "base-account"
  many1 spacenonewline
  v <- accountname
  optional newline
  updateState (\r -> r{baseAccount=v})

skiplines = do
  string "skip-lines"
  many1 spacenonewline
  v <- restofline
  updateState (\r -> r{skipLines=read v})

accountrule :: GenParser Char CsvRules AccountRule
accountrule = do
  many blankorcommentline
  pats <- many1 matchreplacepattern
  guard $ length pats >= 2
  let pats' = init pats
      acct = either (fail.show) id $ runParser accountname () "" $ fst $ last pats
  many blankorcommentline
  return (pats',acct)
 <?> "account rule"

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

validateRules :: CsvRules -> Either String CsvRules
validateRules rules =
 let hasAmount = isJust $ amountField rules
     hasIn = isJust $ amountInField rules
     hasOut = isJust $ amountOutField rules
 in case (hasAmount, hasIn, hasOut) of
    (True, True, _) -> Left "Don't specify amount-in-field when specifying amount-field"
    (True, _, True) -> Left "Don't specify amount-out-field when specifying amount-field"
    (_, False, True) -> Left "Please specify amount-in-field when specifying amount-out-field"
    (_, True, False) -> Left "Please specify amount-out-field when specifying amount-in-field"
    (False, False, False) -> Left "Please specify either amount-field, or amount-in-field and amount-out-field"
    _ -> Right rules

-- csv record conversion
formatD :: CsvRecord -> Bool -> Maybe Int -> Maybe Int -> HledgerFormatField -> String
formatD record leftJustified min max f = case f of 
  FieldNo n       -> maybe "" show $ atMay record n
  -- Some of these might in theory in read from fields
  AccountField         -> ""
  DepthSpacerField     -> ""
  TotalField           -> ""
  DefaultDateField     -> ""
  DescriptionField     -> ""
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
      -- "negate" an amount string. An amount beginning with - or enclosed in parentheses is negative.
      amountstr' = strnegate amountstr where strnegate ('(':s) | not (null s) && last s == ')' = init s
                                             strnegate ('-':s) = s
                                             strnegate s = '-':s
      currency = maybe (fromMaybe "" $ baseCurrency rules) (atDef "" fields) (currencyField rules)
      amountstr'' = currency ++ amountstr'
      amountparse = runParser amountp nullctx "" amountstr''
      a = either (const nullmixedamt) mixed amountparse
      -- Using costOfMixedAmount here to allow complex costs like "10 GBP @@ 15 USD".
      -- Aim is to have "10 GBP @@ 15 USD" applied to account "acct", but have "-15USD" applied to "baseacct"
      baseamount = costOfMixedAmount a
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
              ttags=[],
              tpostings=
                  [posting {paccount=acct, pamount=a, ptransaction=Just t}
                  ,posting {paccount=baseacc, pamount=(-baseamount), ptransaction=Just t}
                  ]
            }
  in t

-- | Convert some date string with unknown format to YYYY/MM/DD.
normaliseDate :: Maybe String -- ^ User-supplied date format: this should be tried in preference to all others
              -> String -> String
normaliseDate mb_user_format s =
    let parsewith = flip (parseTime defaultTimeLocale) s in
    maybe (error' $ "could not parse \""++s++"\" as a date, consider adding a date-format directive or upgrading")
          showDate $
          firstJust $ (map parsewith $
                       maybe [] (:[]) mb_user_format
                       -- the - modifier requires time-1.2.0.5, released
                       -- in 2011/5, so for now we emulate it for wider
                       -- compatibility.  time < 1.2.0.5 also has a buggy
                       -- %y which we don't do anything about.
                       -- ++ [
                       -- "%Y/%m/%d"
                       -- ,"%Y/%-m/%-d"
                       -- ,"%Y-%m-%d"
                       -- ,"%Y-%-m-%-d"
                       -- ,"%m/%d/%Y"
                       -- ,"%-m/%-d/%Y"
                       -- ,"%m-%d-%Y"
                       -- ,"%-m-%-d-%Y"
                       -- ]
                      )
                      ++ [
                       parseTime defaultTimeLocale "%Y/%m/%e" s
                      ,parseTime defaultTimeLocale "%Y-%m-%e" s
                      ,parseTime defaultTimeLocale "%m/%e/%Y" s
                      ,parseTime defaultTimeLocale "%m-%e-%Y" s
                      ,parseTime defaultTimeLocale "%Y/%m/%e" (take 5 s ++ "0" ++ drop 5 s)
                      ,parseTime defaultTimeLocale "%Y-%m-%e" (take 5 s ++ "0" ++ drop 5 s)
                      ,parseTime defaultTimeLocale "%m/%e/%Y" ('0':s)
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

getAmount :: CsvRules -> CsvRecord -> String
getAmount rules fields = case amountField rules of
  Just f  -> maybe "" (atDef "" fields) $ Just f
  Nothing ->
    case (i, o) of
      (x, "") -> x
      ("", x) -> "-"++x
      p -> error' $ "using amount-in-field and amount-out-field, found a value in both fields: "++show p
    where
      i = maybe "" (atDef "" fields) (amountInField rules)
      o = maybe "" (atDef "" fields) (amountOutField rules)

tests_Hledger_Read_CsvReader = TestList (test_parser ++ test_description_parsing)

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
