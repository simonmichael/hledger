{-|

A reader for CSV data, using an extra rules file to help interpret the data.

-}

module Hledger.Read.CsvReader (
  -- * Reader
  reader,
  -- * Misc.
  CsvRecord,
  -- rules,
  rulesFileFor,
  parseRulesFile,
  transactionFromCsvRecord,
  -- * Tests
  tests_Hledger_Read_CsvReader
)
where
import Control.Applicative ((<$>))
import Control.Exception hiding (try)
import Control.Monad
import Control.Monad.Error
-- import Test.HUnit
import Data.Char (toLower, isDigit)
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time.Calendar (Day)
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
import Text.Printf (hPrintf,printf)

import Hledger.Data
import Prelude hiding (getContents)
import Hledger.Utils.UTF8IOCompat (getContents)
import Hledger.Utils
import Hledger.Read.JournalReader (amountp)


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
  let throwerr = throw.userError

  -- parse csv
  records <- (either throwerr id . validateCsv) `fmap` parseCsv csvfile csvdata
  dbg 1 $ ppShow $ take 3 records

  -- identify header lines
  -- let (headerlines, datalines) = identifyHeaderLines records
  --     mfieldnames = lastMay headerlines

  -- parse rules
  let rulesfile = fromMaybe (rulesFileFor csvfile) mrulesfile
  created <- records `seq` ensureRulesFileExists rulesfile
  if created
   then hPrintf stderr "creating default conversion rules file %s, edit this file for better results\n" rulesfile
   else hPrintf stderr "using conversion rules file %s\n" rulesfile
  rules <- either (throwerr.show) id `fmap` parseRulesFile rulesfile
  dbg 1 $ ppShow rules

  -- apply skip directive
  let headerlines = maybe 0 oneorerror $ getDirective "skip" rules
        where
          oneorerror "" = 1
          oneorerror s  = readDef (throwerr $ "could not parse skip value: " ++ show s) s
      records' = drop headerlines records

  -- convert to transactions and return as a journal
  let txns = map (transactionFromCsvRecord rules) records'
  return $ Right nulljournal{jtxns=sortBy (comparing tdate) txns}

parseCsv :: FilePath -> String -> IO (Either ParseError CSV)
parseCsv path csvdata =
  case path of
    "-" -> liftM (parseCSV "(stdin)") getContents
    _   -> return $ parseCSV path csvdata

-- | Return the cleaned up and validated CSV data, or an error.
validateCsv :: Either ParseError CSV -> Either String [CsvRecord]
validateCsv (Left e) = Left $ show e
validateCsv (Right rs) = validate $ filternulls rs
  where
    filternulls = filter (/=[""])
    validate [] = Left "no CSV records found"
    validate rs@(first:_)
      | isJust lessthan2 = let r = fromJust lessthan2 in Left $ printf "CSV record %s has less than two fields" (show r)
      | isJust different = let r = fromJust different in Left $ printf "the first CSV record %s has %d fields but %s has %d" (show first) length1 (show r) (length r)
      | otherwise        = Right rs
      where
        length1   = length first
        lessthan2 = headMay $ filter ((<2).length) rs
        different = headMay $ filter ((/=length1).length) rs

-- -- | The highest (0-based) field index referenced in the field
-- -- definitions, or -1 if no fields are defined.
-- maxFieldIndex :: CsvRules -> Int
-- maxFieldIndex r = maximumDef (-1) $ catMaybes [
--                    dateField r
--                   ,statusField r
--                   ,codeField r
--                   ,amountField r
--                   ,amountInField r
--                   ,amountOutField r
--                   ,currencyField r
--                   ,accountField r
--                   ,account2Field r
--                   ,date2Field r
--                   ]

-- rulesFileFor :: CliOpts -> FilePath -> FilePath
-- rulesFileFor CliOpts{rules_file_=Just f} _ = f
-- rulesFileFor CliOpts{rules_file_=Nothing} csvfile = replaceExtension csvfile ".rules"
rulesFileFor :: FilePath -> FilePath
rulesFileFor = (++ ".rules")

csvFileFor :: FilePath -> FilePath
csvFileFor = reverse . drop 6 . reverse

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
     writeFile f $ newRulesFileContent f
     return True

newRulesFileContent :: FilePath -> String
newRulesFileContent f = unlines
  ["# hledger csv conversion rules for " ++ csvFileFor (takeFileName f)
  ,"# cf http://hledger.org/MANUAL.html"
  ,""
  ,"account1 assets:bank:checking"
  ,""
  ,"fields date, description, amount"
  ,""
  ,"#skip 1"
  ,""
  ,"#date-format %-d/%-m/%Y"
  ,"#date-format %-m/%-d/%Y"
  ,"#date-format %Y-%h-%d"
  ,""
  ,"#currency $"
  ,""
  ,"if ITUNES"
  ," account2 expenses:entertainment"
  ,""
  ,"if (TO|FROM) SAVINGS"
  ," account2 assets:bank:savings\n"
  ]


--------------------------------------------------------------------------------
-- Conversion rules parsing

{-
Grammar for the CSV conversion rules, more or less:

RULES: RULE*

RULE: ( FIELD-LIST | FIELD-ASSIGNMENT | CONDITIONAL-BLOCK | SKIP | DATE-FORMAT | COMMENT | BLANK ) NEWLINE

FIELD-LIST: fields SPACE FIELD-NAME ( SPACE? , SPACE? FIELD-NAME )*

FIELD-NAME: QUOTED-FIELD-NAME | BARE-FIELD-NAME

QUOTED-FIELD-NAME: " (any CHAR except double-quote)+ "

BARE-FIELD-NAME: any CHAR except space, tab, #, ;

FIELD-ASSIGNMENT: JOURNAL-FIELD ASSIGNMENT-SEPARATOR FIELD-VALUE

JOURNAL-FIELD: date | date2 | status | code | description | comment | account1 | account2 | amount | JOURNAL-PSEUDO-FIELD

JOURNAL-PSEUDO-FIELD: amount-in | amount-out | currency

ASSIGNMENT-SEPARATOR: SPACE | ( : SPACE? )

FIELD-VALUE: VALUE (possibly containing CSV-FIELD-REFERENCEs)

CSV-FIELD-REFERENCE: % CSV-FIELD

CSV-FIELD: ( FIELD-NAME | FIELD-NUMBER ) (corresponding to a CSV field)

FIELD-NUMBER: DIGIT+

CONDITIONAL-BLOCK: if ( FIELD-MATCHER NEWLINE )+ INDENTED-BLOCK

FIELD-MATCHER: ( CSV-FIELD-NAME SPACE? )? ( MATCHOP SPACE? )? PATTERNS

MATCHOP: ~

PATTERNS: ( NEWLINE REGEXP )* REGEXP

INDENTED-BLOCK: ( SPACE ( FIELD-ASSIGNMENT | COMMENT ) NEWLINE )+

REGEXP: ( NONSPACE CHAR* ) SPACE?

VALUE: SPACE? ( CHAR* ) SPACE?

COMMENT: SPACE? COMMENT-CHAR VALUE

COMMENT-CHAR: # | ;

NONSPACE: any CHAR not a SPACE-CHAR

BLANK: SPACE?

SPACE: SPACE-CHAR+

SPACE-CHAR: space | tab

CHAR: any character except newline

DIGIT: 0-9

-}

{- |
A set of data definitions and account-matching patterns sufficient to
convert a particular CSV data file into meaningful journal transactions.
-}
data CsvRules = CsvRules {
  rdirectives        :: [(DirectiveName,String)],
  rcsvfieldindexes   :: [(CsvFieldName, CsvFieldIndex)],
  rassignments       :: [(JournalFieldName, FieldTemplate)],
  rconditionalblocks :: [ConditionalBlock]
} deriving (Show, Eq)

type DirectiveName    = String
type CsvFieldName     = String
type CsvFieldIndex    = Int
type JournalFieldName = String
type FieldTemplate    = String
type ConditionalBlock = ([RecordMatcher], [(JournalFieldName, FieldTemplate)]) -- block matches if all RecordMatchers match
type RecordMatcher    = [Regexp] -- match if any regexps match any of the csv fields
-- type FieldMatcher     = (CsvFieldName, [Regexp]) -- match if any regexps match this csv field
type DateFormat       = String
type Regexp           = String

rules = CsvRules {
  rdirectives=[],
  rcsvfieldindexes=[],
  rassignments=[],
  rconditionalblocks=[]
}

addDirective :: (DirectiveName, String) -> CsvRules -> CsvRules
addDirective d r = r{rdirectives=d:rdirectives r}

addAssignment :: (JournalFieldName, FieldTemplate) -> CsvRules -> CsvRules
addAssignment a r = r{rassignments=a:rassignments r}

setIndexesAndAssignmentsFromList :: [CsvFieldName] -> CsvRules -> CsvRules
setIndexesAndAssignmentsFromList fs r = addAssignmentsFromList fs . setCsvFieldIndexesFromList fs $ r

setCsvFieldIndexesFromList :: [CsvFieldName] -> CsvRules -> CsvRules
setCsvFieldIndexesFromList fs r = r{rcsvfieldindexes=zip fs [1..]}

addAssignmentsFromList :: [CsvFieldName] -> CsvRules -> CsvRules
addAssignmentsFromList fs r = foldl' maybeAddAssignment r journalfieldnames
  where
    maybeAddAssignment rules f = (maybe id addAssignmentFromIndex $ elemIndex f fs) rules
      where
        addAssignmentFromIndex i = addAssignment (f, "%"++show (i+1))

addConditionalBlock :: ConditionalBlock -> CsvRules -> CsvRules
addConditionalBlock b r = r{rconditionalblocks=b:rconditionalblocks r}

getDirective :: DirectiveName -> CsvRules -> Maybe FieldTemplate
getDirective directivename = lookup directivename . rdirectives


parseRulesFile :: FilePath -> IO (Either ParseError CsvRules)
parseRulesFile f = do
  s <- readFile' f
  let rules = parseCsvRules f s
  return $ case rules of
             Left e -> Left e
             Right r -> case validateRules r of
                          Left e -> Left $ toParseError e
                          Right r -> Right r
  where
    toParseError s = newErrorMessage (Message s) (initialPos "")

parseCsvRules :: FilePath -> String -> Either ParseError CsvRules
-- parseCsvRules rulesfile s = runParser csvrulesfile nullrules{baseAccount=takeBaseName rulesfile} rulesfile s
parseCsvRules rulesfile s =
  runParser rulesp rules rulesfile s

-- | Return the validated rules, or an error.
validateRules :: CsvRules -> Either String CsvRules
validateRules rules = do
  unless (isAssigned "date")   $ Left "Please specify (at top level) the date field. Eg: date %1\n"
  unless ((amount && not (amountin || amountout)) ||
          (not amount && (amountin && amountout)))
    $ Left "Please specify (at top level) either the amount field, or both the amount-in and amount-out fields. Eg: amount %2\n"
  Right rules
  where
    amount = isAssigned "amount"
    amountin = isAssigned "amount-in"
    amountout = isAssigned "amount-out"
    isAssigned f = isJust $ getEffectiveAssignment rules [] f

-- parsers

rulesp :: GenParser Char CsvRules CsvRules
rulesp = do
  many $ choice'
    [blankorcommentline                                                    <?> "blank or comment line"
    ,(directive        >>= updateState . addDirective)                     <?> "directive"
    ,(fieldnamelist    >>= updateState . setIndexesAndAssignmentsFromList) <?> "field name list"
    ,(fieldassignment  >>= updateState . addAssignment)                    <?> "field assignment"
    ,(conditionalblock >>= updateState . addConditionalBlock)              <?> "conditional block"
    ]
  eof
  r <- getState
  return r{rdirectives=reverse $ rdirectives r
          ,rassignments=reverse $ rassignments r
          ,rconditionalblocks=reverse $ rconditionalblocks r
          }

blankorcommentline = pdbg 1 "trying blankorcommentline" >> choice' [blankline, commentline]
blankline = many spacenonewline >> newline >> return () <?> "blank line"
commentline = many spacenonewline >> commentchar >> restofline >> return () <?> "comment line"
commentchar = oneOf ";#"

directive = do
  pdbg 1 "trying directive"
  d <- choice' $ map string directives
  v <- (((char ':' >> many spacenonewline) <|> many1 spacenonewline) >> directiveval)
       <|> (optional (char ':') >> many spacenonewline >> eolof >> return "")
  return (d,v)
  <?> "directive"

directives =
  ["date-format"
  -- ,"default-account1"
  -- ,"default-currency"
  -- ,"skip-lines" -- old
  ,"skip"
   -- ,"base-account"
   -- ,"base-currency"
  ]

directiveval = anyChar `manyTill` eolof

fieldnamelist = (do
  pdbg 1 "trying fieldnamelist"
  string "fields"
  optional $ char ':'
  many1 spacenonewline
  f <- fieldname
  let separator = many spacenonewline >> char ',' >> many spacenonewline
  fs <- many1 $ (separator >> fromMaybe "" <$> optionMaybe fieldname)
  restofline
  return $ map (map toLower) $ f:fs
  ) <?> "field name list"

fieldname = quotedfieldname <|> barefieldname

quotedfieldname = do
  char '"'
  f <- many1 $ noneOf "\"\n:;#~"
  char '"'
  return f

barefieldname = many1 $ noneOf " \t\n,;#~"

fieldassignment = do
  pdbg 1 "trying fieldassignment"
  f <- journalfieldname
  assignmentseparator
  v <- fieldval
  return (f,v)
  <?> "field assignment"

journalfieldname = pdbg 2 "trying journalfieldname" >> choice' (map string journalfieldnames)

journalfieldnames =
  [-- pseudo fields:
   "amount-in"
  ,"amount-out"
  ,"currency"
   -- standard fields:
  ,"date2"
  ,"date"
  ,"status"
  ,"code"
  ,"description"
  ,"amount"
  ,"account1"
  ,"account2"
  ,"comment"
  ]

assignmentseparator = do
  pdbg 3 "trying assignmentseparator"
  choice [
    -- try (many spacenonewline >> oneOf ":="),
    try (many spacenonewline >> char ':'),
    space
    ]
  many spacenonewline

fieldval = do
  pdbg 2 "trying fieldval"
  anyChar `manyTill` eolof

conditionalblock = do
  pdbg 1 "trying conditionalblock"
  string "if" >> many spacenonewline >> optional newline
  ms <- many1 recordmatcher
  as <- many (many1 spacenonewline >> fieldassignment)
  when (null as) $
    fail "start of conditional block found, but no assignment rules afterward\n(assignment rules in a conditional block should be indented)\n"
  return (ms, as)
  <?> "conditional block"

recordmatcher = do
  pdbg 2 "trying recordmatcher"
  -- pos <- currentPos
  _  <- optional (matchoperator >> many spacenonewline >> optional newline)
  ps <- patterns
  when (null ps) $
    fail "start of record matcher found, but no patterns afterward\n(patterns should not be indented)\n"
  return ps
  <?> "record matcher"

matchoperator = choice' $ map string
  ["~"
  -- ,"!~"
  -- ,"="
  -- ,"!="
  ]

patterns = do
  pdbg 3 "trying patterns"
  ps <- many regexp
  return ps

regexp = do
  pdbg 3 "trying regexp"
  notFollowedBy matchoperator
  c <- nonspace
  cs <- anyChar `manyTill` eolof
  return $ strip $ c:cs

-- fieldmatcher = do
--   pdbg 2 "trying fieldmatcher"
--   f <- fromMaybe "all" `fmap` (optionMaybe $ do
--          f' <- fieldname
--          many spacenonewline
--          return f')
--   char '~'
--   many spacenonewline
--   ps <- patterns
--   let r = "(" ++ intercalate "|" ps ++ ")"
--   return (f,r)
--   <?> "field matcher"

--------------------------------------------------------------------------------
-- Converting CSV records to journal transactions

type CsvRecord = [String]

-- Convert a CSV record to a transaction using the rules, or raise an
-- error if the data can not be parsed.
transactionFromCsvRecord :: CsvRules -> CsvRecord -> Transaction
transactionFromCsvRecord rules record = t
  where
    mdirective       = (`getDirective` rules)
    mfieldtemplate   = getEffectiveAssignment rules record
    render           = renderTemplate rules record
    mskip            = mdirective "skip"
    mdefaultcurrency = mdirective "default-currency"
    mparsedate       = parseDateWithFormatOrDefaultFormats (mdirective "date-format")

    -- render each field using its template and the csv record, and
    -- in some cases parse the rendered string (eg dates and amounts)
    mdateformat = mdirective "date-format"
    date        = render $ fromMaybe "" $ mfieldtemplate "date"
    date'       = fromMaybe (error' $ dateerror "date" date mdateformat) $ mparsedate date
    mdate2      = maybe Nothing (Just . render) $ mfieldtemplate "date2"
    mdate2'     = maybe Nothing (maybe (error' $ dateerror "date2" (fromMaybe "" mdate2) mdateformat) Just . mparsedate) mdate2
    dateerror datefield value mdateformat = unlines
      ["error: could not parse \""++value++"\" as a date using date format "++maybe "\"YYYY/M/D\", \"YYYY-M-D\" or \"YYYY.M.D\"" show mdateformat
      ,"the CSV record is:  "++intercalate ", " (map show record)
      ,"the "++datefield++" rule is:   "++(fromMaybe "required, but missing" $ mfieldtemplate datefield)
      ,"the date-format is: "++fromMaybe "unspecified" mdateformat
      ,"you may need to "
       ++"change your "++datefield++" rule, "
       ++maybe "add a" (const "change your") mdateformat++" date-format rule, "
       ++"or "++maybe "add a" (const "change your") mskip++" skip rule"
      ,"for m/d/y or d/m/y dates, use date-format %-m/%-d/%Y or date-format %-d/%-m/%Y"
      ]
    status      = maybe False ((=="*") . render) $ mfieldtemplate "status"
    code        = maybe "" render $ mfieldtemplate "code"
    description = maybe "" render $ mfieldtemplate "description"
    comment     = maybe "" render $ mfieldtemplate "comment"
    precomment  = maybe "" render $ mfieldtemplate "precomment"
    currency    = maybe (fromMaybe "" mdefaultcurrency) render $ mfieldtemplate "currency"
    amountstr   = (currency++) $ negateIfParenthesised $ getAmountStr rules record
    amount      = either amounterror mixed $ runParser (do {a <- amountp; eof; return a}) nullctx "" amountstr
    amounterror err = error' $ unlines
      ["error: could not parse \""++amountstr++"\" as an amount"
      ,showRecord record
      ,"the amount rule is:      "++(fromMaybe "" $ mfieldtemplate "amount")
      ,"the currency rule is:    "++(fromMaybe "unspecified" $ mfieldtemplate "currency")
      ,"the default-currency is: "++fromMaybe "unspecified" mdefaultcurrency
      ,"the parse error is:      "++show err
      ,"you may need to "
       ++"change your amount or currency rules, "
       ++"or "++maybe "add a" (const "change your") mskip++" skip rule"
      ]
    -- Using costOfMixedAmount here to allow complex costs like "10 GBP @@ 15 USD".
    -- Aim is to have "10 GBP @@ 15 USD" applied to account2, but have "-15USD" applied to account1
    amount1        = costOfMixedAmount amount
    amount2        = (-amount)
    s `or` def  = if null s then def else s
    defaccount1 = fromMaybe "unknown" $ mdirective "default-account1"
    defaccount2 = case isNegativeMixedAmount amount2 of
                   Just True -> "income:unknown"
                   _         -> "expenses:unknown"
    account1    = maybe "" render (mfieldtemplate "account1") `or` defaccount1
    account2    = maybe "" render (mfieldtemplate "account2") `or` defaccount2

    -- build the transaction
    t = nulltransaction{
      tdate                    = date',
      tdate2                   = mdate2',
      tstatus                  = status,
      tcode                    = code,
      tdescription             = description,
      tcomment                 = comment,
      tpreceding_comment_lines = precomment,
      tpostings                =
        [posting {paccount=account2, pamount=amount2, ptransaction=Just t}
        ,posting {paccount=account1, pamount=amount1, ptransaction=Just t}
        ]
      }

getAmountStr :: CsvRules -> CsvRecord -> String
getAmountStr rules record =
 let
   mamount    = getEffectiveAssignment rules record "amount"
   mamountin  = getEffectiveAssignment rules record "amount-in"
   mamountout = getEffectiveAssignment rules record "amount-out"
   render     = fmap (strip . renderTemplate rules record)
 in
  case (render mamount, render mamountin, render mamountout) of
    (Just "", Nothing, Nothing) -> error' $ "amount has no value\n"++showRecord record
    (Just a,  Nothing, Nothing) -> a
    (Nothing, Just "", Just "") -> error' $ "neither amount-in or amount-out has a value\n"++showRecord record
    (Nothing, Just i,  Just "") -> i
    (Nothing, Just "", Just o)  -> negateStr o
    (Nothing, Just _,  Just _)  -> error' $ "both amount-in and amount-out have a value\n"++showRecord record
    _                           -> error' $ "found values for amount and for amount-in/amount-out - please use either amount or amount-in/amount-out\n"++showRecord record

negateIfParenthesised :: String -> String
negateIfParenthesised ('(':s) | lastMay s == Just ')' = negateStr $ init s
negateIfParenthesised s                               = s

negateStr :: String -> String
negateStr ('-':s) = s
negateStr s       = '-':s

-- | Show a (approximate) recreation of the original CSV record.
showRecord :: CsvRecord -> String
showRecord r = "the CSV record is:       "++intercalate ", " (map show r)

-- | Given the conversion rules, a CSV record and a journal entry field name, find
-- the template value ultimately assigned to this field, either at top
-- level or in a matching conditional block.  Conditional blocks'
-- patterns are matched against an approximation of the original CSV
-- record: all the field values with commas intercalated.
getEffectiveAssignment :: CsvRules -> CsvRecord -> JournalFieldName -> Maybe FieldTemplate
getEffectiveAssignment rules record f = lastMay $ assignmentsFor f
  where
    assignmentsFor f = map snd $ filter ((==f).fst) $ toplevelassignments ++ conditionalassignments
      where
        toplevelassignments    = rassignments rules
        conditionalassignments = concatMap snd $ filter blockMatches $ blocksAssigning f
          where
            blocksAssigning f = filter (any ((==f).fst) . snd) $ rconditionalblocks rules
            blockMatches :: ConditionalBlock -> Bool
            blockMatches (matchers,_) = all matcherMatches matchers
              where
                matcherMatches :: RecordMatcher -> Bool
                -- matcherMatches pats = any patternMatches pats
                matcherMatches pats = patternMatches $  "(" ++ intercalate "|" pats ++ ")"
                  where
                    patternMatches :: Regexp -> Bool
                    patternMatches pat = regexMatchesCIRegexCompat pat csvline
                      where
                        csvline = intercalate "," record

renderTemplate ::  CsvRules -> CsvRecord -> FieldTemplate -> String
renderTemplate rules record t = regexReplaceBy "%[A-z0-9]+" replace t
  where
    replace ('%':pat) = maybe pat (\i -> atDef "" record (i-1)) mi
      where
        mi | all isDigit pat = readMay pat
           | otherwise       = lookup pat $ rcsvfieldindexes rules
    replace pat       = pat

-- Parse the date string using the specified date-format, or if unspecified try these default formats:
-- YYYY/MM/DD, YYYY-MM-DD, YYYY.MM.DD, MM/DD/YYYY (month and day can be 1 or 2 digits, year must be 4).
parseDateWithFormatOrDefaultFormats :: Maybe DateFormat -> String -> Maybe Day
parseDateWithFormatOrDefaultFormats mformat s = firstJust $ map parsewith formats
  where
    parsewith = flip (parseTime defaultTimeLocale) s
    formats = maybe
               ["%Y/%-m/%-d"
               ,"%Y-%-m-%-d"
               ,"%Y.%-m.%-d"
               -- ,"%-m/%-d/%Y"
                -- ,parseTime defaultTimeLocale "%Y/%m/%e" (take 5 s ++ "0" ++ drop 5 s)
                -- ,parseTime defaultTimeLocale "%Y-%m-%e" (take 5 s ++ "0" ++ drop 5 s)
                -- ,parseTime defaultTimeLocale "%m/%e/%Y" ('0':s)
                -- ,parseTime defaultTimeLocale "%m-%e-%Y" ('0':s)
               ]
               (:[])
                mformat

--------------------------------------------------------------------------------
-- tests

tests_Hledger_Read_CsvReader = TestList (test_parser)
                               -- ++ test_description_parsing)

-- test_description_parsing = [
--       "description-field 1" ~: assertParseDescription "description-field 1\n" [FormatField False Nothing Nothing (FieldNo 1)]
--     , "description-field 1 " ~: assertParseDescription "description-field 1 \n" [FormatField False Nothing Nothing (FieldNo 1)]
--     , "description-field %(1)" ~: assertParseDescription "description-field %(1)\n" [FormatField False Nothing Nothing (FieldNo 1)]
--     , "description-field %(1)/$(2)" ~: assertParseDescription "description-field %(1)/%(2)\n" [
--           FormatField False Nothing Nothing (FieldNo 1)
--         , FormatLiteral "/"
--         , FormatField False Nothing Nothing (FieldNo 2)
--         ]
--     ]
--   where
--     assertParseDescription string expected = do assertParseEqual (parseDescription string) (rules {descriptionField = expected})
--     parseDescription :: String -> Either ParseError CsvRules
--     parseDescription x = runParser descriptionfieldWrapper rules "(unknown)" x
--     descriptionfieldWrapper :: GenParser Char CsvRules CsvRules
--     descriptionfieldWrapper = do
--       descriptionfield
--       r <- getState
--       return r

test_parser =  [

   "convert rules parsing: empty file" ~: do
     -- let assertMixedAmountParse parseresult mixedamount =
     --         (either (const "parse error") showMixedAmountDebug parseresult) ~?= (showMixedAmountDebug mixedamount)
    assertParseEqual (parseCsvRules "unknown" "") rules

  -- ,"convert rules parsing: accountrule" ~: do
  --    assertParseEqual (parseWithCtx rules accountrule "A\na\n") -- leading blank line required
  --                ([("A",Nothing)], "a")

  ,"convert rules parsing: trailing comments" ~: do
     assertParse (parseWithCtx rules rulesp "skip\n# \n#\n")

  ,"convert rules parsing: trailing blank lines" ~: do
     assertParse (parseWithCtx rules rulesp "skip\n\n  \n")

  -- not supported
  -- ,"convert rules parsing: no final newline" ~: do
  --    assertParse (parseWithCtx rules csvrulesfile "A\na")
  --    assertParse (parseWithCtx rules csvrulesfile "A\na\n# \n#")
  --    assertParse (parseWithCtx rules csvrulesfile "A\na\n\n  ")

                 -- (rules{
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
