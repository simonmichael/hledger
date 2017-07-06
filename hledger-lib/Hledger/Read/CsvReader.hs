{-|

A reader for CSV data, using an extra rules file to help interpret the data.

-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hledger.Read.CsvReader (
  -- * Reader
  reader,
  -- * Misc.
  CsvRecord,
  -- rules,
  rulesFileFor,
  parseRulesFile,
  parseAndValidateCsvRules,
  expandIncludes,
  transactionFromCsvRecord,
  -- * Tests
  tests_Hledger_Read_CsvReader
)
where
import Prelude ()
import Prelude.Compat hiding (getContents)
import Control.Exception hiding (try)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict (StateT, get, modify', evalStateT)
-- import Test.HUnit
import Data.Char (toLower, isDigit, isSpace)
import Data.List.Compat
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Calendar (Day)
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
#else
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale)
#endif
import Safe
import System.Directory (doesFileExist)
import System.FilePath
import Test.HUnit hiding (State)
import Text.CSV (parseCSV, CSV)
import Text.Megaparsec hiding (parse, State)
import Text.Megaparsec.Text
import qualified Text.Parsec as Parsec
import Text.Printf (printf)

import Hledger.Data
import Hledger.Utils.UTF8IOCompat (getContents)
import Hledger.Utils
import Hledger.Read.Common (amountp, statusp, genericSourcePos)


reader :: Reader
reader = Reader
  {rFormat     = "csv"
  ,rExtensions = ["csv"]
  ,rParser     = parse
  ,rExperimental = False
  }

-- | Parse and post-process a "Journal" from CSV data, or give an error.
-- XXX currently ignores the string and reads from the file path
parse :: Maybe FilePath -> Bool -> FilePath -> Text -> ExceptT String IO Journal
parse rulesfile _ f t = do
  r <- liftIO $ readJournalFromCsv rulesfile f t
  case r of Left e -> throwError e
            Right j -> return $ journalNumberAndTieTransactions j
-- XXX does not use parseAndFinaliseJournal like the other readers

-- | Read a Journal from the given CSV data (and filename, used for error
-- messages), or return an error. Proceed as follows:
-- @
-- 1. parse CSV conversion rules from the specified rules file, or from
--    the default rules file for the specified CSV file, if it exists,
--    or throw a parse error; if it doesn't exist, use built-in default rules
-- 2. parse the CSV data, or throw a parse error
-- 3. convert the CSV records to transactions using the rules
-- 4. if the rules file didn't exist, create it with the default rules and filename
-- 5. return the transactions as a Journal 
-- @
readJournalFromCsv :: Maybe FilePath -> FilePath -> Text -> IO (Either String Journal)
readJournalFromCsv Nothing "-" _ = return $ Left "please use --rules-file when reading CSV from stdin"
readJournalFromCsv mrulesfile csvfile csvdata =
 handle (\e -> return $ Left $ show (e :: IOException)) $ do
  let throwerr = throw.userError

  -- parse rules
  let rulesfile = fromMaybe (rulesFileFor csvfile) mrulesfile
  rulesfileexists <- doesFileExist rulesfile
  rulestext <-
    if rulesfileexists
    then do
      dbg1IO "using conversion rules file" rulesfile
      liftIO $ (readFile' rulesfile >>= expandIncludes (takeDirectory rulesfile))
    else return $ defaultRulesText rulesfile
  rules <- liftIO (runExceptT $ parseAndValidateCsvRules rulesfile rulestext) >>= either throwerr return 
  dbg2IO "rules" rules

  -- apply skip directive
  let skip = maybe 0 oneorerror $ getDirective "skip" rules
        where
          oneorerror "" = 1
          oneorerror s  = readDef (throwerr $ "could not parse skip value: " ++ show s) s

  -- parse csv
  -- parsec seems to fail if you pass it "-" here XXX try again with megaparsec
  let parsecfilename = if csvfile == "-" then "(stdin)" else csvfile
  records <- (either throwerr id .
              dbg2 "validateCsv" . validateCsv skip .
              dbg2 "parseCsv")
             `fmap` parseCsv parsecfilename (T.unpack csvdata)
  dbg1IO "first 3 csv records" $ take 3 records

  -- identify header lines
  -- let (headerlines, datalines) = identifyHeaderLines records
  --     mfieldnames = lastMay headerlines

  let 
    -- convert CSV records to transactions
    txns = snd $ mapAccumL
                     (\pos r -> (pos,
                                 transactionFromCsvRecord
                                   (let SourcePos name line col =  pos in
                                    SourcePos name (unsafePos $ unPos line + 1) col)
                                   rules
                                    r))
                     (initialPos parsecfilename) records

    -- Ensure transactions are ordered chronologically.
    -- First, reverse them to get same-date transactions ordered chronologically,
    -- if the CSV records seem to be most-recent-first, ie if there's an explicit 
    -- "newest-first" directive, or if there's more than one date and the first date
    -- is more recent than the last.
    txns' = 
      (if newestfirst || mseemsnewestfirst == Just True then reverse else id) txns
      where
        newestfirst = dbg3 "newestfirst" $ isJust $ getDirective "newest-first" rules
        mseemsnewestfirst = dbg3 "mseemsnewestfirst" $  
          case nub $ map tdate txns of 
            ds | length ds > 1 -> Just $ head ds > last ds 
            _                  -> Nothing
    -- Second, sort by date.
    txns'' = sortBy (comparing tdate) txns'

  when (not rulesfileexists) $ do
    dbg1IO "creating conversion rules file" rulesfile
    writeFile rulesfile $ T.unpack rulestext

  return $ Right nulljournal{jtxns=txns''}

parseCsv :: FilePath -> String -> IO (Either Parsec.ParseError CSV)
parseCsv path csvdata =
  case path of
    "-" -> liftM (parseCSV "(stdin)") getContents
    _   -> return $ parseCSV path csvdata

-- | Return the cleaned up and validated CSV data, or an error.
validateCsv :: Int -> Either Parsec.ParseError CSV -> Either String [CsvRecord]
validateCsv _ (Left e) = Left $ show e
validateCsv numhdrlines (Right rs) = validate $ drop numhdrlines $ filternulls rs
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

defaultRulesText :: FilePath -> Text
defaultRulesText csvfile = T.pack $ unlines
  ["# hledger csv conversion rules for " ++ csvFileFor (takeFileName csvfile)
  ,"# cf http://hledger.org/manual#csv-files"
  ,""
  ,"account1 assets:bank:checking"
  ,""
  ,"fields date, description, amount"
  ,""
  ,"#skip 1"
  ,"#newest-first"
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

RULE: ( FIELD-LIST | FIELD-ASSIGNMENT | CONDITIONAL-BLOCK | SKIP | NEWEST-FIRST | DATE-FORMAT | COMMENT | BLANK ) NEWLINE

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

type CsvRulesParser a = StateT CsvRules Parser a

type DirectiveName    = String
type CsvFieldName     = String
type CsvFieldIndex    = Int
type JournalFieldName = String
type FieldTemplate    = String
type ConditionalBlock = ([RecordMatcher], [(JournalFieldName, FieldTemplate)]) -- block matches if all RecordMatchers match
type RecordMatcher    = [RegexpPattern] -- match if any regexps match any of the csv fields
-- type FieldMatcher     = (CsvFieldName, [RegexpPattern]) -- match if any regexps match this csv field
type DateFormat       = String
type RegexpPattern           = String

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

instance ShowErrorComponent String where
  showErrorComponent = id

-- | An error-throwing action that parses this file's content 
-- as CSV conversion rules, interpolating any included files first, 
-- and runs some extra validation checks.
parseRulesFile :: FilePath -> ExceptT String IO CsvRules
parseRulesFile f = 
  liftIO (readFile' f >>= expandIncludes (takeDirectory f)) >>= parseAndValidateCsvRules f

-- | Inline all files referenced by include directives in this hledger CSV rules text, recursively.
-- Included file paths may be relative to the directory of the provided file path.
-- This is a cheap hack to avoid rewriting the CSV rules parser.
expandIncludes dir content = mapM (expandLine dir) (T.lines content) >>= return . T.unlines
  where
    expandLine dir line =
      case line of
        (T.stripPrefix "include " -> Just f) -> expandIncludes dir' =<< T.readFile f'
          where
            f' = dir </> dropWhile isSpace (T.unpack f)
            dir' = takeDirectory f'
        _ -> return line 

-- | An error-throwing action that parses this text as CSV conversion rules 
-- and runs some extra validation checks. The file path is for error messages.
parseAndValidateCsvRules :: FilePath -> T.Text -> ExceptT String IO CsvRules
parseAndValidateCsvRules rulesfile s = do
  let rules = parseCsvRules rulesfile s
  case rules of
    Left e -> ExceptT $ return $ Left $ parseErrorPretty e
    Right r -> do
               r_ <- liftIO $ runExceptT $ validateRules r
               ExceptT $ case r_ of
                 Left e -> return $ Left $ parseErrorPretty $ toParseError e
                 Right r -> return $ Right r
  where
    toParseError :: forall s. Ord s => s -> ParseError Char s
    toParseError s = (mempty :: ParseError Char s) { errorCustom = S.singleton s}

-- | Parse this text as CSV conversion rules. The file path is for error messages.
parseCsvRules :: FilePath -> T.Text -> Either (ParseError Char Dec) CsvRules
-- parseCsvRules rulesfile s = runParser csvrulesfile nullrules{baseAccount=takeBaseName rulesfile} rulesfile s
parseCsvRules rulesfile s =
  runParser (evalStateT rulesp rules) rulesfile s

-- | Return the validated rules, or an error.
validateRules :: CsvRules -> ExceptT String IO CsvRules
validateRules rules = do
  unless (isAssigned "date")   $ ExceptT $ return $ Left "Please specify (at top level) the date field. Eg: date %1\n"
  unless ((amount && not (amountin || amountout)) ||
          (not amount && (amountin && amountout)))
    $ ExceptT $ return $ Left "Please specify (at top level) either the amount field, or both the amount-in and amount-out fields. Eg: amount %2\n"
  ExceptT $ return $ Right rules
  where
    amount = isAssigned "amount"
    amountin = isAssigned "amount-in"
    amountout = isAssigned "amount-out"
    isAssigned f = isJust $ getEffectiveAssignment rules [] f

-- parsers

rulesp :: CsvRulesParser CsvRules
rulesp = do
  many $ choiceInState
    [blankorcommentlinep                                                <?> "blank or comment line"
    ,(directivep        >>= modify' . addDirective)                     <?> "directive"
    ,(fieldnamelistp    >>= modify' . setIndexesAndAssignmentsFromList) <?> "field name list"
    ,(fieldassignmentp  >>= modify' . addAssignment)                    <?> "field assignment"
    ,(conditionalblockp >>= modify' . addConditionalBlock)              <?> "conditional block"
    ]
  eof
  r <- get
  return r{rdirectives=reverse $ rdirectives r
          ,rassignments=reverse $ rassignments r
          ,rconditionalblocks=reverse $ rconditionalblocks r
          }

blankorcommentlinep :: CsvRulesParser ()
blankorcommentlinep = lift (pdbg 3 "trying blankorcommentlinep") >> choiceInState [blanklinep, commentlinep]

blanklinep :: CsvRulesParser ()
blanklinep = lift (many spacenonewline) >> newline >> return () <?> "blank line"

commentlinep :: CsvRulesParser ()
commentlinep = lift (many spacenonewline) >> commentcharp >> lift restofline >> return () <?> "comment line"

commentcharp :: CsvRulesParser Char
commentcharp = oneOf (";#*" :: [Char])

directivep :: CsvRulesParser (DirectiveName, String)
directivep = (do
  lift $ pdbg 3 "trying directive"
  d <- choiceInState $ map string directives
  v <- (((char ':' >> lift (many spacenonewline)) <|> lift (some spacenonewline)) >> directivevalp)
       <|> (optional (char ':') >> lift (many spacenonewline) >> lift eolof >> return "")
  return (d,v)
  ) <?> "directive"

directives =
  ["date-format"
  -- ,"default-account1"
  -- ,"default-currency"
  -- ,"skip-lines" -- old
  ,"skip"
  ,"newest-first"
   -- ,"base-account"
   -- ,"base-currency"
  ]

directivevalp :: CsvRulesParser String
directivevalp = anyChar `manyTill` lift eolof

fieldnamelistp :: CsvRulesParser [CsvFieldName]
fieldnamelistp = (do
  lift $ pdbg 3 "trying fieldnamelist"
  string "fields"
  optional $ char ':'
  lift (some spacenonewline)
  let separator = lift (many spacenonewline) >> char ',' >> lift (many spacenonewline)
  f <- fromMaybe "" <$> optional fieldnamep
  fs <- some $ (separator >> fromMaybe "" <$> optional fieldnamep)
  lift restofline
  return $ map (map toLower) $ f:fs
  ) <?> "field name list"

fieldnamep :: CsvRulesParser String
fieldnamep = quotedfieldnamep <|> barefieldnamep

quotedfieldnamep :: CsvRulesParser String
quotedfieldnamep = do
  char '"'
  f <- some $ noneOf ("\"\n:;#~" :: [Char])
  char '"'
  return f

barefieldnamep :: CsvRulesParser String
barefieldnamep = some $ noneOf (" \t\n,;#~" :: [Char])

fieldassignmentp :: CsvRulesParser (JournalFieldName, FieldTemplate)
fieldassignmentp = do
  lift $ pdbg 3 "trying fieldassignmentp"
  f <- journalfieldnamep
  assignmentseparatorp
  v <- fieldvalp
  return (f,v)
  <?> "field assignment"

journalfieldnamep :: CsvRulesParser String
journalfieldnamep = lift (pdbg 2 "trying journalfieldnamep") >> choiceInState (map string journalfieldnames)

-- Transaction fields and pseudo fields for CSV conversion. 
-- Names must precede any other name they contain, for the parser 
-- (amount-in before amount; date2 before date). TODO: fix
journalfieldnames = [
   "account1"
  ,"account2"
  ,"amount-in"
  ,"amount-out"
  ,"amount"
  ,"balance"
  ,"code"
  ,"comment"
  ,"currency"
  ,"date2"
  ,"date"
  ,"description"
  ,"status"
  ]

assignmentseparatorp :: CsvRulesParser ()
assignmentseparatorp = do
  lift $ pdbg 3 "trying assignmentseparatorp"
  choice [
    -- try (lift (many spacenonewline) >> oneOf ":="),
    try (lift (many spacenonewline) >> char ':'),
    spaceChar
    ]
  _ <- lift (many spacenonewline)
  return ()

fieldvalp :: CsvRulesParser String
fieldvalp = do
  lift $ pdbg 2 "trying fieldvalp"
  anyChar `manyTill` lift eolof

conditionalblockp :: CsvRulesParser ConditionalBlock
conditionalblockp = do
  lift $ pdbg 3 "trying conditionalblockp"
  string "if" >> lift (many spacenonewline) >> optional newline
  ms <- some recordmatcherp
  as <- many (lift (some spacenonewline) >> fieldassignmentp)
  when (null as) $
    fail "start of conditional block found, but no assignment rules afterward\n(assignment rules in a conditional block should be indented)\n"
  return (ms, as)
  <?> "conditional block"

recordmatcherp :: CsvRulesParser [String]
recordmatcherp = do
  lift $ pdbg 2 "trying recordmatcherp"
  -- pos <- currentPos
  _  <- optional (matchoperatorp >> lift (many spacenonewline) >> optional newline)
  ps <- patternsp
  when (null ps) $
    fail "start of record matcher found, but no patterns afterward\n(patterns should not be indented)\n"
  return ps
  <?> "record matcher"

matchoperatorp :: CsvRulesParser String
matchoperatorp = choiceInState $ map string
  ["~"
  -- ,"!~"
  -- ,"="
  -- ,"!="
  ]

patternsp :: CsvRulesParser [String]
patternsp = do
  lift $ pdbg 3 "trying patternsp"
  ps <- many regexp
  return ps

regexp :: CsvRulesParser String
regexp = do
  lift $ pdbg 3 "trying regexp"
  notFollowedBy matchoperatorp
  c <- lift nonspace
  cs <- anyChar `manyTill` lift eolof
  return $ strip $ c:cs

-- fieldmatcher = do
--   pdbg 2 "trying fieldmatcher"
--   f <- fromMaybe "all" `fmap` (optional $ do
--          f' <- fieldname
--          lift (many spacenonewline)
--          return f')
--   char '~'
--   lift (many spacenonewline)
--   ps <- patterns
--   let r = "(" ++ intercalate "|" ps ++ ")"
--   return (f,r)
--   <?> "field matcher"

--------------------------------------------------------------------------------
-- Converting CSV records to journal transactions

type CsvRecord = [String]

-- Convert a CSV record to a transaction using the rules, or raise an
-- error if the data can not be parsed.
transactionFromCsvRecord :: SourcePos -> CsvRules -> CsvRecord -> Transaction
transactionFromCsvRecord sourcepos rules record = t
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
    status      =
      case mfieldtemplate "status" of
        Nothing  -> Unmarked
        Just str -> either statuserror id .
                    runParser (statusp <* eof) "" .
                    T.pack $ render str
          where
            statuserror err = error' $ unlines
              ["error: could not parse \""++str++"\" as a cleared status (should be *, ! or empty)"
              ,"the parse error is:      "++show err
              ]
    code        = maybe "" render $ mfieldtemplate "code"
    description = maybe "" render $ mfieldtemplate "description"
    comment     = maybe "" render $ mfieldtemplate "comment"
    precomment  = maybe "" render $ mfieldtemplate "precomment"
    currency    = maybe (fromMaybe "" mdefaultcurrency) render $ mfieldtemplate "currency"
    amountstr   = (currency++) $ simplifySign $ getAmountStr rules record
    amount      = either amounterror (Mixed . (:[])) $ runParser (evalStateT (amountp <* eof) mempty) "" $ T.pack amountstr
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
    account1    = T.pack $ maybe "" render (mfieldtemplate "account1") `or` defaccount1
    account2    = T.pack $ maybe "" render (mfieldtemplate "account2") `or` defaccount2
    balance     = maybe Nothing (parsebalance.render) $ mfieldtemplate "balance"
    parsebalance str 
      | all isSpace str  = Nothing
      | otherwise = Just $ either (balanceerror str) id $ runParser (evalStateT (amountp <* eof) mempty) "" $ T.pack $ (currency++) $ simplifySign str
    balanceerror str err = error' $ unlines
      ["error: could not parse \""++str++"\" as balance amount"
      ,showRecord record
      ,"the balance rule is:      "++(fromMaybe "" $ mfieldtemplate "balance")
      ,"the currency rule is:    "++(fromMaybe "unspecified" $ mfieldtemplate "currency")
      ,"the default-currency is: "++fromMaybe "unspecified" mdefaultcurrency
      ,"the parse error is:      "++show err
      ]

    -- build the transaction
    t = nulltransaction{
      tsourcepos               = genericSourcePos sourcepos,
      tdate                    = date',
      tdate2                   = mdate2',
      tstatus                  = status,
      tcode                    = T.pack code,
      tdescription             = T.pack description,
      tcomment                 = T.pack comment,
      tpreceding_comment_lines = T.pack precomment,
      tpostings                =
        [posting {paccount=account2, pamount=amount2, ptransaction=Just t}
        ,posting {paccount=account1, pamount=amount1, ptransaction=Just t, pbalanceassertion=balance}
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

type CsvAmountString = String

-- | Canonicalise the sign in a CSV amount string.
-- Such strings can be parenthesized, which is equivalent to having a minus sign.
-- Also they can end up with a double minus sign, which cancels out.
simplifySign :: CsvAmountString -> CsvAmountString
simplifySign ('(':s) | lastMay s == Just ')' = simplifySign $ negateStr $ init s
simplifySign ('-':'-':s) = s
simplifySign s = s

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
                    patternMatches :: RegexpPattern -> Bool
                    patternMatches pat = regexMatchesCI pat csvline
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
    parsetime =
#if MIN_VERSION_time(1,5,0)
     parseTimeM True
#else
     parseTime
#endif
    parsewith = flip (parsetime defaultTimeLocale) s
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
  --    assertParseEqual (parseWithState rules accountrule "A\na\n") -- leading blank line required
  --                ([("A",Nothing)], "a")

  ,"convert rules parsing: trailing comments" ~: do
     assertParse (parseWithState' rules rulesp "skip\n# \n#\n")

  ,"convert rules parsing: trailing blank lines" ~: do
     assertParse (parseWithState' rules rulesp "skip\n\n  \n")

  ,"convert rules parsing: empty field value" ~: do
     assertParse (parseWithState' rules rulesp "account1 \nif foo\n  account2 foo\n")

  -- not supported
  -- ,"convert rules parsing: no final newline" ~: do
  --    assertParse (parseWithState rules csvrulesfile "A\na")
  --    assertParse (parseWithState rules csvrulesfile "A\na\n# \n#")
  --    assertParse (parseWithState rules csvrulesfile "A\na\n\n  ")

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
