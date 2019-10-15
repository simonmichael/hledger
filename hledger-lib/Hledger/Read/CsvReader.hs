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
{-# LANGUAGE PackageImports #-}

module Hledger.Read.CsvReader (
  -- * Reader
  reader,
  -- * Misc.
  CsvRecord,
  CSV, Record, Field,
  -- rules,
  rulesFileFor,
  parseRulesFile,
  parseAndValidateCsvRules,
  expandIncludes,
  transactionFromCsvRecord,
  printCSV,
  -- * Tests
  tests_CsvReader,
)
where
import Prelude ()
import "base-compat-batteries" Prelude.Compat hiding (fail)
import qualified "base-compat-batteries" Control.Monad.Fail.Compat as Fail (fail)
import Control.Exception          (IOException, handle, throw)
import Control.Monad              (liftM, unless, when)
import Control.Monad.Except       (ExceptT, throwError)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.State.Strict (StateT, get, modify', evalStateT)
import Control.Monad.Trans.Class  (lift)
import Data.Char                  (toLower, isDigit, isSpace, ord)
import Data.Bifunctor             (first)
import "base-compat-batteries" Data.List.Compat
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
import qualified Data.Csv as Cassava
import qualified Data.Csv.Parser.Megaparsec as CassavaMP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Custom
import Text.Printf (printf)

import Hledger.Data
import Hledger.Utils
import Hledger.Read.Common (Reader(..),InputOpts(..),amountp, statusp, genericSourcePos)

type CSV = [Record]

type Record = [Field]

type Field = String

reader :: Reader
reader = Reader
  {rFormat     = "csv"
  ,rExtensions = ["csv"]
  ,rParser     = parse
  ,rExperimental = False
  }

-- | Parse and post-process a "Journal" from CSV data, or give an error.
-- XXX currently ignores the string and reads from the file path
parse :: InputOpts -> FilePath -> Text -> ExceptT String IO Journal
parse iopts f t = do
  let rulesfile = mrules_file_ iopts
  let separator = separator_ iopts
  r <- liftIO $ readJournalFromCsv separator rulesfile f t
  case r of Left e  -> throwError e
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
readJournalFromCsv :: Char -> Maybe FilePath -> FilePath -> Text -> IO (Either String Journal)
readJournalFromCsv _ Nothing "-" _ = return $ Left "please use --rules-file when reading CSV from stdin"
readJournalFromCsv separator mrulesfile csvfile csvdata =
 handle (\(e::IOException) -> return $ Left $ show e) $ do

  -- make and throw an IO exception.. which we catch and convert to an Either above ?
  let throwerr = throw . userError

  -- parse the csv rules
  let rulesfile = fromMaybe (rulesFileFor csvfile) mrulesfile
  rulesfileexists <- doesFileExist rulesfile
  rulestext <-
    if rulesfileexists
    then do
      dbg1IO "using conversion rules file" rulesfile
      readFilePortably rulesfile >>= expandIncludes (takeDirectory rulesfile)
    else 
      return $ defaultRulesText rulesfile
  rules <- either throwerr return $ parseAndValidateCsvRules rulesfile rulestext
  dbg2IO "rules" rules

  -- parse the skip directive's value, if any
  let skiplines = case getDirective "skip" rules of
                    Nothing -> 0
                    Just "" -> 1
                    Just s  -> readDef (throwerr $ "could not parse skip value: " ++ show s) s

  -- parse csv
  -- parsec seems to fail if you pass it "-" here TODO: try again with megaparsec
  let parsecfilename = if csvfile == "-" then "(stdin)" else csvfile
  records <- (either throwerr id .
              dbg2 "validateCsv" . validateCsv rules skiplines .
              dbg2 "parseCsv")
             `fmap` parseCsv separator parsecfilename csvdata
  dbg1IO "first 3 csv records" $ take 3 records

  -- identify header lines
  -- let (headerlines, datalines) = identifyHeaderLines records
  --     mfieldnames = lastMay headerlines

  let
    -- convert CSV records to transactions
    txns = snd $ mapAccumL
                   (\pos r ->
                      let
                        SourcePos name line col = pos
                        line' = (mkPos . (+1) . unPos) line
                        pos' = SourcePos name line' col
                      in
                        (pos, transactionFromCsvRecord pos' rules r)
                   )
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

parseCsv :: Char -> FilePath -> Text -> IO (Either String CSV)
parseCsv separator filePath csvdata =
  case filePath of
    "-" -> liftM (parseCassava separator "(stdin)") T.getContents
    _   -> return $ parseCassava separator filePath csvdata

parseCassava :: Char -> FilePath -> Text -> Either String CSV
parseCassava separator path content =
  either (Left . errorBundlePretty) (Right . parseResultToCsv) <$>
  CassavaMP.decodeWith (decodeOptions separator) Cassava.NoHeader path $
  BL.fromStrict $ T.encodeUtf8 content

decodeOptions :: Char -> Cassava.DecodeOptions
decodeOptions separator = Cassava.defaultDecodeOptions {
                      Cassava.decDelimiter = fromIntegral (ord separator)
                    }

parseResultToCsv :: (Foldable t, Functor t) => t (t B.ByteString) -> CSV
parseResultToCsv = toListList . unpackFields
    where
        toListList = toList . fmap toList
        unpackFields  = (fmap . fmap) (T.unpack . T.decodeUtf8)

printCSV :: CSV -> String
printCSV records = unlined (printRecord `map` records)
    where printRecord = concat . intersperse "," . map printField
          printField f = "\"" ++ concatMap escape f ++ "\""
          escape '"' = "\"\""
          escape x = [x]
          unlined = concat . intersperse "\n"

-- | Return the cleaned up and validated CSV data (can be empty), or an error.
validateCsv :: CsvRules -> Int -> Either String CSV -> Either String [CsvRecord]
validateCsv _ _           (Left err) = Left err
validateCsv rules numhdrlines (Right rs) = validate $ applyConditionalSkips $ drop numhdrlines $ filternulls rs
  where
    filternulls = filter (/=[""])
    skipCount r =
      case getEffectiveAssignment rules r "skip" of
        Nothing -> Nothing
        Just "" -> Just 1
        Just "end" -> Just maxBound
        Just x -> Just (read x)
    applyConditionalSkips [] = []
    applyConditionalSkips (r:rest) =
      case skipCount r of
        Nothing -> r:(applyConditionalSkips rest)
        Just cnt -> applyConditionalSkips (drop (cnt-1) rest)
    validate [] = Right []
    validate rs@(_first:_)
      | isJust lessthan2 = let r = fromJust lessthan2 in
          Left $ printf "CSV record %s has less than two fields" (show r)
      -- | isJust different = let r = fromJust different in
      --     Left $ printf "the first CSV record %s has %d fields but %s has %d"
      --       (show first) length1 (show r) (length r)
      | otherwise        = Right rs
      where
        lessthan2 = headMay $ filter ((<2).length) rs
        -- length1   = length first
        -- different = headMay $ filter ((/=length1).length) rs

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
  ,"fields date, description, amount1"
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

type CsvRulesParser a = StateT CsvRules SimpleTextParser a

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

defrules = CsvRules {
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

-- Not used by hledger; just for lib users, 
-- | An pure-exception-throwing IO action that parses this file's content
-- as CSV conversion rules, interpolating any included files first,
-- and runs some extra validation checks.
parseRulesFile :: FilePath -> ExceptT String IO CsvRules
parseRulesFile f =
  liftIO (readFilePortably f >>= expandIncludes (takeDirectory f))
    >>= either throwError return . parseAndValidateCsvRules f

-- | Inline all files referenced by include directives in this hledger CSV rules text, recursively.
-- Included file paths may be relative to the directory of the provided file path.
-- This is done as a pre-parse step to simplify the CSV rules parser.
expandIncludes :: FilePath -> Text -> IO Text
expandIncludes dir content = mapM (expandLine dir) (T.lines content) >>= return . T.unlines
  where
    expandLine dir line =
      case line of
        (T.stripPrefix "include " -> Just f) -> expandIncludes dir' =<< T.readFile f'
          where
            f' = dir </> dropWhile isSpace (T.unpack f)
            dir' = takeDirectory f'
        _ -> return line

-- | An error-throwing IO action that parses this text as CSV conversion rules
-- and runs some extra validation checks. The file path is used in error messages.
parseAndValidateCsvRules :: FilePath -> T.Text -> Either String CsvRules
parseAndValidateCsvRules rulesfile s =
  case parseCsvRules rulesfile s of
    Left err    -> Left $ customErrorBundlePretty err
    Right rules -> first makeFancyParseError $ validateRules rules
  where
    makeFancyParseError :: String -> String
    makeFancyParseError s = 
      parseErrorPretty (FancyError 0 (S.singleton $ ErrorFail s) :: ParseError Text String)

-- | Parse this text as CSV conversion rules. The file path is for error messages.
parseCsvRules :: FilePath -> T.Text -> Either (ParseErrorBundle T.Text CustomErr) CsvRules
-- parseCsvRules rulesfile s = runParser csvrulesfile nullrules{baseAccount=takeBaseName rulesfile} rulesfile s
parseCsvRules rulesfile s =
  runParser (evalStateT rulesp defrules) rulesfile s

-- | Return the validated rules, or an error.
validateRules :: CsvRules -> Either String CsvRules
validateRules rules = do
  unless (isAssigned "date")   $ Left "Please specify (at top level) the date field. Eg: date %1\n"
  unless ((amount && not (amountin || amountout)) ||
          (not amount && (amountin && amountout)) ||
          balance)
    $ Left $ unlines [
       "Please specify (as a top level CSV rule) either the amount1 field,"
      ,"both the amount1-in and amount1-out fields, or the balance1 field. Eg:"
      ,"amount1 %2\n"
      ,"You can also use amount, or both amount-in and amount-out, or balance,"
      ,"though this syntax is considered legacy." 
      ]
  Right rules
  where
    amount    = isAssigned "amount" || isAssigned "amount1"
    amountin  = isAssigned "amount-in" || isAssigned "amount1-in"
    amountout = isAssigned "amount-out" || isAssigned "amount1-out"
    balance   = isAssigned "balance" || isAssigned "balance1"
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
blankorcommentlinep = lift (dbgparse 3 "trying blankorcommentlinep") >> choiceInState [blanklinep, commentlinep]

blanklinep :: CsvRulesParser ()
blanklinep = lift (skipMany spacenonewline) >> newline >> return () <?> "blank line"

commentlinep :: CsvRulesParser ()
commentlinep = lift (skipMany spacenonewline) >> commentcharp >> lift restofline >> return () <?> "comment line"

commentcharp :: CsvRulesParser Char
commentcharp = oneOf (";#*" :: [Char])

directivep :: CsvRulesParser (DirectiveName, String)
directivep = (do
  lift $ dbgparse 3 "trying directive"
  d <- fmap T.unpack $ choiceInState $ map (lift . string . T.pack) directives
  v <- (((char ':' >> lift (many spacenonewline)) <|> lift (some spacenonewline)) >> directivevalp)
       <|> (optional (char ':') >> lift (skipMany spacenonewline) >> lift eolof >> return "")
  return (d, v)
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
directivevalp = anySingle `manyTill` lift eolof

fieldnamelistp :: CsvRulesParser [CsvFieldName]
fieldnamelistp = (do
  lift $ dbgparse 3 "trying fieldnamelist"
  string "fields"
  optional $ char ':'
  lift (skipSome spacenonewline)
  let separator = lift (skipMany spacenonewline) >> char ',' >> lift (skipMany spacenonewline)
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
  lift $ dbgparse 3 "trying fieldassignmentp"
  f <- journalfieldnamep
  v <- choiceInState [ assignmentseparatorp >> fieldvalp
                     , lift eolof >> return ""
                     ]
  return (f,v)
  <?> "field assignment"

journalfieldnamep :: CsvRulesParser String
journalfieldnamep = do
  lift (dbgparse 2 "trying journalfieldnamep")
  T.unpack <$> choiceInState (map (lift . string . T.pack) journalfieldnames)

-- Transaction fields and pseudo fields for CSV conversion.
-- Names must precede any other name they contain, for the parser
-- (amount-in before amount; date2 before date). TODO: fix
journalfieldnames =
  concat [[ "account" ++ i
          ,"amount" ++ i ++ "-in"
          ,"amount" ++ i ++ "-out"
          ,"amount" ++ i
          ,"balance" ++ i
          ,"comment" ++ i
          ,"currency" ++ i
          ] | x <- [1..9], let i = show x]
  ++
  ["amount-in"
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
  ,"skip" -- skip is not really a field, but we list it here to allow conditional rules that skip records
  ]

assignmentseparatorp :: CsvRulesParser ()
assignmentseparatorp = do
  lift $ dbgparse 3 "trying assignmentseparatorp"
  _ <- choiceInState [ lift (skipMany spacenonewline) >> char ':' >> lift (skipMany spacenonewline)
                     , lift (skipSome spacenonewline)
                     ]
  return ()

fieldvalp :: CsvRulesParser String
fieldvalp = do
  lift $ dbgparse 2 "trying fieldvalp"
  anySingle `manyTill` lift eolof

conditionalblockp :: CsvRulesParser ConditionalBlock
conditionalblockp = do
  lift $ dbgparse 3 "trying conditionalblockp"
  string "if" >> lift (skipMany spacenonewline) >> optional newline
  ms <- some recordmatcherp
  as <- many (lift (skipSome spacenonewline) >> fieldassignmentp)
  when (null as) $
    Fail.fail "start of conditional block found, but no assignment rules afterward\n(assignment rules in a conditional block should be indented)\n"
  return (ms, as)
  <?> "conditional block"

recordmatcherp :: CsvRulesParser [String]
recordmatcherp = do
  lift $ dbgparse 2 "trying recordmatcherp"
  -- pos <- currentPos
  _  <- optional (matchoperatorp >> lift (skipMany spacenonewline) >> optional newline)
  ps <- patternsp
  when (null ps) $
    Fail.fail "start of record matcher found, but no patterns afterward\n(patterns should not be indented)\n"
  return ps
  <?> "record matcher"

matchoperatorp :: CsvRulesParser String
matchoperatorp = fmap T.unpack $ choiceInState $ map string
  ["~"
  -- ,"!~"
  -- ,"="
  -- ,"!="
  ]

patternsp :: CsvRulesParser [String]
patternsp = do
  lift $ dbgparse 3 "trying patternsp"
  ps <- many regexp
  return ps

regexp :: CsvRulesParser String
regexp = do
  lift $ dbgparse 3 "trying regexp"
  notFollowedBy matchoperatorp
  c <- lift nonspace
  cs <- anySingle `manyTill` lift eolof
  return $ strip $ c:cs

-- fieldmatcher = do
--   dbgparse 2 "trying fieldmatcher"
--   f <- fromMaybe "all" `fmap` (optional $ do
--          f' <- fieldname
--          lift (skipMany spacenonewline)
--          return f')
--   char '~'
--   lift (skipMany spacenonewline)
--   ps <- patterns
--   let r = "(" ++ intercalate "|" ps ++ ")"
--   return (f,r)
--   <?> "field matcher"

--------------------------------------------------------------------------------
-- Converting CSV records to journal transactions

type CsvRecord = [String]

showRules rules record =
  unlines $ catMaybes [ (("the "++fld++" rule is: ")++) <$> getEffectiveAssignment rules record fld | fld <- journalfieldnames]

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
    mdate2      = render <$> mfieldtemplate "date2"
    mdate2'     = maybe Nothing (maybe (error' $ dateerror "date2" (fromMaybe "" mdate2) mdateformat) Just . mparsedate) mdate2
    dateerror datefield value mdateformat = unlines
      ["error: could not parse \""++value++"\" as a date using date format "++maybe "\"YYYY/M/D\", \"YYYY-M-D\" or \"YYYY.M.D\"" show mdateformat
      , showRecord record
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
              ,"the parse error is:      "++customErrorBundlePretty err
              ]
    code        = singleline $ maybe "" render $ mfieldtemplate "code"
    description = singleline $ maybe "" render $ mfieldtemplate "description"
    comment     = singleline $ maybe "" render $ mfieldtemplate "comment"
    precomment  = singleline $ maybe "" render $ mfieldtemplate "precomment"

    s `or` def  = if null s then def else s
    parsebalance currency n str
      | all isSpace str  = Nothing
      | otherwise = Just $ (either (balanceerror n str) id $ runParser (evalStateT (amountp <* eof) mempty) "" $ T.pack $ (currency++) $ simplifySign str, nullsourcepos)
    balanceerror n str err = error' $ unlines
      ["error: could not parse \""++str++"\" as balance"++n++" amount"
      ,showRecord record
      ,showRules rules record
      ,"the default-currency is: "++fromMaybe "unspecified" mdefaultcurrency
      ,"the parse error is:      "++customErrorBundlePretty err
      ]

    parsePosting' number accountFld amountFld amountInFld amountOutFld balanceFld commentFld =
      let currency = maybe (fromMaybe "" mdefaultcurrency) render $
                      (mfieldtemplate ("currency"++number) `or `mfieldtemplate "currency")
          amount = chooseAmount rules record currency amountFld amountInFld amountOutFld                      
          account' = ((T.pack . render) <$> (mfieldtemplate accountFld
                                           `or` mdirective ("default-account" ++ number)))
          balance = (parsebalance currency number.render) =<< mfieldtemplate balanceFld
          comment = T.pack $ maybe "" render $ mfieldtemplate commentFld
          account =
            case account' of
              Just account -> Just account
              Nothing ->
                -- If we have amount or balance assertion (which implies potential amount change),
                -- but no account name, lets generate "unknown" account name.
                case (amount, balance) of
                  (Just _, _ ) -> Just "unknown"
                  (_, Just _)  -> Just "unknown"
                  (Nothing, Nothing) -> Nothing
          in
        case account of
          Nothing -> Nothing
          Just account -> 
            Just $ posting {paccount=account, pamount=fromMaybe missingmixedamt amount, ptransaction=Just t', pbalanceassertion=toAssertion <$> balance, pcomment = comment}

    parsePosting number =              
      parsePosting' number
      ("account"++number)
      ("amount"++number)
      ("amount"++number++"-in")
      ("amount"++number++"-out")
      ("balance"++number)
      ("comment" ++ number)
      
    postingLegacy = parsePosting' "" "account1" "amount" "amount-in" "amount-out" "balance" "comment1"
    posting1' = parsePosting "1"
    posting1 =
      case (postingLegacy,posting1') of
        (Just legacy, Nothing) -> Just legacy
        (Nothing, Just posting1) -> Just posting1
        (Just legacy, Just posting1) ->
          -- Here we merge legacy fields such as "amount" with "amount1", etc
          -- Account and Comment would be the same by construction
          let balanceassertion = (pbalanceassertion legacy) `or` (pbalanceassertion posting1)
              amount =
                let al = pamount legacy
                    a1 = pamount posting1
                in
                  if al == a1 then al
                  else
                    case (isZeroMixedAmount al, isZeroMixedAmount a1) of
                      (True, _) -> a1
                      (False, True) -> al
                      (False, False) ->
                        error' $ unlines [ "amount/amount-in/amount-out and amount1/amount1-in/amount1-out produced conflicting values"
                                         , showRecord record
                                         , showRules rules record
                                         , "amount/amount-in/amount-out is " ++ showMixedAmount al
                                         , "amount1/amount1-in/amount1-out is " ++ showMixedAmount a1
                                         ]
          in Just $ posting {paccount=paccount posting1, pamount=amount, ptransaction=Just t', pbalanceassertion=balanceassertion, pcomment = pcomment posting1}
        (Nothing, Nothing) -> Nothing
    postings' = catMaybes $ posting1:[ parsePosting i | x<-[2..9], let i = show x]
    postings =
      case postings' of
        -- To be compatible with the behavior of the old code which allowed two postings only, we enforce
        -- second posting when rules generated just one of them.
        [posting1] -> [posting1,posting{paccount="unknown", pamount=missingmixedamt, ptransaction=Just t'}]
        _ -> postings'
        
    balanced = balanceTransaction Nothing t'
    t =
      case balanced of
        Left _ -> t'
        Right balanced ->
          -- If we managed to balance transaction, lets infer better names for all "unknown" accounts
          t' {tpostings =
              [ originalPosting {paccount=newAccount}
              | (originalPosting,p) <- zip postings (tpostings balanced)
              , let account = paccount p
              , let newAccount =
                      if account/="unknown"
                      then account
                      else case isNegativeMixedAmount (pamount p) of
                             Just True -> "income:unknown"
                             Just False -> "expense:unknown"
                             _ -> "unknown"
              ]}
    -- build the transaction
    t' = nulltransaction{
      tsourcepos               = genericSourcePos sourcepos,
      tdate                    = date',
      tdate2                   = mdate2',
      tstatus                  = status,
      tcode                    = T.pack code,
      tdescription             = T.pack description,
      tcomment                 = T.pack comment,
      tprecedingcomment        = T.pack precomment,
      tpostings                = postings
      }
    toAssertion (a, b) = assertion{
      baamount   = a,
      baposition = b
      }

chooseAmount :: CsvRules -> CsvRecord -> String -> String -> String -> String -> Maybe MixedAmount
chooseAmount rules record currency amountFld amountInFld amountOutFld =
 let
   mamount    = getEffectiveAssignment rules record amountFld
   mamountin  = getEffectiveAssignment rules record amountInFld
   mamountout = getEffectiveAssignment rules record amountOutFld
   parse  amt = notZero =<< (parseAmount currency <$> notEmpty =<< (strip . renderTemplate rules record) <$> amt)
 in
  case (parse mamount, parse mamountin, parse mamountout) of
    (Nothing, Nothing, Nothing) -> Nothing
    (Just a,  Nothing, Nothing) -> Just a
    (Nothing, Just i,  Nothing) -> Just i
    (Nothing, Nothing, Just o)  -> Just $ negate o
    (Nothing, Just i,  Just o)  -> error' $    "both "++amountInFld++" and "++amountOutFld++" have a value\n"
                                            ++ "    "++amountInFld++": "  ++ show i ++ "\n"
                                            ++ "    "++amountOutFld++": " ++ show o ++ "\n"
                                            ++ "    record: "     ++ showRecord record
    _                           -> error' $    "found values for "++amountFld++" and for "++amountInFld++"/"++amountOutFld++"\n"
                                            ++ "please use either "++amountFld++" or "++amountInFld++"/"++amountOutFld++"\n"
                                            ++ "    record: " ++ showRecord record
 where
   notZero amt = if isZeroMixedAmount amt then Nothing else Just amt
   notEmpty str = if str=="" then Nothing else Just str

   parseAmount currency amountstr =
     either (amounterror amountstr) (Mixed . (:[]))
     <$> runParser (evalStateT (amountp <* eof) mempty) ""
     <$> T.pack
     <$> (currency++)
     <$> simplifySign
     <$> amountstr

   amounterror amountstr err = error' $ unlines
     ["error: could not parse \""++fromJust amountstr++"\" as an amount"
     ,showRecord record
     ,showRules rules record
     ,"the default-currency is: "++fromMaybe "unspecified" (getDirective "default-currency" rules)
     ,"the parse error is:      "++customErrorBundlePretty err
     ,"you may need to "
      ++"change your amount or currency rules, "
      ++"or add or change your skip rule"
     ]

type CsvAmountString = String

-- | Canonicalise the sign in a CSV amount string.
-- Such strings can have a minus sign, negating parentheses,
-- or any two of these (which cancels out).
--
-- >>> simplifySign "1"
-- "1"
-- >>> simplifySign "-1"
-- "-1"
-- >>> simplifySign "(1)"
-- "-1"
-- >>> simplifySign "--1"
-- "1"
-- >>> simplifySign "-(1)"
-- "1"
-- >>> simplifySign "(-1)"
-- "1"
-- >>> simplifySign "((1))"
-- "1"
simplifySign :: CsvAmountString -> CsvAmountString
simplifySign ('(':s) | lastMay s == Just ')' = simplifySign $ negateStr $ init s
simplifySign ('-':'(':s) | lastMay s == Just ')' = simplifySign $ init s
simplifySign ('-':'-':s) = s
simplifySign s = s

negateStr :: String -> String
negateStr ('-':s) = s
negateStr s       = '-':s

-- | Show a (approximate) recreation of the original CSV record.
showRecord :: CsvRecord -> String
showRecord r = "the CSV record is:       "++intercalate "," (map show r)

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

-- | Render a field assigment's template, possibly interpolating referenced
-- CSV field values. Outer whitespace is removed from interpolated values.
renderTemplate ::  CsvRules -> CsvRecord -> FieldTemplate -> String
renderTemplate rules record t = regexReplaceBy "%[A-z0-9_-]+" replace t
  where
    replace ('%':pat) = maybe pat (\i -> strip $ atDef "" record (i-1)) mindex
      where
        mindex | all isDigit pat = readMay pat
               | otherwise       = lookup (map toLower pat) $ rcsvfieldindexes rules
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

tests_CsvReader = tests "CsvReader" [
   tests "parseCsvRules" [
     test "empty file" $
      parseCsvRules "unknown" "" `is` Right defrules
    ]
  ,tests "rulesp" [
     test "trailing comments" $
      parseWithState' defrules rulesp "skip\n# \n#\n" `is` Right defrules{rdirectives = [("skip","")]}

    ,test "trailing blank lines" $
      parseWithState' defrules rulesp "skip\n\n  \n" `is` (Right defrules{rdirectives = [("skip","")]})

    ,test "no final newline" $
      parseWithState' defrules rulesp "skip" `is` (Right defrules{rdirectives=[("skip","")]})

    ,test "assignment with empty value" $
      parseWithState' defrules rulesp "account1 \nif foo\n  account2 foo\n" `is`
        (Right defrules{rassignments = [("account1","")], rconditionalblocks = [([["foo"]],[("account2","foo")])]})

    ]
  ]
