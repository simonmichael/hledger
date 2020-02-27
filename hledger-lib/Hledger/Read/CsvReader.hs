{-|

A reader for CSV data, using an extra rules file to help interpret the data.

-}
-- Lots of haddocks in this file are for non-exported types.
-- Here's a command that will render them:
-- stack haddock hledger-lib --fast --no-haddock-deps --haddock-arguments='--ignore-all-exports' --open

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

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
import Hledger.Read.Common (Reader(..),InputOpts(..),amountp, statusp, genericSourcePos, finaliseJournal)

type CSV = [Record]

type Record = [Field]

type Field = String

reader :: Reader
reader = Reader
  {rFormat     = "csv"
  ,rExtensions = ["csv","tsv","ssv"]
  ,rParser     = parse
  ,rExperimental = False
  }

-- | Parse and post-process a "Journal" from CSV data, or give an error.
-- Does not check balance assertions.
-- XXX currently ignores the provided data, reads it from the file path instead.
parse :: InputOpts -> FilePath -> Text -> ExceptT String IO Journal
parse iopts f t = do
  let rulesfile = mrules_file_ iopts
  r <- liftIO $ readJournalFromCsv rulesfile f t
  case r of Left e   -> throwError e
            Right pj -> finaliseJournal iopts{ignore_assertions_=True} f t pj'
              where
                -- finaliseJournal assumes the journal's items are
                -- reversed, as produced by JournalReader's parser.
                -- But here they are already properly ordered. So we'd
                -- better preemptively reverse them once more. XXX inefficient
                pj' = journalReverse pj

-- | Parse special separator names TAB and SPACE, or return the first
-- character. Return Nothing on empty string
parseSeparator :: String -> Maybe Char
parseSeparator = specials . map toLower
  where specials "space" = Just ' '
        specials "tab"   = Just '\t'
        specials (x:_)   = Just x
        specials []      = Nothing

-- | Read a Journal from the given CSV data (and filename, used for error
-- messages), or return an error. Proceed as follows:
--
-- 1. parse CSV conversion rules from the specified rules file, or from
--    the default rules file for the specified CSV file, if it exists,
--    or throw a parse error; if it doesn't exist, use built-in default rules
--
-- 2. parse the CSV data, or throw a parse error
--
-- 3. convert the CSV records to transactions using the rules
--
-- 4. if the rules file didn't exist, create it with the default rules and filename
--
-- 5. return the transactions as a Journal
-- 
readJournalFromCsv :: Maybe FilePath -> FilePath -> Text -> IO (Either String Journal)
readJournalFromCsv Nothing "-" _ = return $ Left "please use --rules-file when reading CSV from stdin"
readJournalFromCsv mrulesfile csvfile csvdata =
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
  let separator = fromMaybe ',' (getDirective "separator" rules >>= parseSeparator)
  dbg2IO "separator" separator
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
    -- First, if the CSV records seem to be most-recent-first (because
    -- there's an explicit "newest-first" directive, or there's more
    -- than one date and the first date is more recent than the last):
    -- reverse them to get same-date transactions ordered chronologically.
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
      case (getEffectiveAssignment rules r "end", getEffectiveAssignment rules r "skip") of
        (Nothing, Nothing) -> Nothing
        (Just _, _) -> Just maxBound
        (Nothing, Just "") -> Just 1
        (Nothing, Just x) -> Just (read x)
    applyConditionalSkips [] = []
    applyConditionalSkips (r:rest) =
      case skipCount r of
        Nothing -> r:(applyConditionalSkips rest)
        Just cnt -> applyConditionalSkips (drop (cnt-1) rest)
    validate [] = Right []
    validate rs@(_first:_)
      | isJust lessthan2 = let r = fromJust lessthan2 in
          Left $ printf "CSV record %s has less than two fields" (show r)
      | otherwise        = Right rs
      where
        lessthan2 = headMay $ filter ((<2).length) rs

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
    -- ^ top-level rules, as (keyword, value) pairs
  rcsvfieldindexes   :: [(CsvFieldName, CsvFieldIndex)],
    -- ^ csv field names and their column number, if declared by a fields list
  rassignments       :: [(HledgerFieldName, FieldTemplate)],
    -- ^ top-level assignments to hledger fields, as (field name, value template) pairs
  rconditionalblocks :: [ConditionalBlock]
    -- ^ conditional blocks, which containing additional assignments/rules to apply to matched csv records
} deriving (Show, Eq)

type CsvRulesParser a = StateT CsvRules SimpleTextParser a

-- | The keyword of a CSV rule - "fields", "skip", "if", etc.
type DirectiveName    = String
-- | CSV field name.
type CsvFieldName     = String
-- | 1-based CSV column number.
type CsvFieldIndex    = Int
-- | Percent symbol followed by a CSV field name or column number. Eg: %date, %1.
type CsvFieldReference = String
-- | One of the standard hledger fields or pseudo-fields that can be assigned to.
-- Eg date, account1, amount, amount1-in, date-format.
type HledgerFieldName = String
-- | A text value to be assigned to a hledger field, possibly
-- containing csv field references to be interpolated.
type FieldTemplate    = String
-- | A strptime date parsing pattern, as supported by Data.Time.Format.
type DateFormat       = String
-- | A regular expression.
type RegexpPattern    = String

-- | A single test for matching a CSV record, in one way or another.
data Matcher =
    RecordMatcher RegexpPattern                   -- ^ match if this regexp matches the overall CSV record
  | FieldMatcher CsvFieldReference RegexpPattern  -- ^ match if this regexp matches the referenced CSV field's value
  deriving (Show, Eq)

-- | A conditional block: a set of CSV record matchers, and a sequence
-- of rules which will be enabled only if one or more of the matchers
-- succeeds.
--
-- Three types of rule are allowed inside conditional blocks: field
-- assignments, skip, end. (A skip or end rule is stored as if it was
-- a field assignment, and executed in validateCsv. XXX)
data ConditionalBlock = CB {
   cbMatchers    :: [Matcher]
  ,cbAssignments :: [(HledgerFieldName, FieldTemplate)]
  } deriving (Show, Eq)

defrules = CsvRules {
  rdirectives=[],
  rcsvfieldindexes=[],
  rassignments=[],
  rconditionalblocks=[]
}

addDirective :: (DirectiveName, String) -> CsvRules -> CsvRules
addDirective d r = r{rdirectives=d:rdirectives r}

addAssignment :: (HledgerFieldName, FieldTemplate) -> CsvRules -> CsvRules
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
    makeFancyParseError errorString =
      parseErrorPretty (FancyError 0 (S.singleton $ ErrorFail errorString) :: ParseError Text String)

-- | Parse this text as CSV conversion rules. The file path is for error messages.
parseCsvRules :: FilePath -> T.Text -> Either (ParseErrorBundle T.Text CustomErr) CsvRules
-- parseCsvRules rulesfile s = runParser csvrulesfile nullrules{baseAccount=takeBaseName rulesfile} rulesfile s
parseCsvRules rulesfile s =
  runParser (evalStateT rulesp defrules) rulesfile s

-- | Return the validated rules, or an error.
validateRules :: CsvRules -> Either String CsvRules
validateRules rules = do
  unless (isAssigned "date")   $ Left "Please specify (at top level) the date field. Eg: date %1\n"
  Right rules
  where
    isAssigned f = isJust $ getEffectiveAssignment rules [] f

-- parsers

rulesp :: CsvRulesParser CsvRules
rulesp = do
  _ <- many $ choiceInState
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

directives :: [String]
directives =
  ["date-format"
  ,"separator"
  -- ,"default-account1"
  -- ,"default-currency"
  -- ,"skip-lines" -- old
  ,"skip"
  ,"newest-first"
   -- ,"base-account"
   -- ,"base-currency"
  , "balance-type"
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

fieldassignmentp :: CsvRulesParser (HledgerFieldName, FieldTemplate)
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
  ,"skip" -- skip and end are not really fields, but we list it here to allow conditional rules that skip records
  ,"end"
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

-- A conditional block: one or more matchers, one per line, followed by one or more indented rules.
conditionalblockp :: CsvRulesParser ConditionalBlock
conditionalblockp = do
  lift $ dbgparse 3 "trying conditionalblockp"
  string "if" >> lift (skipMany spacenonewline) >> optional newline
  ms <- some matcherp
  as <- many (try $ lift (skipSome spacenonewline) >> fieldassignmentp)
  when (null as) $
    Fail.fail "start of conditional block found, but no assignment rules afterward\n(assignment rules in a conditional block should be indented)\n"
  return $ CB{cbMatchers=ms, cbAssignments=as}
  <?> "conditional block"

-- A single matcher, on one line.
matcherp :: CsvRulesParser Matcher
matcherp = try fieldmatcherp <|> recordmatcherp

-- A single whole-record matcher.
-- A pattern on the whole line, not beginning with a csv field reference.
recordmatcherp :: CsvRulesParser Matcher
recordmatcherp = do
  lift $ dbgparse 2 "trying matcherp"
  -- pos <- currentPos
  -- _  <- optional (matchoperatorp >> lift (skipMany spacenonewline) >> optional newline)
  r <- regexp
  -- when (null ps) $
  --   Fail.fail "start of record matcher found, but no patterns afterward\n(patterns should not be indented)\n"
  return $ RecordMatcher r
  <?> "record matcher"

-- | A single matcher for a specific field. A csv field reference
-- (like %date or %1), and a pattern on the rest of the line,
-- optionally space-separated. Eg:
-- %description chez jacques
fieldmatcherp :: CsvRulesParser Matcher
fieldmatcherp = do
  lift $ dbgparse 2 "trying fieldmatcher"
  -- An optional fieldname (default: "all")
  -- f <- fromMaybe "all" `fmap` (optional $ do
  --        f' <- fieldnamep
  --        lift (skipMany spacenonewline)
  --        return f')
  f <- csvfieldreferencep <* lift (skipMany spacenonewline)
  -- optional operator.. just ~ (case insensitive infix regex) for now
  -- _op <- fromMaybe "~" <$> optional matchoperatorp
  lift (skipMany spacenonewline)
  r <- regexp
  return $ FieldMatcher f r
  <?> "field matcher"

csvfieldreferencep :: CsvRulesParser CsvFieldReference
csvfieldreferencep = do
  lift $ dbgparse 3 "trying csvfieldreferencep"
  char '%'
  f <- fieldnamep
  return $ '%' : quoteIfNeeded f

-- A single regular expression
regexp :: CsvRulesParser RegexpPattern
regexp = do
  lift $ dbgparse 3 "trying regexp"
  -- notFollowedBy matchoperatorp
  c <- lift nonspace
  cs <- anySingle `manyTill` lift eolof
  return $ strip $ c:cs

-- -- A match operator, indicating the type of match to perform.
-- -- Currently just ~ meaning case insensitive infix regex match.
-- matchoperatorp :: CsvRulesParser String
-- matchoperatorp = fmap T.unpack $ choiceInState $ map string
--   ["~"
--   -- ,"!~"
--   -- ,"="
--   -- ,"!="
--   ]

--------------------------------------------------------------------------------
-- Converting CSV records to journal transactions

type CsvRecord = [String]

showRules rules record =
  unlines $ catMaybes [ (("the "++fld++" rule is: ")++) <$> getEffectiveAssignment rules record fld | fld <- journalfieldnames]

-- warning: 200 line beast ahead
transactionFromCsvRecord :: SourcePos -> CsvRules -> CsvRecord -> Transaction
transactionFromCsvRecord sourcepos rules record = t
  where
    ----------------------------------------------------------------------
    -- 1. Some helpers

    s `or` def       = if null s then def else s
    mdirective       = (`getDirective` rules)
    mfieldtemplate   = getEffectiveAssignment rules record
    render           = renderTemplate rules record
    mparsedate       = parseDateWithFormatOrDefaultFormats (mdirective "date-format")

    ----------------------------------------------------------------------
    -- 2. Gather the values needed for the transaction itself, by evaluating
    -- the field assignment rules using the CSV record's data, and parsing a
    -- bit more where needed, into dates, amounts, status..

    mdefaultcurrency = mdirective "default-currency"
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
      where
        mskip = mdirective "skip"
    status =
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

    ----------------------------------------------------------------------
    -- 3. Generate the postings

    -- Helper to generate posting N, if sufficient fields have been assigned
    -- for it. N is provided as a string.
    mkPosting ::
      String -> HledgerFieldName -> HledgerFieldName -> HledgerFieldName ->
      HledgerFieldName -> HledgerFieldName -> HledgerFieldName ->
      Maybe (Posting, Bool)
    mkPosting number accountFld amountFld amountInFld amountOutFld balanceFld commentFld =
      let currency = maybe (fromMaybe "" mdefaultcurrency) render $
                      (mfieldtemplate ("currency"++number) `or `mfieldtemplate "currency")
          mamount = chooseAmount rules record currency amountFld amountInFld amountOutFld
          mbalance :: Maybe (Amount, GenericSourcePos) =
            (parsebalance currency number.render) =<< mfieldtemplate balanceFld
            where
              parsebalance currency n str
                | all isSpace str = Nothing
                | otherwise = Just
                    (either (balanceerror n str) id $
                      runParser (evalStateT (amountp <* eof) mempty) "" $
                      T.pack $ (currency++) $ simplifySign str
                    ,nullsourcepos)  -- XXX parse position to show when assertion fails,
                                     -- the csv record's line number would be good
                where
                  balanceerror n str err = error' $ unlines
                    ["error: could not parse \""++str++"\" as balance"++n++" amount"
                    ,showRecord record
                    ,showRules rules record
                    ,"the default-currency is: "++fromMaybe "unspecified" mdefaultcurrency
                    ,"the parse error is:      "++customErrorBundlePretty err
                    ]
          comment = T.pack $ maybe "" render $ mfieldtemplate commentFld
          maccount = ((T.pack . render) <$>
                       (mfieldtemplate accountFld `or` mdirective ("default-account" ++ number)))
          -- figure out the account name to use for this posting, if any, and
          -- whether it is the default unknown account, which may be improved
          -- later, or an explicitly set account, which may not.
          maccountAndIsFinal :: Maybe (AccountName, Bool) =
            case maccount of
              -- accountN is set to the empty string - no posting will be generated
              Just "" -> Nothing
              -- accountN is set (possibly to "expenses:unknown"! cf #1192) -
              -- mark it final
              Just a  -> Just (a, True)
              -- accountN is unset
              Nothing ->
                case (mamount, mbalance) of
                  -- amountN is set, or implied by balanceN - set accountN to
                  -- the default unknown account ("expenses:unknown") and
                  -- allow it to be improved later
                  (Just _, _) -> Just (unknownExpenseAccount, False)
                  (_, Just _) -> Just (unknownExpenseAccount, False)
                  -- amountN is also unset - no posting will be generated
                  (Nothing, Nothing) -> Nothing
      in
        -- if there's an account N, make a posting N
        case maccountAndIsFinal of
          Nothing            -> Nothing
          Just (acct, final) ->
            Just (posting{paccount          = accountNameWithoutPostingType acct
                         ,pamount           = fromMaybe missingmixedamt mamount
                         ,ptransaction      = Just t
                         ,pbalanceassertion = mkBalanceAssertion rules record <$> mbalance
                         ,pcomment          = comment
                         ,ptype             = accountNamePostingType acct}
                 ,final)

    -- Make posting 1 if possible, with special support for old syntax, to
    -- support pre-1.16 rules.
    posting1 = mkPosting "1"
               ("account1" `withAlias` "account")
               ("amount1" `withAlias` "amount")
               ("amount1-in" `withAlias` "amount-in")
               ("amount1-out" `withAlias` "amount-out")
               ("balance1" `withAlias` "balance")
               "comment1" -- comment1 does not have legacy alias
      where
        withAlias fld alias =
          case (mfieldtemplate fld, mfieldtemplate alias) of
            (Just fld, Just alias) -> error' $ unlines
              [ "error: both \"" ++ fld ++ "\" and \"" ++ alias ++ "\" have values."
              , showRecord record
              , showRules rules record
              ]
            (Nothing, Just _) -> alias
            (_, Nothing)      -> fld

    -- Make other postings where possible, and gather all that were generated.
    postings = catMaybes $ posting1 : otherpostings
      where
        otherpostings = [mkPostingN i | x<-[2..9], let i = show x]
        mkPostingN n = mkPosting n
                         ("account"++n) ("amount"++n) ("amount"++n++"-in")
                         ("amount"++n++"-out") ("balance"++n) ("comment"++n)

    -- Adjust the postings to mimic some pre-1.16 behaviour, for compatibility.
    -- And also, wherever default "unknown" accounts were used,
    -- refine these based on the sign of the final posting amount.
    postings' =
      case postings of
        -- when rules generate just one posting, and it's a type that needs to
        -- be balanced, generate the second posting to balance it.
        [(p1,final)] ->
          if ptype p1 == VirtualPosting
          then [p1']
          else [p1', p2]
            where
              p1' = (if final then id else improveUnknownAccountName) p1
              p2 = improveUnknownAccountName
                   nullposting{paccount=unknownExpenseAccount
                              ,pamount=costOfMixedAmount (-pamount p1)
                              ,ptransaction=Just t}

        -- when rules generate exactly two postings, and only the second has
        -- no amount, give it the balancing amount.
        [(p1,final1), (p2,final2)] ->
          case (pamount p1 == missingmixedamt, pamount p2 == missingmixedamt) of
            (False, True) -> [p1',p2']
              where p2' = (if final2 then id else improveUnknownAccountName)
                          p2{pamount=costOfMixedAmount(-(pamount p1))}
            _  -> [p1', p2']
              where p2' = (if final2 then id else improveUnknownAccountName) p2
            where
              p1' = (if final1 then id else improveUnknownAccountName) p1

        -- otherwise, just refine any unknown account names.
        ps -> [(if final then id else improveUnknownAccountName) p | (p,final) <- ps]

    ----------------------------------------------------------------------
    -- 4. Build the transaction (and name it, so postings can reference it).

    t = nulltransaction{
           tsourcepos        = genericSourcePos sourcepos  -- the CSV line number
          ,tdate             = date'
          ,tdate2            = mdate2'
          ,tstatus           = status
          ,tcode             = T.pack code
          ,tdescription      = T.pack description
          ,tcomment          = T.pack comment
          ,tprecedingcomment = T.pack precomment
          ,tpostings         = postings'
          }  

-- | Default account names to use when needed.
unknownExpenseAccount = "expenses:unknown"
unknownIncomeAccount  = "income:unknown"

-- | If this posting has the "expenses:unknown" account name,
-- replace that with "income:unknown" if the amount is negative.
-- The posting's amount should be explicit.
improveUnknownAccountName p@Posting{..}
  | paccount == unknownExpenseAccount
    && fromMaybe False (isNegativeMixedAmount pamount) = p{paccount=unknownIncomeAccount}
  | otherwise = p

-- | Make a balance assertion for the given amount, with the given parse
-- position (to be shown in assertion failures), with the assertion type
-- possibly set by a balance-type rule.
-- The CSV rules and current record are also provided, to be shown in case
-- balance-type's argument is bad (XXX refactor).
mkBalanceAssertion :: CsvRules -> Record -> (Amount, GenericSourcePos) -> BalanceAssertion
mkBalanceAssertion rules record (amt, pos) = assrt{baamount=amt, baposition=pos}
  where
    assrt =
      case getDirective "balance-type" rules of
        Nothing    -> nullassertion
        Just "="   -> nullassertion
        Just "=="  -> nullassertion{batotal=True}
        Just "=*"  -> nullassertion{bainclusive=True}
        Just "==*" -> nullassertion{batotal=True, bainclusive=True}
        Just x     -> error' $ unlines
          [ "balance-type \"" ++ x ++"\" is invalid. Use =, ==, =* or ==*."
          , showRecord record
          , showRules rules record
          ]

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

-- | Given the conversion rules, a CSV record and a hledger field name, find
-- the template value ultimately assigned to this field, if any, by a field
-- assignment at top level or in a conditional block matching this record.
--
-- Note conditional blocks' patterns are matched against an approximation of the
-- CSV record: all the field values, without enclosing quotes, comma-separated.
--
getEffectiveAssignment :: CsvRules -> CsvRecord -> HledgerFieldName -> Maybe FieldTemplate
getEffectiveAssignment rules record f = lastMay $ map snd $ assignments
  where
    -- all active assignments to field f, in order
    assignments = dbg2 "assignments" $ filter ((==f).fst) $ toplevelassignments ++ conditionalassignments
      where
        -- all top level field assignments
        toplevelassignments    = rassignments rules
        -- all field assignments in conditional blocks assigning to field f and active for the current csv record
        conditionalassignments = concatMap cbAssignments $ filter isBlockActive $ blocksAssigning f
          where
            -- all conditional blocks which can potentially assign field f
            blocksAssigning f = filter (any ((==f).fst) . cbAssignments) $ rconditionalblocks rules
            -- does this conditional block match the current csv record ?
            isBlockActive :: ConditionalBlock -> Bool
            isBlockActive CB{..} = any matcherMatches cbMatchers
              where
                -- does this individual matcher match the current csv record ?
                matcherMatches :: Matcher -> Bool
                matcherMatches (RecordMatcher pat) = regexMatchesCI pat wholecsvline
                  where
                    -- a synthetic whole CSV record to match against; note, it has
                    -- no quotes enclosing fields, and is always comma-separated,
                    -- so may differ from the actual record, and may not be valid CSV.
                    wholecsvline = dbg3 "wholecsvline" $ intercalate "," record
                matcherMatches (FieldMatcher csvfieldref pat) = regexMatchesCI pat csvfieldvalue
                  where
                    -- the value of the referenced CSV field to match against.
                    csvfieldvalue = dbg3 "csvfieldvalue" $ replaceCsvFieldReference rules record csvfieldref

-- | Render a field assigment's template, possibly interpolating referenced
-- CSV field values. Outer whitespace is removed from interpolated values.
renderTemplate ::  CsvRules -> CsvRecord -> FieldTemplate -> String
renderTemplate rules record t = regexReplaceBy "%[A-z0-9_-]+" (replaceCsvFieldReference rules record) t

-- | Replace something that looks like a reference to a csv field ("%date" or "%1)
-- with that field's value. If it doesn't look like a field reference, or if we
-- can't find such a field, leave it unchanged.
replaceCsvFieldReference :: CsvRules -> CsvRecord -> CsvFieldReference -> String
replaceCsvFieldReference rules record s@('%':fieldname) = fromMaybe s $ csvFieldValue rules record fieldname
replaceCsvFieldReference _ _ s = s

-- | Get the (whitespace-stripped) value of a CSV field, identified by its name or
-- column number, ("date" or "1"), from the given CSV record, if such a field exists.
csvFieldValue :: CsvRules -> CsvRecord -> CsvFieldName -> Maybe String
csvFieldValue rules record fieldname = do
  fieldindex <- if | all isDigit fieldname -> readMay fieldname
                   | otherwise             -> lookup (map toLower fieldname) $ rcsvfieldindexes rules
  fieldvalue <- strip <$> atMay record (fieldindex-1)
  return fieldvalue

-- | Parse the date string using the specified date-format, or if unspecified try these default formats:
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
      parseCsvRules "unknown" "" @?= Right defrules
   ]
  ,tests "rulesp" [
     test "trailing comments" $
      parseWithState' defrules rulesp "skip\n# \n#\n" @?= Right defrules{rdirectives = [("skip","")]}

    ,test "trailing blank lines" $
      parseWithState' defrules rulesp "skip\n\n  \n" @?= (Right defrules{rdirectives = [("skip","")]})

    ,test "no final newline" $
      parseWithState' defrules rulesp "skip" @?= (Right defrules{rdirectives=[("skip","")]})

    ,test "assignment with empty value" $
      parseWithState' defrules rulesp "account1 \nif foo\n  account2 foo\n" @?=
        (Right defrules{rassignments = [("account1","")], rconditionalblocks = [CB{cbMatchers=[RecordMatcher "foo"],cbAssignments=[("account2","foo")]}]})
   ]
  ,tests "conditionalblockp" [
    test "space after conditional" $ -- #1120
      parseWithState' defrules conditionalblockp "if a\n account2 b\n \n" @?=
        (Right $ CB{cbMatchers=[RecordMatcher "a"],cbAssignments=[("account2","b")]})

  ,tests "csvfieldreferencep" [
    test "number" $ parseWithState' defrules csvfieldreferencep "%1" @?= (Right "%1")
   ,test "name" $ parseWithState' defrules csvfieldreferencep "%date" @?= (Right "%date")
   ,test "quoted name" $ parseWithState' defrules csvfieldreferencep "%\"csv date\"" @?= (Right "%\"csv date\"")
   ]

  ,tests "matcherp" [

    test "recordmatcherp" $
      parseWithState' defrules matcherp "A A\n" @?= (Right $ RecordMatcher "A A")

   ,test "fieldmatcherp.starts-with-%" $
      parseWithState' defrules matcherp "description A A\n" @?= (Right $ RecordMatcher "description A A")

   ,test "fieldmatcherp" $
      parseWithState' defrules matcherp "%description A A\n" @?= (Right $ FieldMatcher "%description" "A A")

   -- ,test "fieldmatcherp with operator" $
   --    parseWithState' defrules matcherp "%description ~ A A\n" @?= (Right $ FieldMatcher "%description" "A A")

   ]

  ,tests "getEffectiveAssignment" [
    let rules = defrules{rcsvfieldindexes=[("csvdate",1)],rassignments=[("date","%csvdate")]}
    
    in test "toplevel" $ getEffectiveAssignment rules ["a","b"] "date" @?= (Just "%csvdate")

   ,let rules = defrules{rcsvfieldindexes=[("csvdate",1)], rconditionalblocks=[CB [FieldMatcher "%csvdate" "a"] [("date","%csvdate")]]}
    in test "conditional" $ getEffectiveAssignment rules ["a","b"] "date" @?= (Just "%csvdate")
       
   ]

  ]

 ]
