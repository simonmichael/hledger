--- * module
--- ** doc
-- In Emacs, use TAB on lines beginning with "-- *" to collapse/expand sections.
{-|

A reader for a CSV rules file. 
This reads the actual data from a file specified by a `source` rule
or from a similarly-named file in the same directory.

Most of the code for reading rules files and csv files is in this module.
-}
-- Lots of haddocks in this file are for non-exported types.
-- Here's a command that will render them:
-- stack haddock hledger-lib --fast --no-haddock-deps --haddock-arguments='--ignore-all-exports' --open

--- ** language
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

--- ** exports
module Hledger.Read.RulesReader (
  -- * Reader
  reader,
  -- * Misc.
  readJournalFromCsv,
  -- readRulesFile,
  -- parseCsvRules,
  -- validateCsvRules,
  -- CsvRules,
  dataFileFor,
  rulesFileFor,
  -- * Tests
  tests_RulesReader,
)
where

--- ** imports
import Prelude hiding (Applicative(..))
import Control.Applicative (Applicative(..))
import Control.Monad              (unless, when, void)
import Control.Monad.Except       (ExceptT(..), liftEither, throwError)
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, get, modify', evalStateT)
import Control.Monad.Trans.Class  (lift)
import Data.Char                  (toLower, isDigit, isSpace, isAlphaNum, ord)
import Data.Bifunctor             (first)
import Data.Functor               ((<&>))
import Data.List (elemIndex, foldl', mapAccumL, nub, sortOn)
import Data.List.Extra (groupOn)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.MemoUgly (memo)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time ( Day, TimeZone, UTCTime, LocalTime, ZonedTime(ZonedTime),
  defaultTimeLocale, getCurrentTimeZone, localDay, parseTimeM, utcToLocalTime, localTimeToUTC, zonedTimeToUTC)
import Safe (atMay, headMay, lastMay, readMay)
import System.FilePath ((</>), takeDirectory, takeExtension, stripExtension, takeFileName)
import qualified Data.Csv as Cassava
import qualified Data.Csv.Parser.Megaparsec as CassavaMegaparsec
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (asum, toList)
import Text.Megaparsec hiding (match, parse)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Custom (parseErrorAt)
import Text.Printf (printf)

import Hledger.Data
import Hledger.Utils
import Hledger.Read.Common (aliasesFromOpts, Reader(..), InputOpts(..), amountp, statusp, journalFinalise, accountnamep, commenttagsp )
import Hledger.Read.CsvUtils
import System.Directory (doesFileExist, getHomeDirectory)
import Data.Either (fromRight)

--- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

--- ** reader
_READER__________________________________________ = undefined  -- VSCode outline separator


reader :: MonadIO m => Reader m
reader = Reader
  {rFormat     = "rules"
  ,rExtensions = ["rules"]
  ,rReadFn     = parse
  ,rParser     = error' "sorry, rules files can't be included"  -- PARTIAL:
  }

isFileName f = takeFileName f == f

getDownloadDir = do
  home <- getHomeDirectory
  return $ home </> "Downloads"  -- XXX

-- | Parse and post-process a "Journal" from the given rules file path, or give an error.
-- A data file is inferred from the @source@ rule, otherwise from a similarly-named file
-- in the same directory.
-- The source rule can specify a glob pattern and supports ~ for home directory.
-- If it is a bare filename it will be relative to the defaut download directory
-- on this system. If is a relative file path it will be relative to the rules
-- file's directory. When a glob pattern matches multiple files, the alphabetically
-- last is used. (Eg in case of multiple numbered downloads, the highest-numbered
-- will be used.)
-- The provided text, or a --rules-file option, are ignored by this reader.
-- Balance assertions are not checked.
parse :: InputOpts -> FilePath -> Text -> ExceptT String IO Journal
parse iopts f _ = do
  rules <- readRulesFile $ dbg4 "reading rules file" f
  -- XXX higher-than usual debug level for file reading to bypass excessive noise from elsewhere, normally 6 or 7
  mdatafile <- liftIO $ do
    dldir <- getDownloadDir
    let rulesdir = takeDirectory f
    let msource = T.unpack <$> getDirective "source" rules
    fs <- case msource of
            Just src -> expandGlob dir (dbg4 "source" src) >>= sortByModTime <&> dbg4 ("matched files"<>desc<>", newest first")
              where (dir,desc) = if isFileName src then (dldir," in download directory") else (rulesdir,"")
            Nothing  -> return [maybe err (dbg4 "inferred source") $ dataFileFor f]  -- shouldn't fail, f has .rules extension
              where err = error' $ "could not infer a data file for " <> f
    return $ dbg4 "data file" $ headMay fs
  case mdatafile of
    Nothing -> return nulljournal  -- data file specified by source rule was not found
    Just dat -> do
      exists <- liftIO $ doesFileExist dat
      if not (dat=="-" || exists)
      then return nulljournal      -- data file inferred from rules file name was not found
      else do
        t <- liftIO $ readFileOrStdinPortably dat
        readJournalFromCsv (Just $ Left rules) dat t
        -- apply any command line account aliases. Can fail with a bad replacement pattern.
        >>= liftEither . journalApplyAliases (aliasesFromOpts iopts)
            -- journalFinalise assumes the journal's items are
            -- reversed, as produced by JournalReader's parser.
            -- But here they are already properly ordered. So we'd
            -- better preemptively reverse them once more. XXX inefficient
            . journalReverse
        >>= journalFinalise iopts{balancingopts_=(balancingopts_ iopts){ignore_assertions_=True}} f ""

--- ** reading rules files
--- *** rules utilities
_RULES_READING__________________________________________ = undefined

-- | Given a rules file path, what would be the corresponding data file ?
-- (Remove a .rules extension.)
dataFileFor :: FilePath -> Maybe FilePath
dataFileFor = stripExtension "rules"

-- | Given a csv file path, what would be the corresponding rules file ?
-- (Add a .rules extension.)
rulesFileFor :: FilePath -> FilePath
rulesFileFor = (++ ".rules")

-- | An exception-throwing IO action that reads and validates
-- the specified CSV rules file (which may include other rules files).
readRulesFile :: FilePath -> ExceptT String IO CsvRules
readRulesFile f =
  liftIO (do
    dbg6IO "using conversion rules file" f
    readFilePortably f >>= expandIncludes (takeDirectory f)
  ) >>= either throwError return . parseAndValidateCsvRules f

-- | Inline all files referenced by include directives in this hledger CSV rules text, recursively.
-- Included file paths may be relative to the directory of the provided file path.
-- This is done as a pre-parse step to simplify the CSV rules parser.
expandIncludes :: FilePath -> Text -> IO Text
expandIncludes dir0 content = mapM (expandLine dir0) (T.lines content) <&> T.unlines
  where
    expandLine dir1 line =
      case line of
        (T.stripPrefix "include " -> Just f) -> expandIncludes dir2 =<< T.readFile f'
          where
            f' = dir1 </> T.unpack (T.dropWhile isSpace f)
            dir2 = takeDirectory f'
        _ -> return line

-- defaultRulesText :: FilePath -> Text
-- defaultRulesText _csvfile = T.pack $ unlines
--   ["# hledger csv conversion rules" --  for " ++ csvFileFor (takeFileName csvfile)
--   ,"# cf http://hledger.org/hledger.html#csv"
--   ,""
--   ,"account1 assets:bank:checking"
--   ,""
--   ,"fields date, description, amount1"
--   ,""
--   ,"#skip 1"
--   ,"#newest-first"
--   ,""
--   ,"#date-format %-d/%-m/%Y"
--   ,"#date-format %-m/%-d/%Y"
--   ,"#date-format %Y-%h-%d"
--   ,""
--   ,"#currency $"
--   ,""
--   ,"if ITUNES"
--   ," account2 expenses:entertainment"
--   ,""
--   ,"if (TO|FROM) SAVINGS"
--   ," account2 assets:bank:savings\n"
--   ]

-- | An error-throwing IO action that parses this text as CSV conversion rules
-- and runs some extra validation checks. The file path is used in error messages.
parseAndValidateCsvRules :: FilePath -> T.Text -> Either String CsvRules
parseAndValidateCsvRules rulesfile s =
  case parseCsvRules rulesfile s of
    Left err    -> Left $ customErrorBundlePretty err
    Right rules -> first makeFancyParseError $ validateCsvRules rules
  where
    makeFancyParseError :: String -> String
    makeFancyParseError errorString =
      parseErrorPretty (FancyError 0 (S.singleton $ ErrorFail errorString) :: ParseError Text String)

instance ShowErrorComponent String where
  showErrorComponent = id

-- | Parse this text as CSV conversion rules. The file path is for error messages.
parseCsvRules :: FilePath -> T.Text -> Either (ParseErrorBundle T.Text HledgerParseErrorData) CsvRules
-- parseCsvRules rulesfile s = runParser csvrulesfile nullrules{baseAccount=takeBaseName rulesfile} rulesfile s
parseCsvRules = runParser (evalStateT rulesp defrules)

-- | Return the validated rules, or an error.
validateCsvRules :: CsvRules -> Either String CsvRules
validateCsvRules rules = do
  unless (isAssigned "date")   $ Left "Please specify (at top level) the date field. Eg: date %1"
  Right rules
  where
    isAssigned f = isJust $ getEffectiveAssignment rules [] f

--- *** rules types
_RULES_TYPES__________________________________________ = undefined

-- | A set of data definitions and account-matching patterns sufficient to
-- convert a particular CSV data file into meaningful journal transactions.
data CsvRules' a = CsvRules' {
  rdirectives        :: [(DirectiveName,Text)],
    -- ^ top-level rules, as (keyword, value) pairs
  rcsvfieldindexes   :: [(CsvFieldName, CsvFieldIndex)],
    -- ^ csv field names and their column number, if declared by a fields list
  rassignments       :: [(HledgerFieldName, FieldTemplate)],
    -- ^ top-level assignments to hledger fields, as (field name, value template) pairs
  rconditionalblocks :: [ConditionalBlock],
    -- ^ conditional blocks, which containing additional assignments/rules to apply to matched csv records
  rblocksassigning :: a -- (String -> [ConditionalBlock])
    -- ^ all conditional blocks which can potentially assign field with a given name (memoized)
}

-- | Type used by parsers. Directives, assignments and conditional blocks
-- are in the reverse order compared to what is in the file and rblocksassigning is non-functional,
-- could not be used for processing CSV records yet
type CsvRulesParsed = CsvRules' ()

-- | Type used after parsing is done. Directives, assignments and conditional blocks
-- are in the same order as they were in the input file and rblocksassigning is functional.
-- Ready to be used for CSV record processing
type CsvRules = CsvRules' (Text -> [ConditionalBlock])  -- XXX simplify

instance Eq CsvRules where
  r1 == r2 = (rdirectives r1, rcsvfieldindexes r1, rassignments r1) ==
             (rdirectives r2, rcsvfieldindexes r2, rassignments r2)

-- Custom Show instance used for debug output: omit the rblocksassigning field, which isn't showable.
instance Show CsvRules where
  show r = "CsvRules { rdirectives = " ++ show (rdirectives r) ++
           ", rcsvfieldindexes = "     ++ show (rcsvfieldindexes r) ++
           ", rassignments = "         ++ show (rassignments r) ++
           ", rconditionalblocks = "   ++ show (rconditionalblocks r) ++
           " }"

type CsvRulesParser a = StateT CsvRulesParsed SimpleTextParser a

-- | The keyword of a CSV rule - "fields", "skip", "if", etc.
type DirectiveName    = Text

-- | CSV field name.
type CsvFieldName     = Text

-- | 1-based CSV column number.
type CsvFieldIndex    = Int

-- | Percent symbol followed by a CSV field name or column number. Eg: %date, %1.
type CsvFieldReference = Text

-- | One of the standard hledger fields or pseudo-fields that can be assigned to.
-- Eg date, account1, amount, amount1-in, date-format.
type HledgerFieldName = Text

-- | A text value to be assigned to a hledger field, possibly
-- containing csv field references to be interpolated.
type FieldTemplate    = Text

-- | A strptime date parsing pattern, as supported by Data.Time.Format.
type DateFormat       = Text

-- | A prefix for a matcher test, either & or none (implicit or).
data MatcherPrefix = And | Not | None
  deriving (Show, Eq)

-- | A single test for matching a CSV record, in one way or another.
data Matcher =
    RecordMatcher MatcherPrefix Regexp                          -- ^ match if this regexp matches the overall CSV record
  | FieldMatcher MatcherPrefix CsvFieldReference Regexp         -- ^ match if this regexp matches the referenced CSV field's value
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

defrules :: CsvRulesParsed
defrules = CsvRules' {
  rdirectives=[],
  rcsvfieldindexes=[],
  rassignments=[],
  rconditionalblocks=[],
  rblocksassigning = ()
  }

-- | Create CsvRules from the content parsed out of the rules file
mkrules :: CsvRulesParsed -> CsvRules
mkrules rules =
  let conditionalblocks = reverse $ rconditionalblocks rules
      maybeMemo = if length conditionalblocks >= 15 then memo else id
  in
    CsvRules' {
    rdirectives=reverse $ rdirectives rules,
    rcsvfieldindexes=rcsvfieldindexes rules,
    rassignments=reverse $ rassignments rules,
    rconditionalblocks=conditionalblocks,
    rblocksassigning = maybeMemo (\f -> filter (any ((==f).fst) . cbAssignments) conditionalblocks)
    }

--- *** rules parsers
_RULES_PARSING__________________________________________ = undefined

{-
Grammar for the CSV conversion rules, more or less:

RULES: RULE*

RULE: ( SOURCE | FIELD-LIST | FIELD-ASSIGNMENT | CONDITIONAL-BLOCK | SKIP | TIMEZONE | NEWEST-FIRST | INTRA-DAY-REVERSED | DATE-FORMAT | DECIMAL-MARK | COMMENT | BLANK ) NEWLINE

SOURCE: source SPACE FILEPATH

FIELD-LIST: fields SPACE FIELD-NAME ( SPACE? , SPACE? FIELD-NAME )*

FIELD-NAME: QUOTED-FIELD-NAME | BARE-FIELD-NAME

QUOTED-FIELD-NAME: " (any CHAR except double-quote)+ "

BARE-FIELD-NAME: any CHAR except space, tab, #, ;

FIELD-ASSIGNMENT: JOURNAL-FIELD ASSIGNMENT-SEPARATOR FIELD-VALUE

JOURNAL-FIELD: date | date2 | status | code | description | comment | account1 | account2 | amount | JOURNAL-PSEUDO-FIELD

JOURNAL-PSEUDO-FIELD: amount-in | amount-out | currency

ASSIGNMENT-SEPARATOR: SPACE | ( : SPACE? )

FIELD-VALUE: VALUE (possibly containing CSV-FIELD-REFERENCEs and REGEX-MATCHGROUP-REFERENCEs)

CSV-FIELD-REFERENCE: % CSV-FIELD

REGEX-MATCHGROUP-REFERENCE: \ DIGIT+

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

COMMENT-CHAR: # | ; | *

NONSPACE: any CHAR not a SPACE-CHAR

BLANK: SPACE?

SPACE: SPACE-CHAR+

SPACE-CHAR: space | tab

CHAR: any character except newline

DIGIT: 0-9

-}

addDirective :: (DirectiveName, Text) -> CsvRulesParsed -> CsvRulesParsed
addDirective d r = r{rdirectives=d:rdirectives r}

addAssignment :: (HledgerFieldName, FieldTemplate) -> CsvRulesParsed -> CsvRulesParsed
addAssignment a r = r{rassignments=a:rassignments r}

setIndexesAndAssignmentsFromList :: [CsvFieldName] -> CsvRulesParsed -> CsvRulesParsed
setIndexesAndAssignmentsFromList fs = addAssignmentsFromList fs . setCsvFieldIndexesFromList fs
  where
    setCsvFieldIndexesFromList :: [CsvFieldName] -> CsvRulesParsed -> CsvRulesParsed
    setCsvFieldIndexesFromList fs' r = r{rcsvfieldindexes=zip fs' [1..]}

    addAssignmentsFromList :: [CsvFieldName] -> CsvRulesParsed -> CsvRulesParsed
    addAssignmentsFromList fs' r = foldl' maybeAddAssignment r journalfieldnames
      where
        maybeAddAssignment rules f = (maybe id addAssignmentFromIndex $ elemIndex f fs') rules
          where
            addAssignmentFromIndex i = addAssignment (f, T.pack $ '%':show (i+1))

addConditionalBlock :: ConditionalBlock -> CsvRulesParsed -> CsvRulesParsed
addConditionalBlock b r = r{rconditionalblocks=b:rconditionalblocks r}

addConditionalBlocks :: [ConditionalBlock] -> CsvRulesParsed -> CsvRulesParsed
addConditionalBlocks bs r = r{rconditionalblocks=bs++rconditionalblocks r}

rulesp :: CsvRulesParser CsvRules
rulesp = do
  _ <- many $ choice
    [blankorcommentlinep                                                <?> "blank or comment line"
    ,(directivep        >>= modify' . addDirective)                     <?> "directive"
    ,(fieldnamelistp    >>= modify' . setIndexesAndAssignmentsFromList) <?> "field name list"
    ,(fieldassignmentp  >>= modify' . addAssignment)                    <?> "field assignment"
    -- conditionalblockp backtracks because it shares "if" prefix with conditionaltablep.
    ,try (conditionalblockp >>= modify' . addConditionalBlock)          <?> "conditional block"
    -- 'reverse' is there to ensure that conditions are added in the order they listed in the file
    ,(conditionaltablep >>= modify' . addConditionalBlocks . reverse)   <?> "conditional table"
    ]
  eof
  mkrules <$> get

blankorcommentlinep :: CsvRulesParser ()
blankorcommentlinep = lift (dbgparse 8 "trying blankorcommentlinep") >> choiceInState [blanklinep, commentlinep]

blanklinep :: CsvRulesParser ()
blanklinep = lift skipNonNewlineSpaces >> newline >> return () <?> "blank line"

commentlinep :: CsvRulesParser ()
commentlinep = lift skipNonNewlineSpaces >> commentcharp >> lift restofline >> return () <?> "comment line"

commentcharp :: CsvRulesParser Char
commentcharp = oneOf (";#*" :: [Char])

directivep :: CsvRulesParser (DirectiveName, Text)
directivep = (do
  lift $ dbgparse 8 "trying directive"
  d <- choiceInState $ map (lift . string) directives
  v <- (((char ':' >> lift (many spacenonewline)) <|> lift (some spacenonewline)) >> directivevalp)
       <|> (optional (char ':') >> lift skipNonNewlineSpaces >> lift eolof >> return "")
  return (d, v)
  ) <?> "directive"

directives :: [Text]
directives =
  ["source"
  ,"date-format"
  ,"decimal-mark"
  ,"separator"
  -- ,"default-account"
  -- ,"default-currency"
  ,"skip"
  ,"timezone"
  ,"newest-first"
  ,"intra-day-reversed"
  , "balance-type"
  ]

directivevalp :: CsvRulesParser Text
directivevalp = T.pack <$> anySingle `manyTill` lift eolof

fieldnamelistp :: CsvRulesParser [CsvFieldName]
fieldnamelistp = (do
  lift $ dbgparse 8 "trying fieldnamelist"
  string "fields"
  optional $ char ':'
  lift skipNonNewlineSpaces1
  let separator = lift skipNonNewlineSpaces >> char ',' >> lift skipNonNewlineSpaces
  f <- fromMaybe "" <$> optional fieldnamep
  fs <- some $ (separator >> fromMaybe "" <$> optional fieldnamep)
  lift restofline
  return . map T.toLower $ f:fs
  ) <?> "field name list"

fieldnamep :: CsvRulesParser Text
fieldnamep = quotedfieldnamep <|> barefieldnamep

quotedfieldnamep :: CsvRulesParser Text
quotedfieldnamep =
    char '"' *> takeWhile1P Nothing (`notElem` ("\"\n:;#~" :: [Char])) <* char '"'

barefieldnamep :: CsvRulesParser Text
barefieldnamep = takeWhile1P Nothing (`notElem` (" \t\n,;#~" :: [Char]))

fieldassignmentp :: CsvRulesParser (HledgerFieldName, FieldTemplate)
fieldassignmentp = do
  lift $ dbgparse 8 "trying fieldassignmentp"
  f <- journalfieldnamep
  v <- choiceInState [ assignmentseparatorp >> fieldvalp
                     , lift eolof >> return ""
                     ]
  return (f,v)
  <?> "field assignment"

journalfieldnamep :: CsvRulesParser Text
journalfieldnamep = do
  lift (dbgparse 8 "trying journalfieldnamep")
  choiceInState $ map (lift . string) journalfieldnames

maxpostings = 99

-- Transaction fields and pseudo fields for CSV conversion.
-- Names must precede any other name they contain, for the parser
-- (amount-in before amount; date2 before date). TODO: fix
journalfieldnames =
  concat [[ "account" <> i
          ,"amount" <> i <> "-in"
          ,"amount" <> i <> "-out"
          ,"amount" <> i
          ,"balance" <> i
          ,"comment" <> i
          ,"currency" <> i
          ] | x <- [maxpostings, (maxpostings-1)..1], let i = T.pack $ show x]
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
  lift $ dbgparse 8 "trying assignmentseparatorp"
  _ <- choiceInState [ lift skipNonNewlineSpaces >> char ':' >> lift skipNonNewlineSpaces
                     , lift skipNonNewlineSpaces1
                     ]
  return ()

fieldvalp :: CsvRulesParser Text
fieldvalp = do
  lift $ dbgparse 8 "trying fieldvalp"
  T.pack <$> anySingle `manyTill` lift eolof

-- A conditional block: one or more matchers, one per line, followed by one or more indented rules.
conditionalblockp :: CsvRulesParser ConditionalBlock
conditionalblockp = do
  lift $ dbgparse 8 "trying conditionalblockp"
  -- "if\nMATCHER" or "if    \nMATCHER" or "if MATCHER"
  start <- getOffset
  string "if" >> ( (newline >> return Nothing)
                  <|> (lift skipNonNewlineSpaces1 >> optional newline))
  ms <- some matcherp
  as <- catMaybes <$>
    many (lift skipNonNewlineSpaces1 >>
          choice [ lift eolof >> return Nothing
                 , fmap Just fieldassignmentp
                 ])
  when (null as) $
    customFailure $ parseErrorAt start $  "start of conditional block found, but no assignment rules afterward\n(assignment rules in a conditional block should be indented)"
  return $ CB{cbMatchers=ms, cbAssignments=as}
  <?> "conditional block"

-- A conditional table: "if" followed by separator, followed by some field names,
-- followed by many lines, each of which has:
-- one matchers, followed by field assignments (as many as there were fields)
conditionaltablep :: CsvRulesParser [ConditionalBlock]
conditionaltablep = do
  lift $ dbgparse 8 "trying conditionaltablep"
  start <- getOffset
  string "if"
  sep <- lift $ satisfy (\c -> not (isAlphaNum c || isSpace c))
  fields <- journalfieldnamep `sepBy1` (char sep)
  newline
  body <- flip manyTill (lift eolof) $ do
    off <- getOffset
    m <- matcherp' $ void $ char sep
    vs <- T.split (==sep) . T.pack <$> lift restofline
    if (length vs /= length fields)
      then customFailure $ parseErrorAt off $ ((printf "line of conditional table should have %d values, but this one has only %d" (length fields) (length vs)) :: String)
      else return (m,vs)
  when (null body) $
    customFailure $ parseErrorAt start $ "start of conditional table found, but no assignment rules afterward"
  return $ flip map body $ \(m,vs) ->
    CB{cbMatchers=[m], cbAssignments=zip fields vs}
  <?> "conditional table"

-- A single matcher, on one line.
matcherp' :: CsvRulesParser () -> CsvRulesParser Matcher
matcherp' end = try (fieldmatcherp end) <|> recordmatcherp end

matcherp :: CsvRulesParser Matcher
matcherp = matcherp' (lift eolof)

-- A single whole-record matcher.
-- A pattern on the whole line, not beginning with a csv field reference.
recordmatcherp :: CsvRulesParser () -> CsvRulesParser Matcher
recordmatcherp end = do
  lift $ dbgparse 8 "trying recordmatcherp"
  -- pos <- currentPos
  -- _  <- optional (matchoperatorp >> lift skipNonNewlineSpaces >> optional newline)
  p <- matcherprefixp
  r <- regexp end
  return $ RecordMatcher p r
  -- when (null ps) $
  --   Fail.fail "start of record matcher found, but no patterns afterward\n(patterns should not be indented)"
  <?> "record matcher"

-- | A single matcher for a specific field. A csv field reference
-- (like %date or %1), and a pattern on the rest of the line,
-- optionally space-separated. Eg:
-- %description chez jacques
fieldmatcherp :: CsvRulesParser () -> CsvRulesParser Matcher
fieldmatcherp end = do
  lift $ dbgparse 8 "trying fieldmatcher"
  -- An optional fieldname (default: "all")
  -- f <- fromMaybe "all" `fmap` (optional $ do
  --        f' <- fieldnamep
  --        lift skipNonNewlineSpaces
  --        return f')
  p <- matcherprefixp
  f <- csvfieldreferencep <* lift skipNonNewlineSpaces
  -- optional operator.. just ~ (case insensitive infix regex) for now
  -- _op <- fromMaybe "~" <$> optional matchoperatorp
  lift skipNonNewlineSpaces
  r <- regexp end
  return $ FieldMatcher p f r
  <?> "field matcher"

matcherprefixp :: CsvRulesParser MatcherPrefix
matcherprefixp = do
  lift $ dbgparse 8 "trying matcherprefixp"
  (char '&' >> lift skipNonNewlineSpaces >> return And) <|> (char '!' >> lift skipNonNewlineSpaces >> return Not) <|> return None

csvfieldreferencep :: CsvRulesParser CsvFieldReference
csvfieldreferencep = do
  lift $ dbgparse 8 "trying csvfieldreferencep"
  char '%'
  T.cons '%' . textQuoteIfNeeded <$> fieldnamep

-- A single regular expression
regexp :: CsvRulesParser () -> CsvRulesParser Regexp
regexp end = do
  lift $ dbgparse 8 "trying regexp"
  -- notFollowedBy matchoperatorp
  c <- lift nonspace
  cs <- anySingle `manyTill` end
  case toRegexCI . T.strip . T.pack $ c:cs of
       Left x -> Fail.fail $ "CSV parser: " ++ x
       Right x -> return x

-- -- A match operator, indicating the type of match to perform.
-- -- Currently just ~ meaning case insensitive infix regex match.
-- matchoperatorp :: CsvRulesParser String
-- matchoperatorp = fmap T.unpack $ choiceInState $ map string
--   ["~"
--   -- ,"!~"
--   -- ,"="
--   -- ,"!="
--   ]

_RULES_LOOKUP__________________________________________ = undefined

getDirective :: DirectiveName -> CsvRules -> Maybe FieldTemplate
getDirective directivename = lookup directivename . rdirectives

-- | Look up the value (template) of a csv rule by rule keyword.
csvRule :: CsvRules -> DirectiveName -> Maybe FieldTemplate
csvRule rules = (`getDirective` rules)

-- | Look up the value template assigned to a hledger field by field
-- list/field assignment rules, taking into account the current record and
-- conditional rules.
hledgerField :: CsvRules -> CsvRecord -> HledgerFieldName -> Maybe FieldTemplate
hledgerField = getEffectiveAssignment

-- | Look up the final value assigned to a hledger field, with csv field
-- references interpolated.
hledgerFieldValue :: CsvRules -> CsvRecord -> HledgerFieldName -> Maybe Text
hledgerFieldValue rules record f = (fmap (renderTemplate rules record f) . hledgerField rules record) f

maybeNegate :: MatcherPrefix -> Bool -> Bool
maybeNegate Not origbool = not origbool
maybeNegate _ origbool = origbool

-- | Given the conversion rules, a CSV record and a hledger field name, find
-- the value template ultimately assigned to this field, if any, by a field
-- assignment at top level or in a conditional block matching this record.
--
-- Note conditional blocks' patterns are matched against an approximation of the
-- CSV record: all the field values, without enclosing quotes, comma-separated.
--
getEffectiveAssignment :: CsvRules -> CsvRecord -> HledgerFieldName -> Maybe FieldTemplate
getEffectiveAssignment rules record f = lastMay $ map snd $ assignments
  where
    -- all active assignments to field f, in order
    assignments = dbg9 "csv assignments" $ filter ((==f).fst) $ toplevelassignments ++ conditionalassignments
    -- all top level field assignments
    toplevelassignments    = rassignments rules 
    -- all field assignments in conditional blocks assigning to field f and active for the current csv record
    conditionalassignments = concatMap cbAssignments $ filter (isBlockActive rules record) $ (rblocksassigning rules) f

-- does this conditional block match the current csv record ?
isBlockActive :: CsvRules -> CsvRecord -> ConditionalBlock -> Bool
isBlockActive rules record CB{..} = any (all matcherMatches) $ groupedMatchers cbMatchers
  where
    -- does this individual matcher match the current csv record ?
    matcherMatches :: Matcher -> Bool
    matcherMatches (RecordMatcher prefix pat) = maybeNegate prefix origbool
      where
        pat' = dbg7 "regex" pat
        -- A synthetic whole CSV record to match against. Note, this can be
        -- different from the original CSV data:
        -- - any whitespace surrounding field values is preserved
        -- - any quotes enclosing field values are removed
        -- - and the field separator is always comma
        -- which means that a field containing a comma will look like two fields.
        wholecsvline = dbg7 "wholecsvline" $ T.intercalate "," record
        origbool = regexMatchText pat' wholecsvline
    matcherMatches (FieldMatcher prefix csvfieldref pat) = maybeNegate prefix origbool
      where
        -- the value of the referenced CSV field to match against.
        csvfieldvalue = dbg7 "csvfieldvalue" $ replaceCsvFieldReference rules record csvfieldref
        origbool = regexMatchText pat csvfieldvalue

    -- | Group matchers into associative pairs based on prefix, e.g.:
    --   A
    --   & B
    --   C
    --   D
    --   & E
    --   => [[A, B], [C], [D, E]]
    groupedMatchers :: [Matcher] -> [[Matcher]]
    groupedMatchers [] = []
    groupedMatchers (x:xs) = (x:ys) : groupedMatchers zs
      where
        (ys, zs) = span (\y -> matcherPrefix y == And) xs
        matcherPrefix :: Matcher -> MatcherPrefix
        matcherPrefix (RecordMatcher prefix _) = prefix
        matcherPrefix (FieldMatcher prefix _ _) = prefix

-- | Render a field assignment's template, possibly interpolating referenced
-- CSV field values. Outer whitespace is removed from interpolated values.
renderTemplate ::  CsvRules -> CsvRecord -> HledgerFieldName -> FieldTemplate -> Text
renderTemplate rules record f t = maybe t mconcat $ parseMaybe
    (many $ takeWhile1P Nothing isNotEscapeChar
        <|> replaceRegexGroupReference rules record f <$> matchp
        <|> replaceCsvFieldReference rules record <$> referencep)
    t
  where
    -- XXX: can we return a parsed Int here?
    matchp = liftA2 T.cons (char '\\') (takeWhile1P (Just "matchref") isDigit) :: Parsec HledgerParseErrorData Text Text
    referencep = liftA2 T.cons (char '%') (takeWhile1P (Just "reference") isFieldNameChar) :: Parsec HledgerParseErrorData Text Text
    isFieldNameChar c = isAlphaNum c || c == '_' || c == '-'
    isNotEscapeChar c = c /='%' && c /= '\\'

-- | Replace something that looks like a Regex match group reference with the
-- resulting match group value after applying the Regex.
replaceRegexGroupReference :: CsvRules -> CsvRecord -> HledgerFieldName -> FieldTemplate -> Text
replaceRegexGroupReference rules record f s = case T.uncons s of
    Just ('\\', group) -> fromMaybe "" $ regexMatchValue rules record f group
    _                  -> s

regexMatchValue :: CsvRules -> CsvRecord -> HledgerFieldName -> Text -> Maybe Text
regexMatchValue rules record f sgroup = let
  matchgroups  = concatMap (getMatchGroups rules record)
               $ concatMap cbMatchers
               $ filter (isBlockActive rules record)
               $ rblocksassigning rules f
  group = (read (T.unpack sgroup) :: Int) - 1 -- adjust to 0-indexing
  in atMay matchgroups group

getMatchGroups :: CsvRules -> CsvRecord -> Matcher -> [Text]
getMatchGroups _ record (RecordMatcher _ regex)  = let
  txt = T.intercalate "," record -- see caveats of wholecsvline, in `isBlockActive`
  in regexMatchTextGroups regex txt
getMatchGroups rules record (FieldMatcher _ fieldref regex) = let
  txt = replaceCsvFieldReference rules record fieldref
  in regexMatchTextGroups regex txt

-- | Replace something that looks like a reference to a csv field ("%date" or "%1)
-- with that field's value. If it doesn't look like a field reference, or if we
-- can't find such a field, replace it with the empty string.
replaceCsvFieldReference :: CsvRules -> CsvRecord -> CsvFieldReference -> Text
replaceCsvFieldReference rules record s = case T.uncons s of
    Just ('%', fieldname) -> fromMaybe "" $ csvFieldValue rules record fieldname
    _                     -> s

-- | Get the (whitespace-stripped) value of a CSV field, identified by its name or
-- column number, ("date" or "1"), from the given CSV record, if such a field exists.
csvFieldValue :: CsvRules -> CsvRecord -> CsvFieldName -> Maybe Text
csvFieldValue rules record fieldname = do
  fieldindex <-
    if T.all isDigit fieldname
    then readMay $ T.unpack fieldname
    else lookup (T.toLower fieldname) $ rcsvfieldindexes rules
  T.strip <$> atMay record (fieldindex-1)

_CSV_READING__________________________________________ = undefined

-- | Read a Journal from the given CSV data (and filename, used for error
-- messages), or return an error. Proceed as follows:
--
-- 1. Conversion rules are provided, or they are parsed from the specified
--    rules file, or from the default rules file for the CSV data file.
--    If rules parsing fails, or the required rules file does not exist, throw an error.
--
-- 2. Parse the CSV data using the rules, or throw an error.
--
-- 3. Convert the CSV records to hledger transactions using the rules.
--
-- 4. Return the transactions as a Journal.
--
readJournalFromCsv :: Maybe (Either CsvRules FilePath) -> FilePath -> Text -> ExceptT String IO Journal
readJournalFromCsv Nothing "-" _ = throwError "please use --rules-file when reading CSV from stdin"
readJournalFromCsv merulesfile csvfile csvtext = do
    -- for now, correctness is the priority here, efficiency not so much

    rules <- case merulesfile of
      Just (Left rs)         -> return rs
      Just (Right rulesfile) -> readRulesFile rulesfile
      Nothing                -> readRulesFile $ rulesFileFor csvfile
    dbg6IO "csv rules" rules

    -- convert the csv data to lines and remove all empty/blank lines
    let csvlines1 = dbg9 "csvlines1" $ filter (not . T.null . T.strip) $ dbg9 "csvlines0" $ T.lines csvtext

    -- if there is a top-level skip rule, skip the specified number of non-empty lines
    skiplines <- case getDirective "skip" rules of
                      Nothing -> return 0
                      Just "" -> return 1
                      Just s  -> maybe (throwError $ "could not parse skip value: " ++ show s) return . readMay $ T.unpack s
    let csvlines2 = dbg9 "csvlines2" $ drop skiplines csvlines1

    -- convert back to text and parse as csv records
    let
      csvtext1 = T.unlines csvlines2
      separator =
        case getDirective "separator" rules >>= parseSeparator of
          Just c           -> c
          _ | ext == "ssv" -> ';'
          _ | ext == "tsv" -> '\t'
          _                -> ','
          where
            ext = map toLower $ drop 1 $ takeExtension csvfile
      -- parsec seemed to fail if you pass it "-" here   -- TODO: try again with megaparsec
      parsecfilename = if csvfile == "-" then "(stdin)" else csvfile
    dbg6IO "using separator" separator
    -- parse csv records
    csvrecords0 <- dbg7 "parseCsv" <$> parseCsv separator parsecfilename csvtext1
    -- remove any records skipped by conditional skip or end rules
    let csvrecords1 = applyConditionalSkips rules csvrecords0
    -- and check the remaining records for any obvious problems
    csvrecords <- liftEither $ dbg7 "validateCsv" <$> validateCsv csvrecords1
    dbg6IO "first 3 csv records" $ take 3 csvrecords

    -- XXX identify header lines some day ?
    -- let (headerlines, datalines) = identifyHeaderLines csvrecords'
    --     mfieldnames = lastMay headerlines

    tzout <- liftIO getCurrentTimeZone
    mtzin <- case getDirective "timezone" rules of
              Nothing -> return Nothing
              Just s  ->
                maybe (throwError $ "could not parse time zone: " ++ T.unpack s) (return.Just) $
                parseTimeM False defaultTimeLocale "%Z" $ T.unpack s
    let
      -- convert CSV records to transactions, saving the CSV line numbers for error positions
      txns = dbg7 "csv txns" $ snd $ mapAccumL
                     (\pos r ->
                        let
                          SourcePos name line col = pos
                          line' = (mkPos . (+1) . unPos) line
                          pos' = SourcePos name line' col
                        in
                          (pos', transactionFromCsvRecord timesarezoned mtzin tzout pos rules r)
                     )
                     (initialPos parsecfilename) csvrecords
        where
          timesarezoned =
            case csvRule rules "date-format" of
              Just f | any (`T.isInfixOf` f) ["%Z","%z","%EZ","%Ez"] -> True
              _ -> False

      -- Do our best to ensure transactions will be ordered chronologically,
      -- from oldest to newest. This is done in several steps:
      -- 1. Intra-day order: if there's an "intra-day-reversed" rule,
      -- assume each day's CSV records were ordered in reverse of the overall date order,
      -- so reverse each day's txns.
      intradayreversed = dbg6 "intra-day-reversed" $ isJust $ getDirective "intra-day-reversed" rules
      txns1 = dbg7 "txns1" $
        (if intradayreversed then concatMap reverse . groupOn tdate else id) txns
      -- 2. Overall date order: now if there's a "newest-first" rule,
      -- or if there's multiple dates and the first is more recent than the last,
      -- assume CSV records were ordered newest dates first,
      -- so reverse all txns.
      newestfirst = dbg6 "newest-first" $ isJust $ getDirective "newest-first" rules
      mdatalooksnewestfirst = dbg6 "mdatalooksnewestfirst" $
        case nub $ map tdate txns of
          ds | length ds > 1 -> Just $ head ds > last ds
          _                  -> Nothing
      txns2 = dbg7 "txns2" $
        (if newestfirst || mdatalooksnewestfirst == Just True then reverse else id) txns1
      -- 3. Disordered dates: in case the CSV records were ordered by chaos,
      -- do a final sort by date. If it was only a few records out of order,
      -- this will hopefully refine any good ordering done by steps 1 and 2.
      txns3 = dbg7 "date-sorted csv txns" $ sortOn tdate txns2

    return nulljournal{jtxns=txns3}

-- | Parse special separator names TAB and SPACE, or return the first
-- character. Return Nothing on empty string
parseSeparator :: Text -> Maybe Char
parseSeparator = specials . T.toLower
  where specials "space" = Just ' '
        specials "tab"   = Just '\t'
        specials xs      = fst <$> T.uncons xs

-- Call parseCassava on a file or stdin, converting the result to ExceptT.
parseCsv :: Char -> FilePath -> Text -> ExceptT String IO [CsvRecord]
parseCsv separator filePath csvtext = ExceptT $
  case filePath of
    "-" -> parseCassava separator "(stdin)" <$> T.getContents
    _   -> return $ if T.null csvtext then Right mempty else parseCassava separator filePath csvtext

-- Parse text into CSV records, using Cassava and the given field separator.
parseCassava :: Char -> FilePath -> Text -> Either String [CsvRecord]
parseCassava separator path content =
  -- XXX we now remove all blank lines before parsing; will Cassava will still produce [""] records ?
  -- filter (/=[""])
  either (Left . errorBundlePretty) (Right . parseResultToCsv) <$>
  CassavaMegaparsec.decodeWith decodeOptions Cassava.NoHeader path $
  BL.fromStrict $ T.encodeUtf8 content
  where
    decodeOptions = Cassava.defaultDecodeOptions {
                      Cassava.decDelimiter = fromIntegral (ord separator)
                    }
    parseResultToCsv :: (Foldable t, Functor t) => t (t B.ByteString) -> [CsvRecord]
    parseResultToCsv = toListList . unpackFields
      where
        toListList = toList . fmap toList
        unpackFields  = (fmap . fmap) T.decodeUtf8

-- | Scan for csv records where a conditional `skip` or `end` rule applies,
-- and apply that rule, removing one or more following records.
applyConditionalSkips :: CsvRules -> [CsvRecord] -> [CsvRecord]
applyConditionalSkips _ [] = []
applyConditionalSkips rules (r:rest) =
  case skipnum r of
    Nothing -> r : applyConditionalSkips rules rest
    Just cnt -> applyConditionalSkips rules $ drop (cnt-1) rest
  where
    skipnum r1 =
      case (getEffectiveAssignment rules r1 "end", getEffectiveAssignment rules r1 "skip") of
        (Nothing, Nothing) -> Nothing
        (Just _, _) -> Just maxBound
        (Nothing, Just "") -> Just 1
        (Nothing, Just x) -> Just (read $ T.unpack x)

-- | Do some validation on the parsed CSV records:
-- check that they all have at least two fields.
validateCsv :: [CsvRecord] -> Either String [CsvRecord]
validateCsv [] = Right []
validateCsv rs@(_first:_) =
  case lessthan2 of
    Just r  -> Left $ printf "CSV record %s has less than two fields" (show r)
    Nothing -> Right rs
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

--- ** converting csv records to transactions

transactionFromCsvRecord :: Bool -> Maybe TimeZone -> TimeZone -> SourcePos -> CsvRules -> CsvRecord -> Transaction
transactionFromCsvRecord timesarezoned mtzin tzout sourcepos rules record = t
  where
    ----------------------------------------------------------------------
    -- 1. Define some helpers:

    rule     = csvRule           rules        :: DirectiveName    -> Maybe FieldTemplate
    -- ruleval  = csvRuleValue      rules record :: DirectiveName    -> Maybe String
    field    = hledgerField      rules record :: HledgerFieldName -> Maybe FieldTemplate
    fieldval = hledgerFieldValue rules record :: HledgerFieldName -> Maybe Text
    mdateformat = rule "date-format"
    parsedate = parseDateWithCustomOrDefaultFormats timesarezoned mtzin tzout mdateformat
    mkdateerror datefield datevalue mdateformat' = T.unpack $ T.unlines
      ["error: could not parse \""<>datevalue<>"\" as a date using date format "
        <>maybe "\"YYYY/M/D\", \"YYYY-M-D\" or \"YYYY.M.D\"" (T.pack . show) mdateformat'
      ,showRecord record
      ,"the "<>datefield<>" rule is:   "<>(fromMaybe "required, but missing" $ field datefield)
      ,"the date-format is: "<>fromMaybe "unspecified" mdateformat'
      ,"you may need to "
        <>"change your "<>datefield<>" rule, "
        <>maybe "add a" (const "change your") mdateformat'<>" date-format rule, "
        <>"or "<>maybe "add a" (const "change your") mskip<>" skip rule"
      ,"for m/d/y or d/m/y dates, use date-format %-m/%-d/%Y or date-format %-d/%-m/%Y"
      ]
      where
        mskip = rule "skip"

    ----------------------------------------------------------------------
    -- 2. Gather values needed for the transaction itself, by evaluating the
    -- field assignment rules using the CSV record's data, and parsing a bit
    -- more where needed (dates, status).

    date        = fromMaybe "" $ fieldval "date"
    -- PARTIAL:
    date'       = fromMaybe (error' $ mkdateerror "date" date mdateformat) $ parsedate date
    mdate2      = fieldval "date2"
    mdate2'     = (maybe (error' $ mkdateerror "date2" (fromMaybe "" mdate2) mdateformat) Just . parsedate) =<< mdate2
    status      =
      case fieldval "status" of
        Nothing -> Unmarked
        Just s  -> either statuserror id $ runParser (statusp <* eof) "" s
          where
            statuserror err = error' . T.unpack $ T.unlines
              ["error: could not parse \""<>s<>"\" as a cleared status (should be *, ! or empty)"
              ,"the parse error is:      "<>T.pack (customErrorBundlePretty err)
              ]
    code        = maybe "" singleline' $ fieldval "code"
    description = maybe "" singleline' $ fieldval "description"
    comment     = maybe "" unescapeNewlines $ fieldval "comment"
    ttags       = fromRight [] $ rtp commenttagsp comment
    precomment  = maybe "" unescapeNewlines $ fieldval "precomment"

    singleline' = T.unwords . filter (not . T.null) . map T.strip . T.lines
    unescapeNewlines = T.intercalate "\n" . T.splitOn "\\n"

    ----------------------------------------------------------------------
    -- 3. Generate the postings for which an account has been assigned
    -- (possibly indirectly due to an amount or balance assignment)

    p1IsVirtual = (accountNamePostingType <$> fieldval "account1") == Just VirtualPosting
    ps = [p | n <- [1..maxpostings]
         ,let cmt  = maybe "" unescapeNewlines $ fieldval ("comment"<> T.pack (show n))
         ,let ptags = fromRight [] $ rtp commenttagsp cmt
         ,let currency = fromMaybe "" (fieldval ("currency"<> T.pack (show n)) <|> fieldval "currency")
         ,let mamount  = getAmount rules record currency p1IsVirtual n
         ,let mbalance = getBalance rules record currency n
         ,Just (acct,isfinal) <- [getAccount rules record mamount mbalance n]  -- skips Nothings
         ,let acct' | not isfinal && acct==unknownExpenseAccount &&
                      fromMaybe False (mamount >>= isNegativeMixedAmount) = unknownIncomeAccount
                    | otherwise = acct
         ,let p = nullposting{paccount          = accountNameWithoutPostingType acct'
                             ,pamount           = fromMaybe missingmixedamt mamount
                             ,ptransaction      = Just t
                             ,pbalanceassertion = mkBalanceAssertion rules record <$> mbalance
                             ,pcomment          = cmt
                             ,ptags             = ptags
                             ,ptype             = accountNamePostingType acct
                             }
         ]

    ----------------------------------------------------------------------
    -- 4. Build the transaction (and name it, so the postings can reference it).

    t = nulltransaction{
           tsourcepos        = (sourcepos, sourcepos)  -- the CSV line number
          ,tdate             = date'
          ,tdate2            = mdate2'
          ,tstatus           = status
          ,tcode             = code
          ,tdescription      = description
          ,tcomment          = comment
          ,ttags             = ttags
          ,tprecedingcomment = precomment
          ,tpostings         = ps
          }

-- | Parse the date string using the specified date-format, or if unspecified
-- the "simple date" formats (YYYY/MM/DD, YYYY-MM-DD, YYYY.MM.DD, leading
-- zeroes optional). If a timezone is provided, we assume the DateFormat
-- produces a zoned time and we localise that to the given timezone.
parseDateWithCustomOrDefaultFormats :: Bool -> Maybe TimeZone -> TimeZone -> Maybe DateFormat -> Text -> Maybe Day
parseDateWithCustomOrDefaultFormats timesarezoned mtzin tzout mformat s = localdate <$> mutctime
  -- this time code can probably be simpler, I'm just happy to get out alive
  where
    localdate :: UTCTime -> Day =
      localDay .
      dbg7 ("time in output timezone "++show tzout) .
      utcToLocalTime tzout
    mutctime :: Maybe UTCTime = asum $ map parseWithFormat formats

    parseWithFormat :: String -> Maybe UTCTime
    parseWithFormat fmt =
      if timesarezoned
      then
        dbg7 "zoned CSV time, expressed as UTC" $
        parseTimeM True defaultTimeLocale fmt $ T.unpack s :: Maybe UTCTime
      else
        -- parse as a local day and time; then if an input timezone is provided,
        -- assume it's in that, otherwise assume it's in the output timezone;
        -- then convert to UTC like the above
        let
          mlocaltime =
            fmap (dbg7 "unzoned CSV time") $
            parseTimeM True defaultTimeLocale fmt $ T.unpack s :: Maybe LocalTime
          localTimeAsZonedTime tz lt =  ZonedTime lt tz
        in
          case mtzin of
            Just tzin ->
              (dbg7 ("unzoned CSV time, declared as "++show tzin++ ", expressed as UTC") .
              localTimeToUTC tzin)
              <$> mlocaltime
            Nothing ->
              (dbg7 ("unzoned CSV time, treated as "++show tzout++ ", expressed as UTC") .
                zonedTimeToUTC .
                localTimeAsZonedTime tzout)
              <$> mlocaltime

    formats = map T.unpack $ maybe
               ["%Y/%-m/%-d"
               ,"%Y-%-m-%-d"
               ,"%Y.%-m.%-d"
               -- ,"%-m/%-d/%Y"
                -- ,parseTimeM TruedefaultTimeLocale "%Y/%m/%e" (take 5 s ++ "0" ++ drop 5 s)
                -- ,parseTimeM TruedefaultTimeLocale "%Y-%m-%e" (take 5 s ++ "0" ++ drop 5 s)
                -- ,parseTimeM TruedefaultTimeLocale "%m/%e/%Y" ('0':s)
                -- ,parseTimeM TruedefaultTimeLocale "%m-%e-%Y" ('0':s)
               ]
               (:[])
                mformat

-- | Figure out the amount specified for posting N, if any.
-- A currency symbol to prepend to the amount, if any, is provided,
-- and whether posting 1 requires balancing or not.
-- This looks for a non-empty amount value assigned to "amountN", "amountN-in", or "amountN-out".
-- For postings 1 or 2 it also looks at "amount", "amount-in", "amount-out".
-- If more than one of these has a value, it looks for one that is non-zero.
-- If there's multiple non-zeros, or no non-zeros but multiple zeros, it throws an error.
getAmount :: CsvRules -> CsvRecord -> Text -> Bool -> Int -> Maybe MixedAmount
getAmount rules record currency p1IsVirtual n =
  -- Warning! Many tricky corner cases here.
  -- Keep synced with:
  -- hledger_csv.m4.md -> CSV FORMAT -> "amount", "Setting amounts",
  -- hledger/test/csv.test -> 13, 31-34
  let
    unnumberedfieldnames = ["amount","amount-in","amount-out"]

    -- amount field names which can affect this posting
    fieldnames = map (("amount"<> T.pack (show n))<>) ["","-in","-out"]
                 -- For posting 1, also recognise the old amount/amount-in/amount-out names.
                 -- For posting 2, the same but only if posting 1 needs balancing.
                 ++ if n==1 || n==2 && not p1IsVirtual then unnumberedfieldnames else []

    -- assignments to any of these field names with non-empty values
    assignments = [(f,a') | f <- fieldnames
                          , Just v <- [T.strip . renderTemplate rules record f <$> hledgerField rules record f]
                          , not $ T.null v
                          -- XXX maybe ignore rule-generated values like "", "-", "$", "-$", "$-" ? cf CSV FORMAT -> "amount", "Setting amounts",
                          , let a = parseAmount rules record currency v
                          -- With amount/amount-in/amount-out, in posting 2,
                          -- flip the sign and convert to cost, as they did before 1.17
                          , let a' = if f `elem` unnumberedfieldnames && n==2 then mixedAmountCost (maNegate a) else a
                          ]

    -- if any of the numbered field names are present, discard all the unnumbered ones
    discardUnnumbered xs = if null numbered then xs else numbered
      where
        numbered = filter (T.any isDigit . fst) xs

    -- discard all zero amounts, unless all amounts are zero, in which case discard all but the first
    discardExcessZeros xs = if null nonzeros then take 1 xs else nonzeros
      where
        nonzeros = filter (not . mixedAmountLooksZero . snd) xs

    -- for -out fields, flip the sign  XXX unless it's already negative ? back compat issues / too confusing ?
    negateIfOut f = if "-out" `T.isSuffixOf` f then maNegate else id

  in case discardExcessZeros $ discardUnnumbered assignments of
      []      -> Nothing
      [(f,a)] -> Just $ negateIfOut f a
      fs      -> error' . T.unpack . textChomp . T.unlines $  -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
          -- PARTIAL:
        ["in CSV rules:"
        ,"While processing " <> showRecord record
        ,"while calculating amount for posting " <> T.pack (show n)
        ] ++
        ["rule \"" <> f <> " " <>
          fromMaybe "" (hledgerField rules record f) <>
          "\" assigned value \"" <> wbToText (showMixedAmountB noColour a) <> "\"" -- XXX not sure this is showing all the right info
          | (f,a) <- fs
        ] ++
        [""
        ,"Multiple non-zero amounts were assigned for an amount field."
        ,"Please ensure just one non-zero amount is assigned, perhaps with an if rule."
        ,"See also: https://hledger.org/hledger.html#setting-amounts"
        ,"(hledger manual -> CSV format -> Tips -> Setting amounts)"
        ]
-- | Figure out the expected balance (assertion or assignment) specified for posting N,
-- if any (and its parse position).
getBalance :: CsvRules -> CsvRecord -> Text -> Int -> Maybe (Amount, SourcePos)
getBalance rules record currency n = do
  v <- (fieldval ("balance"<> T.pack (show n))
        -- for posting 1, also recognise the old field name
        <|> if n==1 then fieldval "balance" else Nothing)
  case v of
    "" -> Nothing
    s  -> Just (
            parseBalanceAmount rules record currency n s
           ,initialPos ""  -- parse position to show when assertion fails,
           )               -- XXX the csv record's line number would be good
  where
    fieldval = fmap T.strip . hledgerFieldValue rules record :: HledgerFieldName -> Maybe Text

-- | Given a non-empty amount string (from CSV) to parse, along with a
-- possibly non-empty currency symbol to prepend,
-- parse as a hledger MixedAmount (as in journal format), or raise an error.
-- The whole CSV record is provided for the error message.
parseAmount :: CsvRules -> CsvRecord -> Text -> Text -> MixedAmount
parseAmount rules record currency s =
    either mkerror mixedAmount $  -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
      -- PARTIAL:
    runParser (evalStateT (amountp <* eof) journalparsestate) "" $
    currency <> simplifySign s
  where
    journalparsestate = nulljournal{jparsedecimalmark=parseDecimalMark rules}
    mkerror e = error' . T.unpack $ T.unlines
      ["error: could not parse \"" <> s <> "\" as an amount"
      ,showRecord record
      ,showRules rules record
      -- ,"the default-currency is: "++fromMaybe "unspecified" (getDirective "default-currency" rules)
      ,"the parse error is:      " <> T.pack (customErrorBundlePretty e)
      ,"you may need to \
        \change your amount*, balance*, or currency* rules, \
        \or add or change your skip rule"
      ]

-- | Show the values assigned to each journal field.
showRules rules record = T.unlines $ catMaybes
  [ (("the "<>fld<>" rule is: ")<>) <$>
    getEffectiveAssignment rules record fld | fld <- journalfieldnames ]

-- | Show a (approximate) recreation of the original CSV record.
showRecord :: CsvRecord -> Text
showRecord r = "CSV record: "<>T.intercalate "," (map (wrap "\"" "\"") r)

-- XXX unify these ^v

-- | Almost but not quite the same as parseAmount.
-- Given a non-empty amount string (from CSV) to parse, along with a
-- possibly non-empty currency symbol to prepend,
-- parse as a hledger Amount (as in journal format), or raise an error.
-- The CSV record and the field's numeric suffix are provided for the error message.
parseBalanceAmount :: CsvRules -> CsvRecord -> Text -> Int -> Text -> Amount
parseBalanceAmount rules record currency n s =
  either (mkerror n s) id $
    runParser (evalStateT (amountp <* eof) journalparsestate) "" $
    currency <> simplifySign s
                  -- the csv record's line number would be good
  where
    journalparsestate = nulljournal{jparsedecimalmark=parseDecimalMark rules}
    mkerror n' s' e = error' . T.unpack $ T.unlines
      ["error: could not parse \"" <> s' <> "\" as balance"<> T.pack (show n') <> " amount"
      ,showRecord record
      ,showRules rules record
      -- ,"the default-currency is: "++fromMaybe "unspecified" mdefaultcurrency
      ,"the parse error is:      "<> T.pack (customErrorBundlePretty e)
      ]

-- Read a valid decimal mark from the decimal-mark rule, if any.
-- If the rule is present with an invalid argument, raise an error.
parseDecimalMark :: CsvRules -> Maybe DecimalMark
parseDecimalMark rules = do
    s <- rules `csvRule` "decimal-mark"
    case T.uncons s of
        Just (c, rest) | T.null rest && isDecimalMark c -> return c
        _ -> error' . T.unpack $ "decimal-mark's argument should be \".\" or \",\" (not \""<>s<>"\")"

-- | Make a balance assertion for the given amount, with the given parse
-- position (to be shown in assertion failures), with the assertion type
-- possibly set by a balance-type rule.
-- The CSV rules and current record are also provided, to be shown in case
-- balance-type's argument is bad (XXX refactor).
mkBalanceAssertion :: CsvRules -> CsvRecord -> (Amount, SourcePos) -> BalanceAssertion
mkBalanceAssertion rules record (amt, pos) = assrt{baamount=amt, baposition=pos}
  where
    assrt =
      case getDirective "balance-type" rules of
        Nothing    -> nullassertion
        Just "="   -> nullassertion
        Just "=="  -> nullassertion{batotal=True}
        Just "=*"  -> nullassertion{bainclusive=True}
        Just "==*" -> nullassertion{batotal=True, bainclusive=True}
        Just x     -> error' . T.unpack $ T.unlines  -- PARTIAL:
          [ "balance-type \"" <> x <>"\" is invalid. Use =, ==, =* or ==*."
          , showRecord record
          , showRules rules record
          ]

-- | Figure out the account name specified for posting N, if any.
-- And whether it is the default unknown account (which may be
-- improved later) or an explicitly set account (which may not).
getAccount :: CsvRules -> CsvRecord -> Maybe MixedAmount -> Maybe (Amount, SourcePos) -> Int -> Maybe (AccountName, Bool)
getAccount rules record mamount mbalance n =
  let
    fieldval = hledgerFieldValue rules record :: HledgerFieldName -> Maybe Text
    maccount = T.strip <$> fieldval ("account"<> T.pack (show n))
  in case maccount of
    -- accountN is set to the empty string - no posting will be generated
    Just "" -> Nothing
    -- accountN is set (possibly to "expenses:unknown"! #1192) - mark it final
    Just a  ->
      -- Check it and reject if invalid.. sometimes people try
      -- to set an amount or comment along with the account name.
      case parsewith (accountnamep >> eof) a of
        Left e  -> usageError $ errorBundlePretty e
        Right _ -> Just (a, True)
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

-- | Default account names to use when needed.
unknownExpenseAccount = "expenses:unknown"
unknownIncomeAccount  = "income:unknown"

type CsvAmountString = Text

-- | Canonicalise the sign in a CSV amount string.
-- Such strings can have a minus sign, parentheses (equivalent to minus),
-- or any two of these (which cancel out),
-- or a plus sign (which is removed),
-- or any sign by itself with no following number (which is removed).
-- See hledger > CSV FORMAT > Tips > Setting amounts.
--
-- These are supported (note, not every possibile combination):
--
-- >>> simplifySign "1"
-- "1"
-- >>> simplifySign "+1"
-- "1"
-- >>> simplifySign "-1"
-- "-1"
-- >>> simplifySign "(1)"
-- "-1"
-- >>> simplifySign "--1"
-- "1"
-- >>> simplifySign "-(1)"
-- "1"
-- >>> simplifySign "-+1"
-- "-1"
-- >>> simplifySign "(-1)"
-- "1"
-- >>> simplifySign "((1))"
-- "1"
-- >>> simplifySign "-"
-- ""
-- >>> simplifySign "()"
-- ""
-- >>> simplifySign "+"
-- ""
simplifySign :: CsvAmountString -> CsvAmountString
simplifySign amtstr
  | Just (' ',t) <- T.uncons amtstr = simplifySign t
  | Just (t,' ') <- T.unsnoc amtstr = simplifySign t
  | Just ('(',t) <- T.uncons amtstr, Just (amt,')') <- T.unsnoc t = simplifySign $ negateStr amt
  | Just ('-',b) <- T.uncons amtstr, Just ('(',t) <- T.uncons b, Just (amt,')') <- T.unsnoc t = simplifySign amt
  | Just ('-',m) <- T.uncons amtstr, Just ('-',amt) <- T.uncons m = amt
  | Just ('-',m) <- T.uncons amtstr, Just ('+',amt) <- T.uncons m = negateStr amt
  | amtstr `elem` ["-","+","()"] = ""
  | Just ('+',amt) <- T.uncons amtstr = simplifySign amt
  | otherwise = amtstr

negateStr :: Text -> Text
negateStr amtstr = case T.uncons amtstr of
    Just ('-',s) -> s
    _            -> T.cons '-' amtstr

--- ** tests
_TESTS__________________________________________ = undefined

tests_RulesReader = testGroup "RulesReader" [
   testGroup "parseCsvRules" [
     testCase "empty file" $
      parseCsvRules "unknown" "" @?= Right (mkrules defrules)
   ]
  ,testGroup "rulesp" [
     testCase "trailing comments" $
      parseWithState' defrules rulesp "skip\n# \n#\n" @?= Right (mkrules $ defrules{rdirectives = [("skip","")]})

    ,testCase "trailing blank lines" $
      parseWithState' defrules rulesp "skip\n\n  \n" @?= (Right (mkrules $ defrules{rdirectives = [("skip","")]}))

    ,testCase "no final newline" $
      parseWithState' defrules rulesp "skip" @?= (Right (mkrules $ defrules{rdirectives=[("skip","")]}))

    ,testCase "assignment with empty value" $
      parseWithState' defrules rulesp "account1 \nif foo\n  account2 foo\n" @?=
        (Right (mkrules $ defrules{rassignments = [("account1","")], rconditionalblocks = [CB{cbMatchers=[RecordMatcher None (toRegex' "foo")],cbAssignments=[("account2","foo")]}]}))
   ]
  ,testGroup "conditionalblockp" [
    testCase "space after conditional" $ -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
       -- #1120
      parseWithState' defrules conditionalblockp "if a\n account2 b\n \n" @?=
        (Right $ CB{cbMatchers=[RecordMatcher None $ toRegexCI' "a"],cbAssignments=[("account2","b")]})

  ],

  testGroup "csvfieldreferencep" [
    testCase "number" $ parseWithState' defrules csvfieldreferencep "%1" @?= (Right "%1")
   ,testCase "name" $ parseWithState' defrules csvfieldreferencep "%date" @?= (Right "%date")
   ,testCase "quoted name" $ parseWithState' defrules csvfieldreferencep "%\"csv date\"" @?= (Right "%\"csv date\"")
   ]

  ,testGroup "matcherp" [

    testCase "recordmatcherp" $
      parseWithState' defrules matcherp "A A\n" @?= (Right $ RecordMatcher None $ toRegexCI' "A A")

   ,testCase "recordmatcherp.starts-with-&" $
      parseWithState' defrules matcherp "& A A\n" @?= (Right $ RecordMatcher And $ toRegexCI' "A A")

   ,testCase "fieldmatcherp.starts-with-%" $
      parseWithState' defrules matcherp "description A A\n" @?= (Right $ RecordMatcher None $ toRegexCI' "description A A")

   ,testCase "fieldmatcherp" $
      parseWithState' defrules matcherp "%description A A\n" @?= (Right $ FieldMatcher None "%description" $ toRegexCI' "A A")

   ,testCase "fieldmatcherp.starts-with-&" $
      parseWithState' defrules matcherp "& %description A A\n" @?= (Right $ FieldMatcher And "%description" $ toRegexCI' "A A")

   -- ,testCase "fieldmatcherp with operator" $
   --    parseWithState' defrules matcherp "%description ~ A A\n" @?= (Right $ FieldMatcher "%description" "A A")

   ]

 ,testGroup "getEffectiveAssignment" [
    let rules = mkrules $ defrules {rcsvfieldindexes=[("csvdate",1)],rassignments=[("date","%csvdate")]}

    in testCase "toplevel" $ getEffectiveAssignment rules ["a","b"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1)], rconditionalblocks=[CB [FieldMatcher None "%csvdate" $ toRegex' "a"] [("date","%csvdate")]]}
    in testCase "conditional" $ getEffectiveAssignment rules ["a","b"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1)], rconditionalblocks=[CB [FieldMatcher Not "%csvdate" $ toRegex' "a"] [("date","%csvdate")]]}
    in testCase "negated-conditional-false" $ getEffectiveAssignment rules ["a","b"] "date" @?= (Nothing)
  
   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1)], rconditionalblocks=[CB [FieldMatcher Not "%csvdate" $ toRegex' "b"] [("date","%csvdate")]]}
    in testCase "negated-conditional-true" $ getEffectiveAssignment rules ["a","b"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1),("description",2)], rconditionalblocks=[CB [FieldMatcher None "%csvdate" $ toRegex' "a", FieldMatcher None "%description" $ toRegex' "b"] [("date","%csvdate")]]}
    in testCase "conditional-with-or-a" $ getEffectiveAssignment rules ["a"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1),("description",2)], rconditionalblocks=[CB [FieldMatcher None "%csvdate" $ toRegex' "a", FieldMatcher None "%description" $ toRegex' "b"] [("date","%csvdate")]]}
    in testCase "conditional-with-or-b" $ getEffectiveAssignment rules ["_", "b"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1),("description",2)], rconditionalblocks=[CB [FieldMatcher None "%csvdate" $ toRegex' "a", FieldMatcher And "%description" $ toRegex' "b"] [("date","%csvdate")]]}
    in testCase "conditional.with-and" $ getEffectiveAssignment rules ["a", "b"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1),("description",2)], rconditionalblocks=[CB [FieldMatcher None "%csvdate" $ toRegex' "a", FieldMatcher And "%description" $ toRegex' "b", FieldMatcher None "%description" $ toRegex' "c"] [("date","%csvdate")]]}
    in testCase "conditional.with-and-or" $ getEffectiveAssignment rules ["_", "c"] "date" @?= (Just "%csvdate")

   ]

 ]
