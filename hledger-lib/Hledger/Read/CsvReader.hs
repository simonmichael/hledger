--- * -*- outline-regexp:"--- \\*"; -*-
--- ** doc
-- In Emacs, use TAB on lines beginning with "-- *" to collapse/expand sections.
{-|

A reader for CSV data, using an extra rules file to help interpret the data.

-}
-- Lots of haddocks in this file are for non-exported types.
-- Here's a command that will render them:
-- stack haddock hledger-lib --fast --no-haddock-deps --haddock-arguments='--ignore-all-exports' --open

--- ** language
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ViewPatterns         #-}

--- ** exports
module Hledger.Read.CsvReader (
  -- * Reader
  reader,
  -- * Misc.
  CSV, CsvRecord, CsvValue,
  csvFileFor,
  rulesFileFor,
  parseRulesFile,
  printCSV,
  -- * Tests
  tests_CsvReader,
)
where

--- ** imports
import Prelude ()
import "base-compat-batteries" Prelude.Compat hiding (fail)
import Control.Applicative        (liftA2)
import Control.Exception          (IOException, handle, throw)
import Control.Monad              (liftM, unless, when)
import Control.Monad.Except       (ExceptT, throwError)
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, get, modify', evalStateT)
import Control.Monad.Trans.Class  (lift)
import Data.Char                  (toLower, isDigit, isSpace, isAlphaNum, isAscii, ord)
import Data.Bifunctor             (first)
import "base-compat-batteries" Data.List.Compat
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.MemoUgly (memo)
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Safe (atMay, headMay, lastMay, readDef, readMay)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeDirectory, takeExtension, takeFileName)
import qualified Data.Csv as Cassava
import qualified Data.Csv.Parser.Megaparsec as CassavaMP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (asum, toList)
import Text.Megaparsec hiding (match, parse)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Custom (customErrorBundlePretty, parseErrorAt)
import Text.Printf (printf)

import Hledger.Data
import Hledger.Utils
import Hledger.Read.Common (aliasesFromOpts,  Reader(..),InputOpts(..), amountp, statusp, genericSourcePos, journalFinalise )

--- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

--- ** some types

type CSV       = [CsvRecord]
type CsvRecord = [CsvValue]
type CsvValue  = Text

--- ** reader

reader :: MonadIO m => Reader m
reader = Reader
  {rFormat     = "csv"
  ,rExtensions = ["csv","tsv","ssv"]
  ,rReadFn     = parse
  ,rParser    = error' "sorry, CSV files can't be included yet"  -- PARTIAL:
  }

-- | Parse and post-process a "Journal" from CSV data, or give an error.
-- Does not check balance assertions.
-- XXX currently ignores the provided data, reads it from the file path instead.
parse :: InputOpts -> FilePath -> Text -> ExceptT String IO Journal
parse iopts f t = do
  let rulesfile = mrules_file_ iopts
  r <- liftIO $ readJournalFromCsv rulesfile f t
  case r of Left e   -> throwError e
            Right pj ->
              -- journalFinalise assumes the journal's items are
              -- reversed, as produced by JournalReader's parser.
              -- But here they are already properly ordered. So we'd
              -- better preemptively reverse them once more. XXX inefficient
              let pj' = journalReverse pj
              -- apply any command line account aliases. Can fail with a bad replacement pattern.
              in case journalApplyAliases (aliasesFromOpts iopts) pj' of
                  Left e -> throwError e
                  Right pj'' -> journalFinalise iopts{balancingopts_=(balancingopts_ iopts){ignore_assertions_=True}} f t pj''

--- ** reading rules files
--- *** rules utilities

-- Not used by hledger; just for lib users, 
-- | An pure-exception-throwing IO action that parses this file's content
-- as CSV conversion rules, interpolating any included files first,
-- and runs some extra validation checks.
parseRulesFile :: FilePath -> ExceptT String IO CsvRules
parseRulesFile f =
  liftIO (readFilePortably f >>= expandIncludes (takeDirectory f))
    >>= either throwError return . parseAndValidateCsvRules f

-- | Given a CSV file path, what would normally be the corresponding rules file ?
rulesFileFor :: FilePath -> FilePath
rulesFileFor = (++ ".rules")

-- | Given a CSV rules file path, what would normally be the corresponding CSV file ?
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

addDirective :: (DirectiveName, Text) -> CsvRulesParsed -> CsvRulesParsed
addDirective d r = r{rdirectives=d:rdirectives r}

addAssignment :: (HledgerFieldName, FieldTemplate) -> CsvRulesParsed -> CsvRulesParsed
addAssignment a r = r{rassignments=a:rassignments r}

setIndexesAndAssignmentsFromList :: [CsvFieldName] -> CsvRulesParsed -> CsvRulesParsed
setIndexesAndAssignmentsFromList fs r = addAssignmentsFromList fs . setCsvFieldIndexesFromList fs $ r

setCsvFieldIndexesFromList :: [CsvFieldName] -> CsvRulesParsed -> CsvRulesParsed
setCsvFieldIndexesFromList fs r = r{rcsvfieldindexes=zip fs [1..]}

addAssignmentsFromList :: [CsvFieldName] -> CsvRulesParsed -> CsvRulesParsed
addAssignmentsFromList fs r = foldl' maybeAddAssignment r journalfieldnames
  where
    maybeAddAssignment rules f = (maybe id addAssignmentFromIndex $ elemIndex f fs) rules
      where
        addAssignmentFromIndex i = addAssignment (f, T.pack $ '%':show (i+1))

addConditionalBlock :: ConditionalBlock -> CsvRulesParsed -> CsvRulesParsed
addConditionalBlock b r = r{rconditionalblocks=b:rconditionalblocks r}

addConditionalBlocks :: [ConditionalBlock] -> CsvRulesParsed -> CsvRulesParsed
addConditionalBlocks bs r = r{rconditionalblocks=bs++rconditionalblocks r}

getDirective :: DirectiveName -> CsvRules -> Maybe FieldTemplate
getDirective directivename = lookup directivename . rdirectives

instance ShowErrorComponent String where
  showErrorComponent = id

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
            f' = dir </> T.unpack (T.dropWhile isSpace f)
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

--- *** rules types

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
-- are in the same order as they were in the unput file and rblocksassigning is functional.
-- Ready to be used for CSV record processing
type CsvRules = CsvRules' (Text -> [ConditionalBlock])

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
data MatcherPrefix = And | None
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

matcherPrefix :: Matcher -> MatcherPrefix
matcherPrefix (RecordMatcher prefix _) = prefix
matcherPrefix (FieldMatcher prefix _ _) = prefix

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
  where (ys, zs) = span (\y -> matcherPrefix y == And) xs

--- *** rules parsers

{-
Grammar for the CSV conversion rules, more or less:

RULES: RULE*

RULE: ( FIELD-LIST | FIELD-ASSIGNMENT | CONDITIONAL-BLOCK | SKIP | NEWEST-FIRST | DATE-FORMAT | DECIMAL-MARK | COMMENT | BLANK ) NEWLINE

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
  r <- get
  return $ mkrules r

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
  ["date-format"
  ,"decimal-mark"
  ,"separator"
  -- ,"default-account"
  -- ,"default-currency"
  ,"skip"
  ,"newest-first"
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
    customFailure $ parseErrorAt start $  "start of conditional block found, but no assignment rules afterward\n(assignment rules in a conditional block should be indented)\n"
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
    m <- matcherp' (char sep >> return ())
    vs <- T.split (==sep) . T.pack <$> lift restofline
    if (length vs /= length fields)
      then customFailure $ parseErrorAt off $ ((printf "line of conditional table should have %d values, but this one has only %d\n" (length fields) (length vs)) :: String)
      else return (m,vs)
  when (null body) $
    customFailure $ parseErrorAt start $ "start of conditional table found, but no assignment rules afterward\n"
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
  --   Fail.fail "start of record matcher found, but no patterns afterward\n(patterns should not be indented)\n"
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
  (char '&' >> lift skipNonNewlineSpaces >> return And) <|> return None

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

--- ** reading csv files

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
      dbg6IO "using conversion rules file" rulesfile
      readFilePortably rulesfile >>= expandIncludes (takeDirectory rulesfile)
    else
      return $ defaultRulesText rulesfile
  rules <- either throwerr return $ parseAndValidateCsvRules rulesfile rulestext
  dbg6IO "csv rules" rules

  -- parse the skip directive's value, if any
  let skiplines = case getDirective "skip" rules of
                    Nothing -> 0
                    Just "" -> 1
                    Just s  -> readDef (throwerr $ "could not parse skip value: " ++ show s) $ T.unpack s

  -- parse csv
  let
    -- parsec seems to fail if you pass it "-" here TODO: try again with megaparsec
    parsecfilename = if csvfile == "-" then "(stdin)" else csvfile
    separator =
      case getDirective "separator" rules >>= parseSeparator of
        Just c           -> c
        _ | ext == "ssv" -> ';'
        _ | ext == "tsv" -> '\t'
        _                -> ','
        where
          ext = map toLower $ drop 1 $ takeExtension csvfile
  dbg6IO "using separator" separator
  records <- (either throwerr id .
              dbg7 "validateCsv" . validateCsv rules skiplines .
              dbg7 "parseCsv")
             `fmap` parseCsv separator parsecfilename csvdata
  dbg6IO "first 3 csv records" $ take 3 records

  -- identify header lines
  -- let (headerlines, datalines) = identifyHeaderLines records
  --     mfieldnames = lastMay headerlines

  let
    -- convert CSV records to transactions
    txns = dbg7 "csv txns" $ snd $ mapAccumL
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
      (if newestfirst || mdataseemsnewestfirst == Just True 
        then dbg7 "reversed csv txns" . reverse else id) 
        txns
      where
        newestfirst = dbg6 "newestfirst" $ isJust $ getDirective "newest-first" rules
        mdataseemsnewestfirst = dbg6 "mdataseemsnewestfirst" $
          case nub $ map tdate txns of
            ds | length ds > 1 -> Just $ head ds > last ds
            _                  -> Nothing
    -- Second, sort by date.
    txns'' = dbg7 "date-sorted csv txns" $ sortBy (comparing tdate) txns'

  when (not rulesfileexists) $ do
    dbg1IO "creating conversion rules file" rulesfile
    T.writeFile rulesfile rulestext

  return $ Right nulljournal{jtxns=txns''}

-- | Parse special separator names TAB and SPACE, or return the first
-- character. Return Nothing on empty string
parseSeparator :: Text -> Maybe Char
parseSeparator = specials . T.toLower
  where specials "space" = Just ' '
        specials "tab"   = Just '\t'
        specials xs      = fst <$> T.uncons xs

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
        unpackFields  = (fmap . fmap) T.decodeUtf8

printCSV :: CSV -> TL.Text
printCSV = TB.toLazyText . unlinesB . map printRecord
    where printRecord = foldMap TB.fromText . intersperse "," . map printField
          printField = wrap "\"" "\"" . T.replace "\"" "\"\""

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
        (Nothing, Just x) -> Just (read $ T.unpack x)
    applyConditionalSkips [] = []
    applyConditionalSkips (r:rest) =
      case skipCount r of
        Nothing -> r:(applyConditionalSkips rest)
        Just cnt -> applyConditionalSkips (drop (cnt-1) rest)
    validate [] = Right []
    validate rs@(_first:_) = case lessthan2 of
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

showRules rules record =
  T.unlines $ catMaybes [ (("the "<>fld<>" rule is: ")<>) <$> getEffectiveAssignment rules record fld | fld <- journalfieldnames]

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
hledgerFieldValue rules record = fmap (renderTemplate rules record) . hledgerField rules record

transactionFromCsvRecord :: SourcePos -> CsvRules -> CsvRecord -> Transaction
transactionFromCsvRecord sourcepos rules record = t
  where
    ----------------------------------------------------------------------
    -- 1. Define some helpers:

    rule     = csvRule           rules        :: DirectiveName    -> Maybe FieldTemplate
    -- ruleval  = csvRuleValue      rules record :: DirectiveName    -> Maybe String
    field    = hledgerField      rules record :: HledgerFieldName -> Maybe FieldTemplate
    fieldval = hledgerFieldValue rules record :: HledgerFieldName -> Maybe Text
    parsedate = parseDateWithCustomOrDefaultFormats (rule "date-format")
    mkdateerror datefield datevalue mdateformat = T.unpack $ T.unlines
      ["error: could not parse \""<>datevalue<>"\" as a date using date format "
        <>maybe "\"YYYY/M/D\", \"YYYY-M-D\" or \"YYYY.M.D\"" (T.pack . show) mdateformat
      ,showRecord record
      ,"the "<>datefield<>" rule is:   "<>(fromMaybe "required, but missing" $ field datefield)
      ,"the date-format is: "<>fromMaybe "unspecified" mdateformat
      ,"you may need to "
        <>"change your "<>datefield<>" rule, "
        <>maybe "add a" (const "change your") mdateformat<>" date-format rule, "
        <>"or "<>maybe "add a" (const "change your") mskip<>" skip rule"
      ,"for m/d/y or d/m/y dates, use date-format %-m/%-d/%Y or date-format %-d/%-m/%Y"
      ]
      where
        mskip = rule "skip"

    ----------------------------------------------------------------------
    -- 2. Gather values needed for the transaction itself, by evaluating the
    -- field assignment rules using the CSV record's data, and parsing a bit
    -- more where needed (dates, status).

    mdateformat = rule "date-format"
    date        = fromMaybe "" $ fieldval "date"
    -- PARTIAL:
    date'       = fromMaybe (error' $ mkdateerror "date" date mdateformat) $ parsedate date
    mdate2      = fieldval "date2"
    mdate2'     = maybe Nothing (maybe (error' $ mkdateerror "date2" (fromMaybe "" mdate2) mdateformat) Just . parsedate) mdate2
    status      =
      case fieldval "status" of
        Nothing -> Unmarked
        Just s  -> either statuserror id $ runParser (statusp <* eof) "" s
          where
            statuserror err = error' . T.unpack $ T.unlines
              ["error: could not parse \""<>s<>"\" as a cleared status (should be *, ! or empty)"
              ,"the parse error is:      "<>T.pack (customErrorBundlePretty err)
              ]
    code        = maybe "" singleline $ fieldval "code"
    description = maybe "" singleline $ fieldval "description"
    comment     = maybe "" singleline $ fieldval "comment"
    precomment  = maybe "" singleline $ fieldval "precomment"

    singleline = T.unwords . filter (not . T.null) . map T.strip . T.lines

    ----------------------------------------------------------------------
    -- 3. Generate the postings for which an account has been assigned
    -- (possibly indirectly due to an amount or balance assignment)

    p1IsVirtual = (accountNamePostingType <$> fieldval "account1") == Just VirtualPosting
    ps = [p | n <- [1..maxpostings]
         ,let comment  = fromMaybe "" $ fieldval ("comment"<> T.pack (show n))
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
                             ,pcomment          = comment
                             ,ptype             = accountNamePostingType acct
                             }
         ]

    ----------------------------------------------------------------------
    -- 4. Build the transaction (and name it, so the postings can reference it).

    t = nulltransaction{
           tsourcepos        = genericSourcePos sourcepos  -- the CSV line number
          ,tdate             = date'
          ,tdate2            = mdate2'
          ,tstatus           = status
          ,tcode             = code
          ,tdescription      = description
          ,tcomment          = comment
          ,tprecedingcomment = precomment
          ,tpostings         = ps
          }  

-- | Figure out the amount specified for posting N, if any.
-- A currency symbol to prepend to the amount, if any, is provided,
-- and whether posting 1 requires balancing or not.
-- This looks for a non-empty amount value assigned to "amountN", "amountN-in", or "amountN-out".
-- For postings 1 or 2 it also looks at "amount", "amount-in", "amount-out".
-- If more than one of these has a value, it looks for one that is non-zero.
-- If there's multiple non-zeros, or no non-zeros but multiple zeros, it throws an error.
getAmount :: CsvRules -> CsvRecord -> Text -> Bool -> Int -> Maybe MixedAmount
getAmount rules record currency p1IsVirtual n =
  -- Warning, many tricky corner cases here.
  -- docs: hledger_csv.m4.md #### amount
  -- tests: hledger/test/csv.test ~ 13, 31-34
  let
    unnumberedfieldnames = ["amount","amount-in","amount-out"]

    -- amount field names which can affect this posting
    fieldnames = map (("amount"<> T.pack(show n))<>) ["","-in","-out"]
                 -- For posting 1, also recognise the old amount/amount-in/amount-out names.
                 -- For posting 2, the same but only if posting 1 needs balancing.
                 ++ if n==1 || n==2 && not p1IsVirtual then unnumberedfieldnames else []

    -- assignments to any of these field names with non-empty values
    assignments = [(f,a') | f <- fieldnames
                          , Just v <- [T.strip . renderTemplate rules record <$> hledgerField rules record f]
                          , not $ T.null v
                          , let a = parseAmount rules record currency v
                          -- With amount/amount-in/amount-out, in posting 2,
                          -- flip the sign and convert to cost, as they did before 1.17
                          , let a' = if f `elem` unnumberedfieldnames && n==2 then mixedAmountCost (maNegate a) else a
                          ]

    -- if any of the numbered field names are present, discard all the unnumbered ones
    assignments' | any isnumbered assignments = filter isnumbered assignments
                 | otherwise                  = assignments
      where
        isnumbered (f,_) = T.any (flip elem ['0'..'9']) f

    -- if there's more than one value and only some are zeros, discard the zeros
    assignments''
      | length assignments' > 1 && not (null nonzeros) = nonzeros
      | otherwise                                      = assignments'
      where nonzeros = filter (not . mixedAmountLooksZero . snd) assignments'

  in case -- dbg0 ("amounts for posting "++show n)
          assignments'' of
      [] -> Nothing
      [(f,a)] | "-out" `T.isSuffixOf` f -> Just (maNegate a)  -- for -out fields, flip the sign
      [(_,a)] -> Just a
      fs      -> error' . T.unpack . T.unlines $ [  -- PARTIAL:
         "multiple non-zero amounts or multiple zero amounts assigned,"
        ,"please ensure just one. (https://hledger.org/csv.html#amount)"
        ,"  " <> showRecord record
        ,"  for posting: " <> T.pack (show n)
        ]
        ++ ["  assignment: " <> f <> " " <>
             fromMaybe "" (hledgerField rules record f) <>
             "\t=> value: " <> wbToText (showMixedAmountB noColour a) -- XXX not sure this is showing all the right info
           | (f,a) <- fs]

-- | Figure out the expected balance (assertion or assignment) specified for posting N,
-- if any (and its parse position).
getBalance :: CsvRules -> CsvRecord -> Text -> Int -> Maybe (Amount, GenericSourcePos)
getBalance rules record currency n = do
  v <- (fieldval ("balance"<> T.pack (show n))
        -- for posting 1, also recognise the old field name
        <|> if n==1 then fieldval "balance" else Nothing)
  case v of
    "" -> Nothing
    s  -> Just (
            parseBalanceAmount rules record currency n s
           ,nullsourcepos  -- parse position to show when assertion fails,
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
    mkerror n s e = error' . T.unpack $ T.unlines
      ["error: could not parse \"" <> s <> "\" as balance"<> T.pack (show n) <> " amount"
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
mkBalanceAssertion :: CsvRules -> CsvRecord -> (Amount, GenericSourcePos) -> BalanceAssertion
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
getAccount :: CsvRules -> CsvRecord -> Maybe MixedAmount -> Maybe (Amount, GenericSourcePos) -> Int -> Maybe (AccountName, Bool)
getAccount rules record mamount mbalance n =
  let
    fieldval = hledgerFieldValue rules record :: HledgerFieldName -> Maybe Text
    maccount = fieldval ("account"<> T.pack (show n))
  in case maccount of
    -- accountN is set to the empty string - no posting will be generated
    Just "" -> Nothing
    -- accountN is set (possibly to "expenses:unknown"! #1192) - mark it final
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

-- | Show a (approximate) recreation of the original CSV record.
showRecord :: CsvRecord -> Text
showRecord r = "record values: "<>T.intercalate "," (map (wrap "\"" "\"") r)

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
      where
        -- all top level field assignments
        toplevelassignments    = rassignments rules
        -- all field assignments in conditional blocks assigning to field f and active for the current csv record
        conditionalassignments = concatMap cbAssignments $ filter isBlockActive $ (rblocksassigning rules) f
          where
            -- does this conditional block match the current csv record ?
            isBlockActive :: ConditionalBlock -> Bool
            isBlockActive CB{..} = any (all matcherMatches) $ groupedMatchers cbMatchers
              where
                -- does this individual matcher match the current csv record ?
                matcherMatches :: Matcher -> Bool
                matcherMatches (RecordMatcher _ pat) = regexMatchText pat' wholecsvline
                  where
                    pat' = dbg7 "regex" pat
                    -- A synthetic whole CSV record to match against. Note, this can be
                    -- different from the original CSV data:
                    -- - any whitespace surrounding field values is preserved
                    -- - any quotes enclosing field values are removed
                    -- - and the field separator is always comma
                    -- which means that a field containing a comma will look like two fields.
                    wholecsvline = dbg7 "wholecsvline" $ T.intercalate "," record
                matcherMatches (FieldMatcher _ csvfieldref pat) = regexMatchText pat csvfieldvalue
                  where
                    -- the value of the referenced CSV field to match against.
                    csvfieldvalue = dbg7 "csvfieldvalue" $ replaceCsvFieldReference rules record csvfieldref

-- | Render a field assignment's template, possibly interpolating referenced
-- CSV field values. Outer whitespace is removed from interpolated values.
renderTemplate ::  CsvRules -> CsvRecord -> FieldTemplate -> Text
renderTemplate rules record t = maybe t mconcat $ parseMaybe
    (many $ takeWhile1P Nothing (/='%')
        <|> replaceCsvFieldReference rules record <$> referencep)
    t
  where
    referencep = liftA2 T.cons (char '%') (takeWhile1P (Just "reference") isDescriptorChar) :: Parsec CustomErr Text Text
    isDescriptorChar c = isAscii c && (isAlphaNum c || c == '_' || c == '-')

-- | Replace something that looks like a reference to a csv field ("%date" or "%1)
-- with that field's value. If it doesn't look like a field reference, or if we
-- can't find such a field, leave it unchanged.
replaceCsvFieldReference :: CsvRules -> CsvRecord -> CsvFieldReference -> Text
replaceCsvFieldReference rules record s = case T.uncons s of
    Just ('%', fieldname) -> fromMaybe s $ csvFieldValue rules record fieldname
    _                     -> s

-- | Get the (whitespace-stripped) value of a CSV field, identified by its name or
-- column number, ("date" or "1"), from the given CSV record, if such a field exists.
csvFieldValue :: CsvRules -> CsvRecord -> CsvFieldName -> Maybe Text
csvFieldValue rules record fieldname = do
  fieldindex <- if | T.all isDigit fieldname -> readMay $ T.unpack fieldname
                   | otherwise               -> lookup (T.toLower fieldname) $ rcsvfieldindexes rules
  fieldvalue <- T.strip <$> atMay record (fieldindex-1)
  return fieldvalue

-- | Parse the date string using the specified date-format, or if unspecified
-- the "simple date" formats (YYYY/MM/DD, YYYY-MM-DD, YYYY.MM.DD, leading
-- zeroes optional).
parseDateWithCustomOrDefaultFormats :: Maybe DateFormat -> Text -> Maybe Day
parseDateWithCustomOrDefaultFormats mformat s = asum $ map parsewith formats
  where
    parsewith = flip (parseTimeM True defaultTimeLocale) (T.unpack s)
    formats = map T.unpack $ maybe
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

--- ** tests

tests_CsvReader = tests "CsvReader" [
   tests "parseCsvRules" [
     test "empty file" $
      parseCsvRules "unknown" "" @?= Right (mkrules defrules)
   ]
  ,tests "rulesp" [
     test "trailing comments" $
      parseWithState' defrules rulesp "skip\n# \n#\n" @?= Right (mkrules $ defrules{rdirectives = [("skip","")]})

    ,test "trailing blank lines" $
      parseWithState' defrules rulesp "skip\n\n  \n" @?= (Right (mkrules $ defrules{rdirectives = [("skip","")]}))

    ,test "no final newline" $
      parseWithState' defrules rulesp "skip" @?= (Right (mkrules $ defrules{rdirectives=[("skip","")]}))

    ,test "assignment with empty value" $
      parseWithState' defrules rulesp "account1 \nif foo\n  account2 foo\n" @?=
        (Right (mkrules $ defrules{rassignments = [("account1","")], rconditionalblocks = [CB{cbMatchers=[RecordMatcher None (toRegex' "foo")],cbAssignments=[("account2","foo")]}]}))
   ]
  ,tests "conditionalblockp" [
    test "space after conditional" $ -- #1120
      parseWithState' defrules conditionalblockp "if a\n account2 b\n \n" @?=
        (Right $ CB{cbMatchers=[RecordMatcher None $ toRegexCI' "a"],cbAssignments=[("account2","b")]})

  ,tests "csvfieldreferencep" [
    test "number" $ parseWithState' defrules csvfieldreferencep "%1" @?= (Right "%1")
   ,test "name" $ parseWithState' defrules csvfieldreferencep "%date" @?= (Right "%date")
   ,test "quoted name" $ parseWithState' defrules csvfieldreferencep "%\"csv date\"" @?= (Right "%\"csv date\"")
   ]

  ,tests "matcherp" [

    test "recordmatcherp" $
      parseWithState' defrules matcherp "A A\n" @?= (Right $ RecordMatcher None $ toRegexCI' "A A")

   ,test "recordmatcherp.starts-with-&" $
      parseWithState' defrules matcherp "& A A\n" @?= (Right $ RecordMatcher And $ toRegexCI' "A A")

   ,test "fieldmatcherp.starts-with-%" $
      parseWithState' defrules matcherp "description A A\n" @?= (Right $ RecordMatcher None $ toRegexCI' "description A A")

   ,test "fieldmatcherp" $
      parseWithState' defrules matcherp "%description A A\n" @?= (Right $ FieldMatcher None "%description" $ toRegexCI' "A A")

   ,test "fieldmatcherp.starts-with-&" $
      parseWithState' defrules matcherp "& %description A A\n" @?= (Right $ FieldMatcher And "%description" $ toRegexCI' "A A")

   -- ,test "fieldmatcherp with operator" $
   --    parseWithState' defrules matcherp "%description ~ A A\n" @?= (Right $ FieldMatcher "%description" "A A")

   ]

  ,tests "getEffectiveAssignment" [
    let rules = mkrules $ defrules {rcsvfieldindexes=[("csvdate",1)],rassignments=[("date","%csvdate")]}

    in test "toplevel" $ getEffectiveAssignment rules ["a","b"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1)], rconditionalblocks=[CB [FieldMatcher None "%csvdate" $ toRegex' "a"] [("date","%csvdate")]]}
    in test "conditional" $ getEffectiveAssignment rules ["a","b"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1),("description",2)], rconditionalblocks=[CB [FieldMatcher None "%csvdate" $ toRegex' "a", FieldMatcher None "%description" $ toRegex' "b"] [("date","%csvdate")]]}
    in test "conditional-with-or-a" $ getEffectiveAssignment rules ["a"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1),("description",2)], rconditionalblocks=[CB [FieldMatcher None "%csvdate" $ toRegex' "a", FieldMatcher None "%description" $ toRegex' "b"] [("date","%csvdate")]]}
    in test "conditional-with-or-b" $ getEffectiveAssignment rules ["_", "b"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1),("description",2)], rconditionalblocks=[CB [FieldMatcher None "%csvdate" $ toRegex' "a", FieldMatcher And "%description" $ toRegex' "b"] [("date","%csvdate")]]}
    in test "conditional.with-and" $ getEffectiveAssignment rules ["a", "b"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1),("description",2)], rconditionalblocks=[CB [FieldMatcher None "%csvdate" $ toRegex' "a", FieldMatcher And "%description" $ toRegex' "b", FieldMatcher None "%description" $ toRegex' "c"] [("date","%csvdate")]]}
    in test "conditional.with-and-or" $ getEffectiveAssignment rules ["_", "c"] "date" @?= (Just "%csvdate")

   ]

  ]

 ]
