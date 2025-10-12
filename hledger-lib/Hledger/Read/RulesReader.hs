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
{-# LANGUAGE CPP    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

--- ** exports
module Hledger.Read.RulesReader (
  -- * Reader
  reader,
  -- * Misc.
  dataFileFor,
  rulesFileFor,
  getRulesFile,
  readRules,
  rulesEncoding,
  readJournalFromCsv,
  parseBalanceAssertionType,
  -- * Tests
  tests_RulesReader,
)
where

--- ** imports
import Prelude hiding (Applicative(..))
import Control.Applicative (Applicative(..))
import Control.Concurrent (forkIO)
import Control.DeepSeq (deepseq)
import Control.Monad (unless, void, when)
import Control.Monad.Except       (ExceptT(..), liftEither, throwError)
import Control.Monad.Fail qualified as Fail
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, get, modify', evalStateT)
import Control.Monad.Trans.Class  (lift)
import Data.Char                  (toLower, isDigit, isSpace, isAlphaNum, ord)
import Data.Bifunctor             (first)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Csv qualified as Cassava
import Data.Csv.Parser.Megaparsec qualified as CassavaMegaparsec
import Data.Encoding (encodingFromStringExplicit, DynEncoding)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.List (elemIndex, mapAccumL, nub, sortOn)
-- import Data.List (elemIndex, mapAccumL, nub, sortOn, isPrefixOf, sortBy)
-- import Data.Ord (Down(..), comparing)
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.List.Extra (groupOn)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.MemoUgly (memo)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Time ( Day, TimeZone, UTCTime, LocalTime, ZonedTime(ZonedTime),
  defaultTimeLocale, getCurrentTimeZone, localDay, parseTimeM, utcToLocalTime, localTimeToUTC, zonedTimeToUTC, utctDay)
import Safe (atMay, headMay, lastMay, readMay)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, getModificationTime, removeFile)
-- import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, getModificationTime, listDirectory, renameFile, doesDirectoryExist)
import System.Exit      (ExitCode(..))
import System.FilePath (stripExtension, takeBaseName, takeDirectory, takeExtension, takeFileName, (<.>), (</>))
import System.IO       (Handle, hClose, hPutStrLn, stderr, hGetContents')
import System.Process  (CreateProcess(..), StdStream(CreatePipe), shell, waitForProcess, withCreateProcess)
import Data.Foldable (asum, toList)
import Text.Megaparsec hiding (match, parse)
import Text.Megaparsec.Char (char, newline, string, digitChar)
import Text.Printf (printf)

import Hledger.Data
import Hledger.Utils
import Hledger.Read.Common (aliasesFromOpts, Reader(..), InputOpts(..), amountp, statusp, journalFinalise, accountnamep, transactioncommentp, postingcommentp )
import Hledger.Write.Csv

--- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

--- ** reader
_READER__________________________________________ = undefined  -- VSCode outline separator


reader :: MonadIO m => Reader m
reader = Reader
  {rFormat     = Rules
  ,rExtensions = ["rules"]
  ,rReadFn     = parse
  ,rParser     = const $ fail "sorry, rules files can't be included yet"
  }

isFileName f = takeFileName f == f

getDownloadDir = do
  home <- getHomeDirectory
  return $ home </> "Downloads"  -- XXX

-- | Read, parse and post-process a "Journal" from the given rules file, or give an error.
-- This particular reader also provides some extra features like data cleaning/generating commands and data archiving.
--
-- The provided input file handle, and the --rules option, are ignored by this reader.
-- Instead, a data file (or data-generating command) is usually specified by the @source@ rule.
-- If there's no source rule, the data file is assumed to be named like the rules file without .rules, in the same directory.
--
-- The source rule supports ~ for home directory: @source ~/Downloads/foo.csv@.
-- If the argument is a bare filename, its directory is assumed to be ~/Downloads: @source foo.csv@.
-- Otherwise if it is a relative path, it is assumed to be relative to the rules file's directory: @source new/foo.csv@.
--
-- The source rule can specify a glob pattern: @source foo*.csv@.
-- If the glob pattern matches multiple files, the newest (last modified) file is used (with one exception, described below).
--
-- The source rule can specify a data-cleaning command, after a @|@ separator: @source foo*.csv | sed -e 's/USD/$/g'@.
-- This command is executed by the user's default shell, receives the data file's content on stdin,
-- and should output CSV data suitable for the conversion rules.
-- A # character can be used to comment out the data-cleaning command: @source foo*.csv  # | ...@.
--
-- Or the source rule can specify just a data-generating command, with no file pattern: @source | foo-csv.sh@.
-- In this case the command receives no input; it should output CSV data suitable for the conversion rules.
--
-- If the archive rule is present:
-- After successfully reading the data file or data command and converting to a journal, while doing a non-dry-run import:
-- the data will be archived in an auto-created data/ directory next to the rules file,
-- with a name based on the rules file and the data file's modification date and extension
-- (or for a data-generating command, the current date and the ".csv" extension).
-- And import will prefer the oldest file matched by a glob pattern (not the newest).
--
-- Balance assertions are not checked by this reader.
--
parse :: InputOpts -> FilePath -> Handle -> ExceptT String IO Journal
parse iopts rulesfile h = do
  lift $ hClose h -- We don't need it (XXX why ?)

  -- The rules reader does a lot; we must be organised.

  -- 1. gather contextual info
  --  gives: import flag, dryrun flag, rulesdir

  let
    args     = progArgs
    import_  = dbg2 "import" $ any (`elem` args) ["import", "imp"]
    dryrun   = dbg2 "dryrun" $ any (`elem` args) ["--dry-run", "--dry"]
    rulesdir = takeDirectory rulesfile

  -- 2. parse the source and archive rules
  --  needs: rules file
  --  gives: file pattern, data cleaning/generating command, archive flag

  -- XXX higher-than usual logging priority for file reading (normally 6 or 7), to bypass excessive noise from elsewhere
  rules <- readRules $ dbg1 "reading rules file" rulesfile
  let
    msourcearg = getDirective "source" rules
      -- Nothing -> error' $ rulesfile ++ " source rule must specify a file pattern or a command"
    -- Surrounding whitespace is removed from the whole source argument and from each part of it.
    -- A # before | makes the rest of line a comment.
    -- A # after | is left for the shell to interpret; it could be part of the command or the start of a comment.
    stripspaces = T.strip
    stripcommentandspaces = stripspaces . T.takeWhile (/= '#')
    mpatandcmd = T.breakOn "|" . stripspaces <$> msourcearg
    mpat = dbg2 "file pattern" $  -- a non-empty file pattern, or nothing
      case T.unpack . stripcommentandspaces . fst <$> mpatandcmd of
        Just s | not $ null s -> Just s
        _ -> Nothing
    mcmd = dbg2 "data command" $  -- a non-empty command, or nothing
      mpatandcmd >>= \sc ->
        let c = T.unpack . stripspaces . T.drop 1 . snd $ sc
        in if null c then Nothing else Just c

    archive = isJust (getDirective "archive" rules)

  -- 3. find the file to be read, if any
  --  needs: file pattern, data command, import flag, archive flag, downloads dir
  --  gives: data file, data file description

  (mdatafile, datafiledesc) <- dbg2 "data file found ?" <$> case (mpat, mcmd) of
    (Nothing, Nothing) -> error' $ "to make " ++ rulesfile ++ " readable,\n please add a 'source' rule with a non-empty file pattern or command"
    (Nothing, Just _) -> return (Nothing, "")
    (Just pat, _) -> do
      dldir <- liftIO getDownloadDir  -- look here for the data file if it's specified without a directory
      let
        (startdir, dirdesc)
          | isFileName pat = (dldir,    " in download directory")
          | otherwise      = (rulesdir, "")
      fs <- liftIO $
        expandGlob startdir pat
        >>= sortByModTime
        <&> dbg2 ("matched files"<>dirdesc<>", oldest first")
      return $
        if import_ && archive
        then (headMay fs, " oldest file")
        else (lastMay fs, " newest file")
    
  -- 4. log which file we are reading/importing/cleaning/generating
  --  needs: data file, data file description, import flag

  case (mdatafile, datafiledesc) of
    (Just f, desc) -> dbg1IO ("trying to " ++ (if import_ then "import" else "read") ++ desc) f
    (Nothing, _)   -> return ()

  -- 5. read raw, cleaned or generated data
  --  needs: file pattern, data file, optional data file encoding, data command
  --  gives: clean data (possibly empty)

  mexistingdatafile <- maybe (return Nothing) (\f -> liftIO $ do
    exists <- doesFileExist f
    return $ if exists then Just f else Nothing
    ) $ mdatafile
  cleandata <- dbg1With (\t -> "read "++(show $ length $ T.lines t)++" lines") <$> case (mpat, mexistingdatafile, mcmd) of

    -- file pattern, but no file found
    (Just _, Nothing, _) -> -- trace "file pattern, but no file found" $
      return ""

    -- file found, and maybe a data cleaning command
    (_, Just f,  mc) -> do  -- trace "file found" $
      mencoding <- rulesEncoding rules
      liftIO $ do
        raw <- openFileOrStdin f >>= hGetContentsPortably mencoding
        maybe (return raw) (\c -> runCommandAsFilter rulesfile (dbg0Msg ("running: "++c) c) raw) mc

    -- no file pattern, but a data generating command
    (Nothing, _, Just cmd) -> -- trace "data generating command" $
      liftIO $ runCommand rulesfile $ dbg0Msg ("running: " ++ cmd) cmd

    -- neither a file pattern nor a data generating command
    (Nothing, _, Nothing) -> -- trace "no file pattern or data generating command" $
      error' $ rulesfile ++ " source rule must specify a file pattern or a command"

  -- 6. convert the clean data to a (possibly empty) journal
  --  needs: clean data, rules, data file if any
  --  gives: journal

  j <- do
    readJournalFromCsv rules (fromMaybe "(cmd)" mdatafile) cleandata Nothing
    -- apply any command line account aliases. Can fail with a bad replacement pattern.
    >>= liftEither . journalApplyAliases (aliasesFromOpts iopts)
        -- journalFinalise assumes the journal's items are
        -- reversed, as produced by JournalReader's parser.
        -- But here they are already properly ordered. So we'd
        -- better preemptively reverse them once more. XXX inefficient
        . journalReverse
    >>= journalFinalise iopts{balancingopts_=(balancingopts_ iopts){ignore_assertions_=True}} rulesfile ""

  -- 7. if non-empty, successfully read and converted, and we're doing a non-dry-run archiving import: archive the data
  --  needs: import/archive/dryrun flags, rules directory, rules file, data file if any, clean data

  when (not (T.null cleandata) && import_ && archive && not dryrun) $
    liftIO $ saveToArchive (rulesdir </> "data") rulesfile mdatafile cleandata

  return j

-- | For the given rules file, run the given shell command, in the rules file's directory.
-- If the command fails, raise an error and show its error output;
-- otherwise return its output, and show any error output as a warning.
runCommand :: FilePath -> String -> IO Text
runCommand rulesfile cmd = do
  let process = (shell cmd) { cwd = Just $ takeDirectory rulesfile, std_out = CreatePipe, std_err = CreatePipe }
  withCreateProcess process $ \_ mhout mherr phandle -> do
    case (mhout, mherr) of
      (Just hout, Just herr) -> do
        out <- T.hGetContents hout
        err <- hGetContents' herr
        exitCode <- waitForProcess phandle
        case exitCode of
          ExitSuccess -> do
            unless (null err) $ warnIO err
            return out
          ExitFailure code ->
            error' $ "in " ++ rulesfile ++ ": command \"" ++ cmd ++ "\" failed with exit code " ++ show code
              ++ (if null err then "" else ":\n" ++ err)
      _ -> error' $ "in " ++ rulesfile ++ ": failed to create pipes for command execution"

-- | For the given rules file, run the given shell command, in the rules file's directory, passing the given text as input.
-- Return the output, or if the command fails, raise an informative error.
runCommandAsFilter :: FilePath -> String -> Text -> IO Text
runCommandAsFilter rulesfile cmd input = do
  let process = (shell cmd) { cwd = Just $ takeDirectory rulesfile, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  withCreateProcess process $ \mhin mhout mherr phandle -> do
    case (mhin, mhout, mherr) of
      (Just hin, Just hout, Just herr) -> do
        forkIO $ T.hPutStr hin input >> hClose hin
        out <- T.hGetContents hout
        err <- hGetContents' herr
        exitCode <- waitForProcess phandle
        case exitCode of
          ExitSuccess -> return out
          ExitFailure code ->
            error' $ "in " ++ rulesfile ++ ": command \"" ++ cmd ++ "\" failed with exit code " ++ show code
              ++ (if null err then "" else ":\n" ++ err)
      _ -> error' $ "in " ++ rulesfile ++ ": failed to create pipes for command execution"

type DirPath = FilePath

-- | Save some successfully imported data
-- (more precisely: data that was successfully read and maybe cleaned, or that was generated, during an import)
-- to the given archive directory, autocreating that if needed, and show informational output on stderr.
-- The arguments are:
-- the archive directory,
-- the rules file (for naming),
-- the data file name, if any,
-- the data that was read, cleaned, or generated.
-- The archive file name will be RULESFILEBASENAME.DATAFILEMODDATEORCURRENTDATE.DATAFILEEXTORCSV.
-- Note for a data generating command, where there's no data file, we use the current date
-- and a .csv file extension (meaning "character-separated values" in this case).
saveToArchive :: DirPath -> FilePath -> Maybe FilePath -> Text -> IO ()
saveToArchive archivedir rulesfile mdatafile cleandata = do
  createDirectoryIfMissing True archivedir
  (_, cleanname) <- archiveFileName rulesfile mdatafile
  let cleanarchive = archivedir </> cleanname
  hPutStrLn stderr $ "archiving " <> cleanarchive
  T.writeFile cleanarchive cleandata
  maybe (return ()) removeFile mdatafile

-- | Figure out the file names to use when archiving, for the given rules file and the given data file if any.
-- The second name is for the final (possibly cleaned) data; the first name has ".orig" added,
-- and is used if both original and cleaned data are being archived. They will be like this:
-- ("RULESFILEBASENAME.orig.DATAFILEMODDATE.DATAFILEEXT", "RULESFILEBASENAME.DATAFILEMODDATE.DATAFILEEXT")
archiveFileName :: FilePath -> Maybe FilePath -> IO (String, String)
archiveFileName rulesfile mdatafile = do
  let base = takeBaseName rulesfile
  case mdatafile of
    Just datafile -> do
      moddate <- (show . utctDay) <$> getModificationTime datafile
      let ext = takeExtension datafile
      return (
         base <.> "orig" <.> moddate <.> ext
        ,base            <.> moddate <.> ext
        )
    Nothing -> do
      let ext = "csv"
      curdate <- show <$> getCurrentDay
      return (
         base <.> "orig" <.> curdate <.> ext
        ,base            <.> curdate <.> ext
        )

-- -- | In the given archive directory, if it exists, find the paths of data files saved for the given rules file.
-- -- They will be reverse sorted by name, ie newest first, assuming normal archive file names.
-- --
-- -- We don't know which extension the data files use, but we look for file names beginning with
-- -- the rules file's base name followed by .YYYY-MM-DD, which will normally be good enough.
-- --
-- archivesFor :: FilePath -> FilePath -> IO [FilePath]
-- archivesFor archivedir rulesfile = do
--   exists <- doesDirectoryExist archivedir
--   if not exists then return []
--   else do
--     let prefix = takeBaseName rulesfile <> "."
--     fs <- listDirectory archivedir
--     return $ map (archivedir </>) $ sortBy (comparing Down)
--       [f | f <- fs,
--         prefix `isPrefixOf` f,
--         let nextpart = takeWhile (/= '.') $ drop (length prefix) f,
--         isJust $ parsedate nextpart
--         ]

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

-- | Return the given rules file path, or if none is given,
-- the default rules file for the given csv file;
-- or if the csv file is "-", raise an error.
getRulesFile :: FilePath -> Maybe FilePath -> FilePath
getRulesFile csvfile mrulesfile =
  case mrulesfile of
    Nothing | csvfile == "-" ->
      error' "please use --rules when reading CSV from stdin"  -- PARTIAL
        -- XXX is this bad ? everything else here uses ExceptT
    Nothing -> rulesFileFor csvfile
    Just f -> f

-- | An exception-throwing IO action that reads and validates
-- the specified CSV rules file (which may include other rules files).
readRules :: FilePath -> ExceptT String IO CsvRules
readRules f =
  liftIO (do
    dbg6IO "using conversion rules file" f
    readFilePortably f >>= expandIncludes (takeDirectory f)
  ) >>= either throwError return . parseAndValidateCsvRules f

-- | Read the encoding specified by the @encoding@ rule, if any.
-- Or throw an error if an unrecognised encoding is specified.
rulesEncoding :: CsvRules -> ExceptT String IO (Maybe DynEncoding)
rulesEncoding rules = do
  case T.unpack <$> getDirective "encoding" rules of
    Nothing     -> return Nothing
    Just encstr -> case encodingFromStringExplicit $ dbg4 "encoding name" encstr of
      Nothing  -> throwError $ "Invalid encoding: " <> encstr
      Just enc -> return . Just $ dbg4 "encoding" enc

-- | Inline all files referenced by include directives in this hledger CSV rules text, recursively.
-- Included file paths may be relative to the directory of the provided file path.
-- Unlike with journal files, this is done as a pre-parse step to simplify the CSV rules parser.
-- Unfortunately this means that the parser won't see accurate file paths and positions with included files.
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
    isAssigned f = isJust $ hledgerField rules [] f

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

-- | A reference to a regular expression match group. Eg \1.
type MatchGroupReference = Text

-- | A strptime date parsing pattern, as supported by Data.Time.Format.
type DateFormat       = Text

-- | A representation of a matcher's prefix, which indicates how it should be
-- interpreted or combined with other matchers.
data MatcherPrefix =
    Or      -- ^ no prefix
  | And     -- ^ &&
  | Not     -- ^ !
  | AndNot  -- ^ && !
  deriving (Show, Eq)

dbgShowMatcherPrefix Or = ""
dbgShowMatcherPrefix And = "&&"
dbgShowMatcherPrefix Not = "&&"
dbgShowMatcherPrefix AndNot = "&& !"

-- | A single test for matching a CSV record, in one way or another.
data Matcher =
    RecordMatcher MatcherPrefix Regexp                          -- ^ match if this regexp matches the overall CSV record
  | FieldMatcher MatcherPrefix CsvFieldReference Regexp         -- ^ match if this regexp matches the referenced CSV field's value
  deriving (Show, Eq)

matcherPrefix :: Matcher -> MatcherPrefix
matcherPrefix (RecordMatcher prefix _) = prefix
matcherPrefix (FieldMatcher prefix _ _) = prefix

matcherSetPrefix :: MatcherPrefix -> Matcher -> Matcher
matcherSetPrefix p (RecordMatcher _ r)  = RecordMatcher p r
matcherSetPrefix p (FieldMatcher _ f r) = FieldMatcher p f r

dbgShowMatcher (RecordMatcher Or r)  = show $ reString r
dbgShowMatcher (RecordMatcher p r)  = unwords [dbgShowMatcherPrefix p, show $ reString r]
dbgShowMatcher (FieldMatcher Or f r) = unwords [T.unpack f, show $ reString r]
dbgShowMatcher (FieldMatcher p f r) = unwords [dbgShowMatcherPrefix p, T.unpack f, show $ reString r]

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

dbgShowConditionalBlock :: ConditionalBlock -> String
dbgShowConditionalBlock = unwords . map dbgShowMatcher . cbMatchers

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

RULE: ( SOURCE | ARCHIVE | FIELD-LIST | FIELD-ASSIGNMENT | CONDITIONAL-BLOCK | SKIP | TIMEZONE | NEWEST-FIRST | INTRA-DAY-REVERSED | DATE-FORMAT | DECIMAL-MARK | COMMENT | BLANK ) NEWLINE

SOURCE: source SPACE FILEPATH

ARCHIVE: archive

FIELD-LIST: fields SPACE FIELD-NAME ( SPACE? , SPACE? FIELD-NAME )*

FIELD-NAME: QUOTED-FIELD-NAME | BARE-FIELD-NAME

QUOTED-FIELD-NAME: " (any CHAR except double-quote)+ "

BARE-FIELD-NAME: (any CHAR except space, tab, #, ;)+

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
  ,"archive"
  ,"encoding"
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
-- followed by many lines, each of which is either:
-- a comment line, or ...
-- one matcher, followed by field assignments (as many as there were fields in the header)
conditionaltablep :: CsvRulesParser [ConditionalBlock]
conditionaltablep = do
  lift $ dbgparse 8 "trying conditionaltablep"
  start <- getOffset
  string "if"
  sep <- lift $ satisfy (\c -> not (isAlphaNum c || isSpace c))
  fields <- journalfieldnamep `sepBy1` (char sep)
  newline
  body <- catMaybes <$> (flip manyTill (lift eolof) $
          choice [ commentlinep >> return Nothing
                 , fmap Just $ bodylinep sep fields
                 ])
  when (null body) $
    customFailure $ parseErrorAt start $ "start of conditional table found, but no assignment rules afterward"
  return $ flip map body $ \(ms,vs) ->
    CB{cbMatchers=ms, cbAssignments=zip fields vs}
  <?> "conditional table"
  where
    bodylinep :: Char -> [Text] -> CsvRulesParser ([Matcher],[FieldTemplate])
    bodylinep sep fields = do
      off <- getOffset
      ms <- matcherp' (lookAhead . void . char $ sep) `manyTill` char sep
      vs <- T.split (==sep) . T.pack <$> lift restofline
      if (length vs /= length fields)
        then customFailure $ parseErrorAt off $ ((printf "line of conditional table should have %d values, but this one has only %d" (length fields) (length vs)) :: String)
        else return (ms,vs)


-- A single matcher, on one line.
-- This tries to parse first as a field matcher, then if that fails, as a whole-record matcher;
-- the goal was to not break legacy whole-record patterns that happened to look a bit like a field matcher
-- (eg, beginning with %, possibly preceded by & or !), or at least not to raise an error.
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
  (do
    char '&' >> optional (char '&') >> lift skipNonNewlineSpaces
    fromMaybe And <$> optional (char '!' >> lift skipNonNewlineSpaces >> return AndNot))
  <|> (char '!' >> lift skipNonNewlineSpaces >> return Not)
  <|> return Or

csvfieldreferencep :: CsvRulesParser CsvFieldReference
csvfieldreferencep = do
  lift $ dbgparse 8 "trying csvfieldreferencep"
  char '%'
  T.cons '%' . textQuoteIfNeeded <$> fieldnamep
    -- XXX this parses any generic field name, which may not actually be a valid CSV field name [#2289]

-- A single regular expression
regexp :: CsvRulesParser () -> CsvRulesParser Regexp
regexp end = do
  lift $ dbgparse 8 "trying regexp"
  -- notFollowedBy matchoperatorp
  c <- lift nonspace
  cs <- anySingle `manyTill` (double_ampersand <|> end)
  case toRegexCI . T.strip . T.pack $ c:cs of
       Left x -> Fail.fail $ "CSV parser: " ++ x
       Right x -> return x
  where
    double_ampersand = lookAhead . void $ string "&&"

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
hledgerField rules record f = fmap
  (either id (lastCBAssignmentTemplate f))
  (getEffectiveAssignment rules record f)

-- | Look up the final value assigned to a hledger field, with csv field
-- references and regular expression match group references interpolated.
hledgerFieldValue :: CsvRules -> CsvRecord -> HledgerFieldName -> Maybe Text
hledgerFieldValue rules record f = (flip fmap) (getEffectiveAssignment rules record f)
  $ either (renderTemplate rules record)
  $ \cb -> let
      t = lastCBAssignmentTemplate f cb
      r = rules { rconditionalblocks = [cb] } -- XXX handle rblocksassigning
      in renderTemplate r record t

lastCBAssignmentTemplate :: HledgerFieldName -> ConditionalBlock -> FieldTemplate
lastCBAssignmentTemplate f = snd . last . filter ((==f).fst) . cbAssignments

maybeNegate :: MatcherPrefix -> Bool -> Bool
maybeNegate Not origbool = not origbool
maybeNegate _   origbool = origbool

-- | Given the conversion rules, a CSV record and a hledger field name, find
-- either the last applicable `ConditionalBlock`, or the final value template
-- assigned to this field by a top-level field assignment, if any exist.
--
-- Note conditional blocks' patterns are matched against an approximation of the
-- CSV record: all the field values, without enclosing quotes, comma-separated.
--
getEffectiveAssignment
  :: CsvRules
     -> CsvRecord
     -> HledgerFieldName
     -> Maybe (Either FieldTemplate ConditionalBlock)
getEffectiveAssignment rules record f = lastMay assignments
  where
    -- all active assignments to field f, in order
    assignments = toplevelassignments ++ conditionalassignments
    -- all top level field assignments
    toplevelassignments    = map (Left . snd) $ filter ((==f).fst) $ rassignments rules
    -- all conditional blocks assigning to field f and active for the current csv record
    conditionalassignments = map Right
                           $ filter (any (==f) . map fst . cbAssignments)
                           $ dbg'
                           $ filter (isBlockActive rules record)
                           $ (rblocksassigning rules) f

    dbg' [] = []
    dbg' ms = dbg2Msg (
      " for the " ++ T.unpack f ++ " field, these if rules matched:"
      ++ concatMap (("\n  " ++) . dbgShowConditionalBlock) ms
      ) ms

-- does this conditional block match the current csv record ?
isBlockActive :: CsvRules -> CsvRecord -> ConditionalBlock -> Bool
isBlockActive rules record CB{..} = any (all matcherMatches) $ groupedMatchers cbMatchers
  where
    -- Does this individual matcher match the current csv record ?
    -- A matcher's target can be a specific CSV field, or the "whole record".
    --
    -- In the former case, note that the field reference must be either numeric or
    -- a csv field name declared by a `fields` rule; anything else will emit a warning to stderr
    -- (to reduce confusion when a hledger field name doesn't work; not an error, to avoid breaking legacy rules; see #2289).
    --
    -- In the latter case, the matched value will be a synthetic CSV record.
    -- Note this will not necessarily be the same as the original CSV record:
    -- the field separator will be comma, and quotes enclosing field values,
    -- and any whitespace outside those quotes, will be removed.
    -- (This means that a field containing a comma will now look like two fields.)
    --
    matcherMatches :: Matcher -> Bool
    matcherMatches = \case
      RecordMatcher prefix             pat -> maybeNegate prefix $ match pat $ recordAsApproximateText record
      FieldMatcher  prefix csvfieldref pat -> maybeNegate prefix $ match pat $
        fromMaybe "" $ replaceCsvFieldReference rules record csvfieldref
        -- (warn msg "") where msg = "if "<>T.unpack csvfieldref<>": this should be a name declared with 'fields', or %NUM"
        --  #2289: we'd like to warn the user when an unknown CSV field is being referenced,
        --  but it's useful to ignore it for easier reuse of rules files.
      where match p v = regexMatchText (dbg7 "regex" p) (dbg7 "value" v)

    -- | Group matchers into associative pairs based on prefix, e.g.:
    --   A
    --   & B
    --   C
    --   D
    --   & E
    --   => [[A, B], [C], [D, E]]
    --  & ! M (and not M) are converted to ! M (not M) within the and groups.
    groupedMatchers :: [Matcher] -> [[Matcher]]
    groupedMatchers [] = []
    groupedMatchers (m:ms) = (m:ands) : groupedMatchers rest
      where
        (andandnots, rest) = span (\a -> matcherPrefix a `elem` [And, AndNot]) ms
        ands = [matcherSetPrefix p a | a <- andandnots, let p = if matcherPrefix a == AndNot then Not else And]

-- | Convert a CSV record to text, for whole-record matching.
-- This will be only an approximation of the original record;
-- values will always be comma-separated,
-- and any enclosing quotes and whitespace outside those quotes will be removed.
recordAsApproximateText :: CsvRecord -> Text
recordAsApproximateText = T.intercalate ","

-- | Render a field assignment's template, possibly interpolating referenced
-- CSV field values or match groups. Outer whitespace is removed from interpolated values.
renderTemplate ::  CsvRules -> CsvRecord -> FieldTemplate -> Text
renderTemplate rules record t =
  maybe t mconcat $ parseMaybe
    (many
      (   literaltextp
      <|> (matchrefp <&> replaceRegexGroupReference rules record)
      <|> (fieldrefp <&> replaceCsvFieldReference   rules record <&> fromMaybe "")
      )
    )
    t
  where
    literaltextp :: SimpleTextParser Text
    literaltextp = some (nonBackslashOrPercent <|> nonRefBackslash <|> nonRefPercent) <&> T.pack
      where
        nonBackslashOrPercent = noneOf ['\\', '%'] <?> "character other than backslash or percent"
        nonRefBackslash = try (char '\\' <* notFollowedBy digitChar) <?> "backslash that does not begin a match group reference"
        nonRefPercent   = try (char '%'  <* notFollowedBy (satisfy isFieldNameChar)) <?> "percent that does not begin a field reference"
    matchrefp    = liftA2 T.cons (char '\\') (takeWhile1P (Just "matchref")  isDigit)
    fieldrefp    = liftA2 T.cons (char '%')  (takeWhile1P (Just "reference") isFieldNameChar)
    isFieldNameChar c = isAlphaNum c || c == '_' || c == '-'

-- | Replace something that looks like a Regex match group reference with the
-- resulting match group value after applying the Regex.
replaceRegexGroupReference :: CsvRules -> CsvRecord -> MatchGroupReference -> Text
replaceRegexGroupReference rules record s = case T.uncons s of
    Just ('\\', group) -> fromMaybe "" $ regexMatchValue rules record group
    _                  -> s

regexMatchValue :: CsvRules -> CsvRecord -> Text -> Maybe Text
regexMatchValue rules record sgroup = let
  matchgroups  = concatMap (getMatchGroups rules record)
               $ concatMap cbMatchers
               $ filter (isBlockActive rules record)
               $ rconditionalblocks rules
               -- XXX adjusted to not use memoized field as caller might be sending a subset of rules with just one CB (hacky)
  group = (read (T.unpack sgroup) :: Int) - 1 -- adjust to 0-indexing
  in atMay matchgroups group

getMatchGroups :: CsvRules -> CsvRecord -> Matcher -> [Text]
getMatchGroups _ record (RecordMatcher _ regex) =
  regexMatchTextGroups regex $ recordAsApproximateText record  -- groups might be wrong
getMatchGroups rules record (FieldMatcher _ fieldref regex) =
  regexMatchTextGroups regex $ fromMaybe "" $ replaceCsvFieldReference rules record fieldref

-- | Replace something that looks like a reference to a csv field ("%date" or "%1)
-- with that field's value. If it doesn't look like a field reference, or if we
-- can't find a csv field with that name, return nothing.
replaceCsvFieldReference :: CsvRules -> CsvRecord -> CsvFieldReference -> Maybe Text
replaceCsvFieldReference rules record s = case T.uncons s of
    Just ('%', fieldname) -> csvFieldValue rules record fieldname
    _                     -> Nothing

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
readJournalFromCsv :: CsvRules -> FilePath -> Text -> Maybe SepFormat -> ExceptT String IO Journal
readJournalFromCsv rules csvfile csvtext sep = do
    -- for now, correctness is the priority here, efficiency not so much

    dbg6IO "csv rules" rules

    -- convert the csv data to lines and remove all empty/blank lines
    let csvlines1 = dbg9 "csvlines1" $ filter (not . T.null . T.strip) $ dbg9 "csvlines0" $ T.lines csvtext

    -- if there is a top-level skip rule, skip the specified number of non-empty lines
    skiplines <- case getDirective "skip" rules of
                      Nothing -> return 0
                      Just "" -> return 1
                      Just s  -> maybe (throwError $ "could not parse skip value: " ++ T.unpack s) return . readMay $ T.unpack s
    let csvlines2 = dbg9 "csvlines2" $ drop skiplines csvlines1

    -- convert back to text and parse as csv records
    let
      csvtext1 = T.unlines csvlines2
      -- The separator in the rules file takes precedence over the extension or prefix
      separator = case getDirective "separator" rules >>= parseSeparator of
        Just c           -> c
        _ | ext == "ssv" -> ';'
        _ | ext == "tsv" -> '\t'
        _                -> 
          case sep of
            Just Csv -> ','
            Just Ssv -> ';'
            Just Tsv -> '\t'
            Nothing -> ','
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
          ds@(d:_) -> Just $ d > last ds
          []       -> Nothing
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
      case (hledgerField rules r1 "end", hledgerField rules r1 "skip") of
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
transactionFromCsvRecord timesarezoned mtzin tzout sourcepos rules record =
  -- log the record and all the transaction fields from this record
  -- XXX avoid possibly-pessimising deepseq if not needed for debug output ?
  dbg2Msg (T.unpack $ showRecord record) $ deepseq t
  t
  where

    ----------------------------------------------------------------------
    -- 1. Define some helpers:

    rule     = csvRule           rules        :: DirectiveName    -> Maybe FieldTemplate
    -- ruleval  = csvRuleValue      rules record :: DirectiveName    -> Maybe String
    field    = hledgerField      rules record :: HledgerFieldName -> Maybe FieldTemplate
    fieldval = hledgerFieldValue rules record :: HledgerFieldName -> Maybe Text
    mdateformat = rule "date-format"
    parseDate = parseDateWithCustomOrDefaultFormats timesarezoned mtzin tzout mdateformat
    mkdateerror datefield datevalue mdateformat' = T.unpack $ T.unlines
      ["could not parse \""<>datevalue<>"\" as a date using date format "
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
    date'       = fromMaybe (error' $ mkdateerror "date" date mdateformat) $ parseDate date
    mdate2      = fieldval "date2"
    mdate2'     = (maybe (error' $ mkdateerror "date2" (fromMaybe "" mdate2) mdateformat) Just . parseDate) =<< mdate2
    status      =
      case fieldval "status" of
        Nothing -> Unmarked
        Just s  -> either statuserror id $ runParser (statusp <* eof) "" s
          where
            statuserror err = error' . T.unpack $ T.unlines
              ["could not parse status value \""<>s<>"\" (should be *, ! or empty)"
              ,"the parse error is:      "<>T.pack (customErrorBundlePretty err)
              ]
    code        = maybe "" singleline' $ fieldval "code"
    description = maybe "" singleline' $ fieldval "description"
    comment     = maybe "" unescapeNewlines $ fieldval "comment"

    -- Convert some parsed comment text back into following comment syntax,
    -- with the semicolons and indents, so it can be parsed again for tags.
    textToFollowingComment :: Text -> Text
    textToFollowingComment = T.stripStart . T.unlines . map (" ;"<>) . T.lines

    ttags       = fromRight [] $ fmap snd $ rtp transactioncommentp $ textToFollowingComment comment
    precomment  = maybe "" unescapeNewlines $ fieldval "precomment"

    singleline' = T.unwords . filter (not . T.null) . map T.strip . T.lines
    unescapeNewlines = T.intercalate "\n" . T.splitOn "\\n"

    ----------------------------------------------------------------------
    -- 3. Generate the postings for which an account has been assigned
    -- (possibly indirectly due to an amount or balance assignment)

    p1IsVirtual = (accountNamePostingType <$> fieldval "account1") == Just VirtualPosting
    ps = [p | n <- [1..maxpostings]
         ,let cmt  = maybe "" unescapeNewlines $ fieldval ("comment"<> T.pack (show n))
          -- Tags in the comment will be parsed and attached to the posting.
          -- A posting date, in the date: tag or in brackets, will also be parsed and applied to the posting.
          -- But it must have a year, or it will be ignored.
          -- A secondary posting date will also be ignored.
         ,let (tags,mdate) =
                fromRight ([],Nothing) $
                fmap (\(_,ts,md,_)->(ts,md)) $
                rtp (postingcommentp Nothing) $
                textToFollowingComment cmt
         ,let currency = fromMaybe "" (fieldval ("currency"<> T.pack (show n)) <|> fieldval "currency")
         ,let mamount  = getAmount rules record currency p1IsVirtual n
         ,let mbalance = getBalance rules record currency n
         ,Just (acct,isfinal) <- [getAccount rules record mamount mbalance n]  -- skips Nothings
         ,let acct' | not isfinal && acct==unknownExpenseAccount &&
                      fromMaybe False (mamount >>= isNegativeMixedAmount) = unknownIncomeAccount
                    | otherwise = acct
         ,let p = nullposting{pdate             = mdate
                             ,paccount          = accountNameWithoutPostingType acct'
                             ,pamount           = fromMaybe missingmixedamt mamount
                             ,ptransaction      = Just t
                             ,pbalanceassertion = mkBalanceAssertion rules record <$> mbalance
                             ,pcomment          = cmt
                             ,ptags             = tags
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
                          , Just v <- [T.strip <$> hledgerFieldValue rules record f]
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
      fs      -> error' . T.unpack . textChomp . T.unlines $
        ["in CSV rules:"
        ,"While processing " <> showRecord record
        ,"while calculating amount for posting " <> T.pack (show n)
        ] ++
        ["rule \"" <> f <> " " <>
          fromMaybe "" (hledgerField rules record f) <>
          "\" assigned value \"" <> wbToText (showMixedAmountB defaultFmt a) <> "\"" -- XXX not sure this is showing all the right info
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
    either mkerror mixedAmount $
    runParser (evalStateT (amountp <* eof) journalparsestate) "" $
    currency <> simplifySign s
  where
    journalparsestate = nulljournal{jparsedecimalmark=parseDecimalMark rules}
    mkerror e = error' . T.unpack $ T.unlines
      ["could not parse \"" <> s <> "\" as an amount"
      ,showRecord record
      ,showRules rules record
      -- ,"the default-currency is: "++fromMaybe "unspecified" (getDirective "default-currency" rules)
      ,"the parse error is:      " <> T.pack (customErrorBundlePretty e)
      ,"you may need to change your amount*, balance*, or currency* rules, or add or change your skip rule"
      ]

-- | Show the values assigned to each journal field.
showRules rules record = T.unlines $ catMaybes
  [ (("the "<>fld<>" rule is: ")<>) <$>
    hledgerField rules record fld | fld <- journalfieldnames ]

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
      ["could not parse \"" <> s' <> "\" as balance"<> T.pack (show n') <> " amount"
      ,showRecord record
      ,showRules rules record
      -- ,"the default-currency is: "++fromMaybe "unspecified" mdefaultcurrency
      ,"the parse error is:      "<> T.pack (customErrorBundlePretty e)
      ]

-- | Show the approximation of the original CSV record, labelled, for debug output.
showRecord :: CsvRecord -> Text
showRecord = ("record: "<>) . recordAsApproximateText

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
        Nothing -> nullassertion
        Just x  ->
          case parseBalanceAssertionType $ T.unpack x of
            Just (total, inclusive) -> nullassertion{batotal=total, bainclusive=inclusive}
            Nothing -> error' . T.unpack $ T.unlines  -- PARTIAL:
              [ "balance-type \"" <> x <>"\" is invalid. Use =, ==, =* or ==*."
              , showRecord record
              , showRules rules record
              ]

-- | Detect from a balance assertion's syntax (=, ==, =*, ==*)
-- whether it is (a) total (multi-commodity) and (b) subaccount-inclusive.
-- Returns nothing if invalid syntax was provided.
parseBalanceAssertionType :: String -> Maybe (Bool, Bool)
parseBalanceAssertionType = \case
  "="   -> Just (False, False)
  "=="  -> Just (True,  False)
  "=*"  -> Just (False, True )
  "==*" -> Just (True,  True )
  _     -> Nothing

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
        (Right (mkrules $ defrules{rassignments = [("account1","")], rconditionalblocks = [CB{cbMatchers=[RecordMatcher Or (toRegex' "foo")],cbAssignments=[("account2","foo")]}]}))
   ]
  ,testGroup "conditionalblockp" [
    testCase "space after conditional" $
      parseWithState' defrules conditionalblockp "if a\n account2 b\n \n" @?=
        (Right $ CB{cbMatchers=[RecordMatcher Or $ toRegexCI' "a"],cbAssignments=[("account2","b")]})
  ],

  testGroup "csvfieldreferencep" [
    testCase "number" $ parseWithState' defrules csvfieldreferencep "%1" @?= (Right "%1")
   ,testCase "name" $ parseWithState' defrules csvfieldreferencep "%date" @?= (Right "%date")
   ,testCase "quoted name" $ parseWithState' defrules csvfieldreferencep "%\"csv date\"" @?= (Right "%\"csv date\"")
   ]

  ,testGroup "recordmatcherp" [

    testCase "recordmatcherp" $
      parseWithState' defrules matcherp "A A\n" @?= (Right $ RecordMatcher Or $ toRegexCI' "A A")

   ,testCase "recordmatcherp.starts-with-&" $
      parseWithState' defrules matcherp "& A A\n" @?= (Right $ RecordMatcher And $ toRegexCI' "A A")

   ,testCase "recordmatcherp.starts-with-&&" $
      parseWithState' defrules matcherp "&& A A\n" @?= (Right $ RecordMatcher And $ toRegexCI' "A A")

   ,testCase "recordmatcherp.starts-with-&&-!" $
      parseWithState' defrules matcherp "&& ! A A\n" @?= (Right $ RecordMatcher AndNot $ toRegexCI' "A A")

   ,testCase "recordmatcherp.does-not-start-with-%" $
      parseWithState' defrules matcherp "description A A\n" @?= (Right $ RecordMatcher Or $ toRegexCI' "description A A")
   ]

  ,testGroup "fieldmatcherp" [
    testCase "fieldmatcherp" $
      parseWithState' defrules matcherp "%description A A\n" @?= (Right $ FieldMatcher Or "%description" $ toRegexCI' "A A")

   ,testCase "fieldmatcherp.starts-with-&" $
      parseWithState' defrules matcherp "& %description A A\n" @?= (Right $ FieldMatcher And "%description" $ toRegexCI' "A A")

   ,testCase "fieldmatcherp.starts-with-&&" $
      parseWithState' defrules matcherp "&& %description A A\n" @?= (Right $ FieldMatcher And "%description" $ toRegexCI' "A A")

   ,testCase "fieldmatcherp.starts-with-&&-!" $
      parseWithState' defrules matcherp "&& ! %description A A\n" @?= (Right $ FieldMatcher AndNot "%description" $ toRegexCI' "A A")

   -- ,testCase "fieldmatcherp with operator" $
   --    parseWithState' defrules matcherp "%description ~ A A\n" @?= (Right $ FieldMatcher "%description" "A A")
   ]


  ,testGroup "regexp" [
    testCase "regexp.ends-before-&&" $
      parseWithState' defrules (regexp eof) "A A && xxx" @?= (Right $ toRegexCI' "A A")
   ,testCase "regexp contains &" $
      parseWithState' defrules (regexp eof) "A & B" @?= (Right $ toRegexCI' "A & B")
   ]

  , let matchers = [RecordMatcher Or (toRegexCI' "A"), RecordMatcher And (toRegexCI' "B")]
        assignments = [("account2", "foo"), ("comment2", "bar")]
        block = CB matchers assignments
    in
   testGroup "Combine multiple matchers on the same line" [
    testCase "conditionalblockp" $
      parseWithState' defrules conditionalblockp "if A && B\n account2 foo\n comment2 bar" @?= (Right block)
   ,testCase "conditionaltablep" $
      parseWithState' defrules conditionaltablep "if,account2,comment2\nA && B,foo,bar" @?= (Right [block])
   ]

 ,testGroup "hledgerField" [
    let rules = mkrules $ defrules {rcsvfieldindexes=[("csvdate",1)],rassignments=[("date","%csvdate")]}

    in testCase "toplevel" $ hledgerField rules ["a","b"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1)], rconditionalblocks=[CB [FieldMatcher Or "%csvdate" $ toRegex' "a"] [("date","%csvdate")]]}
    in testCase "conditional" $ hledgerField rules ["a","b"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1)], rconditionalblocks=[CB [FieldMatcher Not "%csvdate" $ toRegex' "a"] [("date","%csvdate")]]}
    in testCase "negated-conditional-false" $ hledgerField rules ["a","b"] "date" @?= (Nothing)
  
   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1)], rconditionalblocks=[CB [FieldMatcher Not "%csvdate" $ toRegex' "b"] [("date","%csvdate")]]}
    in testCase "negated-conditional-true" $ hledgerField rules ["a","b"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1),("description",2)], rconditionalblocks=[CB [FieldMatcher Or "%csvdate" $ toRegex' "a", FieldMatcher Or "%description" $ toRegex' "b"] [("date","%csvdate")]]}
    in testCase "conditional-with-or-a" $ hledgerField rules ["a"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1),("description",2)], rconditionalblocks=[CB [FieldMatcher Or "%csvdate" $ toRegex' "a", FieldMatcher Or "%description" $ toRegex' "b"] [("date","%csvdate")]]}
    in testCase "conditional-with-or-b" $ hledgerField rules ["_", "b"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1),("description",2)], rconditionalblocks=[CB [FieldMatcher Or "%csvdate" $ toRegex' "a", FieldMatcher And "%description" $ toRegex' "b"] [("date","%csvdate")]]}
    in testCase "conditional.with-and" $ hledgerField rules ["a", "b"] "date" @?= (Just "%csvdate")

   ,let rules = mkrules $ defrules{rcsvfieldindexes=[("csvdate",1),("description",2)], rconditionalblocks=[CB [FieldMatcher Or "%csvdate" $ toRegex' "a", FieldMatcher And "%description" $ toRegex' "b", FieldMatcher Or "%description" $ toRegex' "c"] [("date","%csvdate")]]}
    in testCase "conditional.with-and-or" $ hledgerField rules ["_", "c"] "date" @?= (Just "%csvdate")

   ]

 -- testing match groups (#2158)
 ,testGroup "hledgerFieldValue" $
    let rules = mkrules $ defrules
          { rcsvfieldindexes=[ ("date",1), ("description",2) ]
          , rassignments=[ ("account2","equity"), ("amount1","1") ]
          -- ConditionalBlocks here are in reverse order: mkrules reverses the list
          , rconditionalblocks=[ CB { cbMatchers=[FieldMatcher Or "%description" (toRegex' "PREFIX (.*) - (.*)")]
                                    , cbAssignments=[("account1","account:\\1:\\2")] }
                               , CB { cbMatchers=[FieldMatcher Or "%description" (toRegex' "PREFIX (.*)")]
                                    , cbAssignments=[("account1","account:\\1"), ("comment1","\\1")] }
                               ]
          }
        record = ["2019-02-01","PREFIX Text 1 - Text 2"]
    in [ testCase "scoped match groups forwards" $ hledgerFieldValue rules record "account1" @?= (Just "account:Text 1:Text 2")
       , testCase "scoped match groups backwards" $ hledgerFieldValue rules record "comment1" @?= (Just "Text 1 - Text 2")
       ]
 ]
