--- * -*- outline-regexp:"--- *"; -*-
--- ** doc
-- In Emacs, use TAB on lines beginning with "-- *" to collapse/expand sections.
{-|

A reader for hledger's journal file format
(<http://hledger.org/hledger.html#the-journal-file>).  hledger's journal
format is a compatible subset of c++ ledger's
(<http://ledger-cli.org/3.0/doc/ledger3.html#Journal-Format>), so this
reader should handle many ledger files as well. Example:

@
2012\/3\/24 gift
    expenses:gifts  $10
    assets:cash
@

Journal format supports the include directive which can read files in
other formats, so the other file format readers need to be importable
and invocable here.

Some important parts of journal parsing are therefore kept in
Hledger.Read.Common, to avoid import cycles.

-}

--- ** language

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoMonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

--- ** exports
module Hledger.Read.JournalReader (

  -- * Reader-finding utils
  findReader,
  splitReaderPrefix,

  -- * Reader
  reader,

  -- * Parsing utils
  parseAndFinaliseJournal,
  runJournalParser,
  rjp,
  runErroringJournalParser,
  rejp,

  -- * Parsers used elsewhere
  getParentAccount,
  journalp,
  directivep,
  defaultyeardirectivep,
  marketpricedirectivep,
  datetimep,
  datep,
  modifiedaccountnamep,
  tmpostingrulep,
  statusp,
  emptyorcommentlinep,
  followingcommentp,
  accountaliasp

  -- * Tests
  ,tests_JournalReader
)
where

--- ** imports
import qualified Control.Exception as C
import Control.Monad (forM_, when, void, unless, filterM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.State.Strict (evalStateT,get,modify',put)
import Control.Monad.Trans.Class (lift)
import Data.Char (toLower)
import Data.Either (isRight, lefts)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.String
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Safe
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Printf
import System.FilePath
import "Glob" System.FilePath.Glob hiding (match)
-- import "filepattern" System.FilePattern.Directory

import Hledger.Data
import Hledger.Read.Common
import Hledger.Utils

import qualified Hledger.Read.CsvReader as CsvReader (reader)
import qualified Hledger.Read.RulesReader as RulesReader (reader)
import qualified Hledger.Read.TimeclockReader as TimeclockReader (reader)
import qualified Hledger.Read.TimedotReader as TimedotReader (reader)
import System.Directory (canonicalizePath, doesFileExist)
import Data.Functor ((<&>))

--- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings
--
--- ** parsing utilities

-- | Run a journal parser in some monad. See also: parseWithState.
runJournalParser, rjp
  :: Monad m
  => JournalParser m a -> Text -> m (Either HledgerParseErrors a)
runJournalParser p = runParserT (evalStateT p nulljournal) ""
rjp = runJournalParser

-- | Run an erroring journal parser in some monad. See also: parseWithState.
runErroringJournalParser, rejp
  :: Monad m
  => ErroringJournalParser m a
  -> Text
  -> m (Either FinalParseError (Either HledgerParseErrors a))
runErroringJournalParser p t =
  runExceptT $ runParserT (evalStateT p nulljournal) "" t
rejp = runErroringJournalParser


--- ** reader finding utilities
-- Defined here rather than Hledger.Read so that we can use them in includedirectivep below.

-- The available journal readers, each one handling a particular data format.
readers' :: MonadIO m => [Reader m]
readers' = [
  reader
 ,TimeclockReader.reader
 ,TimedotReader.reader
 ,RulesReader.reader
 ,CsvReader.reader Csv
 ,CsvReader.reader Tsv
 ,CsvReader.reader Ssv
--  ,LedgerReader.reader
 ]

readerNames :: [String]
readerNames = map (show . rFormat) (readers'::[Reader IO])

-- | @findReader mformat mpath@
--
-- Find the reader named by @mformat@, if provided.
-- ("ssv" and "tsv" are recognised as alternate names for the csv reader,
-- which also handles those formats.)
-- Or, if a file path is provided, find the first reader that handles
-- its file extension, if any.
findReader :: MonadIO m => Maybe StorageFormat -> Maybe FilePath -> Maybe (Reader m)
findReader Nothing Nothing     = Nothing
findReader (Just fmt) _        = headMay [r | r <- readers', let rname = rFormat r, rname == fmt]
findReader Nothing (Just path) =
  case prefix of
    Just fmt -> headMay [r | r <- readers', rFormat r == fmt]
    Nothing  -> headMay [r | r <- readers', ext `elem` rExtensions r]
  where
    (prefix,path') = splitReaderPrefix path
    ext            = map toLower $ drop 1 $ takeExtension path'

-- | A prefix used to specify a particular reader to be used for a file path,
-- overriding the file extension. It is a valid reader name followed by a colon.
-- Eg journal:, csv:, timeclock:, timedot:.
-- type ReaderPrefix = String

-- | A file path with an optional reader prefix.
type PrefixedFilePath = FilePath

-- | Separate a file path and its reader prefix, if any.
--
-- >>> splitReaderPrefix "csv:-"
-- (Just csv,"-")
splitReaderPrefix :: PrefixedFilePath -> (Maybe StorageFormat, FilePath)
splitReaderPrefix f =
  let 
    candidates = [(Just r, drop (length r + 1) f) | r <- readerNames ++ ["ssv","tsv"], (r++":") `isPrefixOf` f]
    (strPrefix, newF) = headDef (Nothing, f) candidates
  in case strPrefix of
    Just "csv" -> (Just (Sep Csv), newF)
    Just "tsv" -> (Just (Sep Tsv), newF)
    Just "ssv" -> (Just (Sep Ssv), newF)
    Just "journal" -> (Just Journal', newF)
    Just "timeclock" -> (Just Timeclock, newF)
    Just "timedot" -> (Just Timedot, newF)
    _ -> (Nothing, f)

-- -- | Does this file path have a reader prefix ?
-- hasReaderPrefix :: PrefixedFilePath -> Bool
-- hasReaderPrefix = isJust . fst. splitReaderPrefix

-- -- | Add a reader prefix to a file path, unless it already has one.
-- -- The argument should be a valid reader name.
-- --
-- -- >>> addReaderPrefix "csv" "a.txt"
-- -- >>> "csv:a.txt"
-- -- >>> addReaderPrefix "csv" "timedot:a.txt"
-- -- >>> "timedot:a.txt"
-- addReaderPrefix :: ReaderPrefix -> FilePath -> PrefixedFilePath
-- addReaderPrefix readername f
--   | hasReaderPrefix f = f
--   | otherwise = readername <> ":" <> f

--- ** reader

reader :: MonadIO m => Reader m
reader = Reader
  {rFormat     = Journal'
  ,rExtensions = ["journal", "j", "hledger", "ledger"]
  ,rReadFn     = handleReadFnToTextReadFn parse
  ,rParser    = journalp  -- no need to add command line aliases like journalp'
                           -- when called as a subparser I think
  }

-- | Parse and post-process a "Journal" from hledger's journal file
-- format, or give an error.
parse :: InputOpts -> FilePath -> Text -> ExceptT String IO Journal
parse iopts f = parseAndFinaliseJournal journalp' iopts f
  where
    journalp' = do
      -- reverse parsed aliases to ensure that they are applied in order given on commandline
      mapM_ addAccountAlias (reverse $ aliasesFromOpts iopts)
      journalp

--- ** parsers
--- *** journal

-- | A journal parser. Accumulates and returns a "ParsedJournal",
-- which should be finalised/validated before use.
--
-- >>> rejp (journalp <* eof) "2015/1/1\n a  0\n"
-- Right (Right Journal (unknown) with 1 transactions, 1 accounts)
--
journalp :: MonadIO m => ErroringJournalParser m ParsedJournal
journalp = do
  many addJournalItemP
  eof
  get

-- | A side-effecting parser; parses any kind of journal item
-- and updates the parse state accordingly.
addJournalItemP :: MonadIO m => ErroringJournalParser m ()
addJournalItemP =
  -- all journal line types can be distinguished by the first
  -- character, can use choice without backtracking
  choice [
      directivep
    , transactionp          >>= modify' . addTransaction
    , transactionmodifierp  >>= modify' . addTransactionModifier
    , periodictransactionp  >>= modify' . addPeriodicTransaction
    , marketpricedirectivep >>= modify' . addPriceDirective
    , void (lift emptyorcommentlinep)
    , void (lift multilinecommentp)
    ] <?> "transaction or directive"

--- *** directives

-- | Parse any journal directive and update the parse state accordingly.
-- Cf http://hledger.org/hledger.html#directives,
-- http://ledger-cli.org/3.0/doc/ledger3.html#Command-Directives
directivep :: MonadIO m => ErroringJournalParser m ()
directivep = (do
  optional $ oneOf ['!','@']
  choice [
    includedirectivep
   ,aliasdirectivep
   ,endaliasesdirectivep
   ,accountdirectivep
   ,applyaccountdirectivep
   ,applyfixeddirectivep
   ,applytagdirectivep
   ,assertdirectivep
   ,bucketdirectivep
   ,capturedirectivep
   ,checkdirectivep
   ,commandlineflagdirectivep
   ,commoditydirectivep
   ,commodityconversiondirectivep
   ,decimalmarkdirectivep
   ,defaultyeardirectivep
   ,defaultcommoditydirectivep
   ,definedirectivep
   ,endapplyaccountdirectivep
   ,endapplyfixeddirectivep
   ,endapplytagdirectivep
   ,endapplyyeardirectivep
   ,endtagdirectivep
   ,evaldirectivep
   ,exprdirectivep
   ,ignoredpricecommoditydirectivep
   ,payeedirectivep
   ,pythondirectivep
   ,tagdirectivep
   ,valuedirectivep
   ]
  ) <?> "directive"

-- | Parse an include directive, and the file(s) it refers to, possibly recursively.
-- include's argument is a file path or glob pattern (see findMatchedFiles for details),
-- optionally with a file type prefix. Relative paths are relative to the current file.
includedirectivep :: MonadIO m => ErroringJournalParser m ()
includedirectivep = do
  -- save the position
  off <- getOffset
  pos <- getSourcePos

  -- parse the directive
  string "include"
  lift skipNonNewlineSpaces1
  prefixedglob <- rstrip . T.unpack <$> takeWhileP Nothing (`notElem` [';','\n'])
  lift followingcommentp
  let (mprefix,glb) = splitReaderPrefix prefixedglob
  f <- sourcePosFilePath pos
  when (null $ dbg6 (f <> " include: glob pattern") glb) $
    customFailure $ parseErrorAt off $ "include needs a file path or glob pattern argument"

  -- Find the file or glob-matched files (just the ones from this include directive), with some IO error checking.
  -- Also report whether a glob pattern was used, and not just a literal file path.
  -- (paths, isglob) <- findMatchedFiles off pos glb
  paths <- findMatchedFiles off pos glb

  -- XXX worth the trouble ? no
  -- Comprehensively exclude files already processed. Some complexities here:
  -- If this include directive uses a glob pattern, remove duplicates. 
  -- Ie if this glob pattern matches any files we have already processed (or the current file),
  -- due to multiple includes in sequence or in a cycle, exclude those files so they're not processed again.
  -- If this include directive uses a literal file path, don't remove duplicates.
  -- Multiple includes in sequence will cause the included file to be processed multiple times.
  -- Multiple includes forming a cycle will be detected and reported as an error in parseIncludedFile.
  -- let paths' = if isglob then filter (...) paths else paths

  -- if there was a reader prefix, apply it to all the file paths
  let prefixedpaths = case mprefix of
        Nothing  -> paths
        Just fmt -> map ((show fmt++":")++) paths

  -- Parse each one, as if inlined here.
  -- Reset the position to the `include` line, for error messages.
  setOffset off
  forM_ prefixedpaths $ parseIncludedFile off pos

  where

    -- | Find the files matched by a literal path or a glob pattern.
    -- Examples: foo.j, ../foo/bar.j, timedot:/foo/2020*, *.journal, **.journal.
    --
    -- Uses the current parse context for detecting the current directory and for error messages.
    -- Expands a leading tilde to the user's home directory.
    -- Converts ** without a slash to **/*, like zsh's GLOB_STAR_SHORT, so ** also matches file name parts.
    -- Checks if any matched paths are directories and excludes those.
    -- Converts all matched paths to their canonical form.
    --
    -- Glob patterns never match dot files or files under dot directories,
    -- even if it seems like they should; this is a workaround for Glob bug #49.
    -- This workaround is disabled if the --old-glob flag is present in the command line
    -- (detected with unsafePerformIO; it's not worth a ton of boilerplate).
    -- In that case, be aware ** recursive globs will search intermediate dot directories.

    findMatchedFiles :: (MonadIO m) => Int -> SourcePos -> FilePath -> JournalParser m [FilePath]
    findMatchedFiles off pos globpattern = do

      -- Some notes about the Glob library that we use (related: https://github.com/Deewiant/glob/issues/49):
      -- It does not expand tilde.
      -- It does not canonicalise paths.
      -- The results are not in any particular order.
      -- The results can include directories.
      -- DIRPAT/ is equivalent to DIRPAT, except results will end with // (double slash).
      -- A . or .. path component can match the current or parent directories (including them in the results).
      -- * matches zero or more characters in a file or directory name.
      -- * at the start of a file name ignores dot-named files and directories, by default.
      -- ** (or zero or more consecutive *'s) not followed by slash is equivalent to *.
      -- A **/ component matches any number of directory parts.
      -- A **/ ignores dot-named directories in its starting and ending directories, by default.
      -- But **/ does search intermediate dot-named directories. Eg it can find a/.b/c.

      -- expand a tilde at the start of the glob pattern, or throw an error
      expandedglob <- lift $ expandHomePath globpattern `orRethrowIOError` "failed to expand ~"

      -- get the directory of the including file
      parentfile <- sourcePosFilePath pos
      let cwd = takeDirectory parentfile

      -- Don't allow 3 or more stars.
      when ("***" `isInfixOf` expandedglob) $
        customFailure $ parseErrorAt off $ "Invalid glob pattern: too many stars, use * or **"

      -- Make ** also match file name parts like zsh's GLOB_STAR_SHORT.
      let
        expandedglob' =
          -- ** without a slash is equivalent to **/*
          case regexReplace (toRegex' $ T.pack "\\*\\*([^/\\])") "**/*\\1" expandedglob of
            Right s -> s
            Left  _ -> expandedglob   -- ignore any error, there should be none

      -- Compile as a Pattern. Can throw an error.
      g <- case tryCompileWith compDefault{errorRecovery=False} expandedglob' of
        Left e  -> customFailure $ parseErrorAt off $ "Invalid glob pattern: " ++ e
        Right x -> pure x
      let isglob = not $ isLiteral g

      -- Find all matched paths. These might include directories or the current file.
      paths <- liftIO $ globDir1 g cwd

      -- Exclude any directories or symlinks to directories, and canonicalise, and sort.
      files <- liftIO $
        filterM doesFileExist paths
        >>= mapM canonicalizePath
        <&> sort

      -- Work around a Glob bug with dot dirs: while **/ ignores dot dirs in the starting and ending dirs,
      -- it does search dot dirs in between those two (Glob #49).
      -- This could be inconvenient, eg making it hard to avoid VCS directories in a source tree.
      -- We work around as follows: when any glob was used, paths involving dot dirs are excluded in post processing.
      -- Unfortunately this means valid globs like .dotdir/* can't be used; only literal paths can match
      -- things in dot dirs. An --old-glob command line flag disables this workaround, for backward compatibility.
      oldglobflag <- liftIO $ getFlag ["old-glob"]
      let
        files2 = (if isglob && not oldglobflag then filter (not.hasdotdir) else id) files
          where
            hasdotdir p = any isdotdir $ splitPath p
              where
                isdotdir c = "." `isPrefixOf` c && "/" `isSuffixOf` c

      -- Throw an error if no files were matched.
      when (null files2) $
        customFailure $ parseErrorAt off $ "No files were matched by glob pattern: " ++ globpattern

      -- If a glob was used, exclude the current file, for convenience.
      let
        files3 =
          dbg6 (parentfile <> " include: matched files" <> if isglob then " (excluding current file)" else "") $
          (if isglob then filter (/= parentfile) else id) files2

      return files3

    -- Parse the given included file (and any deeper includes, recursively)
    -- as if it was inlined in the current (parent) file.
    -- The position in the parent file is provided for error messages.
    parseIncludedFile :: MonadIO m => Int -> SourcePos -> PrefixedFilePath -> ErroringJournalParser m ()
    parseIncludedFile off _pos prefixedpath = do
      let (_mprefix,filepath) = splitReaderPrefix prefixedpath

      -- Throw an error if a cycle is detected
      parentj <- get
      let parentfilestack = jincludefilestack parentj
      when (dbg7 "parseIncludedFile: reading" filepath `elem` parentfilestack) $
        customFailure $ parseErrorAt off $ "This included file forms a cycle: " ++ filepath

      -- Read the file's content, or throw an error
      childInput <- lift $ readFilePortably filepath `orRethrowIOError` "failed to read a file"
      let initChildj = newJournalWithParseStateFrom filepath parentj

      -- Choose a reader based on the file path prefix or file extension,
      -- defaulting to JournalReader. Duplicating readJournal a bit here.
      let r = fromMaybe reader $ findReader Nothing (Just prefixedpath)
          parser = rParser r
      dbg7IO "parseIncludedFile: trying reader" (rFormat r)

      -- Parse the file (and its own includes, if any) to a Journal
      -- with file path and source text attached. Or throw an error.
      updatedChildj <- journalAddFile (filepath, childInput) <$>
                        parseIncludeFile parser initChildj filepath childInput

      -- Child journal was parsed successfully; now merge it into the parent journal.
      -- Debug logging is provided for troubleshooting account display order (eg).
      -- The parent journal is the second argument to journalConcat; this means
      -- its parse state is kept, and its lists are appended to child's (which
      -- ultimately produces the right list order, because parent's and child's
      -- lists are in reverse order at this stage. Cf #1909)
      let
        parentj' =
          dbgJournalAcctDeclOrder ("parseChild: child " <> childfilename <> " acct decls: ") updatedChildj
          `journalConcat`
          dbgJournalAcctDeclOrder ("parseChild: parent " <> parentfilename <> " acct decls: ") parentj

          where
            childfilename = takeFileName filepath
            parentfilename = maybe "(unknown)" takeFileName $ headMay $ jincludefilestack parentj  -- XXX more accurate than journalFilePath for some reason

      -- And update the current parse state.
      put parentj'

      where
        newJournalWithParseStateFrom :: FilePath -> Journal -> Journal
        newJournalWithParseStateFrom filepath j = nulljournal{
          jparsedefaultyear      = jparsedefaultyear j
          ,jparsedefaultcommodity = jparsedefaultcommodity j
          ,jparseparentaccounts   = jparseparentaccounts j
          ,jparsedecimalmark      = jparsedecimalmark j
          ,jparsealiases          = jparsealiases j
          ,jdeclaredcommodities           = jdeclaredcommodities j
          -- ,jparsetransactioncount = jparsetransactioncount j
          ,jparsetimeclockentries = jparsetimeclockentries j
          ,jincludefilestack      = filepath : jincludefilestack j
          }

-- Get the canonical path of the file referenced by this parse position.
-- Symbolic links will be dereferenced. This probably will always succeed
-- (since the parse file's path is probably always absolute).
sourcePosFilePath :: (MonadIO m) => SourcePos -> m FilePath
sourcePosFilePath = liftIO . canonicalizePath . sourceName

-- | Lift an IO action into the exception monad, rethrowing any IO
-- error with the given message prepended.
orRethrowIOError :: MonadIO m => IO a -> String -> TextParser m a
orRethrowIOError io msg = do
  eResult <- liftIO $ (Right <$> io) `C.catch` \(e::C.IOException) -> pure $ Left $ printf "%s:\n%s" msg (show e)
  case eResult of
    Right res -> pure res
    Left errMsg -> fail errMsg

-- Parse an account directive, adding its info to the journal's
-- list of account declarations.
accountdirectivep :: JournalParser m ()
accountdirectivep = do
  off <- getOffset -- XXX figure out a more precise position later
  pos <- getSourcePos

  string "account"
  lift skipNonNewlineSpaces1

  -- the account name, possibly modified by preceding alias or apply account directives
  acct <- (notFollowedBy (char '(' <|> char '[') <?> "account name without brackets") >>
          modifiedaccountnamep

  -- maybe a comment, on this and/or following lines
  (cmt, tags) <- lift transactioncommentp

  -- maybe Ledger-style subdirectives (ignored)
  skipMany indentedlinep

  -- an account type may have been set by account type code or a tag;
  -- the latter takes precedence
  let
    metype = parseAccountTypeCode <$> lookup accountTypeTagName tags

  -- update the journal
  addAccountDeclaration (acct, cmt, tags, pos)
  unless (null tags) $ addDeclaredAccountTags acct tags
  case metype of
    Nothing         -> return ()
    Just (Right t)  -> addDeclaredAccountType acct t
    Just (Left err) -> customFailure $ parseErrorAt off err

-- The special tag used for declaring account type. XXX change to "class" ?
accountTypeTagName = "type"

parseAccountTypeCode :: Text -> Either String AccountType
parseAccountTypeCode s =
  case T.toLower s of
    "asset"      -> Right Asset
    "a"          -> Right Asset
    "liability"  -> Right Liability
    "l"          -> Right Liability
    "equity"     -> Right Equity
    "e"          -> Right Equity
    "revenue"    -> Right Revenue
    "r"          -> Right Revenue
    "expense"    -> Right Expense
    "x"          -> Right Expense
    "cash"       -> Right Cash
    "c"          -> Right Cash
    "conversion" -> Right Conversion
    "v"          -> Right Conversion
    _            -> Left err
  where
    err = T.unpack $ "invalid account type code "<>s<>", should be one of " <>
            T.intercalate ", " ["A","L","E","R","X","C","V","Asset","Liability","Equity","Revenue","Expense","Cash","Conversion"]

-- Add an account declaration to the journal, auto-numbering it.
addAccountDeclaration :: (AccountName,Text,[Tag],SourcePos) -> JournalParser m ()
addAccountDeclaration (a,cmt,tags,pos) = do
  modify' (\j ->
             let
               decls = jdeclaredaccounts j
               d     = (a, nullaccountdeclarationinfo{
                              adicomment          = cmt
                             ,aditags             = tags
                             ,adideclarationorder = length decls + 1  -- gets renumbered when Journals are finalised or merged
                             ,adisourcepos        = pos
                             })
             in
               j{jdeclaredaccounts = d:decls})

-- Add a payee declaration to the journal.
addPayeeDeclaration :: (Payee,Text,[Tag]) -> JournalParser m ()
addPayeeDeclaration (p, cmt, tags) =
  modify' (\j@Journal{jdeclaredpayees} -> j{jdeclaredpayees=d:jdeclaredpayees})
             where
               d = (p
                   ,nullpayeedeclarationinfo{
                     pdicomment = cmt
                    ,pditags    = tags
                    })

-- Add a tag declaration to the journal.
addTagDeclaration :: (TagName,Text) -> JournalParser m ()
addTagDeclaration (t, cmt) =
  modify' (\j@Journal{jdeclaredtags} -> j{jdeclaredtags=tagandinfo:jdeclaredtags})
  where
    tagandinfo = (t, nulltagdeclarationinfo{tdicomment=cmt})

indentedlinep :: JournalParser m String
indentedlinep = lift skipNonNewlineSpaces1 >> (rstrip <$> lift restofline)

-- | Parse a one-line or multi-line commodity directive.
--
-- >>> Right _ <- rjp commoditydirectivep "commodity $1.00"
-- >>> Right _ <- rjp commoditydirectivep "commodity $\n  format $1.00"
-- >>> Right _ <- rjp commoditydirectivep "commodity $\n\n" -- a commodity with no format
-- >>> Right _ <- rjp commoditydirectivep "commodity $1.00\n  format $1.00" -- both, what happens ?
commoditydirectivep :: JournalParser m ()
commoditydirectivep = commoditydirectiveonelinep <|> commoditydirectivemultilinep

-- | Parse a one-line commodity directive.
--
-- >>> Right _ <- rjp commoditydirectiveonelinep "commodity $1.00"
-- >>> Right _ <- rjp commoditydirectiveonelinep "commodity $1.00 ; blah\n"
commoditydirectiveonelinep :: JournalParser m ()
commoditydirectiveonelinep = do
  (off, Amount{acommodity,astyle}) <- try $ do
    string "commodity"
    lift skipNonNewlineSpaces1
    off <- getOffset
    amt <- amountp
    pure $ (off, amt)
  lift skipNonNewlineSpaces
  _ <- lift followingcommentp
  let comm = Commodity{csymbol=acommodity, cformat=Just $ dbg7 "style from commodity directive" astyle}
  if isNothing $ asdecimalmark astyle
  then customFailure $ parseErrorAt off pleaseincludedecimalpoint
  else modify' (\j -> j{jdeclaredcommodities=M.insert acommodity comm $ jdeclaredcommodities j})

pleaseincludedecimalpoint :: String
pleaseincludedecimalpoint = chomp $ unlines [
   "Please include a decimal point or decimal comma in commodity directives,"
  ,"to help us parse correctly. It may be followed by zero or more decimal digits."
  ,"Examples:"
  ,"commodity $1000.            ; no thousands mark, decimal period, no decimals"
  ,"commodity 1.234,00 ARS      ; period at thousands, decimal comma, 2 decimals"
  ,"commodity EUR 1 000,000     ; space at thousands, decimal comma, 3 decimals"
  ,"commodity INR1,23,45,678.0  ; comma at thousands/lakhs/crores, decimal period, 1 decimal"
  ]

-- | Parse a multi-line commodity directive, containing 0 or more format subdirectives.
--
-- >>> Right _ <- rjp commoditydirectivemultilinep "commodity $ ; blah \n  format $1.00 ; blah"
commoditydirectivemultilinep :: JournalParser m ()
commoditydirectivemultilinep = do
  string "commodity"
  lift skipNonNewlineSpaces1
  sym <- lift commoditysymbolp
  _ <- lift followingcommentp
  -- read all subdirectives, saving format subdirectives as Lefts
  subdirectives <- many $ indented (eitherP (formatdirectivep sym) (lift restofline))
  let mfmt = lastMay $ lefts subdirectives
  let comm = Commodity{csymbol=sym, cformat=mfmt}
  modify' (\j -> j{jdeclaredcommodities=M.insert sym comm $ jdeclaredcommodities j})
  where
    indented = (lift skipNonNewlineSpaces1 >>)

-- | Parse a format (sub)directive, throwing a parse error if its
-- symbol does not match the one given.
formatdirectivep :: CommoditySymbol -> JournalParser m AmountStyle
formatdirectivep expectedsym = do
  string "format"
  lift skipNonNewlineSpaces1
  off <- getOffset
  Amount{acommodity,astyle} <- amountp
  _ <- lift followingcommentp
  if acommodity==expectedsym
    then
      if isNothing $ asdecimalmark astyle
      then customFailure $ parseErrorAt off pleaseincludedecimalpoint
      else return $ dbg7 "style from format subdirective" astyle
    else customFailure $ parseErrorAt off $
         printf "commodity directive symbol \"%s\" and format directive symbol \"%s\" should be the same" expectedsym acommodity

-- More Ledger directives, ignore for now:
-- apply fixed, apply tag, assert, bucket, A, capture, check, define, expr
applyfixeddirectivep, endapplyfixeddirectivep, applytagdirectivep, endapplytagdirectivep,
  assertdirectivep, bucketdirectivep, capturedirectivep, checkdirectivep, 
  endapplyyeardirectivep, definedirectivep, exprdirectivep, valuedirectivep,
  evaldirectivep, pythondirectivep, commandlineflagdirectivep
  :: JournalParser m ()
applyfixeddirectivep    = do string "apply fixed" >> lift restofline >> return ()
endapplyfixeddirectivep = do string "end apply fixed" >> lift restofline >> return ()
applytagdirectivep      = do string "apply tag" >> lift restofline >> return ()
endapplytagdirectivep   = do string "end apply tag" >> lift restofline >> return ()
endapplyyeardirectivep  = do string "end apply year" >> lift restofline >> return ()
assertdirectivep        = do string "assert"  >> lift restofline >> return ()
bucketdirectivep        = do string "A " <|> string "bucket " >> lift restofline >> return ()
capturedirectivep       = do string "capture" >> lift restofline >> return ()
checkdirectivep         = do string "check"   >> lift restofline >> return ()
definedirectivep        = do string "define"  >> lift restofline >> return ()
exprdirectivep          = do string "expr"    >> lift restofline >> return ()
valuedirectivep         = do string "value"   >> lift restofline >> return ()
evaldirectivep          = do string "eval"   >> lift restofline >> return ()
commandlineflagdirectivep = do string "--" >> lift restofline >> return ()
pythondirectivep = do
  string "python" >> lift restofline
  many $ indentedline <|> blankline
  return ()
  where
    indentedline = lift skipNonNewlineSpaces1 >> lift restofline
    blankline = lift skipNonNewlineSpaces >> newline >> return "" <?> "blank line"

keywordp :: String -> JournalParser m ()
keywordp = void . string . fromString

spacesp :: JournalParser m ()
spacesp = void $ lift skipNonNewlineSpaces1

-- | Backtracking parser similar to string, but allows varying amount of space between words
keywordsp :: String -> JournalParser m ()
keywordsp = try . sequence_ . intersperse spacesp . map keywordp . words

applyaccountdirectivep :: JournalParser m ()
applyaccountdirectivep = do
  keywordsp "apply account" <?> "apply account directive"
  lift skipNonNewlineSpaces1
  parent <- lift accountnamep
  newline
  pushParentAccount parent

endapplyaccountdirectivep :: JournalParser m ()
endapplyaccountdirectivep = do
  keywordsp "end apply account" <?> "end apply account directive"
  popParentAccount

aliasdirectivep :: JournalParser m ()
aliasdirectivep = do
  string "alias"
  lift skipNonNewlineSpaces1
  alias <- lift accountaliasp
  addAccountAlias alias

endaliasesdirectivep :: JournalParser m ()
endaliasesdirectivep = do
  keywordsp "end aliases" <?> "end aliases directive"
  clearAccountAliases

tagdirectivep :: JournalParser m ()
tagdirectivep = do
  string "tag" <?> "tag directive"
  lift skipNonNewlineSpaces1
  tagname <- lift $ T.pack <$> some nonspace
  (comment, _) <- lift transactioncommentp
  skipMany indentedlinep
  addTagDeclaration (tagname,comment)
  return ()

-- end tag or end apply tag
endtagdirectivep :: JournalParser m ()
endtagdirectivep = (do
  string "end"
  lift skipNonNewlineSpaces1
  optional $ string "apply" >> lift skipNonNewlineSpaces1
  string "tag"
  lift skipNonNewlineSpaces
  eol
  return ()
  ) <?> "end tag or end apply tag directive"

payeedirectivep :: JournalParser m ()
payeedirectivep = do
  string "payee" <?> "payee directive"
  lift skipNonNewlineSpaces1
  payee <- lift $ T.strip <$> (try doublequotedtextp <|> noncommenttext1p)
  (comment, tags) <- lift transactioncommentp
  skipMany indentedlinep
  addPayeeDeclaration (payee, comment, tags)
  return ()

defaultyeardirectivep :: JournalParser m ()
defaultyeardirectivep = do
  (string "Y" <|> string "year" <|> string "apply year") <?> "default year"
  lift skipNonNewlineSpaces
  setYear =<< lift yearp

defaultcommoditydirectivep :: JournalParser m ()
defaultcommoditydirectivep = do
  char 'D' <?> "default commodity"
  lift skipNonNewlineSpaces1
  off <- getOffset
  Amount{acommodity,astyle} <- amountp
  lift restofline
  if isNothing $ asdecimalmark astyle
  then customFailure $ parseErrorAt off pleaseincludedecimalpoint
  else setDefaultCommodityAndStyle (acommodity, astyle)

marketpricedirectivep :: JournalParser m PriceDirective
marketpricedirectivep = do
  pos <- getSourcePos
  char 'P' <?> "market price"
  lift skipNonNewlineSpaces
  date <- try (do {LocalTime d _ <- datetimep; return d}) <|> datep -- a time is ignored
  lift skipNonNewlineSpaces1
  symbol <- lift commoditysymbolp
  lift skipNonNewlineSpaces1
  price <- amountp
  lift restofline
  return $ PriceDirective pos date symbol price

ignoredpricecommoditydirectivep :: JournalParser m ()
ignoredpricecommoditydirectivep = do
  char 'N' <?> "ignored-price commodity"
  lift skipNonNewlineSpaces1
  lift commoditysymbolp
  lift restofline
  return ()

commodityconversiondirectivep :: JournalParser m ()
commodityconversiondirectivep = do
  char 'C' <?> "commodity conversion"
  lift skipNonNewlineSpaces1
  amountp
  lift skipNonNewlineSpaces
  char '='
  lift skipNonNewlineSpaces
  amountp
  lift restofline
  return ()

-- | Read a valid decimal mark from the decimal-mark directive e.g
--
-- decimal-mark ,
decimalmarkdirectivep :: JournalParser m ()
decimalmarkdirectivep = do
  string "decimal-mark" <?> "decimal mark"
  lift skipNonNewlineSpaces1
  mark <- satisfy isDecimalMark
  modify' $ \j -> j{jparsedecimalmark=Just mark}
  lift restofline
  return ()

--- *** transactions

-- | Parse a transaction modifier (auto postings) rule.
transactionmodifierp :: JournalParser m TransactionModifier
transactionmodifierp = do
  char '=' <?> "modifier transaction"
  lift skipNonNewlineSpaces
  querytxt <- lift $ T.strip <$> descriptionp
  (_comment, _tags) <- lift transactioncommentp   -- TODO apply these to modified txns ?
  postingrules <- tmpostingrulesp Nothing
  return $ TransactionModifier querytxt postingrules

-- | Parse a periodic transaction rule.
--
-- This reuses periodexprp which parses period expressions on the command line.
-- This is awkward because periodexprp supports relative and partial dates,
-- which we don't really need here, and it doesn't support the notion of a
-- default year set by a Y directive, which we do need to consider here.
-- We resolve it as follows: in periodic transactions' period expressions,
-- if there is a default year Y in effect, partial/relative dates are calculated
-- relative to Y/1/1. If not, they are calculated related to today as usual.
periodictransactionp :: MonadIO m => JournalParser m PeriodicTransaction
periodictransactionp = do
  startpos <- getSourcePos

  -- first line
  char '~' <?> "periodic transaction"
  lift $ skipNonNewlineSpaces

  -- if there's a default year in effect, use Y/1/1 as base for partial/relative dates
  today <- liftIO getCurrentDay
  mdefaultyear <- getYear
  let refdate = case mdefaultyear of
                  Nothing -> today
                  Just y  -> fromGregorian y 1 1
  periodExcerpt <- lift $ excerpt_ $
                    singlespacedtextsatisfying1p (\c -> c /= ';' && c /= '\n')
  let periodtxt = T.strip $ getExcerptText periodExcerpt

  -- first parsing with 'singlespacedtextp', then "re-parsing" with
  -- 'periodexprp' saves 'periodexprp' from having to respect the single-
  -- and double-space parsing rules
  (interval, spn) <- lift $ reparseExcerpt periodExcerpt $ do
    pexp <- periodexprp refdate
    (<|>) eof $ do
      offset1 <- getOffset
      void takeRest
      offset2 <- getOffset
      customFailure $ parseErrorAtRegion offset1 offset2 $
           "remainder of period expression cannot be parsed"
        <> "\nperhaps you need to terminate the period expression with a double space?"
        <> "\na double space is required between period expression and description/comment"
    pure pexp

  status <- lift statusp <?> "cleared status"
  code <- lift codep <?> "transaction code"
  description <- lift $ T.strip <$> descriptionp
  (comment, tags) <- lift transactioncommentp
  -- next lines; use same year determined above
  postings <- postingsp (Just $ first3 $ toGregorian refdate)

  endpos <- getSourcePos
  let sourcepos = (startpos, endpos)

  return $ nullperiodictransaction{
     ptperiodexpr=periodtxt
    ,ptinterval=interval
    ,ptspan=spn
    ,ptsourcepos=sourcepos
    ,ptstatus=status
    ,ptcode=code
    ,ptdescription=description
    ,ptcomment=comment
    ,pttags=tags
    ,ptpostings=postings
    }

-- | Parse a (possibly unbalanced) transaction.
transactionp :: JournalParser m Transaction
transactionp = do
  -- dbgparse 0 "transactionp"
  startpos <- getSourcePos
  date <- datep <?> "transaction"
  edate <- optional (lift $ secondarydatep date) <?> "secondary date"
  lookAhead (lift spacenonewline <|> newline) <?> "whitespace or newline"
  status <- lift statusp <?> "cleared status"
  code <- lift codep <?> "transaction code"
  description <- lift $ T.strip <$> descriptionp
  (comment, tags) <- lift transactioncommentp
  let year = first3 $ toGregorian date
  postings <- postingsp (Just year)
  endpos <- getSourcePos
  let sourcepos = (startpos, endpos)
  return $ txnTieKnot $ Transaction 0 "" sourcepos date edate status code description comment tags postings

--- *** postings

-- Parse the following whitespace-beginning lines as postings, posting
-- tags, and/or comments (inferring year, if needed, from the given date).
postingsp :: Maybe Year -> JournalParser m [Posting]
postingsp mTransactionYear = many (postingp mTransactionYear) <?> "postings"

-- linebeginningwithspaces :: JournalParser m String
-- linebeginningwithspaces = do
--   sp <- lift skipNonNewlineSpaces1
--   c <- nonspace
--   cs <- lift restofline
--   return $ sp ++ (c:cs) ++ "\n"

postingp :: Maybe Year -> JournalParser m Posting
postingp = fmap fst . postingphelper False

-- Parse the following whitespace-beginning lines as transaction posting rules, posting
-- tags, and/or comments (inferring year, if needed, from the given date).
tmpostingrulesp :: Maybe Year -> JournalParser m [TMPostingRule]
tmpostingrulesp mTransactionYear = many (tmpostingrulep mTransactionYear) <?> "posting rules"

tmpostingrulep :: Maybe Year -> JournalParser m TMPostingRule
tmpostingrulep = fmap (uncurry TMPostingRule) . postingphelper True

-- Parse a Posting, and return a flag with whether a multiplier has been detected.
-- The multiplier is used in TMPostingRules.
postingphelper :: Bool -> Maybe Year -> JournalParser m (Posting, Bool)
postingphelper isPostingRule mTransactionYear = do
    -- lift $ dbgparse 0 "postingp"
    (status, account) <- try $ do
      lift skipNonNewlineSpaces1
      status <- lift statusp
      lift skipNonNewlineSpaces
      account <- modifiedaccountnamep
      return (status, account)
    let (ptype, account') = (accountNamePostingType account, textUnbracket account)
    lift skipNonNewlineSpaces
    mult <- if isPostingRule then multiplierp else pure False
    amt <- optional $ amountp' mult
    lift skipNonNewlineSpaces
    massertion <- optional balanceassertionp
    lift skipNonNewlineSpaces
    (comment,tags,mdate,mdate2) <- lift $ postingcommentp mTransactionYear
    let p = posting
            { pdate=mdate
            , pdate2=mdate2
            , pstatus=status
            , paccount=account'
            , pamount=maybe missingmixedamt mixedAmount amt
            , pcomment=comment
            , ptype=ptype
            , ptags=tags
            , pbalanceassertion=massertion
            }
    return (p, mult)
  where
    multiplierp = option False $ True <$ char '*'

--- ** tests

tests_JournalReader = testGroup "JournalReader" [

   let p = lift accountnamep :: JournalParser IO AccountName in
   testGroup "accountnamep" [
     testCase "basic" $ assertParse p "a:b:c"
    -- ,testCase "empty inner component" $ assertParseError p "a::c" ""  -- TODO
    -- ,testCase "empty leading component" $ assertParseError p ":b:c" "x"
    -- ,testCase "empty trailing component" $ assertParseError p "a:b:" "x"
    ]

  -- "Parse a date in YYYY/MM/DD format.
  -- Hyphen (-) and period (.) are also allowed as separators.
  -- The year may be omitted if a default year has been set.
  -- Leading zeroes may be omitted."
  ,testGroup "datep" [
     testCase "YYYY/MM/DD" $ assertParseEq datep "2018/01/01" (fromGregorian 2018 1 1)
    ,testCase "YYYY-MM-DD" $ assertParse datep "2018-01-01"
    ,testCase "YYYY.MM.DD" $ assertParse datep "2018.01.01"
    ,testCase "yearless date with no default year" $ assertParseError datep "1/1" "current year is unknown"
    ,testCase "yearless date with default year" $ do
      let s = "1/1"
      ep <- parseWithState nulljournal{jparsedefaultyear=Just 2018} datep s
      either (assertFailure . ("parse error at "++) . customErrorBundlePretty) (const $ return ()) ep
    ,testCase "no leading zero" $ assertParse datep "2018/1/1"
    ]
  ,testCase "datetimep" $ do
     let
       good  = assertParse datetimep
       bad t = assertParseError datetimep t ""
     good "2011/1/1 00:00"
     good "2011/1/1 23:59:59"
     bad "2011/1/1"
     bad "2011/1/1 24:00:00"
     bad "2011/1/1 00:60:00"
     bad "2011/1/1 00:00:60"
     bad "2011/1/1 3:5:7"
     -- timezone is parsed but ignored
     let t = LocalTime (fromGregorian 2018 1 1) (TimeOfDay 0 0 0)
     assertParseEq datetimep "2018/1/1 00:00-0800" t
     assertParseEq datetimep "2018/1/1 00:00+1234" t

  ,testGroup "periodictransactionp" [

    testCase "more period text in comment after one space" $ assertParseEq periodictransactionp
      "~ monthly from 2018/6 ;In 2019 we will change this\n"
      nullperiodictransaction {
         ptperiodexpr  = "monthly from 2018/6"
        ,ptinterval    = Months 1
        ,ptspan        = DateSpan (Just $ Flex $ fromGregorian 2018 6 1) Nothing
        ,ptsourcepos   = (SourcePos "" (mkPos 1) (mkPos 1), SourcePos "" (mkPos 2) (mkPos 1))
        ,ptdescription = ""
        ,ptcomment     = "In 2019 we will change this\n"
        }

    ,testCase "more period text in description after two spaces" $ assertParseEq periodictransactionp
      "~ monthly from 2018/6   In 2019 we will change this\n"
      nullperiodictransaction {
         ptperiodexpr  = "monthly from 2018/6"
        ,ptinterval    = Months 1
        ,ptspan        = DateSpan (Just $ Flex $ fromGregorian 2018 6 1) Nothing
        ,ptsourcepos   = (SourcePos "" (mkPos 1) (mkPos 1), SourcePos "" (mkPos 2) (mkPos 1))
        ,ptdescription = "In 2019 we will change this"
        ,ptcomment     = ""
        }

    ,testCase "Next year in description" $ assertParseEq periodictransactionp
      "~ monthly  Next year blah blah\n"
      nullperiodictransaction {
         ptperiodexpr  = "monthly"
        ,ptinterval    = Months 1
        ,ptspan        = DateSpan Nothing Nothing
        ,ptsourcepos   = (SourcePos "" (mkPos 1) (mkPos 1), SourcePos "" (mkPos 2) (mkPos 1))
        ,ptdescription = "Next year blah blah"
        ,ptcomment     = ""
        }

    ,testCase "Just date, no description" $ assertParseEq periodictransactionp
      "~ 2019-01-04\n"
      nullperiodictransaction {
         ptperiodexpr  = "2019-01-04"
        ,ptinterval    = NoInterval
        ,ptspan        = DateSpan (Just $ Exact $ fromGregorian 2019 1 4) (Just $ Exact $ fromGregorian 2019 1 5)
        ,ptsourcepos   = (SourcePos "" (mkPos 1) (mkPos 1), SourcePos "" (mkPos 2) (mkPos 1))
        ,ptdescription = ""
        ,ptcomment     = ""
        }

    ,testCase "Just date, no description + empty transaction comment" $ assertParse periodictransactionp
      "~ 2019-01-04\n  ;\n  a  1\n  b\n"

    ]

  ,testGroup "postingp" [
     testCase "basic" $ assertParseEq (postingp Nothing)
      "  expenses:food:dining  $10.00   ; a: a a \n   ; b: b b \n"
      posting{
        paccount="expenses:food:dining",
        pamount=mixedAmount (usd 10),
        pcomment="a: a a\nb: b b\n",
        ptags=[("a","a a"), ("b","b b")]
        }

    ,testCase "posting dates" $ assertParseEq (postingp Nothing)
      " a  1. ; date:2012/11/28, date2=2012/11/29,b:b\n"
      nullposting{
         paccount="a"
        ,pamount=mixedAmount (num 1)
        ,pcomment="date:2012/11/28, date2=2012/11/29,b:b\n"
        ,ptags=[("date", "2012/11/28"), ("date2=2012/11/29,b", "b")] -- TODO tag name parsed too greedily
        ,pdate=Just $ fromGregorian 2012 11 28
        ,pdate2=Nothing  -- Just $ fromGregorian 2012 11 29
        }

    ,testCase "posting dates bracket syntax" $ assertParseEq (postingp Nothing)
      " a  1. ; [2012/11/28=2012/11/29]\n"
      nullposting{
         paccount="a"
        ,pamount=mixedAmount (num 1)
        ,pcomment="[2012/11/28=2012/11/29]\n"
        ,ptags=[]
        ,pdate= Just $ fromGregorian 2012 11 28
        ,pdate2=Just $ fromGregorian 2012 11 29
        }

    ,testCase "quoted commodity symbol with digits" $ assertParse (postingp Nothing) "  a  1 \"DE123\"\n"

    ,testCase "only lot price" $ assertParse (postingp Nothing) "  a  1A {1B}\n"
    ,testCase "fixed lot price" $ assertParse (postingp Nothing) "  a  1A {=1B}\n"
    ,testCase "total lot price" $ assertParse (postingp Nothing) "  a  1A {{1B}}\n"
    ,testCase "fixed total lot price, and spaces" $ assertParse (postingp Nothing) "  a  1A {{  =  1B }}\n"
    ,testCase "lot price before transaction price" $ assertParse (postingp Nothing) "  a  1A {1B} @ 1B\n"
    ,testCase "lot price after transaction price" $ assertParse (postingp Nothing) "  a  1A @ 1B {1B}\n"
    ,testCase "lot price after balance assertion not allowed" $ assertParseError (postingp Nothing) "  a  1A @ 1B = 1A {1B}\n" "unexpected '{'"
    ,testCase "only lot date" $ assertParse (postingp Nothing) "  a  1A [2000-01-01]\n"
    ,testCase "transaction price, lot price, lot date" $ assertParse (postingp Nothing) "  a  1A @ 1B {1B} [2000-01-01]\n"
    ,testCase "lot date, lot price, transaction price" $ assertParse (postingp Nothing) "  a  1A [2000-01-01] {1B} @ 1B\n"

    ,testCase "balance assertion over entire contents of account" $ assertParse (postingp Nothing) "  a  $1 == $1\n"
    ]

  ,testGroup "transactionmodifierp" [

    testCase "basic" $ assertParseEq transactionmodifierp
      "= (some value expr)\n some:postings  1.\n"
      nulltransactionmodifier {
        tmquerytxt = "(some value expr)"
       ,tmpostingrules = [TMPostingRule nullposting{paccount="some:postings", pamount=mixedAmount (num 1)} False]
      }
    ]

  ,testGroup "transactionp" [

     testCase "just a date" $ assertParseEq transactionp "2015/1/1\n" nulltransaction{tdate=fromGregorian 2015 1 1}

    ,testCase "more complex" $ assertParseEq transactionp
      (T.unlines [
        "2012/05/14=2012/05/15 (code) desc  ; tcomment1",
        "    ; tcomment2",
        "    ; ttag1: val1",
        "    * a         $1.00  ; pcomment1",
        "    ; pcomment2",
        "    ; ptag1: val1",
        "    ; ptag2: val2"
        ])
      nulltransaction{
        tsourcepos=(SourcePos "" (mkPos 1) (mkPos 1), SourcePos "" (mkPos 8) (mkPos 1)),  -- 8 because there are 7 lines
        tprecedingcomment="",
        tdate=fromGregorian 2012 5 14,
        tdate2=Just $ fromGregorian 2012 5 15,
        tstatus=Unmarked,
        tcode="code",
        tdescription="desc",
        tcomment="tcomment1\ntcomment2\nttag1: val1\n",
        ttags=[("ttag1","val1")],
        tpostings=[
          nullposting{
            pdate=Nothing,
            pstatus=Cleared,
            paccount="a",
            pamount=mixedAmount (usd 1),
            pcomment="pcomment1\npcomment2\nptag1: val1\nptag2: val2\n",
            ptype=RegularPosting,
            ptags=[("ptag1","val1"),("ptag2","val2")],
            ptransaction=Nothing
            }
          ]
      }

    ,testCase "parses a well-formed transaction" $
      assertBool "" $ isRight $ rjp transactionp $ T.unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries                   $47.18"
        ,"    assets:checking                          $-47.18"
        ,""
        ]

    ,testCase "does not parse a following comment as part of the description" $
      assertParseEqOn transactionp "2009/1/1 a ;comment\n b 1\n" tdescription "a"

    ,testCase "parses a following whitespace line" $
      assertBool "" $ isRight $ rjp transactionp $ T.unlines
        ["2012/1/1"
        ,"  a  1"
        ,"  b"
        ," "
        ]

    ,testCase "parses an empty transaction comment following whitespace line" $
      assertBool "" $ isRight $ rjp transactionp $ T.unlines
        ["2012/1/1"
        ,"  ;"
        ,"  a  1"
        ,"  b"
        ," "
        ]

    ,testCase "comments everywhere, two postings parsed" $
      assertParseEqOn transactionp
        (T.unlines
          ["2009/1/1 x  ; transaction comment"
          ," a  1  ; posting 1 comment"
          ," ; posting 1 comment 2"
          ," b"
          ," ; posting 2 comment"
          ])
        (length . tpostings)
        2

    ]

  -- directives

  ,testGroup "directivep" [
    testCase "supports !" $ do
        assertParseE directivep "!account a\n"
        assertParseE directivep "!D 1.0\n"
     ]

  ,testGroup "accountdirectivep" [
       testCase "with-comment"       $ assertParse accountdirectivep "account a:b  ; a comment\n"
      ,testCase "does-not-support-!" $ assertParseError accountdirectivep "!account a:b\n" ""
      ,testCase "account-type-code"  $ assertParse accountdirectivep "account a:b  ; type:A\n"
      ,testCase "account-type-tag"   $ assertParseStateOn accountdirectivep "account a:b  ; type:asset\n"
        jdeclaredaccounts
        [("a:b", AccountDeclarationInfo{adicomment          = "type:asset\n"
                                       ,aditags             = [("type","asset")]
                                       ,adideclarationorder = 1
                                       ,adisourcepos        = nullsourcepos
                                       })
        ]
      ]

  ,testCase "commodityconversiondirectivep" $ do
     assertParse commodityconversiondirectivep "C 1h = $50.00\n"

  ,testCase "defaultcommoditydirectivep" $ do
      assertParse defaultcommoditydirectivep "D $1,000.0\n"
      assertParseError defaultcommoditydirectivep "D $1000\n" "Please include a decimal point or decimal comma"

  ,testGroup "defaultyeardirectivep" [
      testCase "1000" $ assertParse defaultyeardirectivep "Y 1000" -- XXX no \n like the others
     -- ,testCase "999" $ assertParseError defaultyeardirectivep "Y 999" "bad year number"
     ,testCase "12345" $ assertParse defaultyeardirectivep "Y 12345"
     ]

  ,testCase "ignoredpricecommoditydirectivep" $ do
     assertParse ignoredpricecommoditydirectivep "N $\n"

  ,testGroup "includedirectivep" [
      testCase "include" $ assertParseErrorE includedirectivep "include nosuchfile\n" "No files were matched by glob pattern: nosuchfile"
     ,testCase "glob" $ assertParseErrorE includedirectivep "include nosuchfile*\n" "No files were matched by glob pattern: nosuchfile*"
     ]

  ,testCase "marketpricedirectivep" $ assertParseEq marketpricedirectivep
    "P 2017/01/30 BTC $922.83\n"
    PriceDirective{
      pdsourcepos = nullsourcepos,
      pddate      = fromGregorian 2017 1 30,
      pdcommodity = "BTC",
      pdamount    = usd 922.83
      }

  ,testGroup "payeedirectivep" [
        testCase "simple"             $ assertParse payeedirectivep "payee foo\n"
       ,testCase "with-comment"       $ assertParse payeedirectivep "payee foo ; comment\n"
       ,testCase "double-quoted"      $ assertParse payeedirectivep "payee \"a b\"\n"
       ,testCase "empty        "      $ assertParse payeedirectivep "payee \"\"\n"
       ]

  ,testCase "tagdirectivep" $ do
     assertParse tagdirectivep "tag foo \n"

  ,testCase "endtagdirectivep" $ do
      assertParse endtagdirectivep "end tag \n"
      assertParse endtagdirectivep "end apply tag \n"

  ,testGroup "journalp" [
    testCase "empty file" $ assertParseEqE journalp "" nulljournal
    ]

   -- these are defined here rather than in Common so they can use journalp
  ,testCase "parseAndFinaliseJournal" $ do
      ej <- runExceptT $ parseAndFinaliseJournal journalp definputopts "" "2019-1-1\n"
      let Right j = ej
      assertEqual "" [""] $ journalFilePaths j

  ]
