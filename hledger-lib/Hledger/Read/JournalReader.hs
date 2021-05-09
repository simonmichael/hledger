--- * -*- outline-regexp:"--- *"; -*-
--- ** doc
-- In Emacs, use TAB on lines beginning with "-- *" to collapse/expand sections.
{-|

A reader for hledger's journal file format
(<http://hledger.org/MANUAL.html#the-journal-file>).  hledger's journal
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
  genericSourcePos,
  parseAndFinaliseJournal,
  runJournalParser,
  rjp,

  -- * Parsers used elsewhere
  getParentAccount,
  journalp,
  directivep,
  defaultyeardirectivep,
  marketpricedirectivep,
  datetimep,
  datep,
  modifiedaccountnamep,
  postingp,
  statusp,
  emptyorcommentlinep,
  followingcommentp,
  accountaliasp

  -- * Tests
  ,tests_JournalReader
)
where

--- ** imports
-- import qualified Prelude (fail)
-- import "base-compat-batteries" Prelude.Compat hiding (fail, readFile)
import qualified "base-compat-batteries" Control.Monad.Fail.Compat as Fail (fail)
import qualified Control.Exception as C
import Control.Monad (forM_, when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.State.Strict (get,modify',put)
import Control.Monad.Trans.Class (lift)
import Data.Char (toLower)
import Data.Either (isRight)
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
import Text.Megaparsec.Custom
import Text.Printf
import System.FilePath
import "Glob" System.FilePath.Glob hiding (match)

import Hledger.Data
import Hledger.Read.Common
import Hledger.Utils

import qualified Hledger.Read.TimedotReader as TimedotReader (reader)
import qualified Hledger.Read.TimeclockReader as TimeclockReader (reader)
import qualified Hledger.Read.CsvReader as CsvReader (reader)

--- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

--- ** reader finding utilities
-- Defined here rather than Hledger.Read so that we can use them in includedirectivep below.

-- The available journal readers, each one handling a particular data format.
readers' :: MonadIO m => [Reader m]
readers' = [
  reader
 ,TimeclockReader.reader
 ,TimedotReader.reader
 ,CsvReader.reader
--  ,LedgerReader.reader
 ]

readerNames :: [String]
readerNames = map rFormat (readers'::[Reader IO])

-- | @findReader mformat mpath@
--
-- Find the reader named by @mformat@, if provided.
-- Or, if a file path is provided, find the first reader that handles
-- its file extension, if any.
findReader :: MonadIO m => Maybe StorageFormat -> Maybe FilePath -> Maybe (Reader m)
findReader Nothing Nothing     = Nothing
findReader (Just fmt) _        = headMay [r | r <- readers', rFormat r == fmt]
findReader Nothing (Just path) =
  case prefix of
    Just fmt -> headMay [r | r <- readers', rFormat r == fmt]
    Nothing  -> headMay [r | r <- readers', ext `elem` rExtensions r]
  where
    (prefix,path') = splitReaderPrefix path
    ext            = map toLower $ drop 1 $ takeExtension path'

-- | A file path optionally prefixed by a reader name and colon
-- (journal:, csv:, timedot:, etc.).
type PrefixedFilePath = FilePath

-- | If a filepath is prefixed by one of the reader names and a colon,
-- split that off. Eg "csv:-" -> (Just "csv", "-").
splitReaderPrefix :: PrefixedFilePath -> (Maybe String, FilePath)
splitReaderPrefix f =
  headDef (Nothing, f)
  [(Just r, drop (length r + 1) f) | r <- readerNames, (r++":") `isPrefixOf` f]

--- ** reader

reader :: MonadIO m => Reader m
reader = Reader
  {rFormat     = "journal"
  ,rExtensions = ["journal", "j", "hledger", "ledger"]
  ,rReadFn     = parse
  ,rParser    = journalp  -- no need to add command line aliases like journalp'
                           -- when called as a subparser I think
  }

-- | Parse and post-process a "Journal" from hledger's journal file
-- format, or give an error.
parse :: InputOpts -> FilePath -> Text -> ExceptT String IO Journal
parse iopts = parseAndFinaliseJournal journalp' iopts
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
-- Right (Right Journal  with 1 transactions, 1 accounts)
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
-- Cf http://hledger.org/manual.html#directives,
-- http://ledger-cli.org/3.0/doc/ledger3.html#Command-Directives
directivep :: MonadIO m => ErroringJournalParser m ()
directivep = (do
  optional $ char '!'
  choice [
    includedirectivep
   ,aliasdirectivep
   ,endaliasesdirectivep
   ,accountdirectivep
   ,applyaccountdirectivep
   ,commoditydirectivep
   ,endapplyaccountdirectivep
   ,payeedirectivep
   ,tagdirectivep
   ,endtagdirectivep
   ,defaultyeardirectivep
   ,defaultcommoditydirectivep
   ,commodityconversiondirectivep
   ,ignoredpricecommoditydirectivep
   ]
  ) <?> "directive"

-- | Parse an include directive. include's argument is an optionally
-- file-format-prefixed file path or glob pattern. In the latter case,
-- the prefix is applied to each matched path. Examples:
-- foo.j, foo/bar.j, timedot:foo/2020*.md
includedirectivep :: MonadIO m => ErroringJournalParser m ()
includedirectivep = do
  string "include"
  lift skipNonNewlineSpaces1
  prefixedglob <- T.unpack <$> takeWhileP Nothing (/= '\n') -- don't consume newline yet
  parentoff <- getOffset
  parentpos <- getSourcePos
  let (mprefix,glob) = splitReaderPrefix prefixedglob
  paths <- getFilePaths parentoff parentpos glob
  let prefixedpaths = case mprefix of
        Nothing  -> paths
        Just fmt -> map ((fmt++":")++) paths
  forM_ prefixedpaths $ parseChild parentpos
  void newline

  where
    getFilePaths
      :: MonadIO m => Int -> SourcePos -> FilePath -> JournalParser m [FilePath]
    getFilePaths parseroff parserpos filename = do
        let curdir = takeDirectory (sourceName parserpos)
        filename' <- lift $ expandHomePath filename
                         `orRethrowIOError` (show parserpos ++ " locating " ++ filename)
        -- Compiling filename as a glob pattern works even if it is a literal
        fileglob <- case tryCompileWith compDefault{errorRecovery=False} filename' of
            Right x -> pure x
            Left e -> customFailure $
                        parseErrorAt parseroff $ "Invalid glob pattern: " ++ e
        -- Get all matching files in the current working directory, sorting in
        -- lexicographic order to simulate the output of 'ls'.
        filepaths <- liftIO $ sort <$> globDir1 fileglob curdir
        if (not . null) filepaths
            then pure filepaths
            else customFailure $ parseErrorAt parseroff $
                   "No existing files match pattern: " ++ filename

    parseChild :: MonadIO m => SourcePos -> PrefixedFilePath -> ErroringJournalParser m ()
    parseChild parentpos prefixedpath = do
      let (_mprefix,filepath) = splitReaderPrefix prefixedpath

      parentj <- get
      let parentfilestack = jincludefilestack parentj
      when (filepath `elem` parentfilestack) $
        Fail.fail ("Cyclic include: " ++ filepath)

      childInput <- lift $ readFilePortably filepath
                            `orRethrowIOError` (show parentpos ++ " reading " ++ filepath)
      let initChildj = newJournalWithParseStateFrom filepath parentj

      -- Choose a reader/format based on the file path, or fall back
      -- on journal. Duplicating readJournal a bit here.
      let r = fromMaybe reader $ findReader Nothing (Just prefixedpath)
          parser = rParser r
      dbg6IO "trying reader" (rFormat r)
      updatedChildj <- journalAddFile (filepath, childInput) <$>
                        parseIncludeFile parser initChildj filepath childInput

      -- discard child's parse info,  combine other fields
      put $ updatedChildj <> parentj

    newJournalWithParseStateFrom :: FilePath -> Journal -> Journal
    newJournalWithParseStateFrom filepath j = nulljournal{
      jparsedefaultyear      = jparsedefaultyear j
      ,jparsedefaultcommodity = jparsedefaultcommodity j
      ,jparseparentaccounts   = jparseparentaccounts j
      ,jparsealiases          = jparsealiases j
      ,jcommodities           = jcommodities j
      -- ,jparsetransactioncount = jparsetransactioncount j
      ,jparsetimeclockentries = jparsetimeclockentries j
      ,jincludefilestack      = filepath : jincludefilestack j
      }

-- | Lift an IO action into the exception monad, rethrowing any IO
-- error with the given message prepended.
orRethrowIOError :: MonadIO m => IO a -> String -> TextParser m a
orRethrowIOError io msg = do
  eResult <- liftIO $ (Right <$> io) `C.catch` \(e::C.IOException) -> pure $ Left $ printf "%s:\n%s" msg (show e)
  case eResult of
    Right res -> pure res
    Left errMsg -> Fail.fail errMsg

-- Parse an account directive, adding its info to the journal's
-- list of account declarations.
accountdirectivep :: JournalParser m ()
accountdirectivep = do
  off <- getOffset -- XXX figure out a more precise position later

  string "account"
  lift skipNonNewlineSpaces1

  -- the account name, possibly modified by preceding alias or apply account directives
  acct <- modifiedaccountnamep

  -- maybe an account type code (ALERX) after two or more spaces
  -- XXX added in 1.11, deprecated in 1.13, remove in 1.14
  mtypecode :: Maybe Char <- lift $ optional $ try $ do
    skipNonNewlineSpaces1 -- at least one more space in addition to the one consumed by modifiedaccountp
    choice $ map char "ALERX"

  -- maybe a comment, on this and/or following lines
  (cmt, tags) <- lift transactioncommentp

  -- maybe Ledger-style subdirectives (ignored)
  skipMany indentedlinep

  -- an account type may have been set by account type code or a tag;
  -- the latter takes precedence
  let
    mtypecode' :: Maybe Text = maybe
      (T.singleton <$> mtypecode)
      Just
      $ lookup accountTypeTagName tags
    metype = parseAccountTypeCode <$> mtypecode'

  -- update the journal
  addAccountDeclaration (acct, cmt, tags)
  case metype of
    Nothing         -> return ()
    Just (Right t)  -> addDeclaredAccountType acct t
    Just (Left err) -> customFailure $ parseErrorAt off err

-- The special tag used for declaring account type. XXX change to "class" ?
accountTypeTagName = "type"

parseAccountTypeCode :: Text -> Either String AccountType
parseAccountTypeCode s =
  case T.toLower s of
    "asset"     -> Right Asset
    "a"         -> Right Asset
    "liability" -> Right Liability
    "l"         -> Right Liability
    "equity"    -> Right Equity
    "e"         -> Right Equity
    "revenue"   -> Right Revenue
    "r"         -> Right Revenue
    "expense"   -> Right Expense
    "x"         -> Right Expense
    "cash"      -> Right Cash
    "c"         -> Right Cash
    _           -> Left err
  where
    err = T.unpack $ "invalid account type code "<>s<>", should be one of " <>
            T.intercalate ", " ["A","L","E","R","X","C","Asset","Liability","Equity","Revenue","Expense","Cash"]

-- Add an account declaration to the journal, auto-numbering it.
addAccountDeclaration :: (AccountName,Text,[Tag]) -> JournalParser m ()
addAccountDeclaration (a,cmt,tags) =
  modify' (\j ->
             let
               decls = jdeclaredaccounts j
               d     = (a, nullaccountdeclarationinfo{
                              adicomment          = cmt
                             ,aditags             = tags
                             ,adideclarationorder = length decls + 1
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
    amount <- amountp
    pure $ (off, amount)
  lift skipNonNewlineSpaces
  _ <- lift followingcommentp
  let comm = Commodity{csymbol=acommodity, cformat=Just $ dbg6 "style from commodity directive" astyle}
  if asdecimalpoint astyle == Nothing
  then customFailure $ parseErrorAt off pleaseincludedecimalpoint
  else modify' (\j -> j{jcommodities=M.insert acommodity comm $ jcommodities j})

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
  mformat <- lastMay <$> many (indented $ formatdirectivep sym)
  let comm = Commodity{csymbol=sym, cformat=mformat}
  modify' (\j -> j{jcommodities=M.insert sym comm $ jcommodities j})
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
      if asdecimalpoint astyle == Nothing
      then customFailure $ parseErrorAt off pleaseincludedecimalpoint
      else return $ dbg6 "style from format subdirective" astyle
    else customFailure $ parseErrorAt off $
         printf "commodity directive symbol \"%s\" and format directive symbol \"%s\" should be the same" expectedsym acommodity

keywordp :: String -> JournalParser m ()
keywordp = (() <$) . string . fromString

spacesp :: JournalParser m ()
spacesp = () <$ lift skipNonNewlineSpaces1

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
  _ <- lift $ some nonspace
  lift restofline
  return ()

endtagdirectivep :: JournalParser m ()
endtagdirectivep = do
  (keywordsp "end tag" <|> keywordp "pop") <?> "end tag or pop directive"
  lift restofline
  return ()

payeedirectivep :: JournalParser m ()
payeedirectivep = do
  string "payee" <?> "payee directive"
  lift skipNonNewlineSpaces1
  payee <- lift descriptionp  -- all text until ; or \n
  (comment, tags) <- lift transactioncommentp
  addPayeeDeclaration (payee, comment, tags)
  return ()

defaultyeardirectivep :: JournalParser m ()
defaultyeardirectivep = do
  char 'Y' <?> "default year"
  lift skipNonNewlineSpaces
  setYear =<< lift yearp

defaultcommoditydirectivep :: JournalParser m ()
defaultcommoditydirectivep = do
  char 'D' <?> "default commodity"
  lift skipNonNewlineSpaces1
  off <- getOffset
  Amount{acommodity,astyle} <- amountp
  lift restofline
  if asdecimalpoint astyle == Nothing
  then customFailure $ parseErrorAt off pleaseincludedecimalpoint
  else setDefaultCommodityAndStyle (acommodity, astyle)

marketpricedirectivep :: JournalParser m PriceDirective
marketpricedirectivep = do
  char 'P' <?> "market price"
  lift skipNonNewlineSpaces
  date <- try (do {LocalTime d _ <- datetimep; return d}) <|> datep -- a time is ignored
  lift skipNonNewlineSpaces1
  symbol <- lift commoditysymbolp
  lift skipNonNewlineSpaces
  price <- amountp
  lift restofline
  return $ PriceDirective date symbol price

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

--- *** transactions

-- | Parse a transaction modifier (auto postings) rule.
transactionmodifierp :: JournalParser m TransactionModifier
transactionmodifierp = do
  char '=' <?> "modifier transaction"
  lift skipNonNewlineSpaces
  querytxt <- lift $ T.strip <$> descriptionp
  (_comment, _tags) <- lift transactioncommentp   -- TODO apply these to modified txns ?
  postings <- postingsp Nothing
  return $ TransactionModifier querytxt postings

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

  -- first line
  char '~' <?> "periodic transaction"
  lift $ skipNonNewlineSpaces
  -- a period expression
  off <- getOffset

  -- if there's a default year in effect, use Y/1/1 as base for partial/relative dates
  today <- liftIO getCurrentDay
  mdefaultyear <- getYear
  let refdate = case mdefaultyear of
                  Nothing -> today
                  Just y  -> fromGregorian y 1 1
  periodExcerpt <- lift $ excerpt_ $
                    singlespacedtextsatisfyingp (\c -> c /= ';' && c /= '\n')
  let periodtxt = T.strip $ getExcerptText periodExcerpt

  -- first parsing with 'singlespacedtextp', then "re-parsing" with
  -- 'periodexprp' saves 'periodexprp' from having to respect the single-
  -- and double-space parsing rules
  (interval, span) <- lift $ reparseExcerpt periodExcerpt $ do
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

  -- In periodic transactions, the period expression has an additional constraint:
  case checkPeriodicTransactionStartDate interval span periodtxt of
    Just e -> customFailure $ parseErrorAt off e
    Nothing -> pure ()

  status <- lift statusp <?> "cleared status"
  code <- lift codep <?> "transaction code"
  description <- lift $ T.strip <$> descriptionp
  (comment, tags) <- lift transactioncommentp
  -- next lines; use same year determined above
  postings <- postingsp (Just $ first3 $ toGregorian refdate)

  return $ nullperiodictransaction{
     ptperiodexpr=periodtxt
    ,ptinterval=interval
    ,ptspan=span
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
  let sourcepos = journalSourcePos startpos endpos
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
postingp mTransactionYear = do
  -- lift $ dbgparse 0 "postingp"
  (status, account) <- try $ do
    lift skipNonNewlineSpaces1
    status <- lift statusp
    lift skipNonNewlineSpaces
    account <- modifiedaccountnamep
    return (status, account)
  let (ptype, account') = (accountNamePostingType account, textUnbracket account)
  lift skipNonNewlineSpaces
  amount <- optional amountp
  lift skipNonNewlineSpaces
  massertion <- optional balanceassertionp
  lift skipNonNewlineSpaces
  (comment,tags,mdate,mdate2) <- lift $ postingcommentp mTransactionYear
  return posting
   { pdate=mdate
   , pdate2=mdate2
   , pstatus=status
   , paccount=account'
   , pamount=maybe missingmixedamt mixedAmount amount
   , pcomment=comment
   , ptype=ptype
   , ptags=tags
   , pbalanceassertion=massertion
   }

--- ** tests

tests_JournalReader = tests "JournalReader" [

   let p = lift accountnamep :: JournalParser IO AccountName in
   tests "accountnamep" [
     test "basic" $ assertParse p "a:b:c"
    -- ,test "empty inner component" $ assertParseError p "a::c" ""  -- TODO
    -- ,test "empty leading component" $ assertParseError p ":b:c" "x"
    -- ,test "empty trailing component" $ assertParseError p "a:b:" "x"
    ]

  -- "Parse a date in YYYY/MM/DD format.
  -- Hyphen (-) and period (.) are also allowed as separators.
  -- The year may be omitted if a default year has been set.
  -- Leading zeroes may be omitted."
  ,tests "datep" [
     test "YYYY/MM/DD" $ assertParseEq datep "2018/01/01" (fromGregorian 2018 1 1)
    ,test "YYYY-MM-DD" $ assertParse datep "2018-01-01"
    ,test "YYYY.MM.DD" $ assertParse datep "2018.01.01"
    ,test "yearless date with no default year" $ assertParseError datep "1/1" "current year is unknown"
    ,test "yearless date with default year" $ do
      let s = "1/1"
      ep <- parseWithState nulljournal{jparsedefaultyear=Just 2018} datep s
      either (assertFailure . ("parse error at "++) . customErrorBundlePretty) (const $ return ()) ep
    ,test "no leading zero" $ assertParse datep "2018/1/1"
    ]
  ,test "datetimep" $ do
     let
       good = assertParse datetimep
       bad  = (\t -> assertParseError datetimep t "")
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

  ,tests "periodictransactionp" [

    test "more period text in comment after one space" $ assertParseEq periodictransactionp
      "~ monthly from 2018/6 ;In 2019 we will change this\n"
      nullperiodictransaction {
         ptperiodexpr  = "monthly from 2018/6"
        ,ptinterval    = Months 1
        ,ptspan        = DateSpan (Just $ fromGregorian 2018 6 1) Nothing
        ,ptdescription = ""
        ,ptcomment     = "In 2019 we will change this\n"
        }

    ,test "more period text in description after two spaces" $ assertParseEq periodictransactionp
      "~ monthly from 2018/6   In 2019 we will change this\n"
      nullperiodictransaction {
         ptperiodexpr  = "monthly from 2018/6"
        ,ptinterval    = Months 1
        ,ptspan        = DateSpan (Just $ fromGregorian 2018 6 1) Nothing
        ,ptdescription = "In 2019 we will change this"
        ,ptcomment     = ""
        }

    ,test "Next year in description" $ assertParseEq periodictransactionp
      "~ monthly  Next year blah blah\n"
      nullperiodictransaction {
         ptperiodexpr  = "monthly"
        ,ptinterval    = Months 1
        ,ptspan        = DateSpan Nothing Nothing
        ,ptdescription = "Next year blah blah"
        ,ptcomment     = ""
        }

    ,test "Just date, no description" $ assertParseEq periodictransactionp
      "~ 2019-01-04\n"
      nullperiodictransaction {
         ptperiodexpr  = "2019-01-04"
        ,ptinterval    = NoInterval
        ,ptspan        = DateSpan (Just $ fromGregorian 2019 1 4) (Just $ fromGregorian 2019 1 5)
        ,ptdescription = ""
        ,ptcomment     = ""
        }

    ,test "Just date, no description + empty transaction comment" $ assertParse periodictransactionp
      "~ 2019-01-04\n  ;\n  a  1\n  b\n"

    ]

  ,tests "postingp" [
     test "basic" $ assertParseEq (postingp Nothing)
      "  expenses:food:dining  $10.00   ; a: a a \n   ; b: b b \n"
      posting{
        paccount="expenses:food:dining",
        pamount=mixedAmount (usd 10),
        pcomment="a: a a\nb: b b\n",
        ptags=[("a","a a"), ("b","b b")]
        }

    ,test "posting dates" $ assertParseEq (postingp Nothing)
      " a  1. ; date:2012/11/28, date2=2012/11/29,b:b\n"
      nullposting{
         paccount="a"
        ,pamount=mixedAmount (num 1)
        ,pcomment="date:2012/11/28, date2=2012/11/29,b:b\n"
        ,ptags=[("date", "2012/11/28"), ("date2=2012/11/29,b", "b")] -- TODO tag name parsed too greedily
        ,pdate=Just $ fromGregorian 2012 11 28
        ,pdate2=Nothing  -- Just $ fromGregorian 2012 11 29
        }

    ,test "posting dates bracket syntax" $ assertParseEq (postingp Nothing)
      " a  1. ; [2012/11/28=2012/11/29]\n"
      nullposting{
         paccount="a"
        ,pamount=mixedAmount (num 1)
        ,pcomment="[2012/11/28=2012/11/29]\n"
        ,ptags=[]
        ,pdate= Just $ fromGregorian 2012 11 28
        ,pdate2=Just $ fromGregorian 2012 11 29
        }

    ,test "quoted commodity symbol with digits" $ assertParse (postingp Nothing) "  a  1 \"DE123\"\n"

    ,test "only lot price" $ assertParse (postingp Nothing) "  a  1A {1B}\n"
    ,test "fixed lot price" $ assertParse (postingp Nothing) "  a  1A {=1B}\n"
    ,test "total lot price" $ assertParse (postingp Nothing) "  a  1A {{1B}}\n"
    ,test "fixed total lot price, and spaces" $ assertParse (postingp Nothing) "  a  1A {{  =  1B }}\n"
    ,test "lot price before transaction price" $ assertParse (postingp Nothing) "  a  1A {1B} @ 1B\n"
    ,test "lot price after transaction price" $ assertParse (postingp Nothing) "  a  1A @ 1B {1B}\n"
    ,test "lot price after balance assertion not allowed" $ assertParseError (postingp Nothing) "  a  1A @ 1B = 1A {1B}\n" "unexpected '{'"
    ,test "only lot date" $ assertParse (postingp Nothing) "  a  1A [2000-01-01]\n"
    ,test "transaction price, lot price, lot date" $ assertParse (postingp Nothing) "  a  1A @ 1B {1B} [2000-01-01]\n"
    ,test "lot date, lot price, transaction price" $ assertParse (postingp Nothing) "  a  1A [2000-01-01] {1B} @ 1B\n"

    ,test "balance assertion over entire contents of account" $ assertParse (postingp Nothing) "  a  $1 == $1\n"
    ]

  ,tests "transactionmodifierp" [

    test "basic" $ assertParseEq transactionmodifierp
      "= (some value expr)\n some:postings  1.\n"
      nulltransactionmodifier {
        tmquerytxt = "(some value expr)"
       ,tmpostingrules = [nullposting{paccount="some:postings", pamount=mixedAmount (num 1)}]
      }
    ]

  ,tests "transactionp" [

     test "just a date" $ assertParseEq transactionp "2015/1/1\n" nulltransaction{tdate=fromGregorian 2015 1 1}

    ,test "more complex" $ assertParseEq transactionp
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
        tsourcepos=JournalSourcePos "" (1,7),  -- XXX why 7 here ?
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

    ,test "parses a well-formed transaction" $
      assertBool "" $ isRight $ rjp transactionp $ T.unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries                   $47.18"
        ,"    assets:checking                          $-47.18"
        ,""
        ]

    ,test "does not parse a following comment as part of the description" $
      assertParseEqOn transactionp "2009/1/1 a ;comment\n b 1\n" tdescription "a"

    ,test "parses a following whitespace line" $
      assertBool "" $ isRight $ rjp transactionp $ T.unlines
        ["2012/1/1"
        ,"  a  1"
        ,"  b"
        ," "
        ]

    ,test "parses an empty transaction comment following whitespace line" $
      assertBool "" $ isRight $ rjp transactionp $ T.unlines
        ["2012/1/1"
        ,"  ;"
        ,"  a  1"
        ,"  b"
        ," "
        ]

    ,test "comments everywhere, two postings parsed" $
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

  ,tests "directivep" [
    test "supports !" $ do
        assertParseE directivep "!account a\n"
        assertParseE directivep "!D 1.0\n"
     ]

  ,tests "accountdirectivep" [
       test "with-comment"       $ assertParse accountdirectivep "account a:b  ; a comment\n"
      ,test "does-not-support-!" $ assertParseError accountdirectivep "!account a:b\n" ""
      ,test "account-type-code"  $ assertParse accountdirectivep "account a:b  A\n"
      ,test "account-type-tag"   $ assertParseStateOn accountdirectivep "account a:b  ; type:asset\n"
        jdeclaredaccounts
        [("a:b", AccountDeclarationInfo{adicomment          = "type:asset\n"
                                       ,aditags             = [("type","asset")]
                                       ,adideclarationorder = 1
                                       })
        ]
      ]

  ,test "commodityconversiondirectivep" $ do
     assertParse commodityconversiondirectivep "C 1h = $50.00\n"

  ,test "defaultcommoditydirectivep" $ do
      assertParse defaultcommoditydirectivep "D $1,000.0\n"
      assertParseError defaultcommoditydirectivep "D $1000\n" "Please include a decimal point or decimal comma"

  ,tests "defaultyeardirectivep" [
      test "1000" $ assertParse defaultyeardirectivep "Y 1000" -- XXX no \n like the others
     -- ,test "999" $ assertParseError defaultyeardirectivep "Y 999" "bad year number"
     ,test "12345" $ assertParse defaultyeardirectivep "Y 12345"
     ]

  ,test "ignoredpricecommoditydirectivep" $ do
     assertParse ignoredpricecommoditydirectivep "N $\n"

  ,tests "includedirectivep" [
      test "include" $ assertParseErrorE includedirectivep "include nosuchfile\n" "No existing files match pattern: nosuchfile"
     ,test "glob" $ assertParseErrorE includedirectivep "include nosuchfile*\n" "No existing files match pattern: nosuchfile*"
     ]

  ,test "marketpricedirectivep" $ assertParseEq marketpricedirectivep
    "P 2017/01/30 BTC $922.83\n"
    PriceDirective{
      pddate      = fromGregorian 2017 1 30,
      pdcommodity = "BTC",
      pdamount    = usd 922.83
      }

  ,tests "payeedirectivep" [
       test "simple"             $ assertParse payeedirectivep "payee foo\n"
       ,test "with-comment"       $ assertParse payeedirectivep "payee foo ; comment\n"
       ]

  ,test "tagdirectivep" $ do
     assertParse tagdirectivep "tag foo \n"

  ,test "endtagdirectivep" $ do
      assertParse endtagdirectivep "end tag \n"
      assertParse endtagdirectivep "pop \n"

  ,tests "journalp" [
    test "empty file" $ assertParseEqE journalp "" nulljournal
    ]

   -- these are defined here rather than in Common so they can use journalp
  ,test "parseAndFinaliseJournal" $ do
      ej <- runExceptT $ parseAndFinaliseJournal journalp definputopts "" "2019-1-1\n"
      let Right j = ej
      assertEqual "" [""] $ journalFilePaths j

  ]
