--- * doc
-- Lines beginning "--- *" are collapsible orgstruct nodes. Emacs users,
-- (add-hook 'haskell-mode-hook
--   (lambda () (set-variable 'orgstruct-heading-prefix-regexp "--- " t))
--   'orgstruct-mode)
-- and press TAB on nodes to expand/collapse.

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
here.  Some low-level journal syntax parsers which those readers also
use are therefore defined separately in Hledger.Read.Common, avoiding
import cycles.

-}

--- * module

{-# LANGUAGE CPP, RecordWildCards, NamedFieldPuns, NoMonoLocalBinds, ScopedTypeVariables, FlexibleContexts, TupleSections, OverloadedStrings, PackageImports #-}

module Hledger.Read.JournalReader (
--- * exports

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
  followingcommentp

  -- * Tests
  ,tests_JournalReader
)
where
--- * imports
import Prelude ()
import "base-compat-batteries" Prelude.Compat hiding (readFile)
import qualified Control.Exception as C
import Control.Monad
import Control.Monad.Except (ExceptT(..))
import Control.Monad.State.Strict
import Data.Bifunctor (first)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.String
import Data.List
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
import Hledger.Read.TimeclockReader (timeclockfilep)
import Hledger.Read.TimedotReader (timedotfilep)
import Hledger.Utils

-- $setup
-- >>> :set -XOverloadedStrings

--- * reader

reader :: Reader
reader = Reader
  {rFormat     = "journal"
  ,rExtensions = ["journal", "j", "hledger", "ledger"]
  ,rParser     = parse
  ,rExperimental = False
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

-- | Get the account name aliases from options, if any.
aliasesFromOpts :: InputOpts -> [AccountAlias]
aliasesFromOpts = map (\a -> fromparse $ runParser accountaliasp ("--alias "++quoteIfNeeded a) $ T.pack a)
                  . aliases_

--- * parsers
--- ** journal

-- | A journal parser. Accumulates and returns a "ParsedJournal",
-- which should be finalised/validated before use.
--
-- >>> rjp (journalp <* eof) "2015/1/1\n a  0\n"
-- Right Journal  with 1 transactions, 1 accounts
--
journalp :: MonadIO m => JournalParser m ParsedJournal
journalp = do
  many addJournalItemP
  eof
  get

-- | A side-effecting parser; parses any kind of journal item
-- and updates the parse state accordingly.
addJournalItemP :: MonadIO m => JournalParser m ()
addJournalItemP =
  -- all journal line types can be distinguished by the first
  -- character, can use choice without backtracking
  choice [
      directivep
    , transactionp          >>= modify' . addTransaction
    , transactionmodifierp  >>= modify' . addTransactionModifier
    , periodictransactionp  >>= modify' . addPeriodicTransaction
    , marketpricedirectivep >>= modify' . addMarketPrice
    , void (lift emptyorcommentlinep)
    , void (lift multilinecommentp)
    ] <?> "transaction or directive"

--- ** directives

-- | Parse any journal directive and update the parse state accordingly.
-- Cf http://hledger.org/manual.html#directives,
-- http://ledger-cli.org/3.0/doc/ledger3.html#Command-Directives
directivep :: MonadIO m => JournalParser m ()
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
   ,tagdirectivep
   ,endtagdirectivep
   ,defaultyeardirectivep
   ,defaultcommoditydirectivep
   ,commodityconversiondirectivep
   ,ignoredpricecommoditydirectivep
   ]
  ) <?> "directive"

includedirectivep :: MonadIO m => JournalParser m ()
includedirectivep = do
  string "include"
  lift (skipSome spacenonewline)
  filename <- T.unpack <$> takeWhileP Nothing (/= '\n') -- don't consume newline yet

  parentpos <- getPosition

  filepaths <- getFilePaths parentpos filename

  forM_ filepaths $ parseChild parentpos

  void newline

  where
    getFilePaths parserpos filename = do
        curdir <- lift $ expandPath (takeDirectory $ sourceName parserpos) ""
                         `orRethrowIOError` (show parserpos ++ " locating " ++ filename)
        -- Compiling filename as a glob pattern works even if it is a literal
        fileglob <- case tryCompileWith compDefault{errorRecovery=False} filename of
            Right x -> pure x
            Left e -> customFailure $
                        parseErrorAt parserpos $ "Invalid glob pattern: " ++ e
        -- Get all matching files in the current working directory, sorting in
        -- lexicographic order to simulate the output of 'ls'.
        filepaths <- liftIO $ sort <$> globDir1 fileglob curdir
        if (not . null) filepaths
            then pure filepaths
            else customFailure $ parseErrorAt parserpos $
                   "No existing files match pattern: " ++ filename

    parseChild parentpos filepath = do
        parentfilestack <- fmap sourceName . statePos <$> getParserState
        when (filepath `elem` parentfilestack) $ customFailure $
          parseErrorAt parentpos ("Cyclic include: " ++ filepath)

        childInput <- lift $ readFilePortably filepath
                             `orRethrowIOError` (show parentpos ++ " reading " ++ filepath)

        -- save parent state
        parentParserState <- getParserState
        parentj <- get

        let childj = newJournalWithParseStateFrom parentj

        -- set child state
        setInput childInput
        pushPosition $ initialPos filepath
        put childj

        -- parse include file
        let parsers = [ journalp
                      , timeclockfilep
                      , timedotfilep
                      ] -- can't include a csv file yet, that reader is special
        updatedChildj <- journalAddFile (filepath, childInput) <$>
                        region (withSource childInput) (choiceInState parsers)

        -- restore parent state, prepending the child's parse info
        setParserState parentParserState
        put $ updatedChildj <> parentj
        -- discard child's parse info, prepend its (reversed) list data, combine other fields


newJournalWithParseStateFrom :: Journal -> Journal
newJournalWithParseStateFrom j = mempty{
   jparsedefaultyear      = jparsedefaultyear j
  ,jparsedefaultcommodity = jparsedefaultcommodity j
  ,jparseparentaccounts   = jparseparentaccounts j
  ,jparsealiases          = jparsealiases j
  ,jcommodities           = jcommodities j
  -- ,jparsetransactioncount = jparsetransactioncount j
  ,jparsetimeclockentries = jparsetimeclockentries j
  }

-- | Lift an IO action into the exception monad, rethrowing any IO
-- error with the given message prepended.
orRethrowIOError :: MonadIO m => IO a -> String -> TextParser m a
orRethrowIOError io msg = do
  eResult <- liftIO $ (Right <$> io) `C.catch` \(e::C.IOException) -> pure $ Left $ printf "%s:\n%s" msg (show e)
  case eResult of
    Right res -> pure res
    Left errMsg -> fail errMsg

accountdirectivep :: JournalParser m ()
accountdirectivep = do
  string "account"
  lift (skipSome spacenonewline)
  acct <- modifiedaccountnamep  -- account directives can be modified by alias/apply account
  _ :: Maybe String <- (optional $ lift $ skipSome spacenonewline >> some digitChar)  -- compatibility: ignore account codes supported in 1.9/1.10
  newline
  skipMany indentedlinep
  pushDeclaredAccount acct

indentedlinep :: JournalParser m String
indentedlinep = lift (skipSome spacenonewline) >> (rstrip <$> lift restofline)

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
  (pos, Amount{acommodity,astyle}) <- try $ do
    string "commodity"
    lift (skipSome spacenonewline)
    pos <- getPosition
    amount <- amountp
    pure $ (pos, amount)
  lift (skipMany spacenonewline)
  _ <- lift followingcommentp
  let comm = Commodity{csymbol=acommodity, cformat=Just $ dbg2 "style from commodity directive" astyle}
  if asdecimalpoint astyle == Nothing
  then customFailure $ parseErrorAt pos pleaseincludedecimalpoint
  else modify' (\j -> j{jcommodities=M.insert acommodity comm $ jcommodities j})

pleaseincludedecimalpoint :: String
pleaseincludedecimalpoint = "to avoid ambiguity, please include a decimal separator in commodity directives"

-- | Parse a multi-line commodity directive, containing 0 or more format subdirectives.
--
-- >>> Right _ <- rjp commoditydirectivemultilinep "commodity $ ; blah \n  format $1.00 ; blah"
commoditydirectivemultilinep :: JournalParser m ()
commoditydirectivemultilinep = do
  string "commodity"
  lift (skipSome spacenonewline)
  sym <- lift commoditysymbolp
  _ <- lift followingcommentp
  mformat <- lastMay <$> many (indented $ formatdirectivep sym)
  let comm = Commodity{csymbol=sym, cformat=mformat}
  modify' (\j -> j{jcommodities=M.insert sym comm $ jcommodities j})
  where
    indented = (lift (skipSome spacenonewline) >>)

-- | Parse a format (sub)directive, throwing a parse error if its
-- symbol does not match the one given.
formatdirectivep :: CommoditySymbol -> JournalParser m AmountStyle
formatdirectivep expectedsym = do
  string "format"
  lift (skipSome spacenonewline)
  pos <- getPosition
  Amount{acommodity,astyle} <- amountp
  _ <- lift followingcommentp
  if acommodity==expectedsym
    then 
      if asdecimalpoint astyle == Nothing
      then customFailure $ parseErrorAt pos pleaseincludedecimalpoint
      else return $ dbg2 "style from format subdirective" astyle
    else customFailure $ parseErrorAt pos $
         printf "commodity directive symbol \"%s\" and format directive symbol \"%s\" should be the same" expectedsym acommodity

keywordp :: String -> JournalParser m ()
keywordp = (() <$) . string . fromString

spacesp :: JournalParser m ()
spacesp = () <$ lift (skipSome spacenonewline)

-- | Backtracking parser similar to string, but allows varying amount of space between words
keywordsp :: String -> JournalParser m ()
keywordsp = try . sequence_ . intersperse spacesp . map keywordp . words

applyaccountdirectivep :: JournalParser m ()
applyaccountdirectivep = do
  keywordsp "apply account" <?> "apply account directive"
  lift (skipSome spacenonewline)
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
  lift (skipSome spacenonewline)
  alias <- lift accountaliasp
  addAccountAlias alias

accountaliasp :: TextParser m AccountAlias
accountaliasp = regexaliasp <|> basicaliasp

basicaliasp :: TextParser m AccountAlias
basicaliasp = do
  -- dbgparse 0 "basicaliasp"
  old <- rstrip <$> (some $ noneOf ("=" :: [Char]))
  char '='
  skipMany spacenonewline
  new <- rstrip <$> anyChar `manyTill` eolof  -- eol in journal, eof in command lines, normally
  return $ BasicAlias (T.pack old) (T.pack new)

regexaliasp :: TextParser m AccountAlias
regexaliasp = do
  -- dbgparse 0 "regexaliasp"
  char '/'
  re <- some $ noneOf ("/\n\r" :: [Char]) -- paranoid: don't try to read past line end
  char '/'
  skipMany spacenonewline
  char '='
  skipMany spacenonewline
  repl <- anyChar `manyTill` eolof
  return $ RegexAlias re repl

endaliasesdirectivep :: JournalParser m ()
endaliasesdirectivep = do
  keywordsp "end aliases" <?> "end aliases directive"
  clearAccountAliases

tagdirectivep :: JournalParser m ()
tagdirectivep = do
  string "tag" <?> "tag directive"
  lift (skipSome spacenonewline)
  _ <- lift $ some nonspace
  lift restofline
  return ()

endtagdirectivep :: JournalParser m ()
endtagdirectivep = do
  (keywordsp "end tag" <|> keywordp "pop") <?> "end tag or pop directive"
  lift restofline
  return ()

defaultyeardirectivep :: JournalParser m ()
defaultyeardirectivep = do
  char 'Y' <?> "default year"
  lift (skipMany spacenonewline)
  y <- some digitChar
  let y' = read y
  failIfInvalidYear y
  setYear y'

defaultcommoditydirectivep :: JournalParser m ()
defaultcommoditydirectivep = do
  char 'D' <?> "default commodity"
  lift (skipSome spacenonewline)
  pos <- getPosition
  Amount{acommodity,astyle} <- amountp
  lift restofline
  if asdecimalpoint astyle == Nothing
  then customFailure $ parseErrorAt pos pleaseincludedecimalpoint
  else setDefaultCommodityAndStyle (acommodity, astyle)

marketpricedirectivep :: JournalParser m MarketPrice
marketpricedirectivep = do
  char 'P' <?> "market price"
  lift (skipMany spacenonewline)
  date <- try (do {LocalTime d _ <- datetimep; return d}) <|> datep -- a time is ignored
  lift (skipSome spacenonewline)
  symbol <- lift commoditysymbolp
  lift (skipMany spacenonewline)
  price <- amountp
  lift restofline
  return $ MarketPrice date symbol price

ignoredpricecommoditydirectivep :: JournalParser m ()
ignoredpricecommoditydirectivep = do
  char 'N' <?> "ignored-price commodity"
  lift (skipSome spacenonewline)
  lift commoditysymbolp
  lift restofline
  return ()

commodityconversiondirectivep :: JournalParser m ()
commodityconversiondirectivep = do
  char 'C' <?> "commodity conversion"
  lift (skipSome spacenonewline)
  amountp
  lift (skipMany spacenonewline)
  char '='
  lift (skipMany spacenonewline)
  amountp
  lift restofline
  return ()

--- ** transactions

transactionmodifierp :: JournalParser m TransactionModifier
transactionmodifierp = do
  char '=' <?> "modifier transaction"
  lift (skipMany spacenonewline)
  querytxt <- lift $ T.strip <$> descriptionp
  (_comment, _tags) <- lift transactioncommentp   -- TODO apply these to modified txns ?
  postings <- postingsp Nothing
  return $ TransactionModifier querytxt postings

-- | Parse a periodic transaction
periodictransactionp :: MonadIO m => JournalParser m PeriodicTransaction
periodictransactionp = do

  -- first line
  char '~' <?> "periodic transaction"
  lift $ skipMany spacenonewline
  -- a period expression
  pos <- getPosition
  d <- liftIO getCurrentDay
  (periodtxt, (interval, span)) <- lift $ first T.strip <$> match (periodexprp d)
  -- In periodic transactions, the period expression has an additional constraint:
  case checkPeriodicTransactionStartDate interval span periodtxt of
    Just e -> customFailure $ parseErrorAt pos e
    Nothing -> pure ()
  -- The line can end here, or it can continue with one or more spaces
  -- and then zero or more of the following fields. A bit awkward.
  (status, code, description, (comment, tags)) <-
    (lift eolof >> return (Unmarked, "", "", ("", [])))
    <|>
    (do
      lift $ skipSome spacenonewline 
      s         <- lift statusp
      c         <- lift codep
      desc      <- lift $ T.strip <$> descriptionp
      (cmt, ts) <- lift transactioncommentp
      return (s,c,desc,(cmt,ts))
    )

  -- next lines
  postings <- postingsp (Just $ first3 $ toGregorian d)

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
  startpos <- getPosition
  date <- datep <?> "transaction"
  edate <- optional (lift $ secondarydatep date) <?> "secondary date"
  lookAhead (lift spacenonewline <|> newline) <?> "whitespace or newline"
  status <- lift statusp <?> "cleared status"
  code <- lift codep <?> "transaction code"
  description <- lift $ T.strip <$> descriptionp
  (comment, tags) <- lift transactioncommentp
  let year = first3 $ toGregorian date
  postings <- postingsp (Just year)
  endpos <- getPosition
  let sourcepos = journalSourcePos startpos endpos
  return $ txnTieKnot $ Transaction 0 sourcepos date edate status code description comment tags postings ""

--- ** postings

-- Parse the following whitespace-beginning lines as postings, posting
-- tags, and/or comments (inferring year, if needed, from the given date).
postingsp :: Maybe Year -> JournalParser m [Posting]
postingsp mTransactionYear = many (postingp mTransactionYear) <?> "postings"

-- linebeginningwithspaces :: JournalParser m String
-- linebeginningwithspaces = do
--   sp <- lift (skipSome spacenonewline)
--   c <- nonspace
--   cs <- lift restofline
--   return $ sp ++ (c:cs) ++ "\n"

postingp :: Maybe Year -> JournalParser m Posting
postingp mTransactionYear = do
  -- lift $ dbgparse 0 "postingp"
  (status, account) <- try $ do
    lift (skipSome spacenonewline)
    status <- lift statusp
    lift (skipMany spacenonewline)
    account <- modifiedaccountnamep
    return (status, account)
  let (ptype, account') = (accountNamePostingType account, textUnbracket account)
  lift (skipMany spacenonewline)
  amount <- option missingmixedamt $ Mixed . (:[]) <$> amountp
  massertion <- partialbalanceassertionp
  _ <- fixedlotpricep
  lift (skipMany spacenonewline)
  (comment,tags,mdate,mdate2) <- lift $ postingcommentp mTransactionYear
  return posting
   { pdate=mdate
   , pdate2=mdate2
   , pstatus=status
   , paccount=account'
   , pamount=amount
   , pcomment=comment
   , ptype=ptype
   , ptags=tags
   , pbalanceassertion=massertion
   }

--- * tests

tests_JournalReader = tests "JournalReader" [

   let p = lift accountnamep :: JournalParser IO AccountName in
   tests "accountnamep" [
     test "basic" $ expectParse p "a:b:c"
    ,_test "empty inner component" $ expectParseError p "a::c" ""  -- TODO
    ,_test "empty leading component" $ expectParseError p ":b:c" "x"
    ,_test "empty trailing component" $ expectParseError p "a:b:" "x"
    ]

  -- "Parse a date in YYYY/MM/DD format.
  -- Hyphen (-) and period (.) are also allowed as separators.
  -- The year may be omitted if a default year has been set.
  -- Leading zeroes may be omitted."
  ,test "datep" $ do
    test "YYYY/MM/DD" $ expectParseEq datep "2018/01/01" (fromGregorian 2018 1 1)
    test "YYYY-MM-DD" $ expectParse datep "2018-01-01"
    test "YYYY.MM.DD" $ expectParse datep "2018.01.01"
    test "yearless date with no default year" $ expectParseError datep "1/1" "current year is unknown"
    test "yearless date with default year" $ do 
      ep <- parseWithState mempty{jparsedefaultyear=Just 2018} datep "1/1"
      either (fail.("parse error at "++).parseErrorPretty) (const ok) ep
    test "no leading zero" $ expectParse datep "2018/1/1"

  ,test "datetimep" $ do
      let
        good = expectParse datetimep
        bad = (\t -> expectParseError datetimep t "")
      good "2011/1/1 00:00"
      good "2011/1/1 23:59:59"
      bad "2011/1/1"
      bad "2011/1/1 24:00:00"
      bad "2011/1/1 00:60:00"
      bad "2011/1/1 00:00:60"
      bad "2011/1/1 3:5:7"
      test "timezone is parsed but ignored" $ do
        let t = LocalTime (fromGregorian 2018 1 1) (TimeOfDay 0 0 (fromIntegral 0))
        expectParseEq datetimep "2018/1/1 00:00-0800" t
        expectParseEq datetimep "2018/1/1 00:00+1234" t

  ,tests "periodictransactionp" [

    test "more period text in comment after one space" $ expectParseEq periodictransactionp
      "~ monthly from 2018/6 ;In 2019 we will change this\n" 
      nullperiodictransaction {
         ptperiodexpr  = "monthly from 2018/6"
        ,ptinterval    = Months 1
        ,ptspan        = DateSpan (Just $ fromGregorian 2018 6 1) Nothing
        ,ptstatus      = Unmarked
        ,ptcode        = ""
        ,ptdescription = ""
        ,ptcomment     = "In 2019 we will change this\n"
        ,pttags        = []
        ,ptpostings    = []
        }

     -- TODO #807
    ,_test "more period text in description after two spaces" $ expectParseEq periodictransactionp 
      "~ monthly from 2018/6   In 2019 we will change this\n" 
      nullperiodictransaction {
         ptperiodexpr  = "monthly from 2018/6"
        ,ptinterval    = Months 1
        ,ptspan        = DateSpan (Just $ fromGregorian 2018 6 1) Nothing
        ,ptdescription = "In 2019 we will change this\n"
        }

    ,_test "more period text in description after one space" $ expectParseEq periodictransactionp
      "~ monthly from 2018/6 In 2019 we will change this\n" 
      nullperiodictransaction {
         ptperiodexpr  = "monthly from 2018/6"
        ,ptinterval    = Months 1
        ,ptspan        = DateSpan (Just $ fromGregorian 2018 6 1) Nothing
        ,ptdescription = "In 2019 we will change this\n"
        }

    ,_test "Next year in description" $ expectParseEq periodictransactionp
      "~ monthly  Next year blah blah\n"
      nullperiodictransaction {
         ptperiodexpr  = "monthly"
        ,ptinterval    = Months 1
        ,ptspan        = DateSpan Nothing Nothing
        ,ptdescription = "Next year blah blah\n"
        }

    ]

  ,tests "postingp" [
     test "basic" $ expectParseEq (postingp Nothing) 
      "  expenses:food:dining  $10.00   ; a: a a \n   ; b: b b \n"
      posting{
        paccount="expenses:food:dining", 
        pamount=Mixed [usd 10], 
        pcomment="a: a a\nb: b b\n", 
        ptags=[("a","a a"), ("b","b b")]
        }

    ,test "posting dates" $ expectParseEq (postingp Nothing) 
      " a  1. ; date:2012/11/28, date2=2012/11/29,b:b\n"
      nullposting{
         paccount="a"
        ,pamount=Mixed [num 1]
        ,pcomment="date:2012/11/28, date2=2012/11/29,b:b\n"
        ,ptags=[("date", "2012/11/28"), ("date2=2012/11/29,b", "b")] -- TODO tag name parsed too greedily
        ,pdate=Just $ fromGregorian 2012 11 28
        ,pdate2=Nothing  -- Just $ fromGregorian 2012 11 29
        }

    ,test "posting dates bracket syntax" $ expectParseEq (postingp Nothing) 
      " a  1. ; [2012/11/28=2012/11/29]\n"
      nullposting{
         paccount="a"
        ,pamount=Mixed [num 1]
        ,pcomment="[2012/11/28=2012/11/29]\n"
        ,ptags=[]
        ,pdate= Just $ fromGregorian 2012 11 28 
        ,pdate2=Just $ fromGregorian 2012 11 29
        }

    ,test "quoted commodity symbol with digits" $ expectParse (postingp Nothing) "  a  1 \"DE123\"\n"

    ,test "balance assertion and fixed lot price" $ expectParse (postingp Nothing) "  a  1 \"DE123\" =$1 { =2.2 EUR} \n"
    ]

  ,tests "transactionmodifierp" [

    test "basic" $ expectParseEq transactionmodifierp 
      "= (some value expr)\n some:postings  1.\n"
      nulltransactionmodifier {
        tmquerytxt = "(some value expr)"
       ,tmpostings = [nullposting{paccount="some:postings", pamount=Mixed[num 1]}]
      }
    ]

  ,tests "transactionp" [
  
     test "just a date" $ expectParseEq transactionp "2015/1/1\n" nulltransaction{tdate=fromGregorian 2015 1 1}
  
    ,test "more complex" $ expectParseEq transactionp 
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
        tpreceding_comment_lines="",
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
            pamount=Mixed [usd 1],
            pcomment="pcomment1\npcomment2\nptag1: val1\nptag2: val2\n",
            ptype=RegularPosting,
            ptags=[("ptag1","val1"),("ptag2","val2")],
            ptransaction=Nothing
            }
          ]
      }
  
    ,test "parses a well-formed transaction" $
      expect $ isRight $ rjp transactionp $ T.unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries                   $47.18"
        ,"    assets:checking                          $-47.18"
        ,""
        ]
  
    ,test "does not parse a following comment as part of the description" $
      expectParseEqOn transactionp "2009/1/1 a ;comment\n b 1\n" tdescription "a"
  
    ,test "transactionp parses a following whitespace line" $
      expect $ isRight $ rjp transactionp $ T.unlines
        ["2012/1/1"
        ,"  a  1"
        ,"  b"
        ," "
        ]
  
    ,test "comments everywhere, two postings parsed" $
      expectParseEqOn transactionp 
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
      expectParse directivep "!account a\n"
      expectParse directivep "!D 1.0\n"
    ]

  ,test "accountdirectivep" $ do
    test "account" $ expectParse accountdirectivep "account a:b\n"
    test "does not support !" $ expectParseError accountdirectivep "!account a:b\n" ""

  ,test "commodityconversiondirectivep" $ do
     expectParse commodityconversiondirectivep "C 1h = $50.00\n"

  ,test "defaultcommoditydirectivep" $ do
     expectParse defaultcommoditydirectivep "D $1,000.0\n"
     expectParseError defaultcommoditydirectivep "D $1000\n" "please include a decimal separator"

  ,test "defaultyeardirectivep" $ do
    test "1000" $ expectParse defaultyeardirectivep "Y 1000" -- XXX no \n like the others
    test "999" $ expectParseError defaultyeardirectivep "Y 999" "bad year number"
    test "12345" $ expectParse defaultyeardirectivep "Y 12345"

  ,test "ignoredpricecommoditydirectivep" $ do
     expectParse ignoredpricecommoditydirectivep "N $\n"

  ,test "includedirectivep" $ do
    test "include" $ expectParseError includedirectivep "include nosuchfile\n" "No existing files match pattern: nosuchfile"
    test "glob" $ expectParseError includedirectivep "include nosuchfile*\n" "No existing files match pattern: nosuchfile*"

  ,test "marketpricedirectivep" $ expectParseEq marketpricedirectivep
    "P 2017/01/30 BTC $922.83\n"
    MarketPrice{
      mpdate      = fromGregorian 2017 1 30,
      mpcommodity = "BTC",
      mpamount    = usd 922.83
      }

  ,test "tagdirectivep" $ do
     expectParse tagdirectivep "tag foo \n"

  ,test "endtagdirectivep" $ do
     expectParse endtagdirectivep "end tag \n"
     expectParse endtagdirectivep "pop \n"


  ,tests "journalp" [
    test "empty file" $ expectParseEq journalp "" nulljournal
    ]

  ]
