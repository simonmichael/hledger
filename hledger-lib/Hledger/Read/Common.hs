--- * doc
-- Lines beginning "--- *" are collapsible orgstruct nodes. Emacs users,
-- (add-hook 'haskell-mode-hook
--   (lambda () (set-variable 'orgstruct-heading-prefix-regexp "--- " t))
--   'orgstruct-mode)
-- and press TAB on nodes to expand/collapse.

{-|

Some common parsers and helpers used by several readers.
Some of these might belong in Hledger.Read.JournalReader or Hledger.Read.

-}

--- * module
{-# LANGUAGE CPP, BangPatterns, DeriveDataTypeable, RecordWildCards, NamedFieldPuns, NoMonoLocalBinds, ScopedTypeVariables, FlexibleContexts, TupleSections, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Hledger.Read.Common (
  Reader (..),
  InputOpts (..),
  definputopts,
  rawOptsToInputOpts,

  -- * parsing utilities
  runTextParser,
  rtp,
  runJournalParser,
  rjp,
  runErroringJournalParser,
  rejp,
  genericSourcePos,
  journalSourcePos,
  generateAutomaticPostings,
  parseAndFinaliseJournal,
  parseAndFinaliseJournal',  -- TODO unused ? check addons
  setYear,
  getYear,
  setDefaultCommodityAndStyle,
  getDefaultCommodityAndStyle,
  getDefaultAmountStyle,
  getAmountStyle,
  pushAccount,
  pushParentAccount,
  popParentAccount,
  getParentAccount,
  addAccountAlias,
  getAccountAliases,
  clearAccountAliases,
  journalAddFile,
  parserErrorAt,

  -- * parsers
  -- ** transaction bits
  statusp,
  codep,
  descriptionp,

  -- ** dates
  datep,
  datetimep,
  secondarydatep,

  -- ** account names
  modifiedaccountnamep,
  accountnamep,

  -- ** amounts
  spaceandamountormissingp,
  amountp,
  amountp',
  mamountp',
  commoditysymbolp,
  priceamountp,
  partialbalanceassertionp,
  fixedlotpricep,
  numberp,
  fromRawNumber,
  rawnumberp,

  -- ** comments
  multilinecommentp,
  emptyorcommentlinep,
  followingcommentp,
  followingcommentandtagsp,

  -- ** tags
  commentTags,
  tagsp,

  -- ** bracketed dates
  bracketeddatetagsp
)
where
--- * imports
import Prelude ()
import Prelude.Compat hiding (readFile)
import Control.Monad.Compat
import Control.Monad.Except (ExceptT(..), runExceptT, throwError) --, catchError)
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Char
import Data.Data
import Data.Decimal (DecimalRaw (Decimal), Decimal)
import Data.Default
import Data.Functor.Identity
import Data.List.Compat
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Data.Map as M
import qualified Data.Semigroup as Sem
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Void (Void)
import System.Time (getClockTime)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

import Hledger.Data
import Hledger.Utils
import qualified Hledger.Query as Q (Query(Any))

-- | A hledger journal reader is a triple of storage format name, a
-- detector of that format, and a parser from that format to Journal.
data Reader = Reader {

     -- The canonical name of the format handled by this reader
     rFormat   :: StorageFormat

     -- The file extensions recognised as containing this format
    ,rExtensions :: [String]

     -- A text parser for this format, accepting input options, file
     -- path for error messages and file contents, producing an exception-raising IO
     -- action that returns a journal or error message.
    ,rParser   :: InputOpts -> FilePath -> Text -> ExceptT String IO Journal

     -- Experimental readers are never tried automatically.
    ,rExperimental :: Bool
    }

instance Show Reader where show r = rFormat r ++ " reader"

-- $setup

-- | Various options to use when reading journal files.
-- Similar to CliOptions.inputflags, simplifies the journal-reading functions.
data InputOpts = InputOpts {
     -- files_             :: [FilePath]
     mformat_           :: Maybe StorageFormat  -- ^ a file/storage format to try, unless overridden
                                                --   by a filename prefix. Nothing means try all.
    ,mrules_file_       :: Maybe FilePath       -- ^ a conversion rules file to use (when reading CSV)
    ,aliases_           :: [String]             -- ^ account name aliases to apply
    ,anon_              :: Bool                 -- ^ do light anonymisation/obfuscation of the data 
    ,ignore_assertions_ :: Bool                 -- ^ don't check balance assertions
    ,new_               :: Bool                 -- ^ read only new transactions since this file was last read
    ,new_save_          :: Bool                 -- ^ save latest new transactions state for next time
    ,pivot_             :: String               -- ^ use the given field's value as the account name 
    ,auto_              :: Bool                 -- ^ generate automatic postings when journal is parsed     
 } deriving (Show, Data) --, Typeable)

instance Default InputOpts where def = definputopts

definputopts :: InputOpts
definputopts = InputOpts def def def def def def True def def

rawOptsToInputOpts :: RawOpts -> InputOpts
rawOptsToInputOpts rawopts = InputOpts{
   -- files_             = map (T.unpack . stripquotes . T.pack) $ listofstringopt "file" rawopts
   mformat_           = Nothing
  ,mrules_file_       = maybestringopt "rules-file" rawopts
  ,aliases_           = map (T.unpack . stripquotes . T.pack) $ listofstringopt "alias" rawopts
  ,anon_              = boolopt "anon" rawopts
  ,ignore_assertions_ = boolopt "ignore-assertions" rawopts
  ,new_               = boolopt "new" rawopts
  ,new_save_          = True
  ,pivot_             = stringopt "pivot" rawopts
  ,auto_              = boolopt "auto" rawopts                        
  }

--- * parsing utilities

-- | Run a string parser with no state in the identity monad.
runTextParser, rtp :: TextParser Identity a -> Text -> Either (ParseError Char Void) a
runTextParser p t =  runParser p "" t
rtp = runTextParser

-- XXX odd, why doesn't this take a JournalParser ?
-- | Run a journal parser with a null journal-parsing state.
runJournalParser, rjp :: Monad m => TextParser m a -> Text -> m (Either (ParseError Char Void) a)
runJournalParser p t = runParserT p "" t
rjp = runJournalParser

-- | Run an error-raising journal parser with a null journal-parsing state.
runErroringJournalParser, rejp :: Monad m => ErroringJournalParser m a -> Text -> m (Either String a)
runErroringJournalParser p t =
  runExceptT $
  runJournalParser (evalStateT p mempty)
                   t >>=
  either (throwError . parseErrorPretty) return
rejp = runErroringJournalParser

genericSourcePos :: SourcePos -> GenericSourcePos
genericSourcePos p = GenericSourcePos (sourceName p) (fromIntegral . unPos $ sourceLine p) (fromIntegral . unPos $ sourceColumn p)

journalSourcePos :: SourcePos -> SourcePos -> GenericSourcePos
journalSourcePos p p' = JournalSourcePos (sourceName p) (fromIntegral . unPos $ sourceLine p, fromIntegral $ line')
    where line'
            | (unPos $ sourceColumn p') == 1 = unPos (sourceLine p') - 1
            | otherwise = unPos $ sourceLine p' -- might be at end of file withat last new-line


-- | Generate Automatic postings and add them to the current journal.
generateAutomaticPostings :: Journal -> Journal
generateAutomaticPostings j = j { jtxns = map modifier $ jtxns j }
  where
    modifier = foldr (flip (.) . runModifierTransaction') id mtxns
    runModifierTransaction' = fmap txnTieKnot . runModifierTransaction Q.Any
    mtxns = jmodifiertxns j

-- | Given a megaparsec ParsedJournal parser, input options, file
-- path and file content: parse and post-process a Journal, or give an error.
parseAndFinaliseJournal :: ErroringJournalParser IO ParsedJournal -> InputOpts
                           -> FilePath -> Text -> ExceptT String IO Journal
parseAndFinaliseJournal parser iopts f txt = do
  t <- liftIO getClockTime
  y <- liftIO getCurrentYear
  ep <- runParserT (evalStateT parser nulljournal {jparsedefaultyear=Just y}) f txt
  case ep of
    Right pj -> 
      let pj' = if auto_ iopts then generateAutomaticPostings pj else pj in
      case journalFinalise t f txt (not $ ignore_assertions_ iopts) pj' of
                        Right j -> return j
                        Left e  -> throwError e
    Left e   -> throwError $ parseErrorPretty e

parseAndFinaliseJournal' :: JournalParser Identity ParsedJournal -> InputOpts 
                            -> FilePath -> Text -> ExceptT String IO Journal
parseAndFinaliseJournal' parser iopts f txt = do
  t <- liftIO getClockTime
  y <- liftIO getCurrentYear
  let ep = runParser (evalStateT parser nulljournal {jparsedefaultyear=Just y}) f txt
  case ep of
    Right pj -> 
      let pj' = if auto_ iopts then generateAutomaticPostings pj else pj in      
      case journalFinalise t f txt (not $ ignore_assertions_ iopts) pj' of
                        Right j -> return j
                        Left e  -> throwError e
    Left e   -> throwError $ parseErrorPretty e

setYear :: Year -> JournalParser m ()
setYear y = modify' (\j -> j{jparsedefaultyear=Just y})

getYear :: JournalParser m (Maybe Year)
getYear = fmap jparsedefaultyear get

setDefaultCommodityAndStyle :: (CommoditySymbol,AmountStyle) -> JournalParser m ()
setDefaultCommodityAndStyle cs = modify' (\j -> j{jparsedefaultcommodity=Just cs})

getDefaultCommodityAndStyle :: JournalParser m (Maybe (CommoditySymbol,AmountStyle))
getDefaultCommodityAndStyle = jparsedefaultcommodity `fmap` get

-- | Get amount style associated with default currency.
--
-- Returns 'AmountStyle' used to defined by a latest default commodity directive
-- prior to current position within this file or its parents.
getDefaultAmountStyle :: JournalParser m (Maybe AmountStyle)
getDefaultAmountStyle = fmap snd <$> getDefaultCommodityAndStyle

-- | Lookup currency-specific amount style.
--
-- Returns 'AmountStyle' used in commodity directive within current journal
-- prior to current position or in its parents files.
getAmountStyle :: CommoditySymbol -> JournalParser m (Maybe AmountStyle)
getAmountStyle commodity = do
    specificStyle <-  maybe Nothing cformat . M.lookup commodity . jcommodities <$> get
    defaultStyle <- fmap snd <$> getDefaultCommodityAndStyle
    let effectiveStyle = listToMaybe $ catMaybes [specificStyle, defaultStyle]
    return effectiveStyle

pushAccount :: AccountName -> JournalParser m ()
pushAccount acct = modify' (\j -> j{jaccounts = (acct, Nothing) : jaccounts j})

pushParentAccount :: AccountName -> JournalParser m ()
pushParentAccount acct = modify' (\j -> j{jparseparentaccounts = acct : jparseparentaccounts j})

popParentAccount :: JournalParser m ()
popParentAccount = do
  j <- get
  case jparseparentaccounts j of
    []       -> unexpected (Tokens ('E' :| "nd of apply account block with no beginning"))
    (_:rest) -> put j{jparseparentaccounts=rest}

getParentAccount :: JournalParser m AccountName
getParentAccount = fmap (concatAccountNames . reverse . jparseparentaccounts) get

addAccountAlias :: MonadState Journal m => AccountAlias -> m ()
addAccountAlias a = modify' (\(j@Journal{..}) -> j{jparsealiases=a:jparsealiases})

getAccountAliases :: MonadState Journal m => m [AccountAlias]
getAccountAliases = fmap jparsealiases get

clearAccountAliases :: MonadState Journal m => m ()
clearAccountAliases = modify' (\(j@Journal{..}) -> j{jparsealiases=[]})

-- getTransactionCount :: MonadState Journal m =>  m Integer
-- getTransactionCount = fmap jparsetransactioncount get
--
-- setTransactionCount :: MonadState Journal m => Integer -> m ()
-- setTransactionCount i = modify' (\j -> j{jparsetransactioncount=i})
--
-- -- | Increment the transaction index by one and return the new value.
-- incrementTransactionCount :: MonadState Journal m => m Integer
-- incrementTransactionCount = do
--   modify' (\j -> j{jparsetransactioncount=jparsetransactioncount j + 1})
--   getTransactionCount

journalAddFile :: (FilePath,Text) -> Journal -> Journal
journalAddFile f j@Journal{jfiles=fs} = j{jfiles=fs++[f]}
  -- append, unlike the other fields, even though we do a final reverse,
  -- to compensate for additional reversal due to including/monoid-concatting

-- -- | Terminate parsing entirely, returning the given error message
-- -- with the current parse position prepended.
-- parserError :: String -> ErroringJournalParser a
-- parserError s = do
--   pos <- getPosition
--   parserErrorAt pos s

-- | Terminate parsing entirely, returning the given error message
-- with the given parse position prepended.
parserErrorAt :: Monad m => SourcePos -> String -> ErroringJournalParser m a
parserErrorAt pos s = throwError $ sourcePosPretty pos ++ ":\n" ++ s

--- * parsers

--- ** transaction bits

statusp :: TextParser m Status
statusp =
  choice'
    [ skipMany spacenonewline >> char '*' >> return Cleared
    , skipMany spacenonewline >> char '!' >> return Pending
    , return Unmarked
    ]

codep :: TextParser m Text
codep = option "" $ try $ do
  skipSome spacenonewline
  between (char '(') (char ')') $ takeWhileP Nothing (/= ')')

descriptionp :: JournalParser m Text
descriptionp = takeWhileP Nothing (not . semicolonOrNewline)
  where semicolonOrNewline c = c == ';' || c == '\n'

--- ** dates

-- | Parse a date in YYYY/MM/DD format.
-- Hyphen (-) and period (.) are also allowed as separators.
-- The year may be omitted if a default year has been set.
-- Leading zeroes may be omitted.
datep :: JournalParser m Day
datep = do
  mYear <- getYear
  lift $ datep' mYear

datep' :: Maybe Year -> TextParser m Day
datep' mYear = do
  d1 <- decimal <?> "year or month"
  sep <- satisfy isDateSepChar <?> "date separator"
  d2 <- decimal <?> "month or day"
  fullDate d1 sep d2 <|> partialDate mYear d1 sep d2
  <?> "full or partial date"

  where

  fullDate :: Integer -> Char -> Int -> TextParser m Day
  fullDate year sep1 month = do
    sep2 <- satisfy isDateSepChar <?> "date separator"
    day <- decimal <?> "day"
    let dateStr = show year ++ [sep1] ++ show month ++ [sep2] ++ show day

    when (sep1 /= sep2) $ fail $
      "invalid date (mixing date separators is not allowed): " ++ dateStr

    case fromGregorianValid year month day of
      Nothing -> fail $ "well-formed but invalid date: " ++ dateStr
      Just date -> pure date

  partialDate :: Maybe Year -> Integer -> Char -> Int -> TextParser m Day
  partialDate mYear month sep day = case mYear of
    Just year ->
      case fromGregorianValid year (fromIntegral month) day of
        Nothing -> fail $ "well-formed but invalid date: " ++ dateStr
        Just date -> pure date
      where dateStr = show year ++ [sep] ++ show month ++ [sep] ++ show day

    Nothing -> fail $
      "partial date "++dateStr++" found, but the current year is unknown"
      where dateStr = show month ++ [sep] ++ show day

-- | Parse a date and time in YYYY/MM/DD HH:MM[:SS][+-ZZZZ] format.
-- Hyphen (-) and period (.) are also allowed as date separators.
-- The year may be omitted if a default year has been set.
-- Seconds are optional.
-- The timezone is optional and ignored (the time is always interpreted as a local time).
-- Leading zeroes may be omitted (except in a timezone).
datetimep :: JournalParser m LocalTime
datetimep = do
  day <- datep
  lift $ skipSome spacenonewline
  h <- some digitChar
  let h' = read h
  guard $ h' >= 0 && h' <= 23
  char ':'
  m <- some digitChar
  let m' = read m
  guard $ m' >= 0 && m' <= 59
  s <- optional $ char ':' >> some digitChar
  let s' = case s of Just sstr -> read sstr
                     Nothing   -> 0
  guard $ s' >= 0 && s' <= 59
  {- tz <- -}
  optional $ do
                   plusminus <- oneOf ("-+" :: [Char])
                   d1 <- digitChar
                   d2 <- digitChar
                   d3 <- digitChar
                   d4 <- digitChar
                   return $ plusminus:d1:d2:d3:d4:""
  -- ltz <- liftIO $ getCurrentTimeZone
  -- let tz' = maybe ltz (fromMaybe ltz . parseTime defaultTimeLocale "%z") tz
  -- return $ localTimeToUTC tz' $ LocalTime day $ TimeOfDay h' m' (fromIntegral s')
  return $ LocalTime day $ TimeOfDay h' m' (fromIntegral s')

secondarydatep :: Day -> TextParser m Day
secondarydatep primaryDate = char '=' *> datep' (Just primaryYear)
  where primaryYear = first3 $ toGregorian primaryDate

--- ** account names

-- | Parse an account name, then apply any parent account prefix and/or account aliases currently in effect.
modifiedaccountnamep :: JournalParser m AccountName
modifiedaccountnamep = do
  parent <- getParentAccount
  aliases <- getAccountAliases
  a <- lift accountnamep
  return $
    accountNameApplyAliases aliases $
     -- XXX accountNameApplyAliasesMemo ? doesn't seem to make a difference
    joinAccountNames parent
    a

-- | Parse an account name. Account names start with a non-space, may
-- have single spaces inside them, and are terminated by two or more
-- spaces (or end of input). Also they have one or more components of
-- at least one character, separated by the account separator char.
-- (This parser will also consume one following space, if present.)
accountnamep :: TextParser m AccountName
accountnamep = do
  firstPart <- part
  otherParts <- many $ try $ singleSpace *> part
  let account = T.unwords $ firstPart : otherParts

  let roundTripAccount =
        accountNameFromComponents $ accountNameComponents account
  when (account /= roundTripAccount) $ fail $
    "account name seems ill-formed: " ++ T.unpack account

  pure account
  where
    part = takeWhile1P Nothing (not . isSpace)
    singleSpace = void spacenonewline *> notFollowedBy spacenonewline

--- ** amounts

-- | Parse whitespace then an amount, with an optional left or right
-- currency symbol and optional price, or return the special
-- "missing" marker amount.
spaceandamountormissingp :: Monad m => JournalParser m MixedAmount
spaceandamountormissingp =
  option missingmixedamt $ try $ do
    lift $ skipSome spacenonewline
    Mixed . (:[]) <$> amountp

#ifdef TESTS
assertParseEqual' :: (Show a, Eq a) => (Either ParseError a) -> a -> Assertion
assertParseEqual' parse expected = either (assertFailure.show) (`is'` expected) parse

is' :: (Eq a, Show a) => a -> a -> Assertion
a `is'` e = assertEqual e a

test_spaceandamountormissingp = do
    assertParseEqual' (parseWithState mempty spaceandamountormissingp " $47.18") (Mixed [usd 47.18])
    assertParseEqual' (parseWithState mempty spaceandamountormissingp "$47.18") missingmixedamt
    assertParseEqual' (parseWithState mempty spaceandamountormissingp " ") missingmixedamt
    assertParseEqual' (parseWithState mempty spaceandamountormissingp "") missingmixedamt
#endif

-- | Parse a single-commodity amount, with optional symbol on the left or
-- right, optional unit or total price, and optional (ignored)
-- ledger-style balance assertion or fixed lot price declaration.
amountp :: Monad m => JournalParser m Amount
amountp = try leftsymbolamountp <|> try rightsymbolamountp <|> nosymbolamountp

#ifdef TESTS
test_amountp = do
    assertParseEqual' (parseWithState mempty amountp "$47.18") (usd 47.18)
    assertParseEqual' (parseWithState mempty amountp "$1.") (usd 1 `withPrecision` 0)
  -- ,"amount with unit price" ~: do
    assertParseEqual'
     (parseWithState mempty amountp "$10 @ €0.5")
     (usd 10 `withPrecision` 0 `at` (eur 0.5 `withPrecision` 1))
  -- ,"amount with total price" ~: do
    assertParseEqual'
     (parseWithState mempty amountp "$10 @@ €5")
     (usd 10 `withPrecision` 0 @@ (eur 5 `withPrecision` 0))
#endif

-- | Parse an amount from a string, or get an error.
amountp' :: String -> Amount
amountp' s =
  case runParser (evalStateT (amountp <* eof) mempty) "" (T.pack s) of
    Right amt -> amt
    Left err  -> error' $ show err -- XXX should throwError

-- | Parse a mixed amount from a string, or get an error.
mamountp' :: String -> MixedAmount
mamountp' = Mixed . (:[]) . amountp'

signp :: Num a => TextParser m (a -> a)
signp = char '-' *> pure negate <|> char '+' *> pure id <|> pure id

multiplierp :: TextParser m Bool
multiplierp = option False $ char '*' *> pure True

-- | This is like skipMany but it returns True if at least one element
-- was skipped. This is helpful if you’re just using many to check if
-- the resulting list is empty or not.
skipMany' :: MonadPlus m => m a -> m Bool
skipMany' p = go False
  where
    go !isNull = do
      more <- option False (True <$ p)
      if more
        then go True
        else pure isNull

leftsymbolamountp :: Monad m => JournalParser m Amount
leftsymbolamountp = do
  sign <- lift signp
  m <- lift multiplierp
  c <- lift commoditysymbolp
  suggestedStyle <- getAmountStyle c
  commodityspaced <- lift $ skipMany' spacenonewline
  (q,prec,mdec,mgrps) <- lift $ numberp suggestedStyle
  let s = amountstyle{ascommodityside=L, ascommodityspaced=commodityspaced, asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps}
  p <- priceamountp
  return $ Amount c (sign q) p s m
  <?> "left-symbol amount"

rightsymbolamountp :: Monad m => JournalParser m Amount
rightsymbolamountp = do
  m <- lift multiplierp
  sign <- lift signp
  ambiguousRawNum <- lift rawnumberp
  expMod <- lift . option id $ try exponentp
  commodityspaced <- lift $ skipMany' spacenonewline
  c <- lift commoditysymbolp
  suggestedStyle <- getAmountStyle c
  let (q0,prec0,mdec,mgrps) =
        fromRawNumber $ either (disambiguateNumber suggestedStyle) id ambiguousRawNum
      (q, prec) = expMod (sign q0, prec0)
  p <- priceamountp
  let s = amountstyle{ascommodityside=R, ascommodityspaced=commodityspaced, asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps}
  return $ Amount c q p s m
  <?> "right-symbol amount"

nosymbolamountp :: Monad m => JournalParser m Amount
nosymbolamountp = do
  m <- lift multiplierp
  suggestedStyle <- getDefaultAmountStyle
  (q,prec,mdec,mgrps) <- lift $ numberp suggestedStyle
  p <- priceamountp
  -- apply the most recently seen default commodity and style to this commodityless amount
  defcs <- getDefaultCommodityAndStyle
  let (c,s) = case defcs of
        Just (defc,defs) -> (defc, defs{asprecision=max (asprecision defs) prec})
        Nothing          -> ("", amountstyle{asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps})
  return $ Amount c q p s m
  <?> "no-symbol amount"

commoditysymbolp :: TextParser m CommoditySymbol
commoditysymbolp = (quotedcommoditysymbolp <|> simplecommoditysymbolp) <?> "commodity symbol"

quotedcommoditysymbolp :: TextParser m CommoditySymbol
quotedcommoditysymbolp =
  between (char '"') (char '"') $ takeWhile1P Nothing f
  where f c = c /= ';' && c /= '\n' && c /= '\"'

simplecommoditysymbolp :: TextParser m CommoditySymbol
simplecommoditysymbolp = takeWhile1P Nothing (not . isNonsimpleCommodityChar)

priceamountp :: Monad m => JournalParser m Price
priceamountp = option NoPrice $ try $ do
  lift (skipMany spacenonewline)
  char '@'

  m <- optional $ char '@'
  let priceConstructor = case m of
        Just _  -> TotalPrice
        Nothing -> UnitPrice

  lift (skipMany spacenonewline)
  priceAmount <- amountp -- XXX can parse more prices ad infinitum, shouldn't

  pure $ priceConstructor priceAmount

partialbalanceassertionp :: Monad m => JournalParser m BalanceAssertion
partialbalanceassertionp = optional $ try $ do
  lift (skipMany spacenonewline)
  sourcepos <- genericSourcePos <$> lift getPosition
  char '='
  lift (skipMany spacenonewline)
  a <- amountp -- XXX should restrict to a simple amount
  return (a, sourcepos)

-- balanceassertion :: Monad m => TextParser m (Maybe MixedAmount)
-- balanceassertion =
--     try (do
--           lift (skipMany spacenonewline)
--           string "=="
--           lift (skipMany spacenonewline)
--           a <- amountp -- XXX should restrict to a simple amount
--           return $ Just $ Mixed [a])
--          <|> return Nothing

-- http://ledger-cli.org/3.0/doc/ledger3.html#Fixing-Lot-Prices
fixedlotpricep :: Monad m => JournalParser m (Maybe Amount)
fixedlotpricep = optional $ try $ do
  lift (skipMany spacenonewline)
  char '{'
  lift (skipMany spacenonewline)
  char '='
  lift (skipMany spacenonewline)
  a <- amountp -- XXX should restrict to a simple amount
  lift (skipMany spacenonewline)
  char '}'
  return a

-- | Parse a string representation of a number for its value and display
-- attributes.
--
-- Some international number formats are accepted, eg either period or comma
-- may be used for the decimal point, and the other of these may be used for
-- separating digit groups in the integer part. See
-- http://en.wikipedia.org/wiki/Decimal_separator for more examples.
--
-- This returns: the parsed numeric value, the precision (number of digits
-- seen following the decimal point), the decimal point character used if any,
-- and the digit group style if any.
--
numberp :: Maybe AmountStyle -> TextParser m (Quantity, Int, Maybe Char, Maybe DigitGroupStyle)
numberp suggestedStyle = do
    -- a number is an optional sign followed by a sequence of digits possibly
    -- interspersed with periods, commas, or both
    -- ptrace "numberp"
    sign <- signp
    raw <- either (disambiguateNumber suggestedStyle) id <$> rawnumberp
    dbg8 "numberp suggestedStyle" suggestedStyle `seq` return ()
    let (q, prec, decSep, groups) =
          dbg8 "numberp quantity,precision,mdecimalpoint,mgrps"
            $ fromRawNumber raw
    mExp <- optional $ try $ exponentp
    case mExp of
      Just expFunc
        | isJust groups -> fail "groups and exponent are not mixable"
        | otherwise -> let (q', prec') = expFunc (q, prec)
                       in  pure (sign q', prec', decSep, groups)
      Nothing -> pure (sign q, prec, decSep, groups)
    <?> "numberp"

exponentp :: TextParser m ((Quantity, Int) -> (Quantity, Int))
exponentp = do
    char' 'e'
    exp <- ($) <$> signp <*> (read <$> some digitChar)
    return $ bimap (* 10^^exp) (max 0 . subtract exp)
    <?> "exponentp"

-- | Interpret a raw number as a decimal number.
--
-- Returns:
-- - the decimal number
-- - the precision (number of digits after the decimal point)  
-- - the decimal point character, if any
-- - the digit group style, if any (digit group character and sizes of digit groups)
fromRawNumber :: RawNumber -> (Quantity, Int, Maybe Char, Maybe DigitGroupStyle)
fromRawNumber raw = case raw of

  NoSeparators digitGrp mDecimals ->
    let decimalGrp = maybe mempty snd mDecimals
        (quantity, precision) = toDecimal digitGrp decimalGrp
    in  (quantity, precision, fmap fst mDecimals, Nothing)

  WithSeparators digitSep digitGrps mDecimals ->
    let decimalGrp = maybe mempty snd mDecimals
        (quantity, precision) = toDecimal (mconcat digitGrps) decimalGrp
        digitGroupStyle = DigitGroups digitSep (groupSizes digitGrps)
    in  (quantity, precision, fmap fst mDecimals, Just digitGroupStyle)

  where
    -- Outputs digit group sizes from least significant to most significant
    groupSizes :: [DigitGrp] -> [Int]
    groupSizes digitGrps = reverse $ case map digitGroupLength digitGrps of
      (a:b:cs) | a < b -> b:cs
      gs               -> gs

    toDecimal :: DigitGrp -> DigitGrp -> (Decimal, Int)
    toDecimal preDecimalGrp postDecimalGrp = (quantity, precision)
      where
        quantity = Decimal (fromIntegral precision)
                           (digitGroupNumber $ preDecimalGrp <> postDecimalGrp)
        precision = digitGroupLength postDecimalGrp


disambiguateNumber :: Maybe AmountStyle -> AmbiguousNumber -> RawNumber
disambiguateNumber suggestedStyle (AmbiguousNumber grp1 sep grp2) =
  -- If present, use the suggested style to disambiguate;
  -- otherwise, assume that the separator is a decimal point where possible.
  if isDecimalPointChar sep &&
     maybe True (sep `isValidDecimalBy`) suggestedStyle
  then NoSeparators grp1 (Just (sep, grp2))
  else WithSeparators sep [grp1, grp2] Nothing
  where
    isValidDecimalBy :: Char -> AmountStyle -> Bool
    isValidDecimalBy c = \case
      AmountStyle{asdecimalpoint = Just d} -> d == c
      AmountStyle{asdigitgroups = Just (DigitGroups g _)} -> g /= c
      AmountStyle{asprecision = 0} -> False
      _ -> True

-- | Parse and interpret the structure of a number without external hints.
-- Numbers are digit strings, possibly separated into digit groups by one
-- of two types of separators. (1) Numbers may optionally have a decimal
-- point, which may be either a period or comma. (2) Numbers may
-- optionally contain digit group separators, which must all be either a
-- period, a comma, or a space.
--
-- It is our task to deduce the identities of the decimal point and digit
-- separator characters, based on the allowed syntax. For instance, we
-- make use of the fact that a decimal point can occur at most once and
-- must succeed all digit group separators.
--
-- >>> parseTest rawnumberp "1,234,567.89"
-- Right (WithSeparators ',' ["1","234","567"] (Just ('.',"89")))
-- >>> parseTest rawnumberp "1,000"
-- Left (AmbiguousNumber "1" ',' "000")
-- >>> parseTest rawnumberp "1 000"
-- Right (WithSeparators ' ' ["1","000"] Nothing)
--
rawnumberp :: TextParser m (Either AmbiguousNumber RawNumber)
rawnumberp = label "rawnumberp" $ do
  rawNumber <- fmap Right leadingDecimalPt <|> leadingDigits
  -- Guard against mistyped numbers
  notFollowedBy $ satisfy isDecimalPointChar <|> char ' ' *> digitChar
  return $ dbg8 "rawnumberp" rawNumber
  where

  leadingDecimalPt :: TextParser m RawNumber
  leadingDecimalPt = do
    decPt <- satisfy isDecimalPointChar
    decGrp <- digitgroupp
    pure $ NoSeparators mempty (Just (decPt, decGrp))

  leadingDigits :: TextParser m (Either AmbiguousNumber RawNumber)
  leadingDigits = do
    grp1 <- digitgroupp
    withSeparators grp1 <|> fmap Right (trailingDecimalPt grp1)
                        <|> pure (Right $ NoSeparators grp1 Nothing)

  withSeparators :: DigitGrp -> TextParser m (Either AmbiguousNumber RawNumber)
  withSeparators grp1 = do
    (sep, grp2) <- try $ (,) <$> satisfy isDigitSeparatorChar <*> digitgroupp
    grps <- many $ try $ char sep *> digitgroupp

    let digitGroups = grp1 : grp2 : grps
    fmap Right (withDecimalPt sep digitGroups)
      <|> pure (withoutDecimalPt grp1 sep grp2 grps)

  withDecimalPt :: Char -> [DigitGrp] -> TextParser m RawNumber
  withDecimalPt digitSep digitGroups = do
    decPt <- satisfy $ \c -> isDecimalPointChar c && c /= digitSep
    decDigitGrp <- option mempty digitgroupp

    pure $ WithSeparators digitSep digitGroups (Just (decPt, decDigitGrp))

  withoutDecimalPt
    :: DigitGrp
    -> Char
    -> DigitGrp
    -> [DigitGrp]
    -> Either AmbiguousNumber RawNumber
  withoutDecimalPt grp1 sep grp2 grps
    | null grps && isDecimalPointChar sep =
        Left $ AmbiguousNumber grp1 sep grp2
    | otherwise = Right $ WithSeparators sep (grp1:grp2:grps) Nothing

  trailingDecimalPt :: DigitGrp -> TextParser m RawNumber
  trailingDecimalPt grp1 = do
    decPt <- satisfy isDecimalPointChar
    pure $ NoSeparators grp1 (Just (decPt, mempty))


isDecimalPointChar :: Char -> Bool
isDecimalPointChar c = c == '.' || c == ','

isDigitSeparatorChar :: Char -> Bool
isDigitSeparatorChar c = isDecimalPointChar c || c == ' '


data DigitGrp = DigitGrp {
  digitGroupLength :: Int,
  digitGroupNumber :: Integer
} deriving (Eq)

instance Show DigitGrp where
  show (DigitGrp len num)
    | len > 0 = "\"" ++ padding ++ numStr ++ "\""
    | otherwise = "\"\""
    where numStr = show num
          padding = replicate (len - length numStr) '0'

instance Sem.Semigroup DigitGrp where
  DigitGrp l1 n1 <> DigitGrp l2 n2 = DigitGrp (l1 + l2) (n1 * 10^l2 + n2)

instance Monoid DigitGrp where
  mempty = DigitGrp 0 0
  mappend = (Sem.<>)

digitgroupp :: TextParser m DigitGrp
digitgroupp = label "digit group"
            $ makeGroup <$> takeWhile1P (Just "digit") isDigit
  where
    makeGroup = uncurry DigitGrp . foldl' step (0, 0) . T.unpack
    step (!l, !a) c = (l+1, a*10 + fromIntegral (digitToInt c))


data RawNumber
  = NoSeparators   DigitGrp (Maybe (Char, DigitGrp))        -- 100 or 100. or .100 or 100.50
  | WithSeparators Char [DigitGrp] (Maybe (Char, DigitGrp)) -- 1,000,000 or 1,000.50
  deriving (Show, Eq)

data AmbiguousNumber = AmbiguousNumber DigitGrp Char DigitGrp  -- 1,000
  deriving (Show, Eq)

-- test_numberp = do
--       let s `is` n = assertParseEqual (parseWithState mempty numberp s) n
--           assertFails = assertBool . isLeft . parseWithState mempty numberp
--       assertFails ""
--       "0"          `is` (0, 0, '.', ',', [])
--       "1"          `is` (1, 0, '.', ',', [])
--       "1.1"        `is` (1.1, 1, '.', ',', [])
--       "1,000.1"    `is` (1000.1, 1, '.', ',', [3])
--       "1.00.000,1" `is` (100000.1, 1, ',', '.', [3,2])
--       "1,000,000"  `is` (1000000, 0, '.', ',', [3,3])
--       "1."         `is` (1,   0, '.', ',', [])
--       "1,"         `is` (1,   0, ',', '.', [])
--       ".1"         `is` (0.1, 1, '.', ',', [])
--       ",1"         `is` (0.1, 1, ',', '.', [])
--       assertFails "1,000.000,1"
--       assertFails "1.000,000.1"
--       assertFails "1,000.000.1"
--       assertFails "1,,1"
--       assertFails "1..1"
--       assertFails ".1,"
--       assertFails ",1."

--- ** comments

multilinecommentp :: TextParser m ()
multilinecommentp = startComment *> anyLine `skipManyTill` endComment
  where
    startComment = string "comment" >> skipLine
    endComment = eof <|> string "end comment" *> skipLine

    skipLine = void $ skipMany spacenonewline *> newline
    anyLine = takeWhileP Nothing (\c -> c /= '\n') *> newline

emptyorcommentlinep :: TextParser m ()
emptyorcommentlinep = do
  skipMany spacenonewline
  void linecommentp <|> void newline

-- | Parse a possibly multi-line comment following a semicolon.
followingcommentp :: TextParser m Text
followingcommentp = T.unlines . map snd <$> followingcommentlinesp

followingcommentlinesp :: TextParser m [(SourcePos, Text)]
followingcommentlinesp = do
  skipMany spacenonewline

  samelineComment@(_, samelineCommentText)
    <- try commentp <|> (,) <$> (getPosition <* eolof) <*> pure ""
  newlineComments <- many $ try $ do
    skipSome spacenonewline -- leading whitespace is required
    commentp

  if T.null samelineCommentText && null newlineComments
    then pure []
    else pure $ samelineComment : newlineComments

-- | Parse a possibly multi-line comment following a semicolon, and
-- any tags and/or posting dates within it. Posting dates can be
-- expressed with "date"/"date2" tags and/or bracketed dates.  The
-- dates are parsed in full here so that errors are reported in the
-- right position. Missing years can be inferred if a default date is
-- provided.
--
-- >>> rejp (followingcommentandtagsp (Just $ fromGregorian 2000 1 2)) "; a:b, date:3/4, [=5/6]"
-- Right ("a:b, date:3/4, [=5/6]\n",[("a","b"),("date","3/4")],Just 2000-03-04,Just 2000-05-06)
--
-- Year unspecified and no default provided -> unknown year error, at correct position:
-- >>> rejp (followingcommentandtagsp Nothing) "  ;    xxx   date:3/4\n  ; second line"
-- Left ...1:22...partial date 3/4 found, but the current year is unknown...
--
-- Date tag value contains trailing text - forgot the comma, confused:
-- the syntaxes ?  We'll accept the leading date anyway
-- >>> rejp (followingcommentandtagsp (Just $ fromGregorian 2000 1 2)) "; date:3/4=5/6"
-- Right ("date:3/4=5/6\n",[("date","3/4=5/6")],Just 2000-03-04,Nothing)
--
followingcommentandtagsp
  :: Monad m
  => Maybe Day
  -> ErroringJournalParser m (Text, [Tag], Maybe Day, Maybe Day)
followingcommentandtagsp mdefdate = do
  -- pdbg 0 "followingcommentandtagsp"

  commentLines <- lift followingcommentlinesp
  -- pdbg 0 $ "commentws:" ++ show commentLines

  -- Reparse the comment for any tags.
  tagsWithPositions <- case
    traverse (runTextParserAt tagswithvaluepositions) commentLines of
      Right tss -> pure $ concat tss
      Left e    -> throwError $ parseErrorPretty e

  -- Extract date-tag style posting dates from the tags.
  -- Use the transaction date for defaults, if provided.
  let isDateLabel txt = txt == "date" || txt == "date2"
      isDateTag = isDateLabel . fst . snd
  tagDates <- case traverse tagDate $ filter isDateTag tagsWithPositions of
      Right ds -> pure ds
      Left e   -> throwError $ parseErrorPretty e

  -- Reparse the comment for any bracketed style posting dates.
  -- Use the transaction date for defaults, if provided.
  bracketedDates <- case
    traverse (runTextParserAt (bracketedpostingdatesp mdefdate))
             commentLines of
      Right dss -> pure $ concat dss
      Left e    -> throwError $ parseErrorPretty e

  let pdates = tagDates ++ bracketedDates
      mdate  = fmap snd $ find ((=="date") .fst) pdates
      mdate2 = fmap snd $ find ((=="date2").fst) pdates
  -- pdbg 0 $ "allDates: "++show pdates

  let strippedComment = T.unlines $ map (T.strip . snd) commentLines
      tags = map snd tagsWithPositions
  -- pdbg 0 $ "comment:"++show strippedComment

  pure (strippedComment, tags, mdate, mdate2)

  where
    runTextParserAt parser (pos, txt) =
      runTextParser (setPosition pos *> parser) txt

    tagDate :: (SourcePos, Tag)
            -> Either (ParseError Char Void) (TagName, Day)
    tagDate (pos, (name, value)) =
      fmap (name,) $ runTextParserAt (datep' myear) (pos, value)
      where myear = fmap (first3 . toGregorian) mdefdate

-- A transaction/posting comment must start with a semicolon. This parser
-- discards the leading whitespace of the comment and returns the source
-- position of the comment's first non-whitespace character.
commentp :: TextParser m (SourcePos, Text)
commentp = commentStartingWithp (==';')

-- A line (file-level) comment can start with a semicolon, hash, or star
-- (allowing org nodes). This parser discards the leading whitespace of
-- the comment and returns the source position of the comment's first
-- non-whitespace character.
linecommentp :: TextParser m (SourcePos, Text)
linecommentp =
  commentStartingWithp $ \c -> c == ';' || c == '#' || c == '*'

commentStartingWithp :: (Char -> Bool) -> TextParser m (SourcePos, Text)
commentStartingWithp f = do
  -- ptrace "commentStartingWith"
  satisfy f
  skipMany spacenonewline
  startPos <- getPosition
  content <- takeWhileP Nothing (\c -> c /= '\n')
  optional newline
  return (startPos, content)

--- ** tags

-- | Extract any tags (name:value ended by comma or newline) embedded in a string.
--
-- >>> commentTags "a b:, c:c d:d, e"
-- [("b",""),("c","c d:d")]
--
-- >>> commentTags "a [1/1/1] [1/1] [1], [=1/1/1] [=1/1] [=1] [1/1=1/1/1] [1=1/1/1] b:c"
-- [("b","c")]
--
-- --[("date","1/1/1"),("date","1/1"),("date2","1/1/1"),("date2","1/1"),("date","1/1"),("date2","1/1/1"),("date","1"),("date2","1/1/1")]
--
-- >>> commentTags "\na b:, \nd:e, f"
-- [("b",""),("d","e")]
--
-- >>> commentTags ":value"
-- []
--
commentTags :: Text -> [Tag]
commentTags s = either (const []) id $ runTextParser tagsp s

-- | Parse all tags found in a string.
tagsp :: SimpleTextParser [Tag]
tagsp = map snd <$> tagswithvaluepositions

tagswithvaluepositions :: SimpleTextParser [(SourcePos, Tag)]
tagswithvaluepositions = do
  -- pdbg 0 $ "tagsp"

  -- If we parse in a single pass, we cannot know whether some text
  -- belongs to a tag label until we have reached a colon (in which case
  -- it does) or whitespace (in which case it does not). Therefore, we
  -- hold on to the text until we reach such a break point, and then
  -- decide what to do.

  potentialTagName <- tillNextBreak
  atSpaceChar <|> atColon potentialTagName <|> atEof

  where

    isBreak :: Char -> Bool
    isBreak c = isSpace c || c == ':'

    tillNextBreak :: SimpleTextParser Text
    tillNextBreak = takeWhileP Nothing (not . isBreak)

    tagValue :: SimpleTextParser Text
    tagValue = T.strip <$> takeWhileP Nothing (not . commaOrNewline)
      where commaOrNewline c = c == ',' || c == '\n'

    atSpaceChar :: SimpleTextParser [(SourcePos, Tag)]
    atSpaceChar = skipSome spaceChar *> tagswithvaluepositions

    atColon :: Text -> SimpleTextParser [(SourcePos, Tag)]
    atColon tagName = do
      char ':'
      if T.null tagName
        then tagswithvaluepositions
        else do
          pos <- getPosition
          tagVal <- tagValue
          let tag = (pos, (tagName, tagVal))
          tags <- tagswithvaluepositions
          pure $ tag : tags

    atEof :: SimpleTextParser [(SourcePos, Tag)]
    atEof = eof *> pure []

--- ** posting dates

-- | Parse all bracketed posting dates found in a string. The dates are
-- parsed fully to give useful errors. Missing years can be inferred only
-- if a default date is provided.
--
bracketedpostingdatesp :: Maybe Day -> SimpleTextParser [(TagName,Day)]
bracketedpostingdatesp mdefdate = do
  -- pdbg 0 $ "bracketedpostingdatesp"
  skipMany $ notChar '['
  concat <$> sepEndBy (bracketeddatetagsp mdefdate <|> char '[' *> pure [])
                      (skipMany $ notChar '[')

--- ** bracketed dates

-- | Parse Ledger-style bracketed posting dates ([DATE=DATE2]), as
-- "date" and/or "date2" tags. Anything that looks like an attempt at
-- this (a square-bracketed sequence of 0123456789/-.= containing at
-- least one digit and one date separator) is also parsed, and will
-- throw an appropriate error.
--
-- The dates are parsed in full here so that errors are reported in
-- the right position. A missing year in DATE can be inferred if a
-- default date is provided. A missing year in DATE2 will be inferred
-- from DATE.
--
-- >>> first parseErrorPretty $ rtp (bracketeddatetagsp Nothing) "[2016/1/2=3/4]"
-- Right [("date",2016-01-02),("date2",2016-03-04)]
--
-- >>> first parseErrorPretty $ rtp (bracketeddatetagsp Nothing) "[1]"
-- Left ...not a bracketed date...
--
-- >>> first parseErrorPretty $ rtp (bracketeddatetagsp Nothing) "[2016/1/32]"
-- Left ...1:11:...well-formed but invalid date: 2016/1/32...
--
-- >>> first parseErrorPretty $ rtp (bracketeddatetagsp Nothing) "[1/31]"
-- Left ...1:6:...partial date 1/31 found, but the current year is unknown...
--
-- >>> first parseErrorPretty $ rtp (bracketeddatetagsp Nothing) "[0123456789/-.=/-.=]"
-- Left ...1:13:...expecting month or day...
--
bracketeddatetagsp :: Maybe Day -> SimpleTextParser [(TagName, Day)]
bracketeddatetagsp mdefdate = do
  -- pdbg 0 "bracketeddatetagsp"
  try $ do
    s <- lookAhead
       $ between (char '[') (char ']')
       $ takeWhile1P Nothing isBracketedDateChar
    unless (T.any isDigit s && T.any isDateSepChar s) $
      fail "not a bracketed date"
  -- Looks sufficiently like a bracketed date to commit to parsing a date

  between (char '[') (char ']') $ do
    let myear1 = fmap readYear mdefdate
    md1 <- optional $ datep' myear1

    let myear2 = fmap readYear md1 <|> myear1
    md2 <- optional $ char '=' *> datep' myear2

    pure $ catMaybes [("date",) <$> md1, ("date2",) <$> md2]

  where
    readYear = first3 . toGregorian
    isBracketedDateChar c = isDigit c || isDateSepChar c || c == '='
