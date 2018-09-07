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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

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
  genericSourcePos,
  journalSourcePos,
  applyTransactionModifiers,
  parseAndFinaliseJournal,
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
  transactioncommentp,
  postingcommentp,

  -- ** bracketed dates
  bracketeddatetagsp,

  -- ** misc
  singlespacedtextp,
  singlespacep,

  -- * tests
  tests_Common,
)
where
--- * imports
import Prelude ()
import "base-compat-batteries" Prelude.Compat hiding (readFile)
import "base-compat-batteries" Control.Monad.Compat
import Control.Monad.Except (ExceptT(..), throwError)
import Control.Monad.State.Strict
import Data.Bifunctor (bimap, second)
import Data.Char
import Data.Data
import Data.Decimal (DecimalRaw (Decimal), Decimal)
import Data.Default
import Data.Functor.Identity
import "base-compat-batteries" Data.List.Compat
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Semigroup as Sem
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import System.Time (getClockTime)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Custom

import Hledger.Data
import Hledger.Utils

-- $setup
-- >>> :set -XOverloadedStrings

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
    ,separator_         :: Char                 -- ^ the separator to use (when reading CSV)
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
definputopts = InputOpts def def ',' def def def def True def def

rawOptsToInputOpts :: RawOpts -> InputOpts
rawOptsToInputOpts rawopts = InputOpts{
   -- files_             = map (T.unpack . stripquotes . T.pack) $ listofstringopt "file" rawopts
   mformat_           = Nothing
  ,mrules_file_       = maybestringopt "rules-file" rawopts
  ,separator_         = fromMaybe ',' (maybecharopt "separator" rawopts)
  ,aliases_           = map (T.unpack . stripquotes . T.pack) $ listofstringopt "alias" rawopts
  ,anon_              = boolopt "anon" rawopts
  ,ignore_assertions_ = boolopt "ignore-assertions" rawopts
  ,new_               = boolopt "new" rawopts
  ,new_save_          = True
  ,pivot_             = stringopt "pivot" rawopts
  ,auto_              = boolopt "auto" rawopts                        
  }

--- * parsing utilities

-- | Run a text parser in the identity monad. See also: parseWithState.
runTextParser, rtp :: TextParser Identity a -> Text -> Either (ParseError Char CustomErr) a
runTextParser p t =  runParser p "" t
rtp = runTextParser

-- | Run a journal parser in some monad. See also: parseWithState.
runJournalParser, rjp :: Monad m => JournalParser m a -> Text -> m (Either (ParseError Char CustomErr) a)
runJournalParser p t = runParserT (evalStateT p mempty) "" t
rjp = runJournalParser

genericSourcePos :: SourcePos -> GenericSourcePos
genericSourcePos p = GenericSourcePos (sourceName p) (fromIntegral . unPos $ sourceLine p) (fromIntegral . unPos $ sourceColumn p)

-- | Construct a generic start & end line parse position from start and end megaparsec SourcePos's. 
journalSourcePos :: SourcePos -> SourcePos -> GenericSourcePos
journalSourcePos p p' = JournalSourcePos (sourceName p) (fromIntegral . unPos $ sourceLine p, fromIntegral $ line')
    where line'
            | (unPos $ sourceColumn p') == 1 = unPos (sourceLine p') - 1
            | otherwise = unPos $ sourceLine p' -- might be at end of file withat last new-line


-- | Apply any transaction modifier rules in the journal 
-- (adding automated postings to transactions, eg).
applyTransactionModifiers :: Journal -> Journal
applyTransactionModifiers j = j { jtxns = map applyallmodifiers $ jtxns j }
  where
    applyallmodifiers = 
      foldr (flip (.) . transactionModifierToFunction) id (jtxnmodifiers j)

-- | Given a megaparsec ParsedJournal parser, input options, file
-- path and file content: parse and post-process a Journal, or give an error.
parseAndFinaliseJournal :: JournalParser IO ParsedJournal -> InputOpts
                           -> FilePath -> Text -> ExceptT String IO Journal
parseAndFinaliseJournal parser iopts f txt = do
  t <- liftIO getClockTime
  y <- liftIO getCurrentYear
  ep <- liftIO $ runParserT (evalStateT parser nulljournal {jparsedefaultyear=Just y}) f txt
  case ep of
    Right pj -> 
      let pj' = if auto_ iopts then applyTransactionModifiers pj else pj in
      case journalFinalise t f txt (not $ ignore_assertions_ iopts) pj' of
                        Right j -> return j
                        Left e  -> throwError e
    Left e   -> throwError $ customParseErrorPretty txt e

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
codep = option "" $ do
  try $ do
    skipSome spacenonewline
    char '('
  code <- takeWhileP Nothing $ \c -> c /= ')' && c /= '\n'
  char ')' <?> "closing bracket ')' for transaction code"
  pure code

descriptionp :: TextParser m Text
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
  startPos <- getPosition
  d1 <- decimal <?> "year or month"
  sep <- satisfy isDateSepChar <?> "date separator"
  d2 <- decimal <?> "month or day"
  fullDate startPos d1 sep d2 <|> partialDate startPos mYear d1 sep d2
  <?> "full or partial date"

  where

  fullDate :: SourcePos -> Integer -> Char -> Int -> TextParser m Day
  fullDate startPos year sep1 month = do
    sep2 <- satisfy isDateSepChar <?> "date separator"
    day <- decimal <?> "day"
    endPos <- getPosition
    let dateStr = show year ++ [sep1] ++ show month ++ [sep2] ++ show day

    when (sep1 /= sep2) $ parseErrorAtRegion startPos endPos $
      "invalid date (mixing date separators is not allowed): " ++ dateStr

    case fromGregorianValid year month day of
      Nothing -> parseErrorAtRegion startPos endPos $
                   "well-formed but invalid date: " ++ dateStr
      Just date -> pure $! date

  partialDate
    :: SourcePos -> Maybe Year -> Integer -> Char -> Int -> TextParser m Day
  partialDate startPos mYear month sep day = do
    endPos <- getPosition
    case mYear of
      Just year ->
        case fromGregorianValid year (fromIntegral month) day of
          Nothing -> parseErrorAtRegion startPos endPos $
                      "well-formed but invalid date: " ++ dateStr
          Just date -> pure $! date
        where dateStr = show year ++ [sep] ++ show month ++ [sep] ++ show day

      Nothing -> parseErrorAtRegion startPos endPos $
        "partial date "++dateStr++" found, but the current year is unknown"
        where dateStr = show month ++ [sep] ++ show day

{-# INLINABLE datep' #-}

-- | Parse a date and time in YYYY/MM/DD HH:MM[:SS][+-ZZZZ] format.
-- Hyphen (-) and period (.) are also allowed as date separators.
-- The year may be omitted if a default year has been set.
-- Seconds are optional.
-- The timezone is optional and ignored (the time is always interpreted as a local time).
-- Leading zeroes may be omitted (except in a timezone).
datetimep :: JournalParser m LocalTime
datetimep = do
  mYear <- getYear
  lift $ datetimep' mYear

datetimep' :: Maybe Year -> TextParser m LocalTime
datetimep' mYear = do
  day <- datep' mYear
  skipSome spacenonewline
  time <- timeOfDay
  optional timeZone -- ignoring time zones
  pure $ LocalTime day time

  where
    timeOfDay :: TextParser m TimeOfDay
    timeOfDay = do
      pos1 <- getPosition
      h' <- twoDigitDecimal <?> "hour"
      pos2 <- getPosition
      unless (h' >= 0 && h' <= 23) $ parseErrorAtRegion pos1 pos2
        "invalid time (bad hour)"

      char ':' <?> "':' (hour-minute separator)"
      pos3 <- getPosition
      m' <- twoDigitDecimal <?> "minute"
      pos4 <- getPosition
      unless (m' >= 0 && m' <= 59) $ parseErrorAtRegion pos3 pos4
        "invalid time (bad minute)"

      s' <- option 0 $ do
        char ':' <?> "':' (minute-second separator)"
        pos5 <- getPosition
        s' <- twoDigitDecimal <?> "second"
        pos6 <- getPosition
        unless (s' >= 0 && s' <= 59) $ parseErrorAtRegion pos5 pos6
          "invalid time (bad second)" -- we do not support leap seconds
        pure s'

      pure $ TimeOfDay h' m' (fromIntegral s')

    twoDigitDecimal :: TextParser m Int
    twoDigitDecimal = do
      d1 <- digitToInt <$> digitChar
      d2 <- digitToInt <$> (digitChar <?> "a second digit")
      pure $ d1*10 + d2

    timeZone :: TextParser m String
    timeZone = do
      plusminus <- satisfy $ \c -> c == '-' || c == '+'
      fourDigits <- count 4 (digitChar <?> "a digit (for a time zone)")
      pure $ plusminus:fourDigits

secondarydatep :: Day -> TextParser m Day
secondarydatep primaryDate = char '=' *> datep' (Just primaryYear)
  where primaryYear = first3 $ toGregorian primaryDate

--- ** account names

-- | Parse an account name (plus one following space if present), 
-- then apply any parent account prefix and/or account aliases currently in effect,
-- in that order. (Ie first add the parent account prefix, then rewrite with aliases).
modifiedaccountnamep :: JournalParser m AccountName
modifiedaccountnamep = do
  parent <- getParentAccount
  aliases <- getAccountAliases
  a <- lift accountnamep
  return $!
    accountNameApplyAliases aliases $
     -- XXX accountNameApplyAliasesMemo ? doesn't seem to make a difference
    joinAccountNames parent
    a

-- | Parse an account name, plus one following space if present. 
-- Account names have one or more parts separated by the account separator character,
-- and are terminated by two or more spaces (or end of input). 
-- Each part is at least one character long, may have single spaces inside it,
-- and starts with a non-whitespace.
-- Note, this means "{account}", "%^!" and ";comment" are all accepted
-- (parent parsers usually prevent/consume the last).
-- It should have required parts to start with an alphanumeric;
-- for now it remains as-is for backwards compatibility.
accountnamep :: TextParser m AccountName
accountnamep = singlespacedtextp

-- | Parse any text beginning with a non-whitespace character, until a double space or the end of input.
-- Consumes one of the following spaces, if present.
singlespacedtextp :: TextParser m T.Text
singlespacedtextp = do
  firstPart <- part
  otherParts <- many $ try $ singlespacep *> part
  pure $! T.unwords $ firstPart : otherParts
  where
    part = takeWhile1P Nothing (not . isSpace)

-- | Parse one non-newline whitespace character that is not followed by another one.
singlespacep = void spacenonewline *> notFollowedBy spacenonewline

--- ** amounts

-- | Parse whitespace then an amount, with an optional left or right
-- currency symbol and optional price, or return the special
-- "missing" marker amount.
spaceandamountormissingp :: JournalParser m MixedAmount
spaceandamountormissingp =
  option missingmixedamt $ try $ do
    lift $ skipSome spacenonewline
    Mixed . (:[]) <$> amountp

-- | Parse a single-commodity amount, with optional symbol on the left or
-- right, optional unit or total price, and optional (ignored)
-- ledger-style balance assertion or fixed lot price declaration.
amountp :: JournalParser m Amount
amountp = label "amount" $ do
  amount <- amountwithoutpricep
  lift $ skipMany spacenonewline
  price <- priceamountp
  pure $ amount { aprice = price }

amountwithoutpricep :: JournalParser m Amount
amountwithoutpricep = do
  (mult, sign) <- lift $ (,) <$> multiplierp <*> signp
  leftsymbolamountp mult sign <|> rightornosymbolamountp mult sign

  where

  leftsymbolamountp :: Bool -> (Decimal -> Decimal) -> JournalParser m Amount
  leftsymbolamountp mult sign = label "amount" $ do
    c <- lift commoditysymbolp
    suggestedStyle <- getAmountStyle c
    commodityspaced <- lift $ skipMany' spacenonewline
    sign2 <- lift $ signp
    posBeforeNum <- getPosition
    ambiguousRawNum <- lift rawnumberp
    mExponent <- lift $ optional $ try exponentp
    posAfterNum <- getPosition
    let numRegion = (posBeforeNum, posAfterNum)
    (q,prec,mdec,mgrps) <- lift $ interpretNumber numRegion suggestedStyle ambiguousRawNum mExponent
    let s = amountstyle{ascommodityside=L, ascommodityspaced=commodityspaced, asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps}
    return $ Amount c (sign (sign2 q)) NoPrice s mult

  rightornosymbolamountp :: Bool -> (Decimal -> Decimal) -> JournalParser m Amount
  rightornosymbolamountp mult sign = label "amount" $ do
    posBeforeNum <- getPosition
    ambiguousRawNum <- lift rawnumberp
    mExponent <- lift $ optional $ try exponentp
    posAfterNum <- getPosition
    let numRegion = (posBeforeNum, posAfterNum)
    mSpaceAndCommodity <- lift $ optional $ try $ (,) <$> skipMany' spacenonewline <*> commoditysymbolp
    case mSpaceAndCommodity of
      -- right symbol amount
      Just (commodityspaced, c) -> do
        suggestedStyle <- getAmountStyle c
        (q,prec,mdec,mgrps) <- lift $ interpretNumber numRegion suggestedStyle ambiguousRawNum mExponent
        let s = amountstyle{ascommodityside=R, ascommodityspaced=commodityspaced, asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps}
        return $ Amount c (sign q) NoPrice s mult
      -- no symbol amount
      Nothing -> do
        suggestedStyle <- getDefaultAmountStyle
        (q,prec,mdec,mgrps) <- lift $ interpretNumber numRegion suggestedStyle ambiguousRawNum mExponent
        -- if a default commodity has been set, apply it and its style to this amount
        -- (unless it's a multiplier in an automated posting)
        defcs <- getDefaultCommodityAndStyle
        let (c,s) = case (mult, defcs) of
              (False, Just (defc,defs)) -> (defc, defs{asprecision=max (asprecision defs) prec})
              _ -> ("", amountstyle{asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps})
        return $ Amount c (sign q) NoPrice s mult

  -- For reducing code duplication. Doesn't parse anything. Has the type
  -- of a parser only in order to throw parse errors (for convenience).
  interpretNumber
    :: (SourcePos, SourcePos)
    -> Maybe AmountStyle
    -> Either AmbiguousNumber RawNumber
    -> Maybe Int
    -> TextParser m (Quantity, Int, Maybe Char, Maybe DigitGroupStyle)
  interpretNumber posRegion suggestedStyle ambiguousNum mExp =
    let rawNum = either (disambiguateNumber suggestedStyle) id ambiguousNum
    in  case fromRawNumber rawNum mExp of
          Left errMsg -> uncurry parseErrorAtRegion posRegion errMsg
          Right res -> pure res

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

commoditysymbolp :: TextParser m CommoditySymbol
commoditysymbolp =
  quotedcommoditysymbolp <|> simplecommoditysymbolp <?> "commodity symbol"

quotedcommoditysymbolp :: TextParser m CommoditySymbol
quotedcommoditysymbolp =
  between (char '"') (char '"') $ takeWhile1P Nothing f
  where f c = c /= ';' && c /= '\n' && c /= '\"'

simplecommoditysymbolp :: TextParser m CommoditySymbol
simplecommoditysymbolp = takeWhile1P Nothing (not . isNonsimpleCommodityChar)

priceamountp :: JournalParser m Price
priceamountp = option NoPrice $ do
  char '@'
  priceConstructor <- char '@' *> pure TotalPrice <|> pure UnitPrice

  lift (skipMany spacenonewline)
  priceAmount <- amountwithoutpricep <?> "amount (as a price)"

  pure $ priceConstructor priceAmount

partialbalanceassertionp :: JournalParser m BalanceAssertion
partialbalanceassertionp = optional $ do
  sourcepos <- try $ do
    lift (skipMany spacenonewline)
    sourcepos <- genericSourcePos <$> lift getPosition
    char '='
    pure sourcepos
  lift (skipMany spacenonewline)
  a <- amountp <?> "amount (for a balance assertion or assignment)" -- XXX should restrict to a simple amount
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
fixedlotpricep :: JournalParser m (Maybe Amount)
fixedlotpricep = optional $ do
  try $ do
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
numberp suggestedStyle = label "number" $ do
    -- a number is an optional sign followed by a sequence of digits possibly
    -- interspersed with periods, commas, or both
    -- dbgparse 0 "numberp"
    sign <- signp
    rawNum <- either (disambiguateNumber suggestedStyle) id <$> rawnumberp
    mExp <- optional $ try $ exponentp
    dbg8 "numberp suggestedStyle" suggestedStyle `seq` return ()
    case dbg8 "numberp quantity,precision,mdecimalpoint,mgrps"
           $ fromRawNumber rawNum mExp of
      Left errMsg -> fail errMsg
      Right (q, p, d, g) -> pure (sign q, p, d, g)

exponentp :: TextParser m Int
exponentp = char' 'e' *> signp <*> decimal <?> "exponent"

-- | Interpret a raw number as a decimal number.
--
-- Returns:
-- - the decimal number
-- - the precision (number of digits after the decimal point)  
-- - the decimal point character, if any
-- - the digit group style, if any (digit group character and sizes of digit groups)
fromRawNumber
  :: RawNumber
  -> Maybe Int
  -> Either String
            (Quantity, Int, Maybe Char, Maybe DigitGroupStyle)
fromRawNumber raw mExp = case raw of

  NoSeparators digitGrp mDecimals ->
    let mDecPt = fmap fst mDecimals
        decimalGrp = maybe mempty snd mDecimals

        (quantity, precision) =
          maybe id applyExp mExp $ toQuantity digitGrp decimalGrp

    in  Right (quantity, precision, mDecPt, Nothing)

  WithSeparators digitSep digitGrps mDecimals -> case mExp of
    Nothing -> 
      let mDecPt = fmap fst mDecimals
          decimalGrp = maybe mempty snd mDecimals
          digitGroupStyle = DigitGroups digitSep (groupSizes digitGrps)

          (quantity, precision) = toQuantity (mconcat digitGrps) decimalGrp

      in  Right (quantity, precision, mDecPt, Just digitGroupStyle)
    Just _ -> Left
      "invalid number: mixing digit separators with exponents is not allowed"

  where
    -- Outputs digit group sizes from least significant to most significant
    groupSizes :: [DigitGrp] -> [Int]
    groupSizes digitGrps = reverse $ case map digitGroupLength digitGrps of
      (a:b:cs) | a < b -> b:cs
      gs               -> gs

    toQuantity :: DigitGrp -> DigitGrp -> (Quantity, Int)
    toQuantity preDecimalGrp postDecimalGrp = (quantity, precision)
      where
        quantity = Decimal (fromIntegral precision)
                           (digitGroupNumber $ preDecimalGrp <> postDecimalGrp)
        precision = digitGroupLength postDecimalGrp

    applyExp :: Int -> (Decimal, Int) -> (Decimal, Int)
    applyExp exponent (quantity, precision) =
      (quantity * 10^^exponent, max 0 (precision - exponent))


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
rawnumberp = label "number" $ do
  rawNumber <- fmap Right leadingDecimalPt <|> leadingDigits

  -- Guard against mistyped numbers
  mExtraDecimalSep <- optional $ lookAhead $ satisfy isDecimalPointChar
  when (isJust mExtraDecimalSep) $
    fail "invalid number (invalid use of separator)"

  mExtraFragment <- optional $ lookAhead $ try $
    char ' ' *> getPosition <* digitChar
  case mExtraFragment of
    Just pos -> parseErrorAt pos "invalid number (excessive trailing digits)"
    Nothing -> pure ()

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
  digitGroupLength :: !Int,
  digitGroupNumber :: !Integer
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
digitgroupp = label "digits"
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

--- ** comments

multilinecommentp :: TextParser m ()
multilinecommentp = startComment *> anyLine `skipManyTill` endComment
  where
    startComment = string "comment" *> trailingSpaces
    endComment = eof <|> string "end comment" *> trailingSpaces

    trailingSpaces = skipMany spacenonewline <* newline
    anyLine = void $ takeWhileP Nothing (\c -> c /= '\n') *> newline

{-# INLINABLE multilinecommentp #-}

emptyorcommentlinep :: TextParser m ()
emptyorcommentlinep = do
  skipMany spacenonewline
  skiplinecommentp <|> void newline
  where
    -- A line (file-level) comment can start with a semicolon, hash, or star
    -- (allowing org nodes).
    skiplinecommentp :: TextParser m ()
    skiplinecommentp = do
      satisfy $ \c -> c == ';' || c == '#' || c == '*'
      void $ takeWhileP Nothing (\c -> c /= '\n')
      optional newline
      pure ()

{-# INLINABLE emptyorcommentlinep #-}

-- A parser combinator for parsing (possibly multiline) comments
-- following journal items.
--
-- Several journal items may be followed by comments, which begin with
-- semicolons and extend to the end of the line. Such comments may span
-- multiple lines, but comment lines below the journal item must be
-- preceeded by leading whitespace.
--
-- This parser combinator accepts a parser that consumes all input up
-- until the next newline. This parser should extract the "content" from
-- comments. The resulting parser returns this content plus the raw text
-- of the comment itself.
--
-- See followingcommentp for tests.
--
followingcommentp' :: (Monoid a, Show a) => TextParser m a -> TextParser m (Text, a)
followingcommentp' contentp = do
  skipMany spacenonewline
  -- there can be 0 or 1 sameLine
  sameLine <- try headerp *> ((:[]) <$> match' contentp) <|> pure []
  _ <- eolof
  -- there can be 0 or more nextLines
  nextLines <- many $
    try (skipSome spacenonewline *> headerp) *> match' contentp <* eolof
  let
    -- if there's just a next-line comment, insert an empty same-line comment
    -- so the next-line comment doesn't get rendered as a same-line comment.
    sameLine' | null sameLine && not (null nextLines) = [("",mempty)]
              | otherwise = sameLine 
    (texts, contents) = unzip $ sameLine' ++ nextLines
    strippedCommentText = T.unlines $ map T.strip texts
    commentContent = mconcat contents
  pure (strippedCommentText, commentContent)

  where
    headerp = char ';' *> skipMany spacenonewline

{-# INLINABLE followingcommentp' #-}

-- | Parse the text of a (possibly multiline) comment following a journal item.
--
-- >>> rtp followingcommentp ""   -- no comment
-- Right ""
-- >>> rtp followingcommentp ";"    -- just a (empty) same-line comment. newline is added
-- Right "\n"
-- >>> rtp followingcommentp ";  \n"
-- Right "\n"
-- >>> rtp followingcommentp ";\n ;\n"  -- a same-line and a next-line comment
-- Right "\n\n"
-- >>> rtp followingcommentp "\n ;\n"  -- just a next-line comment. Insert an empty same-line comment so the next-line comment doesn't become a same-line comment.
-- Right "\n\n"
--
followingcommentp :: TextParser m Text
followingcommentp =
  fst <$> followingcommentp' (void $ takeWhileP Nothing (/= '\n'))
{-# INLINABLE followingcommentp #-}


-- | Parse a transaction comment and extract its tags.
--
-- The first line of a transaction may be followed by comments, which
-- begin with semicolons and extend to the end of the line. Transaction
-- comments may span multiple lines, but comment lines below the
-- transaction must be preceeded by leading whitespace.
--
-- 2000/1/1 ; a transaction comment starting on the same line ...
--   ; extending to the next line
--   account1  $1
--   account2
--
-- Tags are name-value pairs.
--
-- >>> let getTags (_,tags) = tags
-- >>> let parseTags = fmap getTags . rtp transactioncommentp
--
-- >>> parseTags "; name1: val1, name2:all this is value2"
-- Right [("name1","val1"),("name2","all this is value2")]
--
-- A tag's name must be immediately followed by a colon, without
-- separating whitespace. The corresponding value consists of all the text
-- following the colon up until the next colon or newline, stripped of
-- leading and trailing whitespace.
--
transactioncommentp :: TextParser m (Text, [Tag])
transactioncommentp = followingcommentp' commenttagsp
{-# INLINABLE transactioncommentp #-}

commenttagsp :: TextParser m [Tag]
commenttagsp = do
  tagName <- fmap (last . T.split isSpace)
            $ takeWhileP Nothing (\c -> c /= ':' && c /= '\n')
  atColon tagName <|> pure [] -- if not ':', then either '\n' or EOF

  where
    atColon :: Text -> TextParser m [Tag]
    atColon name = char ':' *> do
      if T.null name
        then commenttagsp
        else do
          skipMany spacenonewline
          val <- tagValue
          let tag = (name, val)
          (tag:) <$> commenttagsp

    tagValue :: TextParser m Text
    tagValue = do
      val <- T.strip <$> takeWhileP Nothing (\c -> c /= ',' && c /= '\n')
      _ <- optional $ char ','
      pure val

{-# INLINABLE commenttagsp #-}


-- | Parse a posting comment and extract its tags and dates.
--
-- Postings may be followed by comments, which begin with semicolons and
-- extend to the end of the line. Posting comments may span multiple
-- lines, but comment lines below the posting must be preceeded by
-- leading whitespace.
--
-- 2000/1/1
--   account1  $1 ; a posting comment starting on the same line ...
--   ; extending to the next line
--
--   account2
--   ; a posting comment beginning on the next line
--
-- Tags are name-value pairs.
--
-- >>> let getTags (_,tags,_,_) = tags
-- >>> let parseTags = fmap getTags . rtp (postingcommentp Nothing)
--
-- >>> parseTags "; name1: val1, name2:all this is value2"
-- Right [("name1","val1"),("name2","all this is value2")]
--
-- A tag's name must be immediately followed by a colon, without
-- separating whitespace. The corresponding value consists of all the text
-- following the colon up until the next colon or newline, stripped of
-- leading and trailing whitespace.
--
-- Posting dates may be expressed with "date"/"date2" tags or with
-- bracketed date syntax. Posting dates will inherit their year from the
-- transaction date if the year is not specified. We throw parse errors on
-- invalid dates.
--
-- >>> let getDates (_,_,d1,d2) = (d1, d2)
-- >>> let parseDates = fmap getDates . rtp (postingcommentp (Just 2000))
--
-- >>> parseDates "; date: 1/2, date2: 1999/12/31"
-- Right (Just 2000-01-02,Just 1999-12-31)
-- >>> parseDates "; [1/2=1999/12/31]"
-- Right (Just 2000-01-02,Just 1999-12-31)
--
-- Example: tags, date tags, and bracketed dates
-- >>> rtp (postingcommentp (Just 2000)) "; a:b, date:3/4, [=5/6]"
-- Right ("a:b, date:3/4, [=5/6]\n",[("a","b"),("date","3/4")],Just 2000-03-04,Just 2000-05-06)
--
-- Example: extraction of dates from date tags ignores trailing text
-- >>> rtp (postingcommentp (Just 2000)) "; date:3/4=5/6"
-- Right ("date:3/4=5/6\n",[("date","3/4=5/6")],Just 2000-03-04,Nothing)
--
postingcommentp
  :: Maybe Year -> TextParser m (Text, [Tag], Maybe Day, Maybe Day)
postingcommentp mYear = do
  (commentText, (tags, dateTags)) <-
    followingcommentp' (commenttagsanddatesp mYear)
  let mdate  = fmap snd $ find ((=="date") .fst) dateTags
      mdate2 = fmap snd $ find ((=="date2").fst) dateTags
  pure (commentText, tags, mdate, mdate2)
{-# INLINABLE postingcommentp #-}


commenttagsanddatesp
  :: Maybe Year -> TextParser m ([Tag], [DateTag])
commenttagsanddatesp mYear = do
  (txt, dateTags) <- match $ readUpTo ':'
  -- next char is either ':' or '\n' (or EOF)
  let tagName = last (T.split isSpace txt)
  (fmap.second) (dateTags++) (atColon tagName) <|> pure ([], dateTags) -- if not ':', then either '\n' or EOF

  where
    readUpTo :: Char -> TextParser m [DateTag]
    readUpTo end = do
      void $ takeWhileP Nothing (\c -> c /= end && c /= '\n' && c /= '[')
      -- if not '[' then ':' or '\n' or EOF
      atBracket (readUpTo end) <|> pure []

    atBracket :: TextParser m [DateTag] -> TextParser m [DateTag]
    atBracket cont = do
      -- Uses the fact that bracketed date-tags cannot contain newlines
      dateTags <- option [] $ lookAhead (bracketeddatetagsp mYear)
      _ <- char '['
      dateTags' <- cont
      pure $ dateTags ++ dateTags'

    atColon :: Text -> TextParser m ([Tag], [DateTag])
    atColon name = char ':' *> do
      skipMany spacenonewline
      (tags, dateTags) <- case name of
        ""      -> pure ([], [])
        "date"  -> dateValue name
        "date2" -> dateValue name
        _       -> tagValue name
      _ <- optional $ char ','
      bimap (tags++) (dateTags++) <$> commenttagsanddatesp mYear

    dateValue :: Text -> TextParser m ([Tag], [DateTag])
    dateValue name = do
      (txt, (date, dateTags)) <- match' $ do
        date <- datep' mYear
        dateTags <- readUpTo ','
        pure (date, dateTags)
      let val = T.strip txt
      pure $ ( [(name, val)]
             , (name, date) : dateTags )

    tagValue :: Text -> TextParser m ([Tag], [DateTag])
    tagValue name = do
      (txt, dateTags) <- match' $ readUpTo ','
      let val = T.strip txt
      pure $ ( [(name, val)]
             , dateTags )

{-# INLINABLE commenttagsanddatesp #-}


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
-- >>> either (Left . parseErrorPretty) Right $ rtp (bracketeddatetagsp Nothing) "[2016/1/2=3/4]"
-- Right [("date",2016-01-02),("date2",2016-03-04)]
--
-- >>> either (Left . parseErrorPretty) Right $ rtp (bracketeddatetagsp Nothing) "[1]"
-- Left ...not a bracketed date...
--
-- >>> either (Left . parseErrorPretty) Right $ rtp (bracketeddatetagsp Nothing) "[2016/1/32]"
-- Left ...1:11:...well-formed but invalid date: 2016/1/32...
--
-- >>> either (Left . parseErrorPretty) Right $ rtp (bracketeddatetagsp Nothing) "[1/31]"
-- Left ...1:6:...partial date 1/31 found, but the current year is unknown...
--
-- >>> either (Left . parseErrorPretty) Right $ rtp (bracketeddatetagsp Nothing) "[0123456789/-.=/-.=]"
-- Left ...1:13:...expecting month or day...
--
bracketeddatetagsp
  :: Maybe Year -> TextParser m [(TagName, Day)]
bracketeddatetagsp mYear1 = do
  -- dbgparse 0 "bracketeddatetagsp"
  try $ do
    s <- lookAhead
       $ between (char '[') (char ']')
       $ takeWhile1P Nothing isBracketedDateChar
    unless (T.any isDigit s && T.any isDateSepChar s) $
      fail "not a bracketed date"
  -- Looks sufficiently like a bracketed date to commit to parsing a date

  between (char '[') (char ']') $ do
    md1 <- optional $ datep' mYear1

    let mYear2 = fmap readYear md1 <|> mYear1
    md2 <- optional $ char '=' *> datep' mYear2

    pure $ catMaybes [("date",) <$> md1, ("date2",) <$> md2]

  where
    readYear = first3 . toGregorian
    isBracketedDateChar c = isDigit c || isDateSepChar c || c == '='

{-# INLINABLE bracketeddatetagsp #-}


--- ** helper parsers

-- A version of `match` that is strict in the returned text
match' :: TextParser m a -> TextParser m (Text, a)
match' p = do
  (!txt, p) <- match p
  pure (txt, p)

--- * tests

tests_Common = tests "Common" [

  tests "amountp" [
    test "basic"                  $ expectParseEq amountp "$47.18"     (usd 47.18)
   ,test "ends with decimal mark" $ expectParseEq amountp "$1."        (usd 1  `withPrecision` 0)
   ,test "unit price"             $ expectParseEq amountp "$10 @ €0.5" 
      -- not precise enough:
      -- (usd 10 `withPrecision` 0 `at` (eur 0.5 `withPrecision` 1)) -- `withStyle` asdecimalpoint=Just '.'
      amount{
         acommodity="$"
        ,aquantity=10 -- need to test internal precision with roundTo ? I think not 
        ,astyle=amountstyle{asprecision=0, asdecimalpoint=Nothing}
        ,aprice=UnitPrice $
          amount{
             acommodity="€"
            ,aquantity=0.5
            ,astyle=amountstyle{asprecision=1, asdecimalpoint=Just '.'}
            } 
        } 
   ,test "total price"            $ expectParseEq amountp "$10 @@ €5"
      amount{
         acommodity="$"
        ,aquantity=10 
        ,astyle=amountstyle{asprecision=0, asdecimalpoint=Nothing}
        ,aprice=TotalPrice $
          amount{
             acommodity="€"
            ,aquantity=5
            ,astyle=amountstyle{asprecision=0, asdecimalpoint=Nothing}
            } 
        } 
    ]

  ,let p = lift (numberp Nothing) :: JournalParser IO (Quantity, Int, Maybe Char, Maybe DigitGroupStyle) in
   tests "numberp" [
     test "." $ expectParseEq p "0"          (0, 0, Nothing, Nothing)
    ,test "." $ expectParseEq p "1"          (1, 0, Nothing, Nothing)
    ,test "." $ expectParseEq p "1.1"        (1.1, 1, Just '.', Nothing)
    ,test "." $ expectParseEq p "1,000.1"    (1000.1, 1, Just '.', Just $ DigitGroups ',' [3])
    ,test "." $ expectParseEq p "1.00.000,1" (100000.1, 1, Just ',', Just $ DigitGroups '.' [3,2])
    ,test "." $ expectParseEq p "1,000,000"  (1000000, 0, Nothing, Just $ DigitGroups ',' [3,3])  -- could be simplified to [3]
    ,test "." $ expectParseEq p "1."         (1, 0, Just '.', Nothing)
    ,test "." $ expectParseEq p "1,"         (1, 0, Just ',', Nothing)
    ,test "." $ expectParseEq p ".1"         (0.1, 1, Just '.', Nothing)
    ,test "." $ expectParseEq p ",1"         (0.1, 1, Just ',', Nothing)
    ,test "." $ expectParseError p "" ""
    ,test "." $ expectParseError p "1,000.000,1" ""
    ,test "." $ expectParseError p "1.000,000.1" ""
    ,test "." $ expectParseError p "1,000.000.1" ""
    ,test "." $ expectParseError p "1,,1" ""
    ,test "." $ expectParseError p "1..1" ""
    ,test "." $ expectParseError p ".1," ""
    ,test "." $ expectParseError p ",1." ""
    ]
  
  ,tests "spaceandamountormissingp" [
     test "space and amount" $ expectParseEq spaceandamountormissingp " $47.18" (Mixed [usd 47.18])
    ,test "empty string" $ expectParseEq spaceandamountormissingp "" missingmixedamt
    ,_test "just space" $ expectParseEq spaceandamountormissingp " " missingmixedamt  -- XXX should it ?
    -- ,test "just amount" $ expectParseError spaceandamountormissingp "$47.18" ""  -- succeeds, consuming nothing
    ]

  ]
