--- * -*- outline-regexp:"--- \\*"; -*-
--- ** doc
-- In Emacs, use TAB on lines beginning with "-- *" to collapse/expand sections.
{-|

File reading/parsing utilities used by multiple readers, and a good
amount of the parsers for journal format, to avoid import cycles
when JournalReader imports other readers.

Some of these might belong in Hledger.Read.JournalReader or Hledger.Read.

-}

--- ** language
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoMonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

--- ** exports
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
  parseAndFinaliseJournal,
  parseAndFinaliseJournal',
  journalFinalise,
  journalCheckAccountsDeclared,
  journalCheckCommoditiesDeclared,
  journalCheckPayeesDeclared,
  setYear,
  getYear,
  setDefaultCommodityAndStyle,
  getDefaultCommodityAndStyle,
  getDefaultAmountStyle,
  getAmountStyle,
  addDeclaredAccountType,
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

  -- ** account aliases
  accountaliasp,

  -- ** amounts
  spaceandamountormissingp,
  amountp,
  amountp',
  mamountp',
  commoditysymbolp,
  priceamountp,
  balanceassertionp,
  lotpricep,
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
  singlespacedtextsatisfyingp,
  singlespacep,
  skipNonNewlineSpaces,
  skipNonNewlineSpaces1,
  aliasesFromOpts,

  -- * tests
  tests_Common,
)
where

--- ** imports
import Prelude ()
import "base-compat-batteries" Prelude.Compat hiding (fail, readFile)
import Control.Applicative.Permutations (runPermutation, toPermutationWithDefault)
import qualified "base-compat-batteries" Control.Monad.Fail.Compat as Fail (fail)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.State.Strict hiding (fail)
import Data.Bifunctor (bimap, second)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Decimal (DecimalRaw (Decimal), Decimal)
import Data.Default (Default(..))
import Data.Function ((&))
import Data.Functor.Identity (Identity)
import "base-compat-batteries" Data.List.Compat
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import qualified Data.Map as M
import qualified Data.Semigroup as Sem
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorianValid, toGregorian)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Word (Word8)
import System.Time (getClockTime)
import Text.Megaparsec
import Text.Megaparsec.Char (char, char', digitChar, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Custom
  (FinalParseError, attachSource, customErrorBundlePretty,
  finalErrorBundlePretty, parseErrorAt, parseErrorAtRegion)

import Hledger.Data
import Hledger.Utils
import Text.Printf (printf)

--- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

--- ** types

-- main types; a few more below

-- | A hledger journal reader is a triple of storage format name, a
-- detector of that format, and a parser from that format to Journal.
-- The type variable m appears here so that rParserr can hold a
-- journal parser, which depends on it.
data Reader m = Reader {

     -- The canonical name of the format handled by this reader
     rFormat   :: StorageFormat

     -- The file extensions recognised as containing this format
    ,rExtensions :: [String]

     -- The entry point for reading this format, accepting input options, file
     -- path for error messages and file contents, producing an exception-raising IO
     -- action that produces a journal or error message.
    ,rReadFn   :: InputOpts -> FilePath -> Text -> ExceptT String IO Journal

     -- The actual megaparsec parser called by the above, in case
     -- another parser (includedirectivep) wants to use it directly.
    ,rParser :: MonadIO m => ErroringJournalParser m ParsedJournal
    }

instance Show (Reader m) where show r = rFormat r ++ " reader"

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
    ,new_               :: Bool                 -- ^ read only new transactions since this file was last read
    ,new_save_          :: Bool                 -- ^ save latest new transactions state for next time
    ,pivot_             :: String               -- ^ use the given field's value as the account name
    ,auto_              :: Bool                 -- ^ generate automatic postings when journal is parsed
    ,balancingopts_     :: BalancingOpts        -- ^ options for balancing transactions
    ,strict_            :: Bool                 -- ^ do extra error checking (eg, all posted accounts are declared, no prices are inferred)
 } deriving (Show)

instance Default InputOpts where def = definputopts

definputopts :: InputOpts
definputopts = InputOpts
    { mformat_           = Nothing
    , mrules_file_       = Nothing
    , aliases_           = []
    , anon_              = False
    , new_               = False
    , new_save_          = True
    , pivot_             = ""
    , auto_              = False
    , balancingopts_     = def
    , strict_            = False
    }

rawOptsToInputOpts :: RawOpts -> InputOpts
rawOptsToInputOpts rawopts = InputOpts{
     -- files_             = listofstringopt "file" rawopts
     mformat_           = Nothing
    ,mrules_file_       = maybestringopt "rules-file" rawopts
    ,aliases_           = listofstringopt "alias" rawopts
    ,anon_              = boolopt "anon" rawopts
    ,new_               = boolopt "new" rawopts
    ,new_save_          = True
    ,pivot_             = stringopt "pivot" rawopts
    ,auto_              = boolopt "auto" rawopts
    ,balancingopts_     = def{ ignore_assertions_ = boolopt "ignore-assertions" rawopts
                             , infer_prices_      = not noinferprice
                             }
    ,strict_            = boolopt "strict" rawopts
    }
  where noinferprice = boolopt "strict" rawopts || stringopt "args" rawopts == "balancednoautoconversion"

--- ** parsing utilities

-- | Run a text parser in the identity monad. See also: parseWithState.
runTextParser, rtp
  :: TextParser Identity a -> Text -> Either (ParseErrorBundle Text CustomErr) a
runTextParser p t =  runParser p "" t
rtp = runTextParser

-- | Run a journal parser in some monad. See also: parseWithState.
runJournalParser, rjp
  :: Monad m
  => JournalParser m a -> Text -> m (Either (ParseErrorBundle Text CustomErr) a)
runJournalParser p t = runParserT (evalStateT p nulljournal) "" t
rjp = runJournalParser

-- | Run an erroring journal parser in some monad. See also: parseWithState.
runErroringJournalParser, rejp
  :: Monad m
  => ErroringJournalParser m a
  -> Text
  -> m (Either FinalParseError (Either (ParseErrorBundle Text CustomErr) a))
runErroringJournalParser p t =
  runExceptT $ runParserT (evalStateT p nulljournal) "" t
rejp = runErroringJournalParser

genericSourcePos :: SourcePos -> GenericSourcePos
genericSourcePos p = GenericSourcePos (sourceName p) (unPos $ sourceLine p) (unPos $ sourceColumn p)

-- | Construct a generic start & end line parse position from start and end megaparsec SourcePos's.
journalSourcePos :: SourcePos -> SourcePos -> GenericSourcePos
journalSourcePos p p' = JournalSourcePos (sourceName p) (unPos $ sourceLine p, line')
    where line' | (unPos $ sourceColumn p') == 1 = unPos (sourceLine p') - 1
                | otherwise = unPos $ sourceLine p' -- might be at end of file withat last new-line

-- | Given a parser to ParsedJournal, input options, file path and
-- content: run the parser on the content, and finalise the result to
-- get a Journal; or throw an error.
parseAndFinaliseJournal :: ErroringJournalParser IO ParsedJournal -> InputOpts
                           -> FilePath -> Text -> ExceptT String IO Journal
parseAndFinaliseJournal parser iopts f txt = do
  y <- liftIO getCurrentYear
  let initJournal = nulljournal{ jparsedefaultyear = Just y, jincludefilestack = [f] }
  eep <- liftIO $ runExceptT $ runParserT (evalStateT parser initJournal) f txt
  -- TODO: urgh.. clean this up somehow
  case eep of
    Left finalParseError -> throwError $ finalErrorBundlePretty $ attachSource f txt finalParseError
    Right ep -> case ep of
                  Left e   -> throwError $ customErrorBundlePretty e
                  Right pj -> journalFinalise iopts f txt pj

-- | Like parseAndFinaliseJournal but takes a (non-Erroring) JournalParser.
-- Also, applies command-line account aliases before finalising.
-- Used for timeclock/timedot.
-- TODO: get rid of this, use parseAndFinaliseJournal instead
parseAndFinaliseJournal' :: JournalParser IO ParsedJournal -> InputOpts
                           -> FilePath -> Text -> ExceptT String IO Journal
parseAndFinaliseJournal' parser iopts f txt = do
  y <- liftIO getCurrentYear
  let initJournal = nulljournal
        { jparsedefaultyear = Just y
        , jincludefilestack = [f] }
  ep <- liftIO $ runParserT (evalStateT parser initJournal) f txt
  -- see notes above
  case ep of
    Left e   -> throwError $ customErrorBundlePretty e
    Right pj -> 
      -- apply any command line account aliases. Can fail with a bad replacement pattern.
      case journalApplyAliases (aliasesFromOpts iopts) pj of
        Left e    -> throwError e
        Right pj' -> journalFinalise iopts f txt pj'

-- | Post-process a Journal that has just been parsed or generated, in this order:
--
-- - apply canonical amount styles,
--
-- - save misc info and reverse transactions into their original parse order,
--
-- - evaluate balance assignments and balance each transaction,
--
-- - apply transaction modifiers (auto postings) if enabled,
--
-- - check balance assertions if enabled.
--
-- - infer transaction-implied market prices from transaction prices
--
journalFinalise :: InputOpts -> FilePath -> Text -> ParsedJournal -> ExceptT String IO Journal
journalFinalise InputOpts{auto_,balancingopts_,strict_} f txt pj = do
  t <- liftIO getClockTime
  d <- liftIO getCurrentDay
  let pj' =
        pj{jglobalcommoditystyles=fromMaybe M.empty $ commodity_styles_ balancingopts_}  -- save any global commodity styles
        & journalAddFile (f, txt)  -- save the main file's info
        & journalSetLastReadTime t -- save the last read time
        & journalReverse -- convert all lists to the order they were parsed

  -- If in strict mode, check all postings are to declared accounts
  case if strict_ then journalCheckAccountsDeclared pj' else Right () of
    Left e   -> throwError e
    Right () ->

      -- and using declared commodities
      case if strict_ then journalCheckCommoditiesDeclared pj' else Right () of
        Left e   -> throwError e
        Right () ->

          -- Infer and apply canonical styles for each commodity (or throw an error).
          -- This affects transaction balancing/assertions/assignments, so needs to be done early.
          case journalApplyCommodityStyles pj' of
            Left e     -> throwError e
            Right pj'' -> either throwError return $
              pj''
              & (if not auto_ || null (jtxnmodifiers pj'')
                then
                  -- Auto postings are not active.
                  -- Balance all transactions and maybe check balance assertions.
                  journalBalanceTransactions balancingopts_
                else \j -> do  -- Either monad
                  -- Auto postings are active.
                  -- Balance all transactions without checking balance assertions,
                  j' <- journalBalanceTransactions balancingopts_{ignore_assertions_=True} j
                  -- then add the auto postings
                  -- (Note adding auto postings after balancing means #893b fails;
                  -- adding them before balancing probably means #893a, #928, #938 fail.)
                  case journalModifyTransactions d j' of
                    Left e -> throwError e
                    Right j'' -> do
                      -- then apply commodity styles once more, to style the auto posting amounts. (XXX inefficient ?)
                      j''' <- journalApplyCommodityStyles j''
                      -- then check balance assertions.
                      journalBalanceTransactions balancingopts_ j'''
                )
            & fmap journalInferMarketPricesFromTransactions  -- infer market prices from commodity-exchanging transactions

-- | Check that all the journal's transactions have payees declared with
-- payee directives, returning an error message otherwise.
journalCheckPayeesDeclared :: Journal -> Either String ()
journalCheckPayeesDeclared j = sequence_ $ map checkpayee $ jtxns j
  where
    checkpayee t
      | p `elem` ps = Right ()
      | otherwise = Left $
          printf "undeclared payee \"%s\"\nat: %s\n\n%s"
            (T.unpack p)
            (showGenericSourcePos $ tsourcepos t)
            (linesPrepend2 "> " "  " . (<>"\n") . textChomp $ showTransaction t)
      where
        p  = transactionPayee t
        ps = journalPayeesDeclared j

-- | Check that all the journal's postings are to accounts declared with
-- account directives, returning an error message otherwise.
journalCheckAccountsDeclared :: Journal -> Either String ()
journalCheckAccountsDeclared j = sequence_ $ map checkacct $ journalPostings j
  where
    checkacct Posting{paccount,ptransaction}
      | paccount `elem` as = Right ()
      | otherwise = Left $
          (printf "undeclared account \"%s\"\n" (T.unpack paccount))
          ++ case ptransaction of
              Nothing -> ""
              Just t  -> printf "in transaction at: %s\n\n%s"
                          (showGenericSourcePos $ tsourcepos t)
                          (linesPrepend "  " . (<>"\n") . textChomp $ showTransaction t)
      where
        as = journalAccountNamesDeclared j

-- | Check that all the commodities used in this journal's postings have been declared
-- by commodity directives, returning an error message otherwise.
journalCheckCommoditiesDeclared :: Journal -> Either String ()
journalCheckCommoditiesDeclared j =
  sequence_ $ map checkcommodities $ journalPostings j
  where
    checkcommodities Posting{..} =
      case mfirstundeclaredcomm of
        Nothing -> Right ()
        Just c  -> Left $
          (printf "undeclared commodity \"%s\"\n" (T.unpack c))
          ++ case ptransaction of
              Nothing -> ""
              Just t  -> printf "in transaction at: %s\n\n%s"
                          (showGenericSourcePos $ tsourcepos t)
                          (linesPrepend "  " . (<>"\n") . textChomp $ showTransaction t)
      where
        mfirstundeclaredcomm =
          find (`M.notMember` jcommodities j) . map acommodity $
          (maybe id ((:) . baamount) pbalanceassertion) . filter (/= missingamt) $ amountsRaw pamount


setYear :: Year -> JournalParser m ()
setYear y = modify' (\j -> j{jparsedefaultyear=Just y})

getYear :: JournalParser m (Maybe Year)
getYear = fmap jparsedefaultyear get

-- | Get the decimal mark that has been specified for parsing, if any
-- (eg by the CSV decimal-mark rule, or possibly a future journal directive).
-- Return it as an AmountStyle that amount parsers can use.
getDecimalMarkStyle :: JournalParser m (Maybe AmountStyle)
getDecimalMarkStyle = do
  Journal{jparsedecimalmark} <- get
  let mdecmarkStyle = maybe Nothing (\c -> Just $ amountstyle{asdecimalpoint=Just c}) jparsedecimalmark
  return mdecmarkStyle

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

-- | Get the 'AmountStyle' declared by the most recently parsed (in the current or parent files,
-- prior to the current position) commodity directive for the given commodity, if any.
getAmountStyle :: CommoditySymbol -> JournalParser m (Maybe AmountStyle)
getAmountStyle commodity = do
  Journal{jcommodities} <- get
  let mspecificStyle = M.lookup commodity jcommodities >>= cformat
  mdefaultStyle <- fmap snd <$> getDefaultCommodityAndStyle
  return $ listToMaybe $ catMaybes [mspecificStyle, mdefaultStyle]

addDeclaredAccountType :: AccountName -> AccountType -> JournalParser m ()
addDeclaredAccountType acct atype =
  modify' (\j -> j{jdeclaredaccounttypes = M.insertWith (++) atype [acct] (jdeclaredaccounttypes j)})

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
clearAccountAliases = modify' (\j -> j{jparsealiases=[]})

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

-- A version of `match` that is strict in the returned text
match' :: TextParser m a -> TextParser m (Text, a)
match' p = do
  (!txt, p) <- match p
  pure (txt, p)

--- ** parsers
--- *** transaction bits

statusp :: TextParser m Status
statusp =
  choice'
    [ skipNonNewlineSpaces >> char '*' >> return Cleared
    , skipNonNewlineSpaces >> char '!' >> return Pending
    , return Unmarked
    ]

codep :: TextParser m Text
codep = option "" $ do
  try $ do
    skipNonNewlineSpaces1
    char '('
  code <- takeWhileP Nothing $ \c -> c /= ')' && c /= '\n'
  char ')' <?> "closing bracket ')' for transaction code"
  pure code

descriptionp :: TextParser m Text
descriptionp = takeWhileP Nothing (not . semicolonOrNewline)
  where semicolonOrNewline c = c == ';' || c == '\n'

--- *** dates

-- | Parse a date in YYYY-MM-DD format.
-- Slash (/) and period (.) are also allowed as separators.
-- The year may be omitted if a default year has been set.
-- Leading zeroes may be omitted.
datep :: JournalParser m Day
datep = do
  mYear <- getYear
  lift $ datep' mYear

datep' :: Maybe Year -> TextParser m Day
datep' mYear = do
    startOffset <- getOffset
    d1 <- yearorintp <?> "year or month"
    sep <- datesepchar <?> "date separator"
    d2 <- decimal <?> "month or day"
    case d1 of
         Left y  -> fullDate startOffset y sep d2
         Right m -> partialDate startOffset mYear m sep d2
    <?> "full or partial date"
  where
    fullDate :: Int -> Year -> Char -> Month -> TextParser m Day
    fullDate startOffset year sep1 month = do
      sep2 <- satisfy isDateSepChar <?> "date separator"
      day <- decimal <?> "day"
      endOffset <- getOffset
      let dateStr = show year ++ [sep1] ++ show month ++ [sep2] ++ show day

      when (sep1 /= sep2) $ customFailure $ parseErrorAtRegion startOffset endOffset $
        "invalid date: separators are different, should be the same"

      case fromGregorianValid year month day of
        Nothing -> customFailure $ parseErrorAtRegion startOffset endOffset $
                     "well-formed but invalid date: " ++ dateStr
        Just date -> pure $! date

    partialDate :: Int -> Maybe Year -> Month -> Char -> MonthDay -> TextParser m Day
    partialDate startOffset mYear month sep day = do
      endOffset <- getOffset
      case mYear of
        Just year ->
          case fromGregorianValid year month day of
            Nothing -> customFailure $ parseErrorAtRegion startOffset endOffset $
                        "well-formed but invalid date: " ++ dateStr
            Just date -> pure $! date
          where dateStr = show year ++ [sep] ++ show month ++ [sep] ++ show day

        Nothing -> customFailure $ parseErrorAtRegion startOffset endOffset $
          "partial date "++dateStr++" found, but the current year is unknown"
          where dateStr = show month ++ [sep] ++ show day

{-# INLINABLE datep' #-}

-- | Parse a date and time in YYYY-MM-DD HH:MM[:SS][+-ZZZZ] format.
-- Slash (/) and period (.) are also allowed as date separators.
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
  skipNonNewlineSpaces1
  time <- timeOfDay
  optional timeZone -- ignoring time zones
  pure $ LocalTime day time

  where
    timeOfDay :: TextParser m TimeOfDay
    timeOfDay = do
      off1 <- getOffset
      h' <- twoDigitDecimal <?> "hour"
      off2 <- getOffset
      unless (h' >= 0 && h' <= 23) $ customFailure $
        parseErrorAtRegion off1 off2 "invalid time (bad hour)"

      char ':' <?> "':' (hour-minute separator)"
      off3 <- getOffset
      m' <- twoDigitDecimal <?> "minute"
      off4 <- getOffset
      unless (m' >= 0 && m' <= 59) $ customFailure $
        parseErrorAtRegion off3 off4 "invalid time (bad minute)"

      s' <- option 0 $ do
        char ':' <?> "':' (minute-second separator)"
        off5 <- getOffset
        s' <- twoDigitDecimal <?> "second"
        off6 <- getOffset
        unless (s' >= 0 && s' <= 59) $ customFailure $
          parseErrorAtRegion off5 off6 "invalid time (bad second)"
          -- we do not support leap seconds
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

-- | Parse a year number or an Int. Years must contain at least four
-- digits.
yearorintp :: TextParser m (Either Year Int)
yearorintp = do
    yearOrMonth <- takeWhile1P (Just "digit") isDigit
    let n = readDecimal yearOrMonth
    return $ if T.length yearOrMonth >= 4 then Left n else Right (fromInteger n)

--- *** account names

-- | Parse an account name (plus one following space if present),
-- then apply any parent account prefix and/or account aliases currently in effect,
-- in that order. (Ie first add the parent account prefix, then rewrite with aliases).
-- This calls error if any account alias with an invalid regular expression exists.
modifiedaccountnamep :: JournalParser m AccountName
modifiedaccountnamep = do
  parent  <- getParentAccount
  aliases <- getAccountAliases
  -- off1    <- getOffset
  a       <- lift accountnamep
  -- off2    <- getOffset
  -- XXX or accountNameApplyAliasesMemo ? doesn't seem to make a difference (retest that function)
  case accountNameApplyAliases aliases $ joinAccountNames parent a of
    Right a' -> return $! a'
    -- should not happen, regexaliasp will have displayed a better error already:
    -- (XXX why does customFailure cause error to be displayed there, but not here ?)
    -- Left e  -> customFailure $! parseErrorAtRegion off1 off2 err
    Left e   -> error' err  -- PARTIAL:
      where
        err = "problem in account alias applied to "++T.unpack a++": "++e

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


-- | Parse any text beginning with a non-whitespace character, until a
-- double space or the end of input.
-- TODO including characters which normally start a comment (;#) - exclude those ? 
singlespacedtextp :: TextParser m T.Text
singlespacedtextp = singlespacedtextsatisfyingp (const True)

-- | Similar to 'singlespacedtextp', except that the text must only contain
-- characters satisfying the given predicate.
singlespacedtextsatisfyingp :: (Char -> Bool) -> TextParser m T.Text
singlespacedtextsatisfyingp pred = do
  firstPart <- partp
  otherParts <- many $ try $ singlespacep *> partp
  pure $! T.unwords $ firstPart : otherParts
  where
    partp = takeWhile1P Nothing (\c -> pred c && not (isSpace c))

-- | Parse one non-newline whitespace character that is not followed by another one.
singlespacep :: TextParser m ()
singlespacep = spacenonewline *> notFollowedBy spacenonewline

--- *** amounts

-- | Parse whitespace then an amount, with an optional left or right
-- currency symbol and optional price, or return the special
-- "missing" marker amount.
spaceandamountormissingp :: JournalParser m MixedAmount
spaceandamountormissingp =
  option missingmixedamt $ try $ do
    lift $ skipNonNewlineSpaces1
    mixedAmount <$> amountp

-- | Parse a single-commodity amount, with optional symbol on the left
-- or right, followed by, in any order: an optional transaction price,
-- an optional ledger-style lot price, and/or an optional ledger-style
-- lot date. A lot price and lot date will be ignored.
--
-- To parse the amount's quantity (number) we need to know which character 
-- represents a decimal mark. We find it in one of three ways:
--
-- 1. If a decimal mark has been set explicitly in the journal parse state, 
--    we use that
--
-- 2. Or if the journal has a commodity declaration for the amount's commodity,
--    we get the decimal mark from  that
--
-- 3. Otherwise we will parse any valid decimal mark appearing in the
--    number, as long as the number appears well formed.
--
-- Note 3 is the default zero-config case; it means we automatically handle
-- files with any supported decimal mark, but it also allows different decimal marks
-- in  different amounts, which is a bit too loose. There's an open issue.
amountp :: JournalParser m Amount
amountp = label "amount" $ do
  let 
    spaces = lift $ skipNonNewlineSpaces
  amount <- amountwithoutpricep <* spaces
  (mprice, _elotprice, _elotdate) <- runPermutation $
    (,,) <$> toPermutationWithDefault Nothing (Just <$> priceamountp amount <* spaces)
         <*> toPermutationWithDefault Nothing (Just <$> lotpricep <* spaces)
         <*> toPermutationWithDefault Nothing (Just <$> lotdatep <* spaces)
  pure $ amount { aprice = mprice }

amountpnolotpricesp :: JournalParser m Amount
amountpnolotpricesp = label "amount" $ do
  let spaces = lift $ skipNonNewlineSpaces
  amount <- amountwithoutpricep
  spaces
  mprice <- optional $ priceamountp amount <* spaces
  pure $ amount { aprice = mprice }

amountwithoutpricep :: JournalParser m Amount
amountwithoutpricep = do
  (mult, sign) <- lift $ (,) <$> multiplierp <*> signp
  leftsymbolamountp mult sign <|> rightornosymbolamountp mult sign

  where

  leftsymbolamountp :: Bool -> (Decimal -> Decimal) -> JournalParser m Amount
  leftsymbolamountp mult sign = label "amount" $ do
    c <- lift commoditysymbolp
    mdecmarkStyle <- getDecimalMarkStyle
    mcommodityStyle <- getAmountStyle c
    -- XXX amounts of this commodity in periodic transaction rules and auto posting rules ? #1461
    let suggestedStyle = mdecmarkStyle <|> mcommodityStyle
    commodityspaced <- lift skipNonNewlineSpaces'
    sign2 <- lift $ signp
    offBeforeNum <- getOffset
    ambiguousRawNum <- lift rawnumberp
    mExponent <- lift $ optional $ try exponentp
    offAfterNum <- getOffset
    let numRegion = (offBeforeNum, offAfterNum)
    (q,prec,mdec,mgrps) <- lift $ interpretNumber numRegion suggestedStyle ambiguousRawNum mExponent
    let s = amountstyle{ascommodityside=L, ascommodityspaced=commodityspaced, asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps}
    return $ nullamt{acommodity=c, aquantity=sign (sign2 q), aismultiplier=mult, astyle=s, aprice=Nothing}

  rightornosymbolamountp :: Bool -> (Decimal -> Decimal) -> JournalParser m Amount
  rightornosymbolamountp mult sign = label "amount" $ do
    offBeforeNum <- getOffset
    ambiguousRawNum <- lift rawnumberp
    mExponent <- lift $ optional $ try exponentp
    offAfterNum <- getOffset
    let numRegion = (offBeforeNum, offAfterNum)
    mSpaceAndCommodity <- lift $ optional $ try $ (,) <$> skipNonNewlineSpaces' <*> commoditysymbolp
    case mSpaceAndCommodity of
      -- right symbol amount
      Just (commodityspaced, c) -> do
        mdecmarkStyle <- getDecimalMarkStyle
        mcommodityStyle <- getAmountStyle c
        -- XXX amounts of this commodity in periodic transaction rules and auto posting rules ? #1461
        let msuggestedStyle = mdecmarkStyle <|> mcommodityStyle
        (q,prec,mdec,mgrps) <- lift $ interpretNumber numRegion msuggestedStyle ambiguousRawNum mExponent
        let s = amountstyle{ascommodityside=R, ascommodityspaced=commodityspaced, asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps}
        return $ nullamt{acommodity=c, aquantity=sign q, aismultiplier=mult, astyle=s, aprice=Nothing}
      -- no symbol amount
      Nothing -> do
        -- look for a number style to use when parsing, based on
        -- these things we've already parsed, in this order of preference:
        mdecmarkStyle   <- getDecimalMarkStyle   -- a decimal-mark CSV rule
        mcommodityStyle <- getAmountStyle ""     -- a commodity directive for the no-symbol commodity
        mdefaultStyle   <- getDefaultAmountStyle -- a D default commodity directive
        -- XXX no-symbol amounts in periodic transaction rules and auto posting rules ? #1461
        let msuggestedStyle = mdecmarkStyle <|> mcommodityStyle <|> mdefaultStyle
        (q,prec,mdec,mgrps) <- lift $ interpretNumber numRegion msuggestedStyle ambiguousRawNum mExponent
        -- if a default commodity has been set, apply it and its style to this amount
        -- (unless it's a multiplier in an automated posting)
        defcs <- getDefaultCommodityAndStyle
        let (c,s) = case (mult, defcs) of
              (False, Just (defc,defs)) -> (defc, defs{asprecision=max (asprecision defs) prec})
              _ -> ("", amountstyle{asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps})
        return $ nullamt{acommodity=c, aquantity=sign q, aismultiplier=mult, astyle=s, aprice=Nothing}

  -- For reducing code duplication. Doesn't parse anything. Has the type
  -- of a parser only in order to throw parse errors (for convenience).
  interpretNumber
    :: (Int, Int) -- offsets
    -> Maybe AmountStyle
    -> Either AmbiguousNumber RawNumber
    -> Maybe Integer
    -> TextParser m (Quantity, AmountPrecision, Maybe Char, Maybe DigitGroupStyle)
  interpretNumber posRegion msuggestedStyle ambiguousNum mExp =
    let rawNum = either (disambiguateNumber msuggestedStyle) id ambiguousNum
    in  case fromRawNumber rawNum mExp of
          Left errMsg -> customFailure $
                           uncurry parseErrorAtRegion posRegion errMsg
          Right (q,p,d,g) -> pure (q, Precision p, d, g)

-- | Parse an amount from a string, or get an error.
amountp' :: String -> Amount
amountp' s =
  case runParser (evalStateT (amountp <* eof) nulljournal) "" (T.pack s) of
    Right amt -> amt
    Left err  -> error' $ show err  -- PARTIAL: XXX should throwError

-- | Parse a mixed amount from a string, or get an error.
mamountp' :: String -> MixedAmount
mamountp' = mixedAmount . amountp'

-- | Parse a minus or plus sign followed by zero or more spaces,
-- or nothing, returning a function that negates or does nothing.
signp :: Num a => TextParser m (a -> a)
signp = ((char '-' *> pure negate <|> char '+' *> pure id) <* skipNonNewlineSpaces) <|> pure id

multiplierp :: TextParser m Bool
multiplierp = option False $ char '*' *> pure True

commoditysymbolp :: TextParser m CommoditySymbol
commoditysymbolp =
  quotedcommoditysymbolp <|> simplecommoditysymbolp <?> "commodity symbol"

quotedcommoditysymbolp :: TextParser m CommoditySymbol
quotedcommoditysymbolp =
  between (char '"') (char '"') $ takeWhile1P Nothing f
  where f c = c /= ';' && c /= '\n' && c /= '\"'

simplecommoditysymbolp :: TextParser m CommoditySymbol
simplecommoditysymbolp = takeWhile1P Nothing (not . isNonsimpleCommodityChar)

priceamountp :: Amount -> JournalParser m AmountPrice
priceamountp baseAmt = label "transaction price" $ do
  -- https://www.ledger-cli.org/3.0/doc/ledger3.html#Virtual-posting-costs
  parenthesised <- option False $ char '(' >> pure True
  char '@'
  totalPrice <- char '@' *> pure True <|> pure False
  when parenthesised $ void $ char ')'

  lift skipNonNewlineSpaces
  priceAmount <- amountwithoutpricep -- <?> "unpriced amount (specifying a price)"

  let amtsign' = signum $ aquantity baseAmt
      amtsign  = if amtsign' == 0 then 1 else amtsign'

  pure $ if totalPrice
            then TotalPrice priceAmount{aquantity=amtsign * aquantity priceAmount}
            else UnitPrice  priceAmount


balanceassertionp :: JournalParser m BalanceAssertion
balanceassertionp = do
  sourcepos <- genericSourcePos <$> lift getSourcePos
  char '='
  istotal <- fmap isJust $ optional $ try $ char '='
  isinclusive <- fmap isJust $ optional $ try $ char '*'
  lift skipNonNewlineSpaces
  -- this amount can have a price; balance assertions ignore it,
  -- but balance assignments will use it
  a <- amountpnolotpricesp <?> "amount (for a balance assertion or assignment)"
  return BalanceAssertion
    { baamount    = a
    , batotal     = istotal
    , bainclusive = isinclusive
    , baposition  = sourcepos
    }

-- Parse a Ledger-style fixed {=UNITPRICE} or non-fixed {UNITPRICE}
-- or fixed {{=TOTALPRICE}} or non-fixed {{TOTALPRICE}} lot price,
-- and ignore it.
-- https://www.ledger-cli.org/3.0/doc/ledger3.html#Fixing-Lot-Prices .
lotpricep :: JournalParser m ()
lotpricep = label "ledger-style lot price" $ do
  char '{'
  doublebrace <- option False $ char '{' >> pure True
  _fixed <- fmap isJust $ optional $ lift skipNonNewlineSpaces >> char '='
  lift skipNonNewlineSpaces
  _a <- amountwithoutpricep
  lift skipNonNewlineSpaces
  char '}'
  when (doublebrace) $ void $ char '}'
  return ()

-- Parse a Ledger-style lot date [DATE], and ignore it.
-- https://www.ledger-cli.org/3.0/doc/ledger3.html#Fixing-Lot-Prices .
lotdatep :: JournalParser m ()
lotdatep = (do
  char '['
  lift skipNonNewlineSpaces
  _d <- datep
  lift skipNonNewlineSpaces
  char ']'
  return ()
  ) <?> "ledger-style lot date"

-- | Parse a string representation of a number for its value and display
-- attributes.
--
-- Some international number formats are accepted, eg either period or comma
-- may be used for the decimal mark, and the other of these may be used for
-- separating digit groups in the integer part. See
-- http://en.wikipedia.org/wiki/Decimal_separator for more examples.
--
-- This returns: the parsed numeric value, the precision (number of digits
-- seen following the decimal mark), the decimal mark character used if any,
-- and the digit group style if any.
--
numberp :: Maybe AmountStyle -> TextParser m (Quantity, Word8, Maybe Char, Maybe DigitGroupStyle)
numberp suggestedStyle = label "number" $ do
    -- a number is an optional sign followed by a sequence of digits possibly
    -- interspersed with periods, commas, or both
    -- dbgparse 0 "numberp"
    sign <- signp
    rawNum <- either (disambiguateNumber suggestedStyle) id <$> rawnumberp
    mExp <- optional $ try $ exponentp
    dbg7 "numberp suggestedStyle" suggestedStyle `seq` return ()
    case dbg7 "numberp quantity,precision,mdecimalpoint,mgrps"
           $ fromRawNumber rawNum mExp of
      Left errMsg -> Fail.fail errMsg
      Right (q, p, d, g) -> pure (sign q, p, d, g)

exponentp :: TextParser m Integer
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
  -> Maybe Integer
  -> Either String
            (Quantity, Word8, Maybe Char, Maybe DigitGroupStyle)
fromRawNumber (WithSeparators _ _ _) (Just _) =
    Left "invalid number: digit separators and exponents may not be used together"
fromRawNumber raw mExp = do
    (quantity, precision) <- toQuantity (fromMaybe 0 mExp) (digitGroup raw) (decimalGroup raw)
    return (quantity, precision, mDecPt raw, digitGroupStyle raw)
  where
    toQuantity :: Integer -> DigitGrp -> DigitGrp -> Either String (Quantity, Word8)
    toQuantity e preDecimalGrp postDecimalGrp
      | precision < 0   = Right (Decimal 0 (digitGrpNum * 10^(-precision)), 0)
      | precision < 256 = Right (Decimal precision8 digitGrpNum, precision8)
      | otherwise = Left "invalid number: numbers with more than 255 decimal places are currently not supported"
      where
        digitGrpNum = digitGroupNumber $ preDecimalGrp <> postDecimalGrp
        precision   = toInteger (digitGroupLength postDecimalGrp) - e
        precision8  = fromIntegral precision :: Word8

    mDecPt (NoSeparators _ mDecimals)           = fst <$> mDecimals
    mDecPt (WithSeparators _ _ mDecimals)       = fst <$> mDecimals
    decimalGroup (NoSeparators _ mDecimals)     = maybe mempty snd mDecimals
    decimalGroup (WithSeparators _ _ mDecimals) = maybe mempty snd mDecimals
    digitGroup (NoSeparators digitGrp _)        = digitGrp
    digitGroup (WithSeparators _ digitGrps _)   = mconcat digitGrps
    digitGroupStyle (NoSeparators _ _)          = Nothing
    digitGroupStyle (WithSeparators sep grps _) = Just . DigitGroups sep $ groupSizes grps

    -- Outputs digit group sizes from least significant to most significant
    groupSizes :: [DigitGrp] -> [Word8]
    groupSizes digitGrps = reverse $ case map (fromIntegral . digitGroupLength) digitGrps of
      (a:b:cs) | a < b -> b:cs
      gs               -> gs

disambiguateNumber :: Maybe AmountStyle -> AmbiguousNumber -> RawNumber
disambiguateNumber msuggestedStyle (AmbiguousNumber grp1 sep grp2) =
  -- If present, use the suggested style to disambiguate;
  -- otherwise, assume that the separator is a decimal point where possible.
  if isDecimalMark sep &&
     maybe True (sep `isValidDecimalBy`) msuggestedStyle
  then NoSeparators grp1 (Just (sep, grp2))
  else WithSeparators sep [grp1, grp2] Nothing
  where
    isValidDecimalBy :: Char -> AmountStyle -> Bool
    isValidDecimalBy c = \case
      AmountStyle{asdecimalpoint = Just d} -> d == c
      AmountStyle{asdigitgroups = Just (DigitGroups g _)} -> g /= c
      AmountStyle{asprecision = Precision 0} -> False
      _ -> True

-- | Parse and interpret the structure of a number without external hints.
-- Numbers are digit strings, possibly separated into digit groups by one
-- of two types of separators. (1) Numbers may optionally have a decimal
-- mark, which may be either a period or comma. (2) Numbers may
-- optionally contain digit group marks, which must all be either a
-- period, a comma, or a space.
--
-- It is our task to deduce the characters used as decimal mark and
-- digit group mark, based on the allowed syntax. For instance, we
-- make use of the fact that a decimal mark can occur at most once and
-- must be to the right of all digit group marks.
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
  mExtraDecimalSep <- optional $ lookAhead $ satisfy isDecimalMark
  when (isJust mExtraDecimalSep) $
    Fail.fail "invalid number (invalid use of separator)"

  mExtraFragment <- optional $ lookAhead $ try $
    char ' ' *> getOffset <* digitChar
  case mExtraFragment of
    Just off -> customFailure $
                  parseErrorAt off "invalid number (excessive trailing digits)"
    Nothing -> pure ()

  return $ dbg7 "rawnumberp" rawNumber
  where

  leadingDecimalPt :: TextParser m RawNumber
  leadingDecimalPt = do
    decPt <- satisfy isDecimalMark
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
    decPt <- satisfy $ \c -> isDecimalMark c && c /= digitSep
    decDigitGrp <- option mempty digitgroupp

    pure $ WithSeparators digitSep digitGroups (Just (decPt, decDigitGrp))

  withoutDecimalPt
    :: DigitGrp
    -> Char
    -> DigitGrp
    -> [DigitGrp]
    -> Either AmbiguousNumber RawNumber
  withoutDecimalPt grp1 sep grp2 grps
    | null grps && isDecimalMark sep =
        Left $ AmbiguousNumber grp1 sep grp2
    | otherwise = Right $ WithSeparators sep (grp1:grp2:grps) Nothing

  trailingDecimalPt :: DigitGrp -> TextParser m RawNumber
  trailingDecimalPt grp1 = do
    decPt <- satisfy isDecimalMark
    pure $ NoSeparators grp1 (Just (decPt, mempty))

isDigitSeparatorChar :: Char -> Bool
isDigitSeparatorChar c = isDecimalMark c || c == ' '

-- | Some kinds of number literal we might parse.
data RawNumber
  = NoSeparators   DigitGrp (Maybe (Char, DigitGrp))
    -- ^ A number with no digit group marks (eg 100),
    --   or with a leading or trailing comma or period
    --   which (apparently) we interpret as a decimal mark (like 100. or .100)
  | WithSeparators Char [DigitGrp] (Maybe (Char, DigitGrp))
    -- ^ A number with identifiable digit group marks
    --   (eg 1,000,000 or 1,000.50 or 1 000)
  deriving (Show, Eq)

-- | Another kind of number literal: this one contains either a digit
-- group separator or a decimal mark, we're not sure which (eg 1,000 or 100.50).
data AmbiguousNumber = AmbiguousNumber DigitGrp Char DigitGrp
  deriving (Show, Eq)

-- | Description of a single digit group in a number literal.
-- "Thousands" is one well known digit grouping, but there are others.
data DigitGrp = DigitGrp {
  digitGroupLength :: !Word,    -- ^ The number of digits in this group.
                                -- This is Word to avoid the need to do overflow
                                -- checking for the Semigroup instance of DigitGrp.
  digitGroupNumber :: !Integer  -- ^ The natural number formed by this group's digits. This should always be positive.
} deriving (Eq)

-- | A custom show instance, showing digit groups as the parser saw them.
instance Show DigitGrp where
  show (DigitGrp len num) = "\"" ++ padding ++ numStr ++ "\""
    where numStr = show num
          padding = genericReplicate (toInteger len - toInteger (length numStr)) '0'

instance Sem.Semigroup DigitGrp where
  DigitGrp l1 n1 <> DigitGrp l2 n2 = DigitGrp (l1 + l2) (n1 * 10^l2 + n2)

instance Monoid DigitGrp where
  mempty = DigitGrp 0 0
  mappend = (Sem.<>)

digitgroupp :: TextParser m DigitGrp
digitgroupp = label "digits"
            $ makeGroup <$> takeWhile1P (Just "digit") isDigit
  where
    makeGroup = uncurry DigitGrp . T.foldl' step (0, 0)
    step (!l, !a) c = (l+1, a*10 + fromIntegral (digitToInt c))

--- *** comments

multilinecommentp :: TextParser m ()
multilinecommentp = startComment *> anyLine `skipManyTill` endComment
  where
    startComment = string "comment" *> trailingSpaces
    endComment = eof <|> string "end comment" *> trailingSpaces

    trailingSpaces = skipNonNewlineSpaces <* newline
    anyLine = void $ takeWhileP Nothing (\c -> c /= '\n') *> newline

{-# INLINABLE multilinecommentp #-}

-- | A blank or comment line in journal format: a line that's empty or
-- containing only whitespace or whose first non-whitespace character
-- is semicolon, hash, or star.
emptyorcommentlinep :: TextParser m ()
emptyorcommentlinep = do
  skipNonNewlineSpaces
  skiplinecommentp <|> void newline
  where
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
-- preceded by leading whitespace.
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
  skipNonNewlineSpaces
  -- there can be 0 or 1 sameLine
  sameLine <- try headerp *> ((:[]) <$> match' contentp) <|> pure []
  _ <- eolof
  -- there can be 0 or more nextLines
  nextLines <- many $
    try (skipNonNewlineSpaces1 *> headerp) *> match' contentp <* eolof
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
    headerp = char ';' *> skipNonNewlineSpaces

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
  fst <$> followingcommentp' (void $ takeWhileP Nothing (/= '\n'))  -- XXX support \r\n ?
{-# INLINABLE followingcommentp #-}


-- | Parse a transaction comment and extract its tags.
--
-- The first line of a transaction may be followed by comments, which
-- begin with semicolons and extend to the end of the line. Transaction
-- comments may span multiple lines, but comment lines below the
-- transaction must be preceded by leading whitespace.
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
          skipNonNewlineSpaces
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
-- lines, but comment lines below the posting must be preceded by
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
      skipNonNewlineSpaces
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
-- >>> either (Left . customErrorBundlePretty) Right $ rtp (bracketeddatetagsp Nothing) "[2016/1/2=3/4]"
-- Right [("date",2016-01-02),("date2",2016-03-04)]
--
-- >>> either (Left . customErrorBundlePretty) Right $ rtp (bracketeddatetagsp Nothing) "[1]"
-- Left ...not a bracketed date...
--
-- >>> either (Left . customErrorBundlePretty) Right $ rtp (bracketeddatetagsp Nothing) "[2016/1/32]"
-- Left ...1:2:...well-formed but invalid date: 2016/1/32...
--
-- >>> either (Left . customErrorBundlePretty) Right $ rtp (bracketeddatetagsp Nothing) "[1/31]"
-- Left ...1:2:...partial date 1/31 found, but the current year is unknown...
--
-- >>> either (Left . customErrorBundlePretty) Right $ rtp (bracketeddatetagsp Nothing) "[0123456789/-.=/-.=]"
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
      Fail.fail "not a bracketed date"
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

-- | Get the account name aliases from options, if any.
aliasesFromOpts :: InputOpts -> [AccountAlias]
aliasesFromOpts = map (\a -> fromparse $ runParser accountaliasp ("--alias "++quoteIfNeeded a) $ T.pack a)
                  . aliases_

accountaliasp :: TextParser m AccountAlias
accountaliasp = regexaliasp <|> basicaliasp

basicaliasp :: TextParser m AccountAlias
basicaliasp = do
  -- dbgparse 0 "basicaliasp"
  old <- rstrip <$> (some $ noneOf ("=" :: [Char]))
  char '='
  skipNonNewlineSpaces
  new <- rstrip <$> anySingle `manyTill` eolof  -- eol in journal, eof in command lines, normally
  return $ BasicAlias (T.pack old) (T.pack new)

regexaliasp :: TextParser m AccountAlias
regexaliasp = do
  -- dbgparse 0 "regexaliasp"
  char '/'
  off1 <- getOffset
  re <- some $ noneOf ("/\n\r" :: [Char]) -- paranoid: don't try to read past line end
  off2 <- getOffset
  char '/'
  skipNonNewlineSpaces
  char '='
  skipNonNewlineSpaces
  repl <- anySingle `manyTill` eolof
  case toRegexCI $ T.pack re of
    Right r -> return $! RegexAlias r repl
    Left e  -> customFailure $! parseErrorAtRegion off1 off2 e

--- ** tests

tests_Common = tests "Common" [

   tests "amountp" [
    test "basic"                  $ assertParseEq amountp "$47.18"     (usd 47.18)
   ,test "ends with decimal mark" $ assertParseEq amountp "$1."        (usd 1  `withPrecision` Precision 0)
   ,test "unit price"             $ assertParseEq amountp "$10 @ €0.5"
      -- not precise enough:
      -- (usd 10 `withPrecision` 0 `at` (eur 0.5 `withPrecision` 1)) -- `withStyle` asdecimalpoint=Just '.'
      amount{
         acommodity="$"
        ,aquantity=10 -- need to test internal precision with roundTo ? I think not
        ,astyle=amountstyle{asprecision=Precision 0, asdecimalpoint=Nothing}
        ,aprice=Just $ UnitPrice $
          amount{
             acommodity="€"
            ,aquantity=0.5
            ,astyle=amountstyle{asprecision=Precision 1, asdecimalpoint=Just '.'}
            }
        }
   ,test "total price"            $ assertParseEq amountp "$10 @@ €5"
      amount{
         acommodity="$"
        ,aquantity=10
        ,astyle=amountstyle{asprecision=Precision 0, asdecimalpoint=Nothing}
        ,aprice=Just $ TotalPrice $
          amount{
             acommodity="€"
            ,aquantity=5
            ,astyle=amountstyle{asprecision=Precision 0, asdecimalpoint=Nothing}
            }
        }
   ,test "unit price, parenthesised" $ assertParse amountp "$10 (@) €0.5"
   ,test "total price, parenthesised" $ assertParse amountp "$10 (@@) €0.5"
   ]

  ,let p = lift (numberp Nothing) :: JournalParser IO (Quantity, Word8, Maybe Char, Maybe DigitGroupStyle) in
   test "numberp" $ do
     assertParseEq p "0"          (0, 0, Nothing, Nothing)
     assertParseEq p "1"          (1, 0, Nothing, Nothing)
     assertParseEq p "1.1"        (1.1, 1, Just '.', Nothing)
     assertParseEq p "1,000.1"    (1000.1, 1, Just '.', Just $ DigitGroups ',' [3])
     assertParseEq p "1.00.000,1" (100000.1, 1, Just ',', Just $ DigitGroups '.' [3,2])
     assertParseEq p "1,000,000"  (1000000, 0, Nothing, Just $ DigitGroups ',' [3,3])  -- could be simplified to [3]
     assertParseEq p "1."         (1, 0, Just '.', Nothing)
     assertParseEq p "1,"         (1, 0, Just ',', Nothing)
     assertParseEq p ".1"         (0.1, 1, Just '.', Nothing)
     assertParseEq p ",1"         (0.1, 1, Just ',', Nothing)
     assertParseError p "" ""
     assertParseError p "1,000.000,1" ""
     assertParseError p "1.000,000.1" ""
     assertParseError p "1,000.000.1" ""
     assertParseError p "1,,1" ""
     assertParseError p "1..1" ""
     assertParseError p ".1," ""
     assertParseError p ",1." ""
     assertParseEq    p "1.555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555" (1.555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555, 255, Just '.', Nothing)
     assertParseError p "1.5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555" ""

  ,tests "spaceandamountormissingp" [
     test "space and amount" $ assertParseEq spaceandamountormissingp " $47.18" (mixedAmount $ usd 47.18)
    ,test "empty string" $ assertParseEq spaceandamountormissingp "" missingmixedamt
    -- ,test "just space" $ assertParseEq spaceandamountormissingp " " missingmixedamt  -- XXX should it ?
    -- ,test "just amount" $ assertParseError spaceandamountormissingp "$47.18" ""  -- succeeds, consuming nothing
    ]

  ]
