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
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}

--- ** exports
module Hledger.Read.Common (
  Reader (..),
  InputOpts(..),
  HasInputOpts(..),
  definputopts,
  rawOptsToInputOpts,

  -- * parsing utilities
  parseAndFinaliseJournal,
  initialiseAndParseJournal,
  journalFinalise,
  journalAddForecast,
  journalAddAutoPostings,
  setYear,
  getYear,
  setDefaultCommodityAndStyle,
  getDefaultCommodityAndStyle,
  getDefaultAmountStyle,
  getAmountStyle,
  addDeclaredAccountTags,
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
  commoditysymbolp,
  costp,
  balanceassertionp,
  lotcostp,
  numberp,
  fromRawNumber,
  rawnumberp,
  parseamount,
  parseamount',
  parsemixedamount,
  parsemixedamount',

  -- ** comments
  isLineCommentStart,
  isSameLineCommentStart,
  multilinecommentp,
  emptyorcommentlinep,
  emptyorcommentlinep2,
  followingcommentp,
  transactioncommentp,
  commentlinetagsp,
  postingcommentp,

  -- ** bracketed dates
  bracketeddatetagsp,

  -- ** misc
  doublequotedtextp,
  noncommenttextp,
  noncommenttext1p,
  singlespacedtext1p,
  singlespacednoncommenttext1p,
  singlespacedtextsatisfying1p,
  singlespacep,
  skipNonNewlineSpaces,
  skipNonNewlineSpaces1,
  aliasesFromOpts,

  -- * tests
  tests_Common,
)
where

--- ** imports
import Control.Applicative.Permutations (runPermutation, toPermutationWithDefault)
import Control.Monad (foldM, liftM2, when, unless, (>=>), (<=<))
import qualified Control.Monad.Fail as Fail (fail)
import Control.Monad.Except (ExceptT(..), liftEither, withExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState, evalStateT, modify', get, put)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap, second)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Decimal (DecimalRaw (Decimal), Decimal)
import Data.Either (rights)
import Data.Function ((&))
import Data.Functor ((<&>), ($>), void)
import Data.List (find, genericReplicate, union)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import qualified Data.Map as M
import qualified Data.Semigroup as Sem
import Data.Text (Text, stripEnd)
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorianValid, toGregorian)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import Data.Word (Word8)
import System.FilePath (takeFileName)
import Text.Megaparsec
import Text.Megaparsec.Char (char, char', digitChar, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

import Hledger.Data
import Hledger.Query (Query(..), filterQuery, parseQueryTerm, queryEndDate, queryStartDate, queryIsDate, simplifyQuery)
import Hledger.Reports.ReportOptions (ReportOpts(..), queryFromFlags, rawOptsToReportOpts)
import Hledger.Utils
import Hledger.Read.InputOptions

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

instance Show (Reader m) where show r = show (rFormat r) ++ " reader"

-- | Parse an InputOpts from a RawOpts and a provided date.
-- This will fail with a usage error if the forecast period expression cannot be parsed.
rawOptsToInputOpts :: Day -> Bool -> Bool -> RawOpts -> InputOpts
rawOptsToInputOpts day usecoloronstdout postingaccttags rawopts =

    let noinferbalancingcosts = boolopt "strict" rawopts || stringopt "args" rawopts == "balanced"

        -- Do we really need to do all this work just to get the requested end date? This is duplicating
        -- much of reportOptsToSpec.
        ropts = rawOptsToReportOpts day usecoloronstdout rawopts
        argsquery = map fst . rights . map (parseQueryTerm day) $ querystring_ ropts
        datequery = simplifyQuery . filterQuery queryIsDate . And $ queryFromFlags ropts : argsquery

        styles = either err id $ commodityStyleFromRawOpts rawopts
          where err e = error' $ "could not parse commodity-style: '" ++ e ++ "'"  -- PARTIAL:

    in definputopts{
       -- files_             = listofstringopt "file" rawopts
       mformat_           = Nothing
      ,mrules_file_       = maybestringopt "rules" rawopts
      ,aliases_           = listofstringopt "alias" rawopts
      ,anon_              = boolopt "obfuscate" rawopts
      ,new_               = boolopt "new" rawopts
      ,new_save_          = True
      ,pivot_             = stringopt "pivot" rawopts
      ,forecast_          = forecastPeriodFromRawOpts day rawopts
      ,posting_account_tags_ = postingaccttags
      ,verbose_tags_      = boolopt "verbose-tags" rawopts
      ,reportspan_        = DateSpan (Exact <$> queryStartDate False datequery) (Exact <$> queryEndDate False datequery)
      ,auto_              = boolopt "auto" rawopts
      ,infer_equity_      = boolopt "infer-equity" rawopts && conversionop_ ropts /= Just ToCost
      ,infer_costs_       = boolopt "infer-costs" rawopts
      ,balancingopts_     = defbalancingopts{
                                 ignore_assertions_     = boolopt "ignore-assertions" rawopts
                               , infer_balancing_costs_ = not noinferbalancingcosts
                               , commodity_styles_      = Just styles
                               }
      ,strict_            = boolopt "strict" rawopts
      ,_ioDay             = day
      }

-- | Get the date span from --forecast's PERIODEXPR argument, if any.
-- This will fail with a usage error if the period expression cannot be parsed,
-- or if it contains a report interval.
forecastPeriodFromRawOpts :: Day -> RawOpts -> Maybe DateSpan
forecastPeriodFromRawOpts d rawopts = do
    arg <- maybestringopt "forecast" rawopts
    let period = parsePeriodExpr d . stripquotes $ T.pack arg
    return $ if null arg then nulldatespan else either badParse (getSpan arg) period
  where
    badParse e = usageError $ "could not parse forecast period : "++customErrorBundlePretty e
    getSpan arg (interval, requestedspan) = case interval of
        NoInterval -> requestedspan
        _          -> usageError $ "--forecast's argument should not contain a report interval ("
                                 ++ show interval ++ " in \"" ++ arg ++ "\")"

-- | Given the name of the option and the raw options, returns either
-- | * a map of successfully parsed commodity styles, if all options where successfully parsed
-- | * the first option which failed to parse, if one or more options failed to parse
commodityStyleFromRawOpts :: RawOpts -> Either String (M.Map CommoditySymbol AmountStyle)
commodityStyleFromRawOpts rawOpts =
    foldM (\r -> fmap (\(c,a) -> M.insert c a r) . parseCommodity) mempty optList
  where
    optList = listofstringopt "commodity-style" rawOpts
    parseCommodity optStr = case parseamount optStr of
        Left _ -> Left optStr
        Right (Amount acommodity _ astyle _) -> Right (acommodity, astyle)

-- | Given a parser to ParsedJournal, input options, file path and
-- content: run the parser on the content, and finalise the result to
-- get a Journal; or throw an error.
parseAndFinaliseJournal :: ErroringJournalParser IO ParsedJournal -> InputOpts
                           -> FilePath -> Text -> ExceptT String IO Journal
parseAndFinaliseJournal parser iopts f txt =
    initialiseAndParseJournal parser iopts f txt >>= journalFinalise iopts f txt

-- | Given a parser to ParsedJournal, input options, file path and
-- content: run the parser on the content. This is all steps of
-- 'parseAndFinaliseJournal' without the finalisation step, and is used when
-- you need to perform other actions before finalisation, as in parsing
-- Timeclock and Timedot files.
initialiseAndParseJournal :: ErroringJournalParser IO ParsedJournal -> InputOpts
                          -> FilePath -> Text -> ExceptT String IO Journal
initialiseAndParseJournal parser iopts f txt =
    prettyParseErrors $ runParserT (evalStateT parser initJournal) f txt
  where
    y = first3 . toGregorian $ _ioDay iopts
    initJournal = nulljournal{jparsedefaultyear = Just y, jincludefilestack = [f]}
    -- Flatten parse errors and final parse errors, and output each as a pretty String.
    prettyParseErrors :: ExceptT FinalParseError IO (Either (ParseErrorBundle Text HledgerParseErrorData) a)
                      -> ExceptT String IO a
    prettyParseErrors = withExceptT customErrorBundlePretty . liftEither
                    <=< withExceptT (finalErrorBundlePretty . attachSource f txt)

{- HLINT ignore journalFinalise "Redundant <&>" -} -- silence this warning, the code is clearer as is
--  note this activates TH, may slow compilation ? https://github.com/ndmitchell/hlint/blob/master/README.md#customizing-the-hints
--
-- | Post-process a Journal that has just been parsed or generated, in this order:
--
-- - add misc info (file path, read time) 
--
-- - reverse transactions into their original parse order
--
-- - apply canonical commodity styles
--
-- - propagate account tags to postings
--
-- - maybe add forecast transactions
--
-- - propagate account tags to postings (again to affect forecast transactions)
--
-- - maybe add auto postings
--
-- - propagate account tags to postings (again to affect auto postings)
--
-- - evaluate balance assignments and balance each transaction
--
-- - maybe check balance assertions
--
-- - maybe infer costs from equity postings
--
-- - maybe infer equity postings from costs
--
-- - manye infer market prices from costs
--
-- One correctness check (parseable) has already passed when this function is called.
-- Up to four more are performed here:
--
--  - ordereddates (when enabled)
--
--  - assertions (when enabled)
--
--  - autobalanced (and with --strict, balanced ?), in the journalBalanceTransactions step.
--
-- Others (commodities, accounts..) are done later by journalStrictChecks.
--
journalFinalise :: InputOpts -> FilePath -> Text -> ParsedJournal -> ExceptT String IO Journal
journalFinalise iopts@InputOpts{auto_,balancingopts_,infer_costs_,infer_equity_,strict_,posting_account_tags_,verbose_tags_,_ioDay} f txt pj = do
  let
    BalancingOpts{commodity_styles_, ignore_assertions_} = balancingopts_
    fname = "journalFinalise " <> takeFileName f
    lbl = lbl_ fname
    -- Some not so pleasant hacks
    -- We want to know when certain checks have been explicitly requested with the check command,
    -- but it does not run until later. For now, inspect the command line with unsafePerformIO.
    checking checkname = "check" `elem` args && checkname `elem` args where args = progArgs
    -- We will check ordered dates when "check ordereddates" is used.
    checkordereddates = checking "ordereddates"
    -- We will check balance assertions by default, unless -I is used, but always if -s or "check assertions" are used.
    checkassertions = not ignore_assertions_ || strict_ || checking "assertions"

  t <- liftIO getPOSIXTime
  liftEither $
    pj{jglobalcommoditystyles=fromMaybe mempty commodity_styles_}
      &   journalSetLastReadTime t                       -- save the last read time
      &   journalAddFile (f, txt)                        -- save the main file's info
      &   journalReverse                                 -- convert all lists to the order they were parsed
      &   journalAddAccountTypes                         -- build a map of all known account types
            -- XXX does not see conversion accounts generated by journalInferEquityFromCosts below, requiring a workaround in journalCheckAccounts. Do it later ?
      &   journalStyleAmounts                            -- Infer and apply commodity styles (but don't round) - should be done early
      <&> journalAddForecast verbose_tags_ (forecastPeriod iopts pj)   -- Add forecast transactions if enabled
      <&> (if posting_account_tags_ then journalPostingsAddAccountTags else id)     -- Propagate account tags to postings - unless printing a beancount journal
      >>= journalTagCostsAndEquityAndMaybeInferCosts verbose_tags_ False   -- Tag equity conversion postings and redundant costs, to help journalBalanceTransactions ignore them.
      >>= (if auto_ && not (null $ jtxnmodifiers pj)
            then journalAddAutoPostings verbose_tags_ _ioDay balancingopts_  -- Add auto postings if enabled, and account tags if needed. Does preliminary transaction balancing.
            else pure)
      -- XXX how to force debug output here ?
       -- >>= Right . dbg0With (concatMap (T.unpack.showTransaction).jtxns)
       -- >>= \j -> deepseq (concatMap (T.unpack.showTransaction).jtxns $ j) (return j)
      <&> dbg9With (lbl "amounts after styling, forecasting, auto-posting".showJournalAmountsDebug)
      >>= (\j -> if checkordereddates then journalCheckOrdereddates j $> j else Right j)  -- check ordereddates before assertions. The outer parentheses are needed.
      >>= journalBalanceTransactions balancingopts_{ignore_assertions_=not checkassertions}  -- infer balance assignments and missing amounts, and maybe check balance assertions.
      <&> dbg9With (lbl "amounts after transaction-balancing".showJournalAmountsDebug)
      -- <&> dbg9With (("journalFinalise amounts after styling, forecasting, auto postings, transaction balancing"<>).showJournalAmountsDebug)
      >>= journalInferCommodityStyles                    -- infer commodity styles once more now that all posting amounts are present
      -- >>= Right . dbg0With (pshow.journalCommodityStyles)
      >>= (if infer_costs_  then journalTagCostsAndEquityAndMaybeInferCosts verbose_tags_ True else pure)  -- With --infer-costs, infer costs from equity postings where possible
      <&> (if infer_equity_ then journalInferEquityFromCosts verbose_tags_ else id)          -- With --infer-equity, infer equity postings from costs where possible
      <&> dbg9With (lbl "amounts after equity-inferring".showJournalAmountsDebug)
      <&> journalInferMarketPricesFromTransactions       -- infer market prices from commodity-exchanging transactions
      -- <&> traceOrLogAt 6 fname  -- debug logging
      <&> dbgJournalAcctDeclOrder (fname <> ": acct decls           : ")
      <&> journalRenumberAccountDeclarations
      <&> dbgJournalAcctDeclOrder (fname <> ": acct decls renumbered: ")

-- | Apply any auto posting rules to generate extra postings on this journal's transactions.
-- With a true first argument, adds visible tags to generated postings and modified transactions.
journalAddAutoPostings :: Bool -> Day -> BalancingOpts -> Journal -> Either String Journal
journalAddAutoPostings verbosetags d bopts =
    -- Balance all transactions without checking balance assertions,
    journalBalanceTransactions bopts{ignore_assertions_=True}
    -- then add the auto postings
    -- (Note adding auto postings after balancing means #893b fails;
    -- adding them before balancing probably means #893a, #928, #938 fail.)
    >=> journalModifyTransactions verbosetags d

-- | Generate periodic transactions from all periodic transaction rules in the journal.
-- These transactions are added to the in-memory Journal (but not the on-disk file).
--
-- The start & end date for generated periodic transactions are determined in
-- a somewhat complicated way; see the hledger manual -> Periodic transactions.
journalAddForecast :: Bool -> Maybe DateSpan -> Journal -> Journal
journalAddForecast _ Nothing j = j
journalAddForecast verbosetags (Just forecastspan) j = j{jtxns = jtxns j ++ forecasttxns}
  where
    {-# HLINT ignore "Move concatMap out" #-}
    forecasttxns =
        map (txnTieKnot . transactionTransformPostings (styleAmounts $ journalCommodityStyles j))
      . filter (spanContainsDate forecastspan . tdate)
      . concatMap (\pt -> runPeriodicTransaction verbosetags pt forecastspan)
      $ jperiodictxns j

setYear :: Year -> JournalParser m ()
setYear y = modify' (\j -> j{jparsedefaultyear=Just y})

getYear :: JournalParser m (Maybe Year)
getYear = fmap jparsedefaultyear get

dp :: String -> TextParser m ()
dp = const $ return ()  -- no-op
-- dp = dbgparse 1  -- trace parse state at this --debug level

-- | Get the decimal mark that has been specified for parsing, if any
-- (eg by the CSV decimal-mark rule, or possibly a future journal directive).
-- Return it as an AmountStyle that amount parsers can use.
getDecimalMarkStyle :: JournalParser m (Maybe AmountStyle)
getDecimalMarkStyle = do
  Journal{jparsedecimalmark} <- get
  let mdecmarkStyle = (\c -> Just $ amountstyle{asdecimalmark=Just c}) =<< jparsedecimalmark
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
  Journal{jdeclaredcommodities} <- get
  let mspecificStyle = M.lookup commodity jdeclaredcommodities >>= cformat
  mdefaultStyle <- fmap snd <$> getDefaultCommodityAndStyle
  return $ listToMaybe $ catMaybes [mspecificStyle, mdefaultStyle]

addDeclaredAccountTags :: AccountName -> [Tag] -> JournalParser m ()
addDeclaredAccountTags acct atags =
  modify' (\j -> j{jdeclaredaccounttags = M.insertWith (flip union) acct atags (jdeclaredaccounttags j)})

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
  (!txt, p') <- match p
  pure (txt, p')

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

-- | Parse possibly empty text until a semicolon or newline.
-- Whitespace is preserved (for now - perhaps helps preserve alignment 
-- of same-line comments ?).
descriptionp :: TextParser m Text
descriptionp = noncommenttextp <?> "description"

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
      Right m -> partialDate startOffset mYear m d2
    <?> "full or partial date"
  where
    fullDate :: Int -> Year -> Char -> Month -> TextParser m Day
    fullDate startOffset year sep month = do
      sep2 <- satisfy isDateSepChar <?> "date separator"
      day <- decimal <?> "day"
      endOffset <- getOffset
      when (sep /= sep2) $ 
        customFailure $ parseErrorAtRegion startOffset endOffset $
          "This date has different separators, please use consistent separators."
      case fromGregorianValid year month day of
        Nothing -> 
          customFailure $ parseErrorAtRegion startOffset endOffset $
            "This is not a valid date, please fix it."
        Just date -> pure $! date

    partialDate :: Int -> Maybe Year -> Month -> MonthDay -> TextParser m Day
    partialDate startOffset myr month day = do
      endOffset <- getOffset
      case myr of
        Just year ->
          case fromGregorianValid year month day of
            Nothing -> 
              customFailure $ parseErrorAtRegion startOffset endOffset $
                "This is not a valid date, please fix it."
            Just date -> pure $! date

        Nothing ->
          customFailure $ parseErrorAtRegion startOffset endOffset $
            "This partial date can not be parsed because the current year is unknown.\n"
            ++"Please make it a full date, or add a default year directive."

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
  als     <- getAccountAliases
  -- off1    <- getOffset
  a       <- lift accountnamep
  -- off2    <- getOffset
  -- XXX or accountNameApplyAliasesMemo ? doesn't seem to make a difference (retest that function)
  case accountNameApplyAliases als $ joinAccountNames parent a of
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
accountnamep = singlespacedtext1p

-- | Parse a single line of possibly empty text enclosed in double quotes.
doublequotedtextp :: TextParser m Text
doublequotedtextp = between (char '"') (char '"') $
  takeWhileP Nothing $ \c -> not $ isNewline c || c == '"'

-- | Parse possibly empty text, including whitespace, 
-- until a comment start (semicolon) or newline.
noncommenttextp :: TextParser m T.Text
noncommenttextp = takeWhileP Nothing (\c -> not $ isSameLineCommentStart c || isNewline c)

-- | Parse non-empty text, including whitespace, 
-- until a comment start (semicolon) or newline.
noncommenttext1p :: TextParser m T.Text
noncommenttext1p = takeWhile1P Nothing (\c -> not $ isSameLineCommentStart c || isNewline c)

-- | Parse non-empty, single-spaced text starting and ending with non-whitespace,
-- until a double space or newline.
singlespacedtext1p :: TextParser m T.Text
singlespacedtext1p = singlespacedtextsatisfying1p (const True)

-- | Parse non-empty, single-spaced text starting and ending with non-whitespace,
-- until a comment start (semicolon), double space, or newline.
singlespacednoncommenttext1p :: TextParser m T.Text
singlespacednoncommenttext1p = singlespacedtextsatisfying1p (not . isSameLineCommentStart)

-- | Parse non-empty, single-spaced text starting and ending with non-whitespace,
-- where all characters satisfy the given predicate.
singlespacedtextsatisfying1p :: (Char -> Bool) -> TextParser m T.Text
singlespacedtextsatisfying1p f = do
  firstPart <- partp
  otherParts <- many $ try $ singlespacep *> partp
  pure $! T.unwords $ firstPart : otherParts
  where
    partp = takeWhile1P Nothing (\c -> f c && not (isSpace c))

-- | Parse one non-newline whitespace character that is not followed by another one.
singlespacep :: TextParser m ()
singlespacep = spacenonewline *> notFollowedBy spacenonewline

--- *** amounts

-- | Parse whitespace then an amount, or return the special "missing" marker amount.
spaceandamountormissingp :: JournalParser m MixedAmount
spaceandamountormissingp =
  option missingmixedamt $ try $ do
    lift $ skipNonNewlineSpaces1
    mixedAmount <$> amountp

-- | Parse a single-commodity amount, applying the default commodity if there is no commodity symbol;
-- optionally followed by, in any order:
-- a Ledger-style cost, Ledger-style valuation expression, and/or Ledger-style cost basis, which is one or more of
-- lot cost, lot date, and/or lot note (we loosely call this triple the lot's cost basis).
-- The cost basis makes it a lot rather than just an amount. Both cost basis info and valuation expression
-- are discarded for now.
-- The main amount's sign is significant; here are the possibilities and their interpretation.
-- Also imagine an optional VALUATIONEXPR added to any of these (omitted for clarity):
-- @
--
--   AMT                         -- acquiring an amount
--   AMT COST                    -- acquiring an amount at some cost
--   AMT COST COSTBASIS          -- acquiring a lot at some cost, saving its cost basis
--   AMT COSTBASIS COST          -- like the above
--   AMT COSTBASIS               -- like the above with cost same as the cost basis
--
--  -AMT                         -- releasing an amount
--  -AMT SELLPRICE               -- releasing an amount at some selling price
--  -AMT SELLPRICE COSTBASISSEL  -- releasing a lot at some selling price, selecting it by its cost basis
--  -AMT COSTBASISSEL SELLPRICE  -- like the above
--  -AMT COSTBASISSEL            -- like the above with selling price same as the selected lot's cost basis amount
--
-- COST/SELLPRICE can be @ UNITAMT, @@ TOTALAMT, (@) UNITAMT, or (@@) TOTALAMT. The () are ignored.
-- COSTBASIS    is one or more of {LOTCOST}, [LOTDATE], (LOTNOTE), in any order, with LOTCOST defaulting to COST.
-- COSTBASISSEL is one or more of {LOTCOST}, [LOTDATE], (LOTNOTE), in any order.
-- {LOTCOST} can be {UNITAMT}, {{TOTALAMT}}, {=UNITAMT}, or {{=TOTALAMT}}. The = is ignored.
-- VALUATIONEXPR can be ((VALUE AMOUNT)) or ((VALUE FUNCTION)).
--
-- @
-- Ledger amount syntax is really complex.
-- Rule of thumb: curly braces, parentheses, and/or square brackets
-- in an amount means a Ledger-style cost basis is involved.
--
-- To parse an amount's numeric quantity we need to know which character 
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
--    (This means we handle files with any supported decimal mark without configuration,
--    but it also allows different decimal marks in  different amounts,
--    which is a bit too loose. There's an open issue.)
--
amountp :: JournalParser m Amount
amountp = amountp' False

-- An amount with optional cost, valuation, and/or cost basis, as described above.
-- A flag indicates whether we are parsing a multiplier amount;
-- if not, a commodity-less amount will have the default commodity applied to it.
amountp' :: Bool -> JournalParser m Amount
amountp' mult =
  -- dbg "amountp'" $ 
  label "amount" $ do
  let spaces = lift $ skipNonNewlineSpaces
  amt <- simpleamountp mult <* spaces
  (mcost, _valuationexpr, _mlotcost, _mlotdate, _mlotnote) <- runPermutation $
    -- costp, valuationexprp, lotnotep all parse things beginning with parenthesis, try needed
    (,,,,) <$> toPermutationWithDefault Nothing (Just <$> try (costp amt) <* spaces)
          <*> toPermutationWithDefault Nothing (Just <$> valuationexprp <* spaces)  -- XXX no try needed here ?
          <*> toPermutationWithDefault Nothing (Just <$> lotcostp <* spaces)
          <*> toPermutationWithDefault Nothing (Just <$> lotdatep <* spaces)
          <*> toPermutationWithDefault Nothing (Just <$> lotnotep <* spaces)
  pure $ amt { acost = mcost }

-- An amount with optional cost, but no cost basis.
amountnobasisp :: JournalParser m Amount
amountnobasisp =
  -- dbg "amountnobasisp" $ 
  label "amount" $ do
  let spaces = lift $ skipNonNewlineSpaces
  amt <- simpleamountp False
  spaces
  mprice <- optional $ costp amt <* spaces
  pure $ amt { acost = mprice }

-- An amount with no cost or cost basis.
-- A flag indicates whether we are parsing a multiplier amount;
-- if not, a commodity-less amount will have the default commodity applied to it.
simpleamountp :: Bool -> JournalParser m Amount
simpleamountp mult = 
  -- dbg "simpleamountp" $
  do
  sign <- lift signp
  leftsymbolamountp sign <|> rightornosymbolamountp sign

  where
  -- An amount with commodity symbol on the left.
  leftsymbolamountp :: (Decimal -> Decimal) -> JournalParser m Amount
  leftsymbolamountp sign = label "amount" $ do
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
    let s = amountstyle{ascommodityside=L, ascommodityspaced=commodityspaced, asprecision=prec, asdecimalmark=mdec, asdigitgroups=mgrps}
    return nullamt{acommodity=c, aquantity=sign (sign2 q), astyle=s, acost=Nothing}

  -- An amount with commodity symbol on the right or no commodity symbol.
  -- A no-symbol amount will have the default commodity applied to it
  -- unless we are parsing a multiplier amount (*AMT).
  rightornosymbolamountp :: (Decimal -> Decimal) -> JournalParser m Amount
  rightornosymbolamountp sign = label "amount" $ do
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
        let s = amountstyle{ascommodityside=R, ascommodityspaced=commodityspaced, asprecision=prec, asdecimalmark=mdec, asdigitgroups=mgrps}
        return nullamt{acommodity=c, aquantity=sign q, astyle=s, acost=Nothing}
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
              _ -> ("", amountstyle{asprecision=prec, asdecimalmark=mdec, asdigitgroups=mgrps})
        return nullamt{acommodity=c, aquantity=sign q, astyle=s, acost=Nothing}

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

-- | Try to parse a single-commodity amount from a string
parseamount :: String -> Either HledgerParseErrors Amount
parseamount s = runParser (evalStateT (amountp <* eof) nulljournal) "" (T.pack s)

-- | Parse a single-commodity amount from a string, or get an error.
parseamount' :: String -> Amount
parseamount' s =
  case parseamount s of
    Right amt -> amt
    Left err  -> error' $ show err  -- PARTIAL: XXX should throwError

-- | Like parseamount', but returns a MixedAmount.
parsemixedamount :: String -> Either HledgerParseErrors MixedAmount
parsemixedamount = fmap mixedAmount . parseamount

-- | Like parseamount', but returns a MixedAmount.
parsemixedamount' :: String -> MixedAmount
parsemixedamount' = mixedAmount . parseamount'

-- | Parse a minus or plus sign followed by zero or more spaces,
-- or nothing, returning a function that negates or does nothing.
signp :: Num a => TextParser m (a -> a)
signp = ((char '-' $> negate <|> char '+' $> id) <* skipNonNewlineSpaces) <|> pure id

commoditysymbolp :: TextParser m CommoditySymbol
commoditysymbolp =
  quotedcommoditysymbolp <|> simplecommoditysymbolp <?> "commodity symbol"

quotedcommoditysymbolp :: TextParser m CommoditySymbol
quotedcommoditysymbolp =
  between (char '"') (char '"') $ takeWhileP Nothing f
  where f c = c /= ';' && c /= '\n' && c /= '\"'

simplecommoditysymbolp :: TextParser m CommoditySymbol
simplecommoditysymbolp = takeWhile1P Nothing (not . isNonsimpleCommodityChar)

-- | Ledger-style cost notation:
-- @ UNITAMT, @@ TOTALAMT, (@) UNITAMT, or (@@) TOTALAMT. The () are ignored.
costp :: Amount -> JournalParser m AmountCost
costp baseAmt =
  -- dbg "costp" $
  label "transaction price" $ do
  -- https://www.ledger-cli.org/3.0/doc/ledger3.html#Virtual-posting-costs
  parenthesised <- option False $ char '(' >> pure True
  char '@'
  totalCost <- char '@' $> True <|> pure False
  when parenthesised $ void $ char ')'

  lift skipNonNewlineSpaces
  priceAmount <- simpleamountp False -- <?> "unpriced amount (specifying a price)"

  let amtsign' = signum $ aquantity baseAmt
      amtsign  = if amtsign' == 0 then 1 else amtsign'

  pure $ if totalCost
            then TotalCost priceAmount{aquantity=amtsign * aquantity priceAmount}
            else UnitCost  priceAmount

-- | A valuation function or value can be written in double parentheses after an amount.
valuationexprp :: JournalParser m ()
valuationexprp =
  -- dbg "valuationexprp" $
  label "valuation expression" $ do
  string "(("
  _ <- T.strip . T.pack <$> (many $ noneOf [')','\n'])  -- XXX other line endings ?
  string "))"
  return ()

balanceassertionp :: JournalParser m BalanceAssertion
balanceassertionp = do
  sourcepos <- getSourcePos
  char '='
  istotal <- fmap isJust $ optional $ try $ char '='
  isinclusive <- fmap isJust $ optional $ try $ char '*'
  lift skipNonNewlineSpaces
  -- this amount can have a cost, but not a cost basis.
  -- balance assertions ignore it, but balance assignments will use it
  a <- amountnobasisp <?> "amount (for a balance assertion or assignment)"
  return BalanceAssertion
    { baamount    = a
    , batotal     = istotal
    , bainclusive = isinclusive
    , baposition  = sourcepos
    }

-- Parse a Ledger-style lot cost,
-- {UNITCOST} or {{TOTALCOST}} or {=FIXEDUNITCOST} or {{=FIXEDTOTALCOST}},
-- and discard it.
lotcostp :: JournalParser m ()
lotcostp =
  -- dbg "lotcostp" $
  label "ledger-style lot cost" $ do
  char '{'
  doublebrace <- option False $ char '{' >> pure True
  lift skipNonNewlineSpaces
  _fixed <- fmap isJust $ optional $ char '='
  lift skipNonNewlineSpaces
  _a <- simpleamountp False
  lift skipNonNewlineSpaces
  char '}'
  when (doublebrace) $ void $ char '}'

-- Parse a Ledger-style [LOTDATE], and discard it.
lotdatep :: JournalParser m ()
lotdatep =
  -- dbg "lotdatep" $
  label "ledger-style lot date" $ do
  char '['
  lift skipNonNewlineSpaces
  _d <- datep
  lift skipNonNewlineSpaces
  char ']'
  return ()

-- Parse a Ledger-style (LOT NOTE), and discard it.
lotnotep :: JournalParser m ()
lotnotep =
  -- dbg "lotnotep" $
  label "ledger-style lot note" $ do
  char '('
  lift skipNonNewlineSpaces
  _note <- stripEnd . T.pack <$> (many $ noneOf [')','\n'])  -- XXX other line endings ?
  char ')'
  return ()

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
fromRawNumber (WithSeparators{}) (Just _) =
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
      AmountStyle{asdecimalmark = Just d} -> d == c
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
isDigitSeparatorChar c = isDecimalMark c || isDigitSeparatorSpaceChar c

-- | Kinds of unicode space character we accept as digit group marks.
-- See also https://en.wikipedia.org/wiki/Decimal_separator#Digit_grouping .
isDigitSeparatorSpaceChar :: Char -> Bool
isDigitSeparatorSpaceChar c =
     c == ' '  -- space
  || c == ' '  -- no-break space
  || c == ' '  -- en space
  || c == ' '  -- em space
  || c == ' '  -- punctuation space
  || c == ' '  -- thin space
  || c == ' '  -- narrow no-break space
  || c == ' '  -- medium mathematical space

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
  show (DigitGrp len n) = "\"" ++ padding ++ numStr ++ "\""
    where numStr = show n
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
    anyLine = void $ takeWhileP Nothing (/='\n') *> newline

{-# INLINABLE multilinecommentp #-}

-- | A blank or comment line in journal format: a line that's empty or
-- containing only whitespace or whose first non-whitespace character
-- is semicolon, hash, or star. See also emptyorcommentlinep2.
emptyorcommentlinep :: TextParser m ()
emptyorcommentlinep = do
  dp "emptyorcommentlinep"
  skipNonNewlineSpaces
  skiplinecommentp <|> void newline
  where
    skiplinecommentp :: TextParser m ()
    skiplinecommentp = do
      satisfy isLineCommentStart
      void $ takeWhileP Nothing (/= '\n')
      optional newline
      pure ()

{-# INLINABLE emptyorcommentlinep #-}

-- | A newer comment line parser.
-- Parses a line which is empty, all blanks, or whose first non-blank character is one of those provided.
emptyorcommentlinep2 :: [Char] -> TextParser m ()
emptyorcommentlinep2 cs =
  label ("empty line or comment line beginning with "++cs) $ do
    dp "emptyorcommentlinep2"
    skipNonNewlineSpaces
    void newline <|> void commentp
    where
      commentp = do
        choice (map (some.char) cs)
        takeWhileP Nothing (/='\n') <* newline

-- | Is this a character that, as the first non-whitespace on a line,
-- starts a comment line ?
isLineCommentStart :: Char -> Bool
isLineCommentStart '#' = True
isLineCommentStart '*' = True
isLineCommentStart ';' = True
isLineCommentStart _   = False

-- | Is this a character that, appearing anywhere within a line,
-- starts a comment ?
isSameLineCommentStart :: Char -> Bool
isSameLineCommentStart ';' = True
isSameLineCommentStart _   = False

-- | Parse a comment following a journal item, possibly continued on multiple lines,
-- and return the comment text.
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
  fst <$> followingcommentpWith (void $ takeWhileP Nothing (/= '\n'))  -- XXX support \r\n ?

{-# INLINABLE followingcommentp #-}

-- | Parse a following comment, possibly continued on multiple lines,
-- using the provided line parser to parse each line.
-- This returns the comment text, and the combined results from the line parser.
--
-- Following comments begin with a semicolon and extend to the end of the line.
-- They can optionally be continued on the next lines,
-- where each next line begins with an indent and another semicolon.
-- (This parser expects to see these semicolons and indents.)
--
-- Like Ledger, we sometimes allow data to be embedded in comments.
-- account directive comments and transaction comments can contain tags,
-- and posting comments can contain tags or bracketed posting dates.
-- This helper lets us handle these variations. 
-- The line parser should consume all input up until the next newline.
-- See followingcommentp for some tests.
--
followingcommentpWith :: (Monoid a, Show a) => TextParser m a -> TextParser m (Text, a)
followingcommentpWith contentp = do
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

{-# INLINABLE followingcommentpWith #-}


-- Parse the tags from a single comment line, eg for use with followingcommentpWith.
-- XXX what part of a comment line ? leading whitespace / semicolon or not ?
commentlinetagsp :: TextParser m [Tag]
commentlinetagsp = do
  -- XXX sketchy
  tagName <- (last . T.split isSpace) <$> takeWhileP Nothing (\c -> c /= ':' && c /= '\n')
  atColon tagName <|> pure [] -- if not ':', then either '\n' or EOF

  where
    atColon :: Text -> TextParser m [Tag]
    atColon name = char ':' *> do
      if T.null name
        then commentlinetagsp
        else do
          skipNonNewlineSpaces
          val <- tagValue
          let tag = (name, val)
          (tag:) <$> commentlinetagsp

    tagValue :: TextParser m Text
    tagValue = do
      val <- T.strip <$> takeWhileP Nothing (\c -> c /= ',' && c /= '\n')
      _ <- optional $ char ','
      pure val

{-# INLINABLE commentlinetagsp #-}


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
transactioncommentp = followingcommentpWith commentlinetagsp
{-# INLINABLE transactioncommentp #-}


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
    followingcommentpWith (commenttagsanddatesp mYear)
  let mdate  = snd <$> find ((=="date") .fst) dateTags
      mdate2 = snd <$> find ((=="date2").fst) dateTags
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
-- Left ...1:2:...This is not a valid date...
--
-- >>> either (Left . customErrorBundlePretty) Right $ rtp (bracketeddatetagsp Nothing) "[1/31]"
-- Left ...1:2:...This partial date can not be parsed because the current year is unknown...
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
  (off1, off2, re) <- between (char '/') (char '/') $ do
    off1 <- getOffset
    re <- fmap T.concat . some $
             (T.singleton <$> noneOf ("/\\\n\r" :: [Char]))               -- paranoid: don't try to read past line end
             <|> string "\\/"                                             -- allow escaping forward slashes
             <|> (liftM2 T.cons (char '\\') (T.singleton <$> anySingle))  -- Otherwise leave backslashes in
    off2 <- getOffset
    return (off1, off2, re)
  skipNonNewlineSpaces
  char '='
  skipNonNewlineSpaces
  repl <- anySingle `manyTill` eolof
  case toRegexCI re of
    Right r -> return $! RegexAlias r repl
    Left e  -> customFailure $! parseErrorAtRegion off1 off2 e

--- ** tests

tests_Common = testGroup "Common" [

   testGroup "amountp" [
    testCase "basic"                  $ assertParseEq amountp "$47.18"     (usd 47.18)
   ,testCase "ends with decimal mark" $ assertParseEq amountp "$1."        (usd 1  `withPrecision` Precision 0)
   ,testCase "unit price"             $ assertParseEq amountp "$10 @ €0.5"
      -- not precise enough:
      -- (usd 10 `withPrecision` 0 `at` (eur 0.5 `withPrecision` 1)) -- `withStyle` asdecimalmark=Just '.'
      nullamt{
         acommodity="$"
        ,aquantity=10 -- need to test internal precision with roundTo ? I think not
        ,astyle=amountstyle{asprecision=Precision 0, asdecimalmark=Nothing}
        ,acost=Just $ UnitCost $
          nullamt{
             acommodity="€"
            ,aquantity=0.5
            ,astyle=amountstyle{asprecision=Precision 1, asdecimalmark=Just '.'}
            }
        }
   ,testCase "total price"            $ assertParseEq amountp "$10 @@ €5"
      nullamt{
         acommodity="$"
        ,aquantity=10
        ,astyle=amountstyle{asprecision=Precision 0, asdecimalmark=Nothing}
        ,acost=Just $ TotalCost $
          nullamt{
             acommodity="€"
            ,aquantity=5
            ,astyle=amountstyle{asprecision=Precision 0, asdecimalmark=Nothing}
            }
        }
   ,testCase "unit price, parenthesised" $ assertParse amountp "$10 (@) €0.5"
   ,testCase "total price, parenthesised" $ assertParse amountp "$10 (@@) €0.5"
   ]

  ,let p = lift (numberp Nothing) :: JournalParser IO (Quantity, Word8, Maybe Char, Maybe DigitGroupStyle) in
   testCase "numberp" $ do
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

  ,testGroup "spaceandamountormissingp" [
     testCase "space and amount" $ assertParseEq spaceandamountormissingp " $47.18" (mixedAmount $ usd 47.18)
    ,testCase "empty string" $ assertParseEq spaceandamountormissingp "" missingmixedamt
    -- ,testCase "just space" $ assertParseEq spaceandamountormissingp " " missingmixedamt  -- XXX should it ?
    -- ,testCase "just amount" $ assertParseError spaceandamountormissingp "$47.18" ""  -- succeeds, consuming nothing
    ]

  ]


