-- {-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE CPP, RecordWildCards, NoMonoLocalBinds, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
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

-}

module Hledger.Read.JournalReader (
  -- * Reader
  reader,
  -- * Parsers used elsewhere
  parseJournalWith,
  genericSourcePos,
  getParentAccount,
  journal,
  directive,
  defaultyeardirective,
  marketpricedirective,
  datetimep,
  codep,
  accountnamep,
  modifiedaccountnamep,
  postingp,
  amountp,
  amountp',
  mamountp',
  numberp,
  statusp,
  emptyorcommentlinep,
  followingcommentp,
  accountaliasp
  -- * Tests
  ,tests_Hledger_Read_JournalReader
#ifdef TESTS
  -- disabled by default, HTF not available on windows
  ,htf_thisModulesTests
  ,htf_Hledger_Read_JournalReader_importedTests
#endif
)
where
import Prelude ()
import Prelude.Compat hiding (readFile)
import qualified Control.Exception as C
import Control.Monad.Compat
import Control.Monad.Except (ExceptT(..), liftIO, runExceptT, throwError, catchError, mapExceptT)
import Control.Monad.State (MonadState, MonadIO, StateT, get, put, modify', evalState, evalStateT)
import Data.Char (isNumber)
import Data.List.Compat
import Data.List.Split (wordsBy)
import Data.Maybe
import Data.Time.Calendar
import Data.Time.LocalTime
import Safe (headDef, lastDef)
import Test.HUnit
#ifdef TESTS
import Test.Framework
import Text.Megaparsec.Error
#endif
import Text.Megaparsec hiding (parse)
import Text.Printf
import System.FilePath
import System.Time (getClockTime)

import Hledger.Data
import Hledger.Utils

hideStateT :: Monad m => ExceptT e (StateT s m) a -> s -> ExceptT e m a
hideStateT x s = mapExceptT (\p -> evalStateT p s) x

-- standard reader exports

reader :: Reader
reader = Reader format detect parse

format :: String
format = "journal"

-- | Does the given file path and data look like it might be hledger's journal format ?
detect :: FilePath -> String -> Bool
detect f s
  | f /= "-"  = takeExtension f `elem` ['.':format, ".j"]  -- from a file: yes if the extension is .journal or .j
  -- from stdin: yes if we can see something that looks like a journal entry (digits in column 0 with the next line indented)
  | otherwise = regexMatches "^[0-9]+.*\n[ \t]+" s

-- | Parse and post-process a "Journal" from hledger's journal file
-- format, or give an error.
parse :: Maybe FilePath -> Bool -> FilePath -> String -> ExceptT String IO Journal
parse _ = parseJournalWith journal

-- parsing utils

genericSourcePos :: SourcePos -> GenericSourcePos
genericSourcePos p = GenericSourcePos (sourceName p) (sourceLine p) (sourceColumn p)

-- | Flatten a list of JournalUpdate's (journal-transforming
-- monadic actions which can do IO or raise an exception) into a
-- single equivalent action.
combineJournalUpdates :: [JournalUpdate] -> JournalUpdate
combineJournalUpdates us = foldl' (flip (.)) id <$> sequence us
-- XXX may be contributing to excessive stack use

-- cf http://neilmitchell.blogspot.co.uk/2015/09/detecting-space-leaks.html
-- $ ./devprof +RTS -K576K -xc
-- *** Exception (reporting due to +RTS -xc): (THUNK_STATIC), stack trace: 
--   Hledger.Read.JournalReader.combineJournalUpdates.\,
--   called from Hledger.Read.JournalReader.combineJournalUpdates,
--   called from Hledger.Read.JournalReader.fixedlotprice,
--   called from Hledger.Read.JournalReader.partialbalanceassertion,
--   called from Hledger.Read.JournalReader.getDefaultCommodityAndStyle,
--   called from Hledger.Read.JournalReader.priceamount,
--   called from Hledger.Read.JournalReader.nosymbolamount,
--   called from Hledger.Read.JournalReader.numberp,
--   called from Hledger.Read.JournalReader.rightsymbolamount,
--   called from Hledger.Read.JournalReader.simplecommoditysymbol,
--   called from Hledger.Read.JournalReader.quotedcommoditysymbol,
--   called from Hledger.Read.JournalReader.commoditysymbol,
--   called from Hledger.Read.JournalReader.signp,
--   called from Hledger.Read.JournalReader.leftsymbolamount,
--   called from Hledger.Read.JournalReader.amountp,
--   called from Hledger.Read.JournalReader.spaceandamountormissing,
--   called from Hledger.Read.JournalReader.accountnamep.singlespace,
--   called from Hledger.Utils.Parse.nonspace,
--   called from Hledger.Read.JournalReader.accountnamep,
--   called from Hledger.Read.JournalReader.getAccountAliases,
--   called from Hledger.Read.JournalReader.getParentAccount,
--   called from Hledger.Read.JournalReader.modifiedaccountnamep,
--   called from Hledger.Read.JournalReader.postingp,
--   called from Hledger.Read.JournalReader.postings,
--   called from Hledger.Read.JournalReader.commentStartingWith,
--   called from Hledger.Read.JournalReader.semicoloncomment,
--   called from Hledger.Read.JournalReader.followingcommentp,
--   called from Hledger.Read.JournalReader.descriptionp,
--   called from Hledger.Read.JournalReader.codep,
--   called from Hledger.Read.JournalReader.statusp,
--   called from Hledger.Utils.Parse.spacenonewline,
--   called from Hledger.Read.JournalReader.secondarydatep,
--   called from Hledger.Data.Dates.datesepchar,
--   called from Hledger.Read.JournalReader.datep,
--   called from Hledger.Read.JournalReader.transaction,
--   called from Hledger.Utils.Parse.choice',
--   called from Hledger.Read.JournalReader.directive,
--   called from Hledger.Read.JournalReader.emptyorcommentlinep,
--   called from Hledger.Read.JournalReader.multilinecommentp,
--   called from Hledger.Read.JournalReader.journal.journalItem,
--   called from Hledger.Read.JournalReader.journal,
--   called from Hledger.Read.JournalReader.parseJournalWith,
--   called from Hledger.Read.readJournal.tryReaders.firstSuccessOrBestError,
--   called from Hledger.Read.readJournal.tryReaders,
--   called from Hledger.Read.readJournal,
--   called from Main.main,
--   called from Main.CAF
-- Stack space overflow: current size 33568 bytes.

-- | Given a JournalUpdate-generating parsec parser, file path and data string,
-- parse and post-process a Journal so that it's ready to use, or give an error.
parseJournalWith :: (ParsecT [Char] (ExceptT String (StateT JournalContext IO)) (JournalUpdate,JournalContext)) -> Bool -> FilePath -> String -> ExceptT String IO Journal
parseJournalWith p assrt f s = do
  tc <- liftIO getClockTime
  tl <- liftIO getCurrentLocalTime
  y <- liftIO getCurrentYear
  r <- hideStateT (runParserT p f s) nullctx{ctxYear=Just y}
  case r of
    Right (updates,ctx) -> do
                           j <- updates `ap` return nulljournal
                           case journalFinalise tc tl f s ctx assrt j of
                             Right j'  -> return j'
                             Left estr -> throwError estr
    Left e -> throwError $ show e

setYear :: (Stream [Char] Char, MonadState JournalContext m)  => Integer -> ParsecT [Char] m ()
setYear y = modify' (\ctx -> ctx{ctxYear=Just y})

getYear :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT s m (Maybe Integer)
getYear = liftM ctxYear get

setDefaultCommodityAndStyle :: (Stream [Char] Char, MonadState JournalContext m) => (Commodity,AmountStyle) -> ParsecT [Char] m ()
setDefaultCommodityAndStyle cs = modify' (\ctx -> ctx{ctxDefaultCommodityAndStyle=Just cs})

getDefaultCommodityAndStyle :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m (Maybe (Commodity,AmountStyle))
getDefaultCommodityAndStyle = ctxDefaultCommodityAndStyle `fmap` get

pushParentAccount :: (Stream [Char] Char, MonadState JournalContext m) => String -> ParsecT [Char] m ()
pushParentAccount parent = modify' addParentAccount
    where addParentAccount ctx0 = ctx0 { ctxAccount = parent : ctxAccount ctx0 }

popParentAccount :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m ()
popParentAccount = do ctx0 <- get
                      case ctxAccount ctx0 of
                        [] -> unexpected "End of account block with no beginning"
                        (_:rest) -> put $ ctx0 { ctxAccount = rest }

getParentAccount :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m String
getParentAccount = liftM (concatAccountNames . reverse . ctxAccount) get

addAccountAlias :: (Stream [Char] Char, MonadState JournalContext m) => AccountAlias -> ParsecT [Char] m ()
addAccountAlias a = modify' (\(ctx@Ctx{..}) -> ctx{ctxAliases=a:ctxAliases})

getAccountAliases :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m [AccountAlias]
getAccountAliases = liftM ctxAliases get

clearAccountAliases :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m ()
clearAccountAliases = modify' (\(ctx@Ctx{..}) -> ctx{ctxAliases=[]})

-- parsers

-- | Top-level journal parser. Returns a single composite, I/O performing,
-- error-raising "JournalUpdate" (and final "JournalContext") which can be
-- applied to an empty journal to get the final result.
journal :: ParsecT [Char] (ExceptT String (StateT JournalContext IO)) (JournalUpdate,JournalContext)
journal = do
  journalupdates <- many journalItem
  eof
  finalctx <- get
  return $ (combineJournalUpdates journalupdates, finalctx)
    where
      -- As all journal line types can be distinguished by the first
      -- character, excepting transactions versus empty (blank or
      -- comment-only) lines, can use choice w/o try
      journalItem = choice [ directive
                           , liftM (return . addTransaction) transaction
                           , liftM (return . addModifierTransaction) modifiertransaction
                           , liftM (return . addPeriodicTransaction) periodictransaction
                           , liftM (return . addMarketPrice) marketpricedirective
                           , emptyorcommentlinep >> return (return id)
                           , multilinecommentp >> return (return id)
                           ] <?> "journal transaction or directive"

-- cf http://ledger-cli.org/3.0/doc/ledger3.html#Command-Directives
directive :: (MonadState JournalContext m, MonadIO m) => ParsecT [Char] (ExceptT String m) JournalUpdate
directive = do
  optional $ char '!'
  choice' [
    includedirective
   ,aliasdirective
   ,endaliasesdirective
   ,accountdirective
   ,enddirective
   ,tagdirective
   ,endtagdirective
   ,defaultyeardirective
   ,defaultcommoditydirective
   ,commodityconversiondirective
   ,ignoredpricecommoditydirective
   ]
  <?> "directive"

includedirective :: (MonadState JournalContext m, MonadIO m) => ParsecT [Char] (ExceptT String m) JournalUpdate
includedirective = do
  string "include"
  some spacenonewline
  filename <- restofline
  outerState <- get
  outerPos <- getPosition
  let curdir = takeDirectory (sourceName outerPos)
  let (u::ExceptT String IO (Journal -> Journal, JournalContext)) = do
       filepath <- expandPath curdir filename
       txt <- readFileOrError outerPos filepath
       let inIncluded = show outerPos ++ " in included file " ++ show filename ++ ":\n"
       r <- hideStateT (runParserT journal filepath txt) outerState
       case r of
         Right (ju, ctx) -> do
                            u <- combineJournalUpdates [ return $ journalAddFile (filepath,txt)
                                                       , ju
                                                       ] `catchError` (throwError . (inIncluded ++))
                            return (u, ctx)
         Left err -> throwError $ inIncluded ++ show err
       where readFileOrError pos fp =
                ExceptT $ liftM Right (readFile' fp) `C.catch`
                  \e -> return $ Left $ printf "%s reading %s:\n%s" (show pos) fp (show (e::C.IOException))
  r <- liftIO $ runExceptT u
  case r of
    Left err -> return $ throwError err
    Right (ju, _finalparsectx) -> return $ ExceptT $ return $ Right ju

journalAddFile :: (FilePath,String) -> Journal -> Journal
journalAddFile f j@Journal{files=fs} = j{files=fs++[f]}
 -- NOTE: first encountered file to left, to avoid a reverse

accountdirective :: (MonadState JournalContext m, MonadIO m) => ParsecT [Char] (ExceptT String m) JournalUpdate
accountdirective = do
  string "account"
  some spacenonewline
  parent <- accountnamep
  newline
  pushParentAccount parent
  -- return $ return id
  return $ ExceptT $ return $ Right id

enddirective :: (MonadState JournalContext m, MonadIO m) => ParsecT [Char] (ExceptT String m) JournalUpdate
enddirective = do
  string "end"
  popParentAccount
  -- return (return id)
  return $ ExceptT $ return $ Right id

aliasdirective :: (MonadState JournalContext m, MonadIO m) => ParsecT [Char] (ExceptT String m) JournalUpdate
aliasdirective = do
  string "alias"
  some spacenonewline
  alias <- accountaliasp
  addAccountAlias alias
  return $ return id

accountaliasp :: Stream [Char] Char => ParsecT [Char] m AccountAlias
accountaliasp = regexaliasp <|> basicaliasp

basicaliasp :: Stream [Char] Char => ParsecT [Char] m AccountAlias
basicaliasp = do
  -- pdbg 0 "basicaliasp"
  old <- rstrip <$> (some $ noneOf "=")
  char '='
  many spacenonewline
  new <- rstrip <$> anyChar `manyTill` eolof  -- don't require a final newline, good for cli options
  return $ BasicAlias old new

regexaliasp :: Stream [Char] Char => ParsecT [Char] m AccountAlias
regexaliasp = do
  -- pdbg 0 "regexaliasp"
  char '/'
  re <- some $ noneOf "/\n\r" -- paranoid: don't try to read past line end
  char '/'
  many spacenonewline
  char '='
  many spacenonewline
  repl <- rstrip <$> anyChar `manyTill` eolof
  return $ RegexAlias re repl

endaliasesdirective :: (MonadState JournalContext m, MonadIO m) => ParsecT [Char] (ExceptT String m) JournalUpdate
endaliasesdirective = do
  string "end aliases"
  clearAccountAliases
  return (return id)

tagdirective :: (MonadState JournalContext m, MonadIO m) => ParsecT [Char] (ExceptT String m) JournalUpdate
tagdirective = do
  string "tag" <?> "tag directive"
  some spacenonewline
  _ <- some nonspace
  restofline
  return $ return id

endtagdirective :: (MonadState JournalContext m, MonadIO m) => ParsecT [Char] (ExceptT String m) JournalUpdate
endtagdirective = do
  (string "end tag" <|> string "pop") <?> "end tag or pop directive"
  restofline
  return $ return id

defaultyeardirective :: (MonadState JournalContext m, MonadIO m) => ParsecT [Char] (ExceptT String m) JournalUpdate
defaultyeardirective = do
  char 'Y' <?> "default year"
  many spacenonewline
  y <- some digitChar
  let y' = read y
  failIfInvalidYear y
  setYear y'
  return $ return id

defaultcommoditydirective :: (MonadState JournalContext m, MonadIO m) => ParsecT [Char] (ExceptT String m) JournalUpdate
defaultcommoditydirective = do
  char 'D' <?> "default commodity"
  some spacenonewline
  Amount{..} <- amountp
  setDefaultCommodityAndStyle (acommodity, astyle)
  restofline
  return $ return id

marketpricedirective :: (MonadState JournalContext m, MonadIO m) => ParsecT [Char] (ExceptT String m) MarketPrice
marketpricedirective = do
  char 'P' <?> "market price"
  many spacenonewline
  date <- try (do {LocalTime d _ <- datetimep; return d}) <|> datep -- a time is ignored
  some spacenonewline
  symbol <- commoditysymbol
  many spacenonewline
  price <- amountp
  restofline
  return $ MarketPrice date symbol price

ignoredpricecommoditydirective :: (MonadState JournalContext m, MonadIO m) => ParsecT [Char] (ExceptT String m) JournalUpdate
ignoredpricecommoditydirective = do
  char 'N' <?> "ignored-price commodity"
  some spacenonewline
  commoditysymbol
  restofline
  return $ return id

commodityconversiondirective :: (MonadState JournalContext m, MonadIO m) => ParsecT [Char] (ExceptT String m) JournalUpdate
commodityconversiondirective = do
  char 'C' <?> "commodity conversion"
  some spacenonewline
  amountp
  many spacenonewline
  char '='
  many spacenonewline
  amountp
  restofline
  return $ return id

modifiertransaction :: ParsecT [Char] (ExceptT String (StateT JournalContext IO)) ModifierTransaction
modifiertransaction = do
  char '=' <?> "modifier transaction"
  many spacenonewline
  valueexpr <- restofline
  postings <- postings
  return $ ModifierTransaction valueexpr postings

periodictransaction :: ParsecT [Char] (ExceptT String (StateT JournalContext IO)) PeriodicTransaction
periodictransaction = do
  char '~' <?> "periodic transaction"
  many spacenonewline
  periodexpr <- restofline
  postings <- postings
  return $ PeriodicTransaction periodexpr postings

-- | Parse a (possibly unbalanced) transaction.
transaction :: ParsecT [Char] (ExceptT String (StateT JournalContext IO)) Transaction
transaction = do
  -- ptrace "transaction"
  sourcepos <- genericSourcePos <$> getPosition
  date <- datep <?> "transaction"
  edate <- optional (secondarydatep date) <?> "secondary date"
  lookAhead (spacenonewline <|> newline) <?> "whitespace or newline"
  status <- statusp <?> "cleared status"
  code <- codep <?> "transaction code"
  description <- descriptionp >>= return . strip
  comment <- try followingcommentp <|> (newline >> return "")
  let tags = tagsInComment comment
  postings <- postings
  return $ txnTieKnot $ Transaction sourcepos date edate status code description comment tags postings ""

descriptionp = many (noneOf ";\n")

#ifdef TESTS
test_transaction = do
    let s `gives` t = do
                        let p = parseWithCtx nullctx transaction s
                        assertBool $ isRight p
                        let Right t2 = p
                            -- same f = assertEqual (f t) (f t2)
                        assertEqual (tdate t) (tdate t2)
                        assertEqual (tdate2 t) (tdate2 t2)
                        assertEqual (tstatus t) (tstatus t2)
                        assertEqual (tcode t) (tcode t2)
                        assertEqual (tdescription t) (tdescription t2)
                        assertEqual (tcomment t) (tcomment t2)
                        assertEqual (ttags t) (ttags t2)
                        assertEqual (tpreceding_comment_lines t) (tpreceding_comment_lines t2)
                        assertEqual (show $ tpostings t) (show $ tpostings t2)
    -- "0000/01/01\n\n" `gives` nulltransaction
    unlines [
      "2012/05/14=2012/05/15 (code) desc  ; tcomment1",
      "    ; tcomment2",
      "    ; ttag1: val1",
      "    * a         $1.00  ; pcomment1",
      "    ; pcomment2",
      "    ; ptag1: val1",
      "    ; ptag2: val2"
      ]
     `gives`
     nulltransaction{
      tdate=parsedate "2012/05/14",
      tdate2=Just $ parsedate "2012/05/15",
      tstatus=Uncleared,
      tcode="code",
      tdescription="desc",
      tcomment=" tcomment1\n tcomment2\n ttag1: val1\n",
      ttags=[("ttag1","val1")],
      tpostings=[
        nullposting{
          pstatus=Cleared,
          paccount="a",
          pamount=Mixed [usd 1],
          pcomment=" pcomment1\n pcomment2\n ptag1: val1\n  ptag2: val2\n",
          ptype=RegularPosting,
          ptags=[("ptag1","val1"),("ptag2","val2")],
          ptransaction=Nothing
          }
        ],
      tpreceding_comment_lines=""
      }
    unlines [
      "2015/1/1",
      ]
     `gives`
     nulltransaction{
      tdate=parsedate "2015/01/01",
      }

    assertRight $ parseWithCtx nullctx transaction $ unlines
      ["2007/01/28 coopportunity"
      ,"    expenses:food:groceries                   $47.18"
      ,"    assets:checking                          $-47.18"
      ,""
      ]

    -- transaction should not parse just a date
    assertLeft $ parseWithCtx nullctx transaction "2009/1/1\n"

    -- transaction should not parse just a date and description
    assertLeft $ parseWithCtx nullctx transaction "2009/1/1 a\n"

    -- transaction should not parse a following comment as part of the description
    let p = parseWithCtx nullctx transaction "2009/1/1 a ;comment\n b 1\n"
    assertRight p
    assertEqual "a" (let Right p' = p in tdescription p')

    -- parse transaction with following whitespace line
    assertRight $ parseWithCtx nullctx transaction $ unlines
        ["2012/1/1"
        ,"  a  1"
        ,"  b"
        ," "
        ]

    let p = parseWithCtx nullctx transaction $ unlines
             ["2009/1/1 x  ; transaction comment"
             ," a  1  ; posting 1 comment"
             ," ; posting 1 comment 2"
             ," b"
             ," ; posting 2 comment"
             ]
    assertRight p
    assertEqual 2 (let Right t = p in length $ tpostings t)
#endif

-- | Parse a date in YYYY/MM/DD format.
-- Hyphen (-) and period (.) are also allowed as separators.
-- The year may be omitted if a default year has been set.
-- Leading zeroes may be omitted.
datep :: (MonadState JournalContext m) => Stream [Char] t => ParsecT [Char] m Day
datep = do
  -- hacky: try to ensure precise errors for invalid dates
  -- XXX reported error position is not too good
  -- pos <- genericSourcePos <$> getPosition
  datestr <- do
    c <- digitChar
    cs <- many $ choice' [digitChar, datesepchar]
    return $ c:cs
  let sepchars = nub $ sort $ filter (`elem` datesepchars) datestr
  when (length sepchars /= 1) $ fail $ "bad date, different separators used: " ++ datestr
  let dateparts = wordsBy (`elem` datesepchars) datestr
  currentyear <- getYear
  [y,m,d] <- case (dateparts,currentyear) of
              ([m,d],Just y)  -> return [show y,m,d]
              ([_,_],Nothing) -> fail $ "partial date "++datestr++" found, but the current year is unknown"
              ([y,m,d],_)     -> return [y,m,d]
              _               -> fail $ "bad date: " ++ datestr
  let maybedate = fromGregorianValid (read y) (read m) (read d)
  case maybedate of
    Nothing   -> fail $ "bad date: " ++ datestr
    Just date -> return date
  <?> "full or partial date"

-- | Parse a date and time in YYYY/MM/DD HH:MM[:SS][+-ZZZZ] format.
-- Hyphen (-) and period (.) are also allowed as date separators.
-- The year may be omitted if a default year has been set.
-- Seconds are optional.
-- The timezone is optional and ignored (the time is always interpreted as a local time).
-- Leading zeroes may be omitted (except in a timezone).
datetimep :: (MonadState JournalContext m) => Stream [Char] Char => ParsecT [Char] m LocalTime
datetimep = do
  day <- datep
  some spacenonewline
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
                   plusminus <- oneOf "-+"
                   d1 <- digitChar
                   d2 <- digitChar
                   d3 <- digitChar
                   d4 <- digitChar
                   return $ plusminus:d1:d2:d3:d4:""
  -- ltz <- liftIO $ getCurrentTimeZone
  -- let tz' = maybe ltz (fromMaybe ltz . parseTime defaultTimeLocale "%z") tz
  -- return $ localTimeToUTC tz' $ LocalTime day $ TimeOfDay h' m' (fromIntegral s')
  return $ LocalTime day $ TimeOfDay h' m' (fromIntegral s')

secondarydatep :: (Stream [Char] Char, MonadState JournalContext m) => Day -> ParsecT [Char] m Day
secondarydatep primarydate = do
  char '='
  -- kludgy way to use primary date for default year
  let withDefaultYear d p = do
        y <- getYear
        let (y',_,_) = toGregorian d in setYear y'
        r <- p
        when (isJust y) $ setYear $ fromJust y
        return r
  edate <- withDefaultYear primarydate datep
  return edate

statusp :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m ClearedStatus
statusp =
  choice'
    [ many spacenonewline >> char '*' >> return Cleared
    , many spacenonewline >> char '!' >> return Pending
    , return Uncleared
    ]
    <?> "cleared status"

codep :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m String
codep = try (do { some spacenonewline; char '(' <?> "codep"; code <- anyChar `manyTill` char ')'; return code } ) <|> return ""

-- Parse the following whitespace-beginning lines as postings, posting tags, and/or comments.
postings :: Stream [Char] Char => ParsecT [Char] (ExceptT String (StateT JournalContext IO)) [Posting]
postings = many (try postingp) <?> "postings"

-- linebeginningwithspaces :: Stream [Char] m Char => ParsecT [Char] JournalContext m String
-- linebeginningwithspaces = do
--   sp <- many1 spacenonewline
--   c <- nonspace
--   cs <- restofline
--   return $ sp ++ (c:cs) ++ "\n"

postingp :: Stream [Char] Char => ParsecT [Char] (ExceptT String (StateT JournalContext IO)) Posting
postingp = do
  some spacenonewline
  status <- statusp
  many spacenonewline
  account <- modifiedaccountnamep
  let (ptype, account') = (accountNamePostingType account, unbracket account)
  amount <- spaceandamountormissing
  massertion <- partialbalanceassertion
  _ <- fixedlotprice
  many spacenonewline
  ctx <- get
  comment <- try followingcommentp <|> (newline >> return "")
  let tags = tagsInComment comment
  -- oh boy
  date <- case dateValueFromTags tags of
        Nothing -> return Nothing
        Just v -> case evalState (runParserT (datep <* eof) "" v) ctx of
                    Right d -> return $ Just d
                    Left err -> fail $ show err
  date2 <- case date2ValueFromTags tags of
        Nothing -> return Nothing
        Just v -> case evalState (runParserT (datep <* eof) "" v) ctx of
                    Right d -> return $ Just d
                    Left err -> fail $ show err
  return posting
   { pdate=date
   , pdate2=date2
   , pstatus=status
   , paccount=account'
   , pamount=amount
   , pcomment=comment
   , ptype=ptype
   , ptags=tags
   , pbalanceassertion=massertion
   }

#ifdef TESTS
test_postingp = do
    let s `gives` ep = do
                         let parse = parseWithCtx nullctx postingp s
                         assertBool -- "postingp parser"
                           $ isRight parse
                         let Right ap = parse
                             same f = assertEqual (f ep) (f ap)
                         same pdate
                         same pstatus
                         same paccount
                         same pamount
                         same pcomment
                         same ptype
                         same ptags
                         same ptransaction
    "  expenses:food:dining  $10.00   ; a: a a \n   ; b: b b \n" `gives`
      posting{paccount="expenses:food:dining", pamount=Mixed [usd 10], pcomment=" a: a a \n b: b b \n", ptags=[("a","a a"), ("b","b b")]}

    " a  1 ; [2012/11/28]\n" `gives`
      ("a" `post` num 1){pcomment=" [2012/11/28]\n"
                        ,ptags=[("date","2012/11/28")]
                        ,pdate=parsedateM "2012/11/28"}

    " a  1 ; a:a, [=2012/11/28]\n" `gives`
      ("a" `post` num 1){pcomment=" a:a, [=2012/11/28]\n"
                        ,ptags=[("a","a"), ("date2","2012/11/28")]
                        ,pdate=Nothing}

    " a  1 ; a:a\n  ; [2012/11/28=2012/11/29],b:b\n" `gives`
      ("a" `post` num 1){pcomment=" a:a\n [2012/11/28=2012/11/29],b:b\n"
                        ,ptags=[("a","a"), ("date","2012/11/28"), ("date2","2012/11/29"), ("b","b")]
                        ,pdate=parsedateM "2012/11/28"}

    assertBool -- "postingp parses a quoted commodity with numbers"
      (isRight $ parseWithCtx nullctx postingp "  a  1 \"DE123\"\n")

  -- ,"postingp parses balance assertions and fixed lot prices" ~: do
    assertBool (isRight $ parseWithCtx nullctx postingp "  a  1 \"DE123\" =$1 { =2.2 EUR} \n")

    -- let parse = parseWithCtx nullctx postingp " a\n ;next-line comment\n"
    -- assertRight parse
    -- let Right p = parse
    -- assertEqual "next-line comment\n" (pcomment p)
    -- assertEqual (Just nullmixedamt) (pbalanceassertion p)
#endif

-- | Parse an account name, then apply any parent account prefix and/or account aliases currently in effect.
modifiedaccountnamep :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m AccountName
modifiedaccountnamep = do
  parent <- getParentAccount
  aliases <- getAccountAliases
  a <- accountnamep
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
accountnamep :: Stream [Char] Char => ParsecT [Char] m AccountName
accountnamep = do
    a <- do
      c <- nonspace
      cs <- striptrailingspace <$> many (nonspace <|> singlespace)
      return $ c:cs
    when (accountNameFromComponents (accountNameComponents a) /= a)
         (fail $ "account name seems ill-formed: "++a)
    return a
    where
      singlespace = try (do {spacenonewline; do {notFollowedBy spacenonewline; return ' '}})
      striptrailingspace "" = ""
      striptrailingspace s  = if last s == ' ' then init s else s

-- accountnamechar = notFollowedBy (oneOf "()[]") >> nonspace
--     <?> "account name character (non-bracket, non-parenthesis, non-whitespace)"

-- | Parse whitespace then an amount, with an optional left or right
-- currency symbol and optional price, or return the special
-- "missing" marker amount.
spaceandamountormissing :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m MixedAmount
spaceandamountormissing =
  try (do
        some spacenonewline
        (Mixed . (:[])) `fmap` amountp <|> return missingmixedamt
      ) <|> return missingmixedamt

#ifdef TESTS
assertParseEqual' :: (Show a, Eq a) => (Either ParseError a) -> a -> Assertion
assertParseEqual' parse expected = either (assertFailure.show) (`is'` expected) parse

is' :: (Eq a, Show a) => a -> a -> Assertion
a `is'` e = assertEqual e a

test_spaceandamountormissing = do
    assertParseEqual' (parseWithCtx nullctx spaceandamountormissing " $47.18") (Mixed [usd 47.18])
    assertParseEqual' (parseWithCtx nullctx spaceandamountormissing "$47.18") missingmixedamt
    assertParseEqual' (parseWithCtx nullctx spaceandamountormissing " ") missingmixedamt
    assertParseEqual' (parseWithCtx nullctx spaceandamountormissing "") missingmixedamt
#endif

-- | Parse a single-commodity amount, with optional symbol on the left or
-- right, optional unit or total price, and optional (ignored)
-- ledger-style balance assertion or fixed lot price declaration.
amountp :: (Stream [Char] t, MonadState JournalContext m) => ParsecT [Char] m Amount
amountp = try leftsymbolamount <|> try rightsymbolamount <|> nosymbolamount

#ifdef TESTS
test_amountp = do
    assertParseEqual' (parseWithCtx nullctx amountp "$47.18") (usd 47.18)
    assertParseEqual' (parseWithCtx nullctx amountp "$1.") (usd 1 `withPrecision` 0)
  -- ,"amount with unit price" ~: do
    assertParseEqual'
     (parseWithCtx nullctx amountp "$10 @ €0.5")
     (usd 10 `withPrecision` 0 `at` (eur 0.5 `withPrecision` 1))
  -- ,"amount with total price" ~: do
    assertParseEqual'
     (parseWithCtx nullctx amountp "$10 @@ €5")
     (usd 10 `withPrecision` 0 @@ (eur 5 `withPrecision` 0))
#endif

-- | Parse an amount from a string, or get an error.
amountp' :: String -> Amount
amountp' s =
  case evalState (runParserT (amountp <* eof) "" s) nullctx of
    Right t -> t
    Left err -> error' $ show err

-- | Parse a mixed amount from a string, or get an error.
mamountp' :: String -> MixedAmount
mamountp' = Mixed . (:[]) . amountp'

signp :: (Stream [Char] t, MonadState JournalContext m) => ParsecT [Char] m String
signp = do
  sign <- optional $ oneOf "+-"
  return $ case sign of Just '-' -> "-"
                        _        -> ""

leftsymbolamount :: (Stream [Char] t, MonadState JournalContext m) => ParsecT [Char] m Amount
leftsymbolamount = do
  sign <- signp
  c <- commoditysymbol
  sp <- many spacenonewline
  (q,prec,mdec,mgrps) <- numberp
  let s = amountstyle{ascommodityside=L, ascommodityspaced=not $ null sp, asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps}
  p <- priceamount
  let applysign = if sign=="-" then negate else id
  return $ applysign $ Amount c q p s
  <?> "left-symbol amount"

rightsymbolamount :: (Stream [Char] t, MonadState JournalContext m) => ParsecT [Char] m Amount
rightsymbolamount = do
  (q,prec,mdec,mgrps) <- numberp
  sp <- many spacenonewline
  c <- commoditysymbol
  p <- priceamount
  let s = amountstyle{ascommodityside=R, ascommodityspaced=not $ null sp, asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps}
  return $ Amount c q p s
  <?> "right-symbol amount"

nosymbolamount :: (Stream [Char] t, MonadState JournalContext m) => ParsecT [Char] m Amount
nosymbolamount = do
  (q,prec,mdec,mgrps) <- numberp
  p <- priceamount
  -- apply the most recently seen default commodity and style to this commodityless amount
  defcs <- getDefaultCommodityAndStyle
  let (c,s) = case defcs of
        Just (defc,defs) -> (defc, defs{asprecision=max (asprecision defs) prec})
        Nothing          -> ("", amountstyle{asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps})
  return $ Amount c q p s
  <?> "no-symbol amount"

commoditysymbol :: (Stream [Char] t, MonadState JournalContext m) => ParsecT [Char] m String
commoditysymbol = (quotedcommoditysymbol <|> simplecommoditysymbol) <?> "commodity symbol"

quotedcommoditysymbol :: (Stream [Char] t, MonadState JournalContext m) => ParsecT [Char] m String
quotedcommoditysymbol = do
  char '"'
  s <- some $ noneOf ";\n\""
  char '"'
  return s

simplecommoditysymbol :: (Stream [Char] t, MonadState JournalContext m) => ParsecT [Char] m String
simplecommoditysymbol = some (noneOf nonsimplecommoditychars)

priceamount :: (Stream [Char] t, MonadState JournalContext m) => ParsecT [Char] m Price
priceamount =
    try (do
          many spacenonewline
          char '@'
          try (do
                char '@'
                many spacenonewline
                a <- amountp -- XXX can parse more prices ad infinitum, shouldn't
                return $ TotalPrice a)
           <|> (do
            many spacenonewline
            a <- amountp -- XXX can parse more prices ad infinitum, shouldn't
            return $ UnitPrice a))
         <|> return NoPrice

partialbalanceassertion :: (Stream [Char] t, MonadState JournalContext m) => ParsecT [Char] m (Maybe MixedAmount)
partialbalanceassertion =
    try (do
          many spacenonewline
          char '='
          many spacenonewline
          a <- amountp -- XXX should restrict to a simple amount
          return $ Just $ Mixed [a])
         <|> return Nothing

-- balanceassertion :: Stream [Char] m Char => ParsecT [Char] JournalContext m (Maybe MixedAmount)
-- balanceassertion =
--     try (do
--           many spacenonewline
--           string "=="
--           many spacenonewline
--           a <- amountp -- XXX should restrict to a simple amount
--           return $ Just $ Mixed [a])
--          <|> return Nothing

-- http://ledger-cli.org/3.0/doc/ledger3.html#Fixing-Lot-Prices
fixedlotprice :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m (Maybe Amount)
fixedlotprice =
    try (do
          many spacenonewline
          char '{'
          many spacenonewline
          char '='
          many spacenonewline
          a <- amountp -- XXX should restrict to a simple amount
          many spacenonewline
          char '}'
          return $ Just a)
         <|> return Nothing

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
numberp :: (Stream [Char] t, MonadState JournalContext m) => ParsecT [Char] m (Quantity, Int, Maybe Char, Maybe DigitGroupStyle)
numberp = do
  -- a number is an optional sign followed by a sequence of digits possibly
  -- interspersed with periods, commas, or both
  -- ptrace "numberp"
  sign <- signp
  parts <- some $ choice' [some digitChar, some $ char ',', some $ char '.']
  dbg8 "numberp parsed" (sign,parts) `seq` return ()

  -- check the number is well-formed and identify the decimal point and digit
  -- group separator characters used, if any
  let (numparts, puncparts) = partition numeric parts
      (ok, mdecimalpoint, mseparator) =
          case (numparts, puncparts) of
            ([],_)     -> (False, Nothing, Nothing)  -- no digits, not ok
            (_,[])     -> (True, Nothing, Nothing)   -- digits with no punctuation, ok
            (_,[[d]])  -> (True, Just d, Nothing)    -- just a single punctuation of length 1, assume it's a decimal point
            (_,[_])    -> (False, Nothing, Nothing)  -- a single punctuation of some other length, not ok
            (_,_:_:_)  ->                                       -- two or more punctuations
              let (s:ss, d) = (init puncparts, last puncparts)  -- the leftmost is a separator and the rightmost may be a decimal point
              in if (any ((/=1).length) puncparts               -- adjacent punctuation chars, not ok
                     || any (s/=) ss                            -- separator chars vary, not ok
                     || head parts == s)                        -- number begins with a separator char, not ok
                 then (False, Nothing, Nothing)
                 else if s == d
                      then (True, Nothing, Just $ head s)       -- just one kind of punctuation - must be separators
                      else (True, Just $ head d, Just $ head s) -- separator(s) and a decimal point
  when (not ok) (fail $ "number seems ill-formed: "++concat parts)

  -- get the digit group sizes and digit group style if any
  let (intparts',fracparts') = span ((/= mdecimalpoint) . Just . head) parts
      (intparts, fracpart) = (filter numeric intparts', filter numeric fracparts')
      groupsizes = reverse $ case map length intparts of
                               (a:b:cs) | a < b -> b:cs
                               gs               -> gs
      mgrps = maybe Nothing (Just . (`DigitGroups` groupsizes)) $ mseparator

  -- put the parts back together without digit group separators, get the precision and parse the value
  let int = concat $ "":intparts
      frac = concat $ "":fracpart
      precision = length frac
      int' = if null int then "0" else int
      frac' = if null frac then "0" else frac
      quantity = read $ sign++int'++"."++frac' -- this read should never fail

  return $ dbg8 "numberp quantity,precision,mdecimalpoint,mgrps" (quantity,precision,mdecimalpoint,mgrps)
  <?> "numberp"
  where
    numeric = isNumber . headDef '_'

-- test_numberp = do
--       let s `is` n = assertParseEqual (parseWithCtx nullctx numberp s) n
--           assertFails = assertBool . isLeft . parseWithCtx nullctx numberp
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

-- comment parsers

multilinecommentp :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m ()
multilinecommentp = do
  string "comment" >> many spacenonewline >> newline
  go
  where
    go = try (eof <|> (string "end comment" >> newline >> return ()))
         <|> (anyLine >> go)
    anyLine = anyChar `manyTill` newline

emptyorcommentlinep :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m ()
emptyorcommentlinep = do
  many spacenonewline >> (comment <|> (many spacenonewline >> newline >> return ""))
  return ()

followingcommentp :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m String
followingcommentp =
  -- ptrace "followingcommentp"
  do samelinecomment <- many spacenonewline >> (try semicoloncomment <|> (newline >> return ""))
     newlinecomments <- many (try (some spacenonewline >> semicoloncomment))
     return $ unlines $ samelinecomment:newlinecomments

comment :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m String
comment = commentStartingWith commentchars

commentchars :: [Char]
commentchars = "#;*"

semicoloncomment :: (Stream [Char] Char, MonadState JournalContext m) => ParsecT [Char] m String
semicoloncomment = commentStartingWith ";"

commentStartingWith :: (Stream [Char] Char, MonadState JournalContext m) => String -> ParsecT [Char] m String
commentStartingWith cs = do
  -- ptrace "commentStartingWith"
  oneOf cs
  many spacenonewline
  l <- anyChar `manyTill` eolof
  optional newline
  return l

tagsInComment :: String -> [Tag]
tagsInComment c = concatMap tagsInCommentLine $ lines c'
  where
    c' = ledgerDateSyntaxToTags c

tagsInCommentLine :: String -> [Tag]
tagsInCommentLine = catMaybes . map maybetag . map strip . splitAtElement ','
  where
    maybetag s = case evalState (runParserT (tag <* eof) "" s) nullctx of
                  Right t -> Just t
                  Left _ -> Nothing

tag = do
  -- ptrace "tag"
  n <- tagname
  v <- tagvalue
  return (n,v)

tagname = do
  -- ptrace "tagname"
  n <- some $ noneOf ": \t"
  char ':'
  return n

tagvalue = do
  -- ptrace "tagvalue"
  v <- anyChar `manyTill` ((char ',' >> return ()) <|> eolof)
  return $ strip $ reverse $ dropWhile (==',') $ reverse $ strip v

ledgerDateSyntaxToTags :: String -> String
ledgerDateSyntaxToTags = regexReplaceBy "\\[[-.\\/0-9=]+\\]" replace
  where
    replace ('[':s) | lastDef ' ' s == ']' = replace' $ init s
    replace s = s

    replace' s | isdate s = datetag s
    replace' ('=':s) | isdate s = date2tag s
    replace' s | last s =='=' && isdate (init s) = datetag (init s)
    replace' s | length ds == 2 && isdate d1 && isdate d1 = datetag d1 ++ date2tag d2
      where
        ds = splitAtElement '=' s
        d1 = headDef "" ds
        d2 = lastDef "" ds
    replace' s = s

    isdate = isJust . parsedateM
    datetag s = "date:"++s++", "
    date2tag s = "date2:"++s++", "

#ifdef TESTS
test_ledgerDateSyntaxToTags = do
     assertEqual "date2:2012/11/28, " $ ledgerDateSyntaxToTags "[=2012/11/28]"
#endif

dateValueFromTags, date2ValueFromTags :: [Tag] -> Maybe String
dateValueFromTags  ts = maybe Nothing (Just . snd) $ find ((=="date") . fst) ts
date2ValueFromTags ts = maybe Nothing (Just . snd) $ find ((=="date2") . fst) ts


tests_Hledger_Read_JournalReader = TestList $ concat [
    -- test_numberp
 ]

{- old hunit tests

tests_Hledger_Read_JournalReader = TestList $ concat [
    test_numberp,
    test_amountp,
    test_spaceandamountormissing,
    test_tagcomment,
    test_inlinecomment,
    test_comments,
    test_ledgerDateSyntaxToTags,
    test_postingp,
    test_transaction,
    [
   "modifiertransaction" ~: do
     assertParse (parseWithCtx nullctx modifiertransaction "= (some value expr)\n some:postings  1\n")

  ,"periodictransaction" ~: do
     assertParse (parseWithCtx nullctx periodictransaction "~ (some period expr)\n some:postings  1\n")

  ,"directive" ~: do
     assertParse (parseWithCtx nullctx directive "!include /some/file.x\n")
     assertParse (parseWithCtx nullctx directive "account some:account\n")
     assertParse (parseWithCtx nullctx (directive >> directive) "!account a\nend\n")

  ,"comment" ~: do
     assertParse (parseWithCtx nullctx comment "; some comment \n")
     assertParse (parseWithCtx nullctx comment " \t; x\n")
     assertParse (parseWithCtx nullctx comment "#x")

  ,"datep" ~: do
     assertParse (parseWithCtx nullctx datep "2011/1/1")
     assertParseFailure (parseWithCtx nullctx datep "1/1")
     assertParse (parseWithCtx nullctx{ctxYear=Just 2011} datep "1/1")

  ,"datetimep" ~: do
      let p = do {t <- datetimep; eof; return t}
          bad = assertParseFailure . parseWithCtx nullctx p
          good = assertParse . parseWithCtx nullctx p
      bad "2011/1/1"
      bad "2011/1/1 24:00:00"
      bad "2011/1/1 00:60:00"
      bad "2011/1/1 00:00:60"
      good "2011/1/1 00:00"
      good "2011/1/1 23:59:59"
      good "2011/1/1 3:5:7"
      -- timezone is parsed but ignored
      let startofday = LocalTime (fromGregorian 2011 1 1) (TimeOfDay 0 0 (fromIntegral 0))
      assertParseEqual (parseWithCtx nullctx p "2011/1/1 00:00-0800") startofday
      assertParseEqual (parseWithCtx nullctx p "2011/1/1 00:00+1234") startofday

  ,"defaultyeardirective" ~: do
     assertParse (parseWithCtx nullctx defaultyeardirective "Y 2010\n")
     assertParse (parseWithCtx nullctx defaultyeardirective "Y 10001\n")

  ,"marketpricedirective" ~:
    assertParseEqual (parseWithCtx nullctx marketpricedirective "P 2004/05/01 XYZ $55.00\n") (MarketPrice (parsedate "2004/05/01") "XYZ" $ usd 55)

  ,"ignoredpricecommoditydirective" ~: do
     assertParse (parseWithCtx nullctx ignoredpricecommoditydirective "N $\n")

  ,"defaultcommoditydirective" ~: do
     assertParse (parseWithCtx nullctx defaultcommoditydirective "D $1,000.0\n")

  ,"commodityconversiondirective" ~: do
     assertParse (parseWithCtx nullctx commodityconversiondirective "C 1h = $50.00\n")

  ,"tagdirective" ~: do
     assertParse (parseWithCtx nullctx tagdirective "tag foo \n")

  ,"endtagdirective" ~: do
     assertParse (parseWithCtx nullctx endtagdirective "end tag \n")
     assertParse (parseWithCtx nullctx endtagdirective "pop \n")

  ,"accountnamep" ~: do
    assertBool "accountnamep parses a normal account name" (isRight $ parsewith accountnamep "a:b:c")
    assertBool "accountnamep rejects an empty inner component" (isLeft $ parsewith accountnamep "a::c")
    assertBool "accountnamep rejects an empty leading component" (isLeft $ parsewith accountnamep ":b:c")
    assertBool "accountnamep rejects an empty trailing component" (isLeft $ parsewith accountnamep "a:b:")

  ,"leftsymbolamount" ~: do
    assertParseEqual (parseWithCtx nullctx leftsymbolamount "$1")  (usd 1 `withPrecision` 0)
    assertParseEqual (parseWithCtx nullctx leftsymbolamount "$-1") (usd (-1) `withPrecision` 0)
    assertParseEqual (parseWithCtx nullctx leftsymbolamount "-$1") (usd (-1) `withPrecision` 0)

  ,"amount" ~: do
     let -- | compare a parse result with an expected amount, showing the debug representation for clarity
         assertAmountParse parseresult amount =
             (either (const "parse error") showAmountDebug parseresult) ~?= (showAmountDebug amount)
     assertAmountParse (parseWithCtx nullctx amountp "1 @ $2")
       (num 1 `withPrecision` 0 `at` (usd 2 `withPrecision` 0))

 ]]
-}

