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
{-# LANGUAGE CPP, RecordWildCards, NamedFieldPuns, NoMonoLocalBinds, ScopedTypeVariables, FlexibleContexts, TupleSections, OverloadedStrings #-}

module Hledger.Read.Common
where
--- * imports
import Prelude ()
import Prelude.Compat hiding (readFile)
import Control.Monad.Compat
import Control.Monad.Except (ExceptT(..), liftIO, runExceptT, throwError) --, catchError)
import Data.Char (isNumber)
import Data.Functor.Identity
import Data.List.Compat
import Data.List.Split (wordsBy)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Safe
import System.Time (getClockTime)
import Text.Parsec hiding (parse)

import Hledger.Data
import Hledger.Utils

-- $setup

--- * parsing utils

-- | A parser of strings with generic user state, monad and return type.
type StringParser u m a = ParsecT String u m a

-- | A parser of strict text with generic user state, monad and return type.
type TextParser u m a = ParsecT Text u m a

-- | A text parser with journal-parsing state.
type JournalParser m a = TextParser Journal m a

-- | A journal parser that runs in IO and can throw an error mid-parse.
type ErroringJournalParser a = JournalParser (ExceptT String IO) a

-- | Run a string parser with no state in the identity monad.
runStringParser, rsp :: StringParser () Identity a -> String -> Either ParseError a
runStringParser p s = runIdentity $ runParserT p () "" s
rsp = runStringParser

-- | Run a string parser with no state in the identity monad.
runTextParser, rtp :: TextParser () Identity a -> Text -> Either ParseError a
runTextParser p t = runIdentity $ runParserT p () "" t
rtp = runTextParser

-- | Run a journal parser with a null journal-parsing state.
runJournalParser, rjp :: Monad m => JournalParser m a -> Text -> m (Either ParseError a)
runJournalParser p t = runParserT p mempty "" t
rjp = runJournalParser

-- | Run an error-raising journal parser with a null journal-parsing state.
runErroringJournalParser, rejp :: ErroringJournalParser a -> Text -> IO (Either String a)
runErroringJournalParser p t = runExceptT $ runJournalParser p t >>= either (throwError.show) return
rejp = runErroringJournalParser

genericSourcePos :: SourcePos -> GenericSourcePos
genericSourcePos p = GenericSourcePos (sourceName p) (sourceLine p) (sourceColumn p)

-- | Given a parsec ParsedJournal parser, file path and data string,
-- parse and post-process a ready-to-use Journal, or give an error.
parseAndFinaliseJournal :: ErroringJournalParser ParsedJournal -> Bool -> FilePath -> Text -> ExceptT String IO Journal
parseAndFinaliseJournal parser assrt f txt = do
  t <- liftIO getClockTime
  y <- liftIO getCurrentYear
  m <- liftIO getCurrentMonth
  ep <- runParserT parser nulljournal{jparsedefaultyear=Just (y,Just m)} f txt
  case ep of
    Right pj -> case journalFinalise t f txt assrt pj of
                        Right j -> return j
                        Left e  -> throwError e
    Left e   -> throwError $ show e

setYear :: Monad m => (Integer,Maybe Int) -> JournalParser m ()
setYear (y,m) = modifyState (\j -> j{jparsedefaultyear=Just (y,m)})

getYear :: Monad m => JournalParser m (Maybe (Integer,Maybe Int))
getYear = fmap jparsedefaultyear getState

setDefaultCommodityAndStyle :: Monad m => (CommoditySymbol,AmountStyle) -> JournalParser m ()
setDefaultCommodityAndStyle cs = modifyState (\j -> j{jparsedefaultcommodity=Just cs})

getDefaultCommodityAndStyle :: Monad m => JournalParser m (Maybe (CommoditySymbol,AmountStyle))
getDefaultCommodityAndStyle = jparsedefaultcommodity `fmap` getState

pushAccount :: Monad m => AccountName -> JournalParser m ()
pushAccount acct = modifyState (\j -> j{jaccounts = acct : jaccounts j})

pushParentAccount :: Monad m => AccountName -> JournalParser m ()
pushParentAccount acct = modifyState (\j -> j{jparseparentaccounts = acct : jparseparentaccounts j})

popParentAccount :: Monad m => JournalParser m ()
popParentAccount = do
  j <- getState
  case jparseparentaccounts j of
    []       -> unexpected "End of apply account block with no beginning"
    (_:rest) -> setState j{jparseparentaccounts=rest}

getParentAccount :: Monad m => JournalParser m AccountName
getParentAccount = fmap (concatAccountNames . reverse . jparseparentaccounts) getState

addAccountAlias :: Monad m => AccountAlias -> JournalParser m ()
addAccountAlias a = modifyState (\(j@Journal{..}) -> j{jparsealiases=a:jparsealiases})

getAccountAliases :: Monad m => JournalParser m [AccountAlias]
getAccountAliases = fmap jparsealiases getState

clearAccountAliases :: Monad m => JournalParser m ()
clearAccountAliases = modifyState (\(j@Journal{..}) -> j{jparsealiases=[]})

getTransactionCount :: Monad m => JournalParser m Integer
getTransactionCount = fmap jparsetransactioncount getState

setTransactionCount :: Monad m => Integer -> JournalParser m ()
setTransactionCount i = modifyState (\j -> j{jparsetransactioncount=i})

-- | Increment the transaction index by one and return the new value.
incrementTransactionCount :: Monad m => JournalParser m Integer
incrementTransactionCount = do
  modifyState (\j -> j{jparsetransactioncount=jparsetransactioncount j + 1})
  getTransactionCount

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
parserErrorAt :: SourcePos -> String -> ErroringJournalParser a
parserErrorAt pos s = throwError $ show pos ++ ":\n" ++ s

--- * parsers
--- ** transaction bits

statusp :: Monad m => JournalParser m ClearedStatus
statusp =
  choice'
    [ many spacenonewline >> char '*' >> return Cleared
    , many spacenonewline >> char '!' >> return Pending
    , return Uncleared
    ]
    <?> "cleared status"

codep :: Monad m => JournalParser m String
codep = try (do { many1 spacenonewline; char '(' <?> "codep"; anyChar `manyTill` char ')' } ) <|> return ""

descriptionp :: Monad m => JournalParser m String
descriptionp = many (noneOf ";\n")

--- ** dates

-- | Parse a date in YYYY/MM/DD format.
-- Hyphen (-) and period (.) are also allowed as separators.
-- Year/month may be omitted if default values have been set.
-- Leading zeroes may be omitted.
datep :: Monad m => JournalParser m Day
datep = do
  -- hacky: try to ensure precise errors for invalid dates
  -- XXX reported error position is not too good
  -- pos <- genericSourcePos <$> getPosition
  datestr <- do
    c <- digit
    cs <- many $ choice' [digit, datesepchar]
    return $ c:cs
  let sepchars = nub $ sort $ filter (`elem` datesepchars) datestr
  when (length sepchars > 1) $ fail $ "bad date, different separators used: " ++ datestr
  let dateparts = wordsBy (`elem` datesepchars) datestr
  currentyear <- getYear
  [y,m,d] <- case (dateparts,currentyear) of
              ([d],Just (y,Just m))  -> return [show y,show m,d]
              ([m,d],Just (y,_))     -> return [show y,m,d]
              ([_,_],Nothing)        -> fail $ "partial date "++datestr++" found, but the current year is unknown"
              ([_],Nothing)          -> fail $ "partial date "++datestr++" found, but the current year is unknown"
              ([_],Just (_,Nothing)) -> fail $ "partial date "++datestr++" found, but the current month is unknown"
              ([y,m,d],_)            -> return [y,m,d]
              _                      -> fail $ "bad date: " ++ datestr
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
datetimep :: Monad m => JournalParser m LocalTime
datetimep = do
  day <- datep
  many1 spacenonewline
  h <- many1 digit
  let h' = read h
  guard $ h' >= 0 && h' <= 23
  char ':'
  m <- many1 digit
  let m' = read m
  guard $ m' >= 0 && m' <= 59
  s <- optionMaybe $ char ':' >> many1 digit
  let s' = case s of Just sstr -> read sstr
                     Nothing   -> 0
  guard $ s' >= 0 && s' <= 59
  {- tz <- -}
  optionMaybe $ do
                   plusminus <- oneOf "-+"
                   d1 <- digit
                   d2 <- digit
                   d3 <- digit
                   d4 <- digit
                   return $ plusminus:d1:d2:d3:d4:""
  -- ltz <- liftIO $ getCurrentTimeZone
  -- let tz' = maybe ltz (fromMaybe ltz . parseTime defaultTimeLocale "%z") tz
  -- return $ localTimeToUTC tz' $ LocalTime day $ TimeOfDay h' m' (fromIntegral s')
  return $ LocalTime day $ TimeOfDay h' m' (fromIntegral s')

secondarydatep :: Monad m => Day -> JournalParser m Day
secondarydatep primarydate = do
  char '='
  -- kludgy way to use primary date for default year/month
  let withDefaultYear d p = do
        y <- getYear
        let (y',m',_) = toGregorian d in setYear (y',Just m')
        r <- p
        when (isJust y) $ setYear $ fromJust y -- XXX
        -- mapM setYear <$> y
        return r
  withDefaultYear primarydate datep

-- |
-- >> parsewith twoorthreepartdatestringp "2016/01/2"
-- Right "2016/01/2"
-- twoorthreepartdatestringp = do
--   n1 <- many1 digit
--   c <- datesepchar
--   n2 <- many1 digit
--   mn3 <- optionMaybe $ char c >> many1 digit
--   return $ n1 ++ c:n2 ++ maybe "" (c:) mn3

--- ** account names

-- | Parse an account name, then apply any parent account prefix and/or account aliases currently in effect.
modifiedaccountnamep :: Monad m => JournalParser m AccountName
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
accountnamep :: Monad m => TextParser u m AccountName
accountnamep = do
    astr <- do
      c <- nonspace
      cs <- striptrailingspace <$> many (nonspace <|> singlespace)
      return $ c:cs
    let a = T.pack astr
    when (accountNameFromComponents (accountNameComponents a) /= a)
         (fail $ "account name seems ill-formed: "++astr)
    return a
    where
      singlespace = try (do {spacenonewline; do {notFollowedBy spacenonewline; return ' '}})
      striptrailingspace "" = ""
      striptrailingspace s  = if last s == ' ' then init s else s

-- accountnamechar = notFollowedBy (oneOf "()[]") >> nonspace
--     <?> "account name character (non-bracket, non-parenthesis, non-whitespace)"

--- ** amounts

-- | Parse whitespace then an amount, with an optional left or right
-- currency symbol and optional price, or return the special
-- "missing" marker amount.
spaceandamountormissingp :: Monad m => JournalParser m MixedAmount
spaceandamountormissingp =
  try (do
        many1 spacenonewline
        (Mixed . (:[])) `fmap` amountp <|> return missingmixedamt
      ) <|> return missingmixedamt

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
  case runParser (amountp <* eof) mempty "" (T.pack s) of
    Right amt -> amt
    Left err  -> error' $ show err -- XXX should throwError

-- | Parse a mixed amount from a string, or get an error.
mamountp' :: String -> MixedAmount
mamountp' = Mixed . (:[]) . amountp'

signp :: Monad m => JournalParser m String
signp = do
  sign <- optionMaybe $ oneOf "+-"
  return $ case sign of Just '-' -> "-"
                        _        -> ""

leftsymbolamountp :: Monad m => JournalParser m Amount
leftsymbolamountp = do
  sign <- signp
  c <- commoditysymbolp
  sp <- many spacenonewline
  (q,prec,mdec,mgrps) <- numberp
  let s = amountstyle{ascommodityside=L, ascommodityspaced=not $ null sp, asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps}
  p <- priceamountp
  let applysign = if sign=="-" then negate else id
  return $ applysign $ Amount c q p s
  <?> "left-symbol amount"

rightsymbolamountp :: Monad m => JournalParser m Amount
rightsymbolamountp = do
  (q,prec,mdec,mgrps) <- numberp
  sp <- many spacenonewline
  c <- commoditysymbolp
  p <- priceamountp
  let s = amountstyle{ascommodityside=R, ascommodityspaced=not $ null sp, asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps}
  return $ Amount c q p s
  <?> "right-symbol amount"

nosymbolamountp :: Monad m => JournalParser m Amount
nosymbolamountp = do
  (q,prec,mdec,mgrps) <- numberp
  p <- priceamountp
  -- apply the most recently seen default commodity and style to this commodityless amount
  defcs <- getDefaultCommodityAndStyle
  let (c,s) = case defcs of
        Just (defc,defs) -> (defc, defs{asprecision=max (asprecision defs) prec})
        Nothing          -> ("", amountstyle{asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps})
  return $ Amount c q p s
  <?> "no-symbol amount"

commoditysymbolp :: Monad m => JournalParser m CommoditySymbol
commoditysymbolp = (quotedcommoditysymbolp <|> simplecommoditysymbolp) <?> "commodity symbol"

quotedcommoditysymbolp :: Monad m => JournalParser m CommoditySymbol
quotedcommoditysymbolp = do
  char '"'
  s <- many1 $ noneOf ";\n\""
  char '"'
  return $ T.pack s

simplecommoditysymbolp :: Monad m => JournalParser m CommoditySymbol
simplecommoditysymbolp = T.pack <$> many1 (noneOf nonsimplecommoditychars)

priceamountp :: Monad m => JournalParser m Price
priceamountp =
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

partialbalanceassertionp :: Monad m => JournalParser m (Maybe MixedAmount)
partialbalanceassertionp =
    try (do
          many spacenonewline
          char '='
          many spacenonewline
          a <- amountp -- XXX should restrict to a simple amount
          return $ Just $ Mixed [a])
         <|> return Nothing

-- balanceassertion :: Monad m => JournalParser m (Maybe MixedAmount)
-- balanceassertion =
--     try (do
--           many spacenonewline
--           string "=="
--           many spacenonewline
--           a <- amountp -- XXX should restrict to a simple amount
--           return $ Just $ Mixed [a])
--          <|> return Nothing

-- http://ledger-cli.org/3.0/doc/ledger3.html#Fixing-Lot-Prices
fixedlotpricep :: Monad m => JournalParser m (Maybe Amount)
fixedlotpricep =
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
numberp :: Monad m => JournalParser m (Quantity, Int, Maybe Char, Maybe DigitGroupStyle)
numberp = do
  -- a number is an optional sign followed by a sequence of digits possibly
  -- interspersed with periods, commas, or both
  -- ptrace "numberp"
  sign <- signp
  parts <- many1 $ choice' [many1 digit, many1 $ char ',', many1 $ char '.']
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
              in if any ((/=1).length) puncparts               -- adjacent punctuation chars, not ok
                    || any (s/=) ss                            -- separator chars vary, not ok
                    || head parts == s                        -- number begins with a separator char, not ok
                 then (False, Nothing, Nothing)
                 else if s == d
                      then (True, Nothing, Just $ head s)       -- just one kind of punctuation - must be separators
                      else (True, Just $ head d, Just $ head s) -- separator(s) and a decimal point
  unless ok $ fail $ "number seems ill-formed: "++concat parts

  -- get the digit group sizes and digit group style if any
  let (intparts',fracparts') = span ((/= mdecimalpoint) . Just . head) parts
      (intparts, fracpart) = (filter numeric intparts', filter numeric fracparts')
      groupsizes = reverse $ case map length intparts of
                               (a:b:cs) | a < b -> b:cs
                               gs               -> gs
      mgrps = (`DigitGroups` groupsizes) <$> mseparator

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

multilinecommentp :: Monad m => JournalParser m ()
multilinecommentp = do
  string "comment" >> many spacenonewline >> newline
  go
  where
    go = try (eof <|> (string "end comment" >> newline >> return ()))
         <|> (anyLine >> go)
    anyLine = anyChar `manyTill` newline

emptyorcommentlinep :: Monad m => JournalParser m ()
emptyorcommentlinep = do
  many spacenonewline >> (commentp <|> (many spacenonewline >> newline >> return ""))
  return ()

-- | Parse a possibly multi-line comment following a semicolon.
followingcommentp :: Monad m => JournalParser m Text
followingcommentp =
  -- ptrace "followingcommentp"
  do samelinecomment <- many spacenonewline >> (try semicoloncommentp <|> (newline >> return ""))
     newlinecomments <- many (try (many1 spacenonewline >> semicoloncommentp))
     return $ T.unlines $ samelinecomment:newlinecomments

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
-- Left ...line 1, column 22...year is unknown...
--
-- Date tag value contains trailing text - forgot the comma, confused:
-- the syntaxes ?  We'll accept the leading date anyway
-- >>> rejp (followingcommentandtagsp (Just $ fromGregorian 2000 1 2)) "; date:3/4=5/6"
-- Right ("date:3/4=5/6\n",[("date","3/4=5/6")],Just 2000-03-04,Nothing)
--
followingcommentandtagsp :: Maybe Day -> ErroringJournalParser (Text, [Tag], Maybe Day, Maybe Day)
followingcommentandtagsp mdefdate = do
  -- pdbg 0 "followingcommentandtagsp"

  -- Parse a single or multi-line comment, starting on this line or the next one.
  -- Save the starting position and preserve all whitespace for the subsequent re-parsing,
  -- to get good error positions.
  startpos <- getPosition
  commentandwhitespace :: String <- do
    let semicoloncommentp' = (:) <$> char ';' <*> anyChar `manyTill` eolof
    sp1 <- many spacenonewline
    l1  <- try semicoloncommentp' <|> (newline >> return "")
    ls  <- many $ try ((++) <$> many1 spacenonewline <*> semicoloncommentp')
    return $ unlines $ (sp1 ++ l1) : ls
  let comment = T.pack $ unlines $ map (lstrip . dropWhile (==';') . strip) $ lines commentandwhitespace
  -- pdbg 0 $ "commentws:"++show commentandwhitespace
  -- pdbg 0 $ "comment:"++show comment

  -- Reparse the comment for any tags.
  tags <- case runTextParser (setPosition startpos >> tagsp) $ T.pack commentandwhitespace of
            Right ts -> return ts
            Left e   -> throwError $ show e
  -- pdbg 0 $ "tags: "++show tags

  -- Reparse the comment for any posting dates. Use the transaction date for defaults, if provided.
  epdates <- liftIO $ rejp (setPosition startpos >> postingdatesp mdefdate) $ T.pack commentandwhitespace
  pdates <- case epdates of
              Right ds -> return ds
              Left e   -> throwError e
  -- pdbg 0 $ "pdates: "++show pdates
  let mdate  = headMay $ map snd $ filter ((=="date").fst)  pdates
      mdate2 = headMay $ map snd $ filter ((=="date2").fst) pdates

  return (comment, tags, mdate, mdate2)

commentp :: Monad m => JournalParser m Text
commentp = commentStartingWithp commentchars

commentchars :: [Char]
commentchars = "#;*"

semicoloncommentp :: Monad m => JournalParser m Text
semicoloncommentp = commentStartingWithp ";"

commentStartingWithp :: Monad m => [Char] -> JournalParser m Text
commentStartingWithp cs = do
  -- ptrace "commentStartingWith"
  oneOf cs
  many spacenonewline
  l <- anyChar `manyTill` eolof
  optional newline
  return $ T.pack l

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
commentTags :: Text -> [Tag]
commentTags s =
  case runTextParser tagsp s of
    Right r -> r
    Left _  -> [] -- shouldn't happen

-- | Parse all tags found in a string.
tagsp :: TextParser u Identity [Tag]
tagsp = -- do
  -- pdbg 0 $ "tagsp"
  many (try (nontagp >> tagp))

-- | Parse everything up till the first tag.
--
-- >>> rtp nontagp "\na b:, \nd:e, f"
-- Right "\na "
nontagp :: TextParser u Identity String
nontagp = -- do
  -- pdbg 0 "nontagp"
  -- anyChar `manyTill` (lookAhead (try (tagorbracketeddatetagsp Nothing >> return ()) <|> eof))
  anyChar `manyTill` lookAhead (try (void tagp) <|> eof)
  -- XXX costly ?

-- | Tags begin with a colon-suffixed tag name (a word beginning with
-- a letter) and are followed by a tag value (any text up to a comma
-- or newline, whitespace-stripped).
--
-- >>> rtp tagp "a:b b , c AuxDate: 4/2"
-- Right ("a","b b")
--
tagp :: Monad m => TextParser u m Tag
tagp = do
  -- pdbg 0 "tagp"
  n <- tagnamep
  v <- tagvaluep
  return (n,v)

-- |
-- >>> rtp tagnamep "a:"
-- Right "a"
tagnamep :: Monad m => TextParser u m Text
tagnamep = -- do
  -- pdbg 0 "tagnamep"
  T.pack <$> many1 (noneOf ": \t\n") <* char ':'

tagvaluep :: Monad m => TextParser u m Text
tagvaluep = do
  -- ptrace "tagvalue"
  v <- anyChar `manyTill` (void (try (char ',')) <|> eolof)
  return $ T.pack $ strip $ reverse $ dropWhile (==',') $ reverse $ strip v

--- ** posting dates

-- | Parse all posting dates found in a string. Posting dates can be
-- expressed with date/date2 tags and/or bracketed dates.  The dates
-- are parsed fully to give useful errors. Missing years can be
-- inferred only if a default date is provided.
--
postingdatesp :: Maybe Day -> ErroringJournalParser [(TagName,Day)]
postingdatesp mdefdate = do
  -- pdbg 0 $ "postingdatesp"
  let p = ((:[]) <$> datetagp mdefdate) <|> bracketeddatetagsp mdefdate
      nonp =
         many (notFollowedBy p >> anyChar)
         -- anyChar `manyTill` (lookAhead (try (p >> return ()) <|> eof))
  concat <$> many (try (nonp >> p))

--- ** date tags

-- | Date tags are tags with name "date" or "date2". Their value is
-- parsed as a date, using the provided default date if any for
-- inferring a missing year if needed. Any error in date parsing is
-- reported and terminates parsing.
--
-- >>> rejp (datetagp Nothing) "date: 2000/1/2 "
-- Right ("date",2000-01-02)
--
-- >>> rejp (datetagp (Just $ fromGregorian 2001 2 3)) "date2:3/4"
-- Right ("date2",2001-03-04)
--
-- >>> rejp (datetagp Nothing) "date:  3/4"
-- Left ...line 1, column 9...year is unknown...
--
datetagp :: Maybe Day -> ErroringJournalParser (TagName,Day)
datetagp mdefdate = do
  -- pdbg 0 "datetagp"
  string "date"
  n <- T.pack . fromMaybe "" <$> optionMaybe (string "2")
  char ':'
  startpos <- getPosition
  v <- tagvaluep
  -- re-parse value as a date.
  j <- getState 
  ep <- parseWithState
    j{jparsedefaultyear=(\(y,m,_) -> (y,Just m)).toGregorian <$> mdefdate}
    -- The value extends to a comma, newline, or end of file.
    -- It seems like ignoring any extra stuff following a date
    -- gives better errors here.
    (do
        setPosition startpos
        datep) -- <* eof)
    v
  case ep
    of Left e  -> throwError $ show e
       Right d -> return ("date"<>n, d)

--- ** bracketed dates

-- tagorbracketeddatetagsp :: Monad m => Maybe Day -> TextParser u m [Tag]
-- tagorbracketeddatetagsp mdefdate =
--   bracketeddatetagsp mdefdate <|> ((:[]) <$> tagp)

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
-- >>> rejp (bracketeddatetagsp Nothing) "[2016/1/2=3/4]"
-- Right [("date",2016-01-02),("date2",2016-03-04)]
--
-- >>> rejp (bracketeddatetagsp Nothing) "[1]"
-- Left ...not a bracketed date...
--
-- >>> rejp (bracketeddatetagsp Nothing) "[2016/1/32]"
-- Left ...line 1, column 11...bad date...
--
-- >>> rejp (bracketeddatetagsp Nothing) "[1/31]"
-- Left ...line 1, column 6...year is unknown...
--
-- >>> rejp (bracketeddatetagsp Nothing) "[0123456789/-.=/-.=]"
-- Left ...line 1, column 15...bad date, different separators...
--
bracketeddatetagsp :: Maybe Day -> ErroringJournalParser [(TagName, Day)]
bracketeddatetagsp mdefdate = do
  -- pdbg 0 "bracketeddatetagsp"
  char '['
  startpos <- getPosition
  let digits = "0123456789"
  s <- many1 (oneOf $ '=':digits++datesepchars)
  char ']'
  unless (any (`elem` s) digits && any (`elem` datesepchars) s) $
    parserFail "not a bracketed date"

  -- looks sufficiently like a bracketed date, now we
  -- re-parse as dates and throw any errors
  j <- getState
  ep <- parseWithState
    j{jparsedefaultyear=(\(y,m,_) -> (y,Just m)).toGregorian <$> mdefdate}
    (do
        setPosition startpos
        md1 <- optionMaybe datep
        maybe (return ()) (setYear.(\(y,m,_) -> (y,Just m)).toGregorian) md1
        md2 <- optionMaybe $ char '=' >> datep
        eof
        return (md1,md2)
    )
    (T.pack s)
  case ep
    of Left e          -> throwError $ show e
       Right (md1,md2) -> return $ catMaybes
         [("date",) <$> md1, ("date2",) <$> md2]

