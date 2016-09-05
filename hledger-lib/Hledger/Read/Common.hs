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
import Control.Monad.Except (ExceptT(..), runExceptT, throwError) --, catchError)
import Control.Monad.State.Strict
import Data.Char (isNumber)
import Data.Functor.Identity
import Data.List.Compat
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Split (wordsBy)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Safe
import System.Time (getClockTime)
import Text.Megaparsec hiding (parse,State)
import Text.Megaparsec.Text

import Hledger.Data
import Hledger.Utils

-- $setup

--- * parsing utils

-- | Run a string parser with no state in the identity monad.
runTextParser, rtp :: TextParser Identity a -> Text -> Either (ParseError Char Dec) a
runTextParser p t =  runParser p "" t
rtp = runTextParser

-- | Run a journal parser with a null journal-parsing state.
runJournalParser, rjp :: Monad m => TextParser m a -> Text -> m (Either (ParseError Char Dec) a)
runJournalParser p t = runParserT p "" t
rjp = runJournalParser

-- | Run an error-raising journal parser with a null journal-parsing state.
runErroringJournalParser, rejp :: ErroringJournalParser a -> Text -> IO (Either String a)
runErroringJournalParser p t =
  runExceptT $
  runJournalParser (evalStateT p mempty)
                   t >>=
  either (throwError . parseErrorPretty) return
rejp = runErroringJournalParser

genericSourcePos :: SourcePos -> GenericSourcePos
genericSourcePos p = GenericSourcePos (sourceName p) (fromIntegral . unPos $ sourceLine p) (fromIntegral . unPos $ sourceColumn p)

-- | Given a parsec ParsedJournal parser, file path and data string,
-- parse and post-process a ready-to-use Journal, or give an error.
parseAndFinaliseJournal :: ErroringJournalParser ParsedJournal -> Bool -> FilePath -> Text -> ExceptT String IO Journal
parseAndFinaliseJournal parser assrt f txt = do
  t <- liftIO getClockTime
  y <- liftIO getCurrentYear
  ep <- runParserT (evalStateT parser nulljournal {jparsedefaultyear=Just y}) f txt
  case ep of
    Right pj -> case journalFinalise t f txt assrt pj of
                        Right j -> return j
                        Left e  -> throwError e
    Left e   -> throwError $ parseErrorPretty e

parseAndFinaliseJournal' :: JournalParser ParsedJournal -> Bool -> FilePath -> Text -> ExceptT String IO Journal
parseAndFinaliseJournal' parser assrt f txt = do
  t <- liftIO getClockTime
  y <- liftIO getCurrentYear
  let ep = runParser (evalStateT parser nulljournal {jparsedefaultyear=Just y}) f txt
  case ep of
    Right pj -> case journalFinalise t f txt assrt pj of
                        Right j -> return j
                        Left e  -> throwError e
    Left e   -> throwError $ parseErrorPretty e

setYear :: Year -> JournalStateParser m ()
setYear y = modify' (\j -> j{jparsedefaultyear=Just y})

getYear :: JournalStateParser m (Maybe Year)
getYear = fmap jparsedefaultyear get

setDefaultCommodityAndStyle :: (CommoditySymbol,AmountStyle) -> ErroringJournalParser ()
setDefaultCommodityAndStyle cs = modify' (\j -> j{jparsedefaultcommodity=Just cs})

getDefaultCommodityAndStyle :: JournalStateParser m (Maybe (CommoditySymbol,AmountStyle))
getDefaultCommodityAndStyle = jparsedefaultcommodity `fmap` get

pushAccount :: AccountName -> ErroringJournalParser ()
pushAccount acct = modify' (\j -> j{jaccounts = acct : jaccounts j})

pushParentAccount :: AccountName -> ErroringJournalParser ()
pushParentAccount acct = modify' (\j -> j{jparseparentaccounts = acct : jparseparentaccounts j})

popParentAccount :: ErroringJournalParser ()
popParentAccount = do
  j <- get
  case jparseparentaccounts j of
    []       -> unexpected (Tokens ('E' :| "nd of apply account block with no beginning"))
    (_:rest) -> put j{jparseparentaccounts=rest}

getParentAccount :: ErroringJournalParser AccountName
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
parserErrorAt :: SourcePos -> String -> ErroringJournalParser a
parserErrorAt pos s = throwError $ sourcePosPretty pos ++ ":\n" ++ s

--- * parsers
--- ** transaction bits

statusp :: TextParser m ClearedStatus
statusp =
  choice'
    [ many spacenonewline >> char '*' >> return Cleared
    , many spacenonewline >> char '!' >> return Pending
    , return Uncleared
    ]
    <?> "cleared status"

codep :: TextParser m String
codep = try (do { some spacenonewline; char '(' <?> "codep"; anyChar `manyTill` char ')' } ) <|> return ""

descriptionp :: ErroringJournalParser String
descriptionp = many (noneOf (";\n" :: [Char]))

--- ** dates

-- | Parse a date in YYYY/MM/DD format.
-- Hyphen (-) and period (.) are also allowed as separators.
-- The year may be omitted if a default year has been set.
-- Leading zeroes may be omitted.
datep :: JournalStateParser m Day
datep = do
  -- hacky: try to ensure precise errors for invalid dates
  -- XXX reported error position is not too good
  -- pos <- genericSourcePos <$> getPosition
  datestr <- do
    c <- digitChar
    cs <- lift $ many $ choice' [digitChar, datesepchar]
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
datetimep :: ErroringJournalParser LocalTime
datetimep = do
  day <- datep
  lift $ some spacenonewline
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

secondarydatep :: Day -> ErroringJournalParser Day
secondarydatep primarydate = do
  char '='
  -- kludgy way to use primary date for default year
  let withDefaultYear d p = do
        y <- getYear
        let (y',_,_) = toGregorian d in setYear y'
        r <- p
        when (isJust y) $ setYear $ fromJust y -- XXX
        -- mapM setYear <$> y
        return r
  withDefaultYear primarydate datep

-- |
-- >> parsewith twoorthreepartdatestringp "2016/01/2"
-- Right "2016/01/2"
-- twoorthreepartdatestringp = do
--   n1 <- some digitChar
--   c <- datesepchar
--   n2 <- some digitChar
--   mn3 <- optional $ char c >> some digitChar
--   return $ n1 ++ c:n2 ++ maybe "" (c:) mn3

--- ** account names

-- | Parse an account name, then apply any parent account prefix and/or account aliases currently in effect.
modifiedaccountnamep :: ErroringJournalParser AccountName
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
spaceandamountormissingp :: ErroringJournalParser MixedAmount
spaceandamountormissingp =
  try (do
        lift $ some spacenonewline
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
amountp :: Monad m => JournalStateParser m Amount
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

signp :: TextParser m String
signp = do
  sign <- optional $ oneOf ("+-" :: [Char])
  return $ case sign of Just '-' -> "-"
                        _        -> ""

leftsymbolamountp :: Monad m => JournalStateParser m Amount
leftsymbolamountp = do
  sign <- lift signp
  c <- lift commoditysymbolp
  sp <- lift $ many spacenonewline
  (q,prec,mdec,mgrps) <- lift numberp
  let s = amountstyle{ascommodityside=L, ascommodityspaced=not $ null sp, asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps}
  p <- priceamountp
  let applysign = if sign=="-" then negate else id
  return $ applysign $ Amount c q p s
  <?> "left-symbol amount"

rightsymbolamountp :: Monad m => JournalStateParser m Amount
rightsymbolamountp = do
  (q,prec,mdec,mgrps) <- lift numberp
  sp <- lift $ many spacenonewline
  c <- lift commoditysymbolp
  p <- priceamountp
  let s = amountstyle{ascommodityside=R, ascommodityspaced=not $ null sp, asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps}
  return $ Amount c q p s
  <?> "right-symbol amount"

nosymbolamountp :: Monad m => JournalStateParser m Amount
nosymbolamountp = do
  (q,prec,mdec,mgrps) <- lift numberp
  p <- priceamountp
  -- apply the most recently seen default commodity and style to this commodityless amount
  defcs <- getDefaultCommodityAndStyle
  let (c,s) = case defcs of
        Just (defc,defs) -> (defc, defs{asprecision=max (asprecision defs) prec})
        Nothing          -> ("", amountstyle{asprecision=prec, asdecimalpoint=mdec, asdigitgroups=mgrps})
  return $ Amount c q p s
  <?> "no-symbol amount"

commoditysymbolp :: TextParser m CommoditySymbol
commoditysymbolp = (quotedcommoditysymbolp <|> simplecommoditysymbolp) <?> "commodity symbol"

quotedcommoditysymbolp :: TextParser m CommoditySymbol
quotedcommoditysymbolp = do
  char '"'
  s <- some $ noneOf (";\n\"" :: [Char])
  char '"'
  return $ T.pack s

simplecommoditysymbolp :: TextParser m CommoditySymbol
simplecommoditysymbolp = T.pack <$> some (noneOf nonsimplecommoditychars)

priceamountp :: Monad m => JournalStateParser m Price
priceamountp =
    try (do
          lift (many spacenonewline)
          char '@'
          try (do
                char '@'
                lift (many spacenonewline)
                a <- amountp -- XXX can parse more prices ad infinitum, shouldn't
                return $ TotalPrice a)
           <|> (do
            lift (many spacenonewline)
            a <- amountp -- XXX can parse more prices ad infinitum, shouldn't
            return $ UnitPrice a))
         <|> return NoPrice

partialbalanceassertionp :: ErroringJournalParser (Maybe MixedAmount)
partialbalanceassertionp =
    try (do
          lift (many spacenonewline)
          char '='
          lift (many spacenonewline)
          a <- amountp -- XXX should restrict to a simple amount
          return $ Just $ Mixed [a])
         <|> return Nothing

-- balanceassertion :: Monad m => TextParser m (Maybe MixedAmount)
-- balanceassertion =
--     try (do
--           lift (many spacenonewline)
--           string "=="
--           lift (many spacenonewline)
--           a <- amountp -- XXX should restrict to a simple amount
--           return $ Just $ Mixed [a])
--          <|> return Nothing

-- http://ledger-cli.org/3.0/doc/ledger3.html#Fixing-Lot-Prices
fixedlotpricep :: ErroringJournalParser (Maybe Amount)
fixedlotpricep =
    try (do
          lift (many spacenonewline)
          char '{'
          lift (many spacenonewline)
          char '='
          lift (many spacenonewline)
          a <- amountp -- XXX should restrict to a simple amount
          lift (many spacenonewline)
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
numberp :: TextParser m (Quantity, Int, Maybe Char, Maybe DigitGroupStyle)
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

multilinecommentp :: ErroringJournalParser ()
multilinecommentp = do
  string "comment" >> lift (many spacenonewline) >> newline
  go
  where
    go = try (eof <|> (string "end comment" >> newline >> return ()))
         <|> (anyLine >> go)
    anyLine = anyChar `manyTill` newline

emptyorcommentlinep :: ErroringJournalParser ()
emptyorcommentlinep = do
  lift (many spacenonewline) >> (commentp <|> (lift (many spacenonewline) >> newline >> return ""))
  return ()

-- | Parse a possibly multi-line comment following a semicolon.
followingcommentp :: ErroringJournalParser Text
followingcommentp =
  -- ptrace "followingcommentp"
  do samelinecomment <- lift (many spacenonewline) >> (try semicoloncommentp <|> (newline >> return ""))
     newlinecomments <- many (try (lift (some spacenonewline) >> semicoloncommentp))
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
-- Left ...1:22...partial date 3/4 found, but the current year is unknown...
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
    sp1 <- lift (many spacenonewline)
    l1  <- try (lift semicoloncommentp') <|> (newline >> return "")
    ls  <- lift . many $ try ((++) <$> some spacenonewline <*> semicoloncommentp')
    return $ unlines $ (sp1 ++ l1) : ls
  let comment = T.pack $ unlines $ map (lstrip . dropWhile (==';') . strip) $ lines commentandwhitespace
  -- pdbg 0 $ "commentws:"++show commentandwhitespace
  -- pdbg 0 $ "comment:"++show comment

  -- Reparse the comment for any tags.
  tags <- case runTextParser (setPosition startpos >> tagsp) $ T.pack commentandwhitespace of
            Right ts -> return ts
            Left e   -> throwError $ parseErrorPretty e
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

commentp :: ErroringJournalParser Text
commentp = commentStartingWithp commentchars

commentchars :: [Char]
commentchars = "#;*"

semicoloncommentp :: ErroringJournalParser Text
semicoloncommentp = commentStartingWithp ";"

commentStartingWithp :: [Char] -> ErroringJournalParser Text
commentStartingWithp cs = do
  -- ptrace "commentStartingWith"
  oneOf cs
  lift (many spacenonewline)
  l <- anyChar `manyTill` (lift eolof)
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
tagsp :: Parser [Tag]
tagsp = -- do
  -- pdbg 0 $ "tagsp"
  many (try (nontagp >> tagp))

-- | Parse everything up till the first tag.
--
-- >>> rtp nontagp "\na b:, \nd:e, f"
-- Right "\na "
nontagp :: Parser String
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
tagp :: Parser Tag
tagp = do
  -- pdbg 0 "tagp"
  n <- tagnamep
  v <- tagvaluep
  return (n,v)

-- |
-- >>> rtp tagnamep "a:"
-- Right "a"
tagnamep :: Parser Text
tagnamep = -- do
  -- pdbg 0 "tagnamep"
  T.pack <$> some (noneOf (": \t\n" :: [Char])) <* char ':'

tagvaluep :: TextParser m Text
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
-- Left ...1:9...partial date 3/4 found, but the current year is unknown...
--
datetagp :: Maybe Day -> ErroringJournalParser (TagName,Day)
datetagp mdefdate = do
  -- pdbg 0 "datetagp"
  string "date"
  n <- T.pack . fromMaybe "" <$> optional (string "2")
  char ':'
  startpos <- getPosition
  v <- lift tagvaluep
  -- re-parse value as a date.
  j <- get
  let ep :: Either (ParseError Char Dec) Day
      ep = parseWithState'
             j{jparsedefaultyear=first3.toGregorian <$> mdefdate}
             -- The value extends to a comma, newline, or end of file.
             -- It seems like ignoring any extra stuff following a date
             -- gives better errors here.
             (do
                 setPosition startpos
                 datep) -- <* eof)
             v
  case ep
    of Left e  -> throwError $ parseErrorPretty e
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
-- Left ...1:11:...bad date: 2016/1/32...
--
-- >>> rejp (bracketeddatetagsp Nothing) "[1/31]"
-- Left ...1:6:...partial date 1/31 found, but the current year is unknown...
--
-- >>> rejp (bracketeddatetagsp Nothing) "[0123456789/-.=/-.=]"
-- Left ...1:15:...bad date, different separators...
--
bracketeddatetagsp :: Maybe Day -> ErroringJournalParser [(TagName, Day)]
bracketeddatetagsp mdefdate = do
  -- pdbg 0 "bracketeddatetagsp"
  char '['
  startpos <- getPosition
  let digits = "0123456789"
  s <- some (oneOf $ '=':digits++datesepchars)
  char ']'
  unless (any (`elem` s) digits && any (`elem` datesepchars) s) $
    fail "not a bracketed date"

  -- looks sufficiently like a bracketed date, now we
  -- re-parse as dates and throw any errors
  j <- get
  let ep :: Either (ParseError Char Dec) (Maybe Day, Maybe Day)
      ep = parseWithState'
             j{jparsedefaultyear=first3.toGregorian <$> mdefdate}
             (do
               setPosition startpos
               md1 <- optional datep
               maybe (return ()) (setYear.first3.toGregorian) md1
               md2 <- optional $ char '=' >> datep
               eof
               return (md1,md2)
             )
             (T.pack s)
  case ep
    of Left e          -> throwError $ parseErrorPretty e
       Right (md1,md2) -> return $ catMaybes
         [("date",) <$> md1, ("date2",) <$> md2]

