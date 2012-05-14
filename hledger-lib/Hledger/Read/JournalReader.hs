{-# LANGUAGE RecordWildCards #-}
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
  getParentAccount,
  journal,
  directive,
  defaultyeardirective,
  historicalpricedirective,
  datetime,
  accountname,
  amount,
  emptyline,
  -- * Tests
  tests_Hledger_Read_JournalReader
)
where
import Control.Monad
import Control.Monad.Error
import Data.Char (isNumber)
import Data.Either (partitionEithers)
import Data.List
import Data.List.Split (wordsBy)
import Data.Maybe
import Data.Time.Calendar
import Data.Time.LocalTime
import Safe (headDef)
import Test.HUnit
import Text.ParserCombinators.Parsec hiding (parse)
import Text.Printf
import System.FilePath
import System.Time (getClockTime)

import Hledger.Data
import Hledger.Utils
import Prelude hiding (readFile)
import Hledger.Utils.UTF8IOCompat (readFile)


-- standard reader exports

reader :: Reader
reader = Reader format detect parse

format :: String
format = "journal"

-- | Does the given file path and data provide hledger's journal file format ?
detect :: FilePath -> String -> Bool
detect f _ = takeExtension f == format

-- | Parse and post-process a "Journal" from hledger's journal file
-- format, or give an error.
parse :: Maybe FilePath -> FilePath -> String -> ErrorT String IO Journal
parse _ = parseJournalWith journal

-- parsing utils

-- | Flatten a list of JournalUpdate's into a single equivalent one.
combineJournalUpdates :: [JournalUpdate] -> JournalUpdate
combineJournalUpdates us = liftM (foldr (.) id) $ sequence us

-- | Given a JournalUpdate-generating parsec parser, file path and data string,
-- parse and post-process a Journal so that it's ready to use, or give an error.
parseJournalWith :: (GenParser Char JournalContext (JournalUpdate,JournalContext)) -> FilePath -> String -> ErrorT String IO Journal
parseJournalWith p f s = do
  tc <- liftIO getClockTime
  tl <- liftIO getCurrentLocalTime
  y <- liftIO getCurrentYear
  case runParser p nullctx{ctxYear=Just y} f s of
    Right (updates,ctx) -> do
                           j <- updates `ap` return nulljournal
                           case journalFinalise tc tl f s ctx j of
                             Right j'  -> return j'
                             Left estr -> throwError estr
    Left e -> throwError $ show e

setYear :: Integer -> GenParser tok JournalContext ()
setYear y = updateState (\ctx -> ctx{ctxYear=Just y})

getYear :: GenParser tok JournalContext (Maybe Integer)
getYear = liftM ctxYear getState

setCommodity :: Commodity -> GenParser tok JournalContext ()
setCommodity c = updateState (\ctx -> ctx{ctxCommodity=Just c})

getCommodity :: GenParser tok JournalContext (Maybe Commodity)
getCommodity = liftM ctxCommodity getState

pushParentAccount :: String -> GenParser tok JournalContext ()
pushParentAccount parent = updateState addParentAccount
    where addParentAccount ctx0 = ctx0 { ctxAccount = parent : ctxAccount ctx0 }

popParentAccount :: GenParser tok JournalContext ()
popParentAccount = do ctx0 <- getState
                      case ctxAccount ctx0 of
                        [] -> unexpected "End of account block with no beginning"
                        (_:rest) -> setState $ ctx0 { ctxAccount = rest }

getParentAccount :: GenParser tok JournalContext String
getParentAccount = liftM (concatAccountNames . reverse . ctxAccount) getState

addAccountAlias :: (AccountName,AccountName) -> GenParser tok JournalContext ()
addAccountAlias a = updateState (\(ctx@Ctx{..}) -> ctx{ctxAliases=a:ctxAliases})

getAccountAliases :: GenParser tok JournalContext [(AccountName,AccountName)]
getAccountAliases = liftM ctxAliases getState

clearAccountAliases :: GenParser tok JournalContext ()
clearAccountAliases = updateState (\(ctx@Ctx{..}) -> ctx{ctxAliases=[]})

-- parsers

-- | Top-level journal parser. Returns a single composite, I/O performing,
-- error-raising "JournalUpdate" (and final "JournalContext") which can be
-- applied to an empty journal to get the final result.
journal :: GenParser Char JournalContext (JournalUpdate,JournalContext)
journal = do
  journalupdates <- many journalItem
  eof
  finalctx <- getState
  return $ (combineJournalUpdates journalupdates, finalctx)
    where 
      -- As all journal line types can be distinguished by the first
      -- character, excepting transactions versus empty (blank or
      -- comment-only) lines, can use choice w/o try
      journalItem = choice [ directive
                           , liftM (return . addTransaction) transaction
                           , liftM (return . addModifierTransaction) modifiertransaction
                           , liftM (return . addPeriodicTransaction) periodictransaction
                           , liftM (return . addHistoricalPrice) historicalpricedirective
                           , emptyline >> return (return id)
                           ] <?> "journal transaction or directive"

emptyline :: GenParser Char JournalContext ()
emptyline = do many spacenonewline
               optional $ (char ';' <?> "comment") >> many (noneOf "\n")
               newline
               return ()

comment :: GenParser Char JournalContext String
comment = do
  many1 $ char ';'
  many spacenonewline
  many (noneOf "\n")
  <?> "comment"

commentline :: GenParser Char JournalContext String
commentline = do
  many spacenonewline
  s <- comment
  optional newline
  eof
  return s
  <?> "comment"

-- cf http://ledger-cli.org/3.0/doc/ledger3.html#Command-Directives
directive :: GenParser Char JournalContext JournalUpdate
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

includedirective :: GenParser Char JournalContext JournalUpdate
includedirective = do
  string "include"
  many1 spacenonewline
  filename <- restofline
  outerState <- getState
  outerPos <- getPosition
  return $ do filepath <- expandPath outerPos filename
              txt <- readFileOrError outerPos filepath
              let inIncluded = show outerPos ++ " in included file " ++ show filename ++ ":\n"
              case runParser journal outerState filepath txt of
                Right (ju,_) -> combineJournalUpdates [return $ journalAddFile (filepath,txt), ju] `catchError` (throwError . (inIncluded ++))
                Left err     -> throwError $ inIncluded ++ show err
      where readFileOrError pos fp =
                ErrorT $ liftM Right (readFile fp) `catch`
                  \err -> return $ Left $ printf "%s reading %s:\n%s" (show pos) fp (show err)

journalAddFile :: (FilePath,String) -> Journal -> Journal
journalAddFile f j@Journal{files=fs} = j{files=fs++[f]}

accountdirective :: GenParser Char JournalContext JournalUpdate
accountdirective = do
  string "account"
  many1 spacenonewline
  parent <- accountname
  newline
  pushParentAccount parent
  return $ return id

enddirective :: GenParser Char JournalContext JournalUpdate
enddirective = do
  string "end"
  popParentAccount
  return (return id)

aliasdirective :: GenParser Char JournalContext JournalUpdate
aliasdirective = do
  string "alias"
  many1 spacenonewline
  orig <- many1 $ noneOf "="
  char '='
  alias <- restofline
  addAccountAlias (accountNameWithoutPostingType $ strip orig
                  ,accountNameWithoutPostingType $ strip alias)
  return $ return id

endaliasesdirective :: GenParser Char JournalContext JournalUpdate
endaliasesdirective = do
  string "end aliases"
  clearAccountAliases
  return (return id)

tagdirective :: GenParser Char JournalContext JournalUpdate
tagdirective = do
  string "tag" <?> "tag directive"
  many1 spacenonewline
  _ <- many1 nonspace
  restofline
  return $ return id

endtagdirective :: GenParser Char JournalContext JournalUpdate
endtagdirective = do
  (string "end tag" <|> string "pop") <?> "end tag or pop directive"
  restofline
  return $ return id

defaultyeardirective :: GenParser Char JournalContext JournalUpdate
defaultyeardirective = do
  char 'Y' <?> "default year"
  many spacenonewline
  y <- many1 digit
  let y' = read y
  failIfInvalidYear y
  setYear y'
  return $ return id

defaultcommoditydirective :: GenParser Char JournalContext JournalUpdate
defaultcommoditydirective = do
  char 'D' <?> "default commodity"
  many1 spacenonewline
  a <- amount
  -- amount always returns a MixedAmount containing one Amount, but let's be safe
  let as = amounts a
  when (not $ null as) $ setCommodity $ commodity $ head as
  restofline
  return $ return id

historicalpricedirective :: GenParser Char JournalContext HistoricalPrice
historicalpricedirective = do
  char 'P' <?> "historical price"
  many spacenonewline
  date <- try (do {LocalTime d _ <- datetime; return d}) <|> date -- a time is ignored
  many1 spacenonewline
  symbol <- commoditysymbol
  many spacenonewline
  price <- amount
  restofline
  return $ HistoricalPrice date symbol price

ignoredpricecommoditydirective :: GenParser Char JournalContext JournalUpdate
ignoredpricecommoditydirective = do
  char 'N' <?> "ignored-price commodity"
  many1 spacenonewline
  commoditysymbol
  restofline
  return $ return id

commodityconversiondirective :: GenParser Char JournalContext JournalUpdate
commodityconversiondirective = do
  char 'C' <?> "commodity conversion"
  many1 spacenonewline
  amount
  many spacenonewline
  char '='
  many spacenonewline
  amount
  restofline
  return $ return id

modifiertransaction :: GenParser Char JournalContext ModifierTransaction
modifiertransaction = do
  char '=' <?> "modifier transaction"
  many spacenonewline
  valueexpr <- restofline
  postings <- postings
  return $ ModifierTransaction valueexpr postings

periodictransaction :: GenParser Char JournalContext PeriodicTransaction
periodictransaction = do
  char '~' <?> "periodic transaction"
  many spacenonewline
  periodexpr <- restofline
  postings <- postings
  return $ PeriodicTransaction periodexpr postings

-- | Parse a (possibly unbalanced) ledger transaction.
transaction :: GenParser Char JournalContext Transaction
transaction = do
  date <- date <?> "transaction"
  edate <- optionMaybe (effectivedate date) <?> "effective date"
  status <- status <?> "cleared flag"
  code <- code <?> "transaction code"
  -- now there can be whitespace followed by a description and/or comment/metadata comment
  let pdescription = many (noneOf ";\n") >>= return . strip
  (description, inlinecomment, inlinemd) <-
    try (do many1 spacenonewline
            d <- pdescription
            (c, m) <- ledgerInlineCommentOrMetadata
            return (d,c,m))
    <|> (newline >> return ("", [], []))
  (nextlinecomments, nextlinemds) <- ledgerCommentsAndMetadata
  let comment = intercalate "\n" $ inlinecomment ++ map ("    ; "++) nextlinecomments
      mds = inlinemd ++ nextlinemds

  postings <- postings
  return $ txnTieKnot $ Transaction date edate status code description comment mds postings ""

-- | Parse a date in YYYY/MM/DD format. Fewer digits are allowed. The year
-- may be omitted if a default year has already been set.
date :: GenParser Char JournalContext Day
date = do
  -- hacky: try to ensure precise errors for invalid dates
  -- XXX reported error position is not too good
  -- pos <- getPosition
  datestr <- many1 $ choice' [digit, datesepchar]
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

-- | Parse a date and time in YYYY/MM/DD HH:MM[:SS][+-ZZZZ] format.  Any
-- timezone will be ignored; the time is treated as local time.  Fewer
-- digits are allowed, except in the timezone. The year may be omitted if
-- a default year has already been set.
datetime :: GenParser Char JournalContext LocalTime
datetime = do
  day <- date
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

effectivedate :: Day -> GenParser Char JournalContext Day
effectivedate actualdate = do
  char '='
  -- kludgy way to use actual date for default year
  let withDefaultYear d p = do
        y <- getYear
        let (y',_,_) = toGregorian d in setYear y'
        r <- p
        when (isJust y) $ setYear $ fromJust y
        return r
  edate <- withDefaultYear actualdate date
  return edate

status :: GenParser Char JournalContext Bool
status = try (do { many spacenonewline; char '*' <?> "status"; return True } ) <|> return False

code :: GenParser Char JournalContext String
code = try (do { many1 spacenonewline; char '(' <?> "code"; code <- anyChar `manyTill` char ')'; return code } ) <|> return ""

type Tag = (String, String)

ledgerInlineCommentOrMetadata :: GenParser Char JournalContext ([String],[Tag])
ledgerInlineCommentOrMetadata = try (do {md <- metadatacomment; newline; return ([], [md])})
                               <|> (do {c <- comment; newline; return ([c], [])})
                               <|> (newline >> return ([], []))

ledgerCommentsAndMetadata :: GenParser Char JournalContext ([String],[Tag])
ledgerCommentsAndMetadata = do
  comormds <- many $ choice' [(liftM Right metadataline)
                             ,(do {many1 spacenonewline; c <- comment; newline; return $ Left c }) -- XXX fix commentnewline
                             ]
  return $ partitionEithers comormds

-- a comment line containing a metadata declaration, eg:
-- ; name: value
metadataline :: GenParser Char JournalContext (String,String)
metadataline = do
  many1 spacenonewline
  md <- metadatacomment
  newline
  return md

-- a comment containing a ledger-style metadata declaration, like:
-- ; name: some value
metadatacomment :: GenParser Char JournalContext (String,String)
metadatacomment = do
  many1 $ char ';'
  many spacenonewline
  name <- many1 $ noneOf ": \t"
  char ':'
  many spacenonewline
  value <- many (noneOf "\n")
  return (name,value)
  <?> "metadata comment"

-- Parse the following whitespace-beginning lines as postings, posting metadata, and/or comments.
-- complicated to handle intermixed comment and metadata lines.. make me better ?
postings :: GenParser Char JournalContext [Posting]
postings = many1 posting <?> "postings"
            
-- linebeginningwithspaces :: GenParser Char JournalContext String
-- linebeginningwithspaces = do
--   sp <- many1 spacenonewline
--   c <- nonspace
--   cs <- restofline
--   return $ sp ++ (c:cs) ++ "\n"

posting :: GenParser Char JournalContext Posting
posting = do
  many1 spacenonewline
  status <- status
  many spacenonewline
  account <- modifiedaccountname
  let (ptype, account') = (accountNamePostingType account, unbracket account)
  amount <- spaceandamountormissing
  many spacenonewline
  (inlinecomment, inlinemd) <- ledgerInlineCommentOrMetadata
  (nextlinecomments, nextlinemds) <- ledgerCommentsAndMetadata
  let comment = intercalate "\n" $ inlinecomment ++ map ("    ; "++) nextlinecomments
      mds = inlinemd ++ nextlinemds

  return (Posting status account' amount comment ptype mds Nothing)

-- | Parse an account name, then apply any parent account prefix and/or account aliases currently in effect.
modifiedaccountname :: GenParser Char JournalContext AccountName
modifiedaccountname = do
  a <- accountname
  prefix <- getParentAccount
  let prefixed = prefix `joinAccountNames` a
  aliases <- getAccountAliases
  return $ accountNameApplyAliases aliases prefixed

-- | Parse an account name. Account names may have single spaces inside
-- them, and are terminated by two or more spaces. They should have one or
-- more components of at least one character, separated by the account
-- separator char.
accountname :: GenParser Char st AccountName
accountname = do
    a <- many1 (nonspace <|> singlespace)
    let a' = striptrailingspace a
    when (accountNameFromComponents (accountNameComponents a') /= a')
         (fail $ "accountname seems ill-formed: "++a')
    return a'
    where 
      singlespace = try (do {spacenonewline; do {notFollowedBy spacenonewline; return ' '}})
      -- couldn't avoid consuming a final space sometimes, harmless
      striptrailingspace s = if last s == ' ' then init s else s

-- accountnamechar = notFollowedBy (oneOf "()[]") >> nonspace
--     <?> "account name character (non-bracket, non-parenthesis, non-whitespace)"

-- | Parse whitespace then an amount, with an optional left or right
-- currency symbol and optional price, or return the special
-- "missing"" marker amount.
spaceandamountormissing :: GenParser Char JournalContext MixedAmount
spaceandamountormissing =
  try (do
        many1 spacenonewline
        amount <|> return missingamt
      ) <|> return missingamt

tests_spaceandamountormissing = [
   "spaceandamountormissing" ~: do
    assertParseEqual (parseWithCtx nullctx spaceandamountormissing " $47.18") (Mixed [dollars 47.18])
    assertParseEqual (parseWithCtx nullctx spaceandamountormissing "$47.18") missingamt
    assertParseEqual (parseWithCtx nullctx spaceandamountormissing " ") missingamt
    assertParseEqual (parseWithCtx nullctx spaceandamountormissing "") missingamt
 ]

-- | Parse an amount, with an optional left or right currency symbol and
-- optional price.
amount :: GenParser Char JournalContext MixedAmount
amount = try leftsymbolamount <|> try rightsymbolamount <|> nosymbolamount

tests_amount = [
   "amount" ~: do
    assertParseEqual (parseWithCtx nullctx amount "$47.18") (Mixed [dollars 47.18])
    assertParseEqual (parseWithCtx nullctx amount "$1.")
                (Mixed [Amount Commodity {symbol="$",side=L,spaced=False,decimalpoint='.',precision=0,separator=',',separatorpositions=[]} 1 Nothing])
  ,"amount with unit price" ~: do
    assertParseEqual
     (parseWithCtx nullctx amount "$10 @ €0.5")
     (Mixed [Amount{commodity=dollar{precision=0},
                    quantity=10,
                    price=(Just $ UnitPrice $ Mixed [Amount{commodity=euro{precision=1},
                                                            quantity=0.5,
                                                            price=Nothing}])}])
  ,"amount with total price" ~: do
    assertParseEqual
     (parseWithCtx nullctx amount "$10 @@ €5")
     (Mixed [Amount{commodity=dollar{precision=0},
                    quantity=10,
                    price=(Just $ TotalPrice $ Mixed [Amount{commodity=euro{precision=0},
                                                             quantity=5,
                                                             price=Nothing}])}])
 ]

leftsymbolamount :: GenParser Char JournalContext MixedAmount
leftsymbolamount = do
  sign <- optionMaybe $ string "-"
  let applysign = if isJust sign then negate else id
  sym <- commoditysymbol 
  sp <- many spacenonewline
  (q,p,d,s,spos) <- number
  pri <- priceamount
  let c = Commodity {symbol=sym,side=L,spaced=not $ null sp,decimalpoint=d,precision=p,separator=s,separatorpositions=spos}
  return $ applysign $ Mixed [Amount c q pri]
  <?> "left-symbol amount"

rightsymbolamount :: GenParser Char JournalContext MixedAmount
rightsymbolamount = do
  (q,p,d,s,spos) <- number
  sp <- many spacenonewline
  sym <- commoditysymbol
  pri <- priceamount
  let c = Commodity {symbol=sym,side=R,spaced=not $ null sp,decimalpoint=d,precision=p,separator=s,separatorpositions=spos}
  return $ Mixed [Amount c q pri]
  <?> "right-symbol amount"

nosymbolamount :: GenParser Char JournalContext MixedAmount
nosymbolamount = do
  (q,p,d,s,spos) <- number
  pri <- priceamount
  defc <- getCommodity
  let c = fromMaybe Commodity{symbol="",side=L,spaced=False,decimalpoint=d,precision=p,separator=s,separatorpositions=spos} defc
  return $ Mixed [Amount c q pri]
  <?> "no-symbol amount"

commoditysymbol :: GenParser Char JournalContext String
commoditysymbol = (quotedcommoditysymbol <|> simplecommoditysymbol) <?> "commodity symbol"

quotedcommoditysymbol :: GenParser Char JournalContext String
quotedcommoditysymbol = do
  char '"'
  s <- many1 $ noneOf ";\n\""
  char '"'
  return s

simplecommoditysymbol :: GenParser Char JournalContext String
simplecommoditysymbol = many1 (noneOf nonsimplecommoditychars)

priceamount :: GenParser Char JournalContext (Maybe Price)
priceamount =
    try (do
          many spacenonewline
          char '@'
          try (do
                char '@'
                many spacenonewline
                a <- amount -- XXX can parse more prices ad infinitum, shouldn't
                return $ Just $ TotalPrice a)
           <|> (do
            many spacenonewline
            a <- amount -- XXX can parse more prices ad infinitum, shouldn't
            return $ Just $ UnitPrice a))
         <|> return Nothing

-- | Parse a numeric quantity for its value and display attributes.  Some
-- international number formats (cf
-- http://en.wikipedia.org/wiki/Decimal_separator) are accepted: either
-- period or comma may be used for the decimal point, and the other of
-- these may be used for separating digit groups in the integer part (eg a
-- thousands separator).  This returns the numeric value, the precision
-- (number of digits to the right of the decimal point), the decimal point
-- and separator characters (defaulting to . and ,), and the positions of
-- separators (counting leftward from the decimal point, the last is
-- assumed to repeat).
number :: GenParser Char JournalContext (Quantity, Int, Char, Char, [Int])
number = do
  sign <- optionMaybe $ string "-"
  parts <- many1 $ choice' [many1 digit, many1 $ char ',', many1 $ char '.']
  let numeric = isNumber . headDef '_'
      (_, puncparts) = partition numeric parts
      (ok,decimalpoint',separator') =
          case puncparts of
            []     -> (True, Nothing, Nothing)  -- no punctuation chars
            [d:""] -> (True, Just d, Nothing)   -- just one punctuation char, assume it's a decimal point
            [_]    -> (False, Nothing, Nothing) -- adjacent punctuation chars, not ok
            _:_:_  -> let (s:ss, d) = (init puncparts, last puncparts) -- two or more punctuation chars
                     in if (any ((/=1).length) puncparts  -- adjacent punctuation chars, not ok
                            || any (s/=) ss                -- separator chars differ, not ok
                            || head parts == s)            -- number begins with a separator char, not ok
                         then (False, Nothing, Nothing)
                         else if s == d
                               then (True, Nothing, Just $ head s) -- just one kind of punctuation, assume separator chars
                               else (True, Just $ head d, Just $ head s) -- separators and a decimal point
  when (not ok) (fail $ "number seems ill-formed: "++concat parts)
  let (intparts',fracparts') = span ((/= decimalpoint') . Just . head) parts
      (intparts, fracpart) = (filter numeric intparts', filter numeric fracparts')
      separatorpositions = reverse $ map length $ drop 1 intparts
      int = concat $ "":intparts
      frac = concat $ "":fracpart
      precision = length frac
      int' = if null int then "0" else int
      frac' = if null frac then "0" else frac
      sign' = fromMaybe "" sign
      quantity = read $ sign'++int'++"."++frac' -- this read should never fail
      (decimalpoint, separator) = case (decimalpoint', separator') of (Just d,  Just s)   -> (d,s)
                                                                      (Just '.',Nothing)  -> ('.',',')
                                                                      (Just ',',Nothing)  -> (',','.')
                                                                      (Nothing, Just '.') -> (',','.')
                                                                      (Nothing, Just ',') -> ('.',',')
                                                                      _                   -> ('.',',')
  return (quantity,precision,decimalpoint,separator,separatorpositions)
  <?> "number"

tests_Hledger_Read_JournalReader = TestList $ concat [
    tests_amount,
    tests_spaceandamountormissing,
    [
    "number" ~: do
      let s `is` n = assertParseEqual (parseWithCtx nullctx number s) n
          assertFails = assertBool "" . isLeft . parseWithCtx nullctx number 
      assertFails ""
      "0"          `is` (0, 0, '.', ',', [])
      "1"          `is` (1, 0, '.', ',', [])
      "1.1"        `is` (1.1, 1, '.', ',', [])
      "1,000.1"    `is` (1000.1, 1, '.', ',', [3])
      "1.00.000,1" `is` (100000.1, 1, ',', '.', [3,2])
      "1,000,000"  `is` (1000000, 0, '.', ',', [3,3])
      "1."         `is` (1,   0, '.', ',', [])
      "1,"         `is` (1,   0, ',', '.', [])
      ".1"         `is` (0.1, 1, '.', ',', [])
      ",1"         `is` (0.1, 1, ',', '.', [])
      assertFails "1,000.000,1"
      assertFails "1.000,000.1"
      assertFails "1,000.000.1"
      assertFails "1,,1"
      assertFails "1..1"
      assertFails ".1,"
      assertFails ",1."

   ,"transaction" ~: do
    assertParseEqual (parseWithCtx nullctx transaction entry1_str) entry1
    assertBool "transaction should not parse just a date"
                   $ isLeft $ parseWithCtx nullctx transaction "2009/1/1\n"
    assertBool "transaction should require some postings"
                   $ isLeft $ parseWithCtx nullctx transaction "2009/1/1 a\n"
    let t = parseWithCtx nullctx transaction "2009/1/1 a ;comment\n b 1\n"
    assertBool "transaction should not include a comment in the description"
                   $ either (const False) ((== "a") . tdescription) t

  ,"modifiertransaction" ~: do
     assertParse (parseWithCtx nullctx modifiertransaction "= (some value expr)\n some:postings  1\n")

  ,"periodictransaction" ~: do
     assertParse (parseWithCtx nullctx periodictransaction "~ (some period expr)\n some:postings  1\n")

  ,"directive" ~: do
     assertParse (parseWithCtx nullctx directive "!include /some/file.x\n")
     assertParse (parseWithCtx nullctx directive "account some:account\n")
     assertParse (parseWithCtx nullctx (directive >> directive) "!account a\nend\n")

  ,"commentline" ~: do
     assertParse (parseWithCtx nullctx commentline "; some comment \n")
     assertParse (parseWithCtx nullctx commentline " \t; x\n")
     assertParse (parseWithCtx nullctx commentline ";x")

  ,"date" ~: do
     assertParse (parseWithCtx nullctx date "2011/1/1")
     assertParseFailure (parseWithCtx nullctx date "1/1")
     assertParse (parseWithCtx nullctx{ctxYear=Just 2011} date "1/1")

  ,"datetime" ~: do
      let p = do {t <- datetime; eof; return t}
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

  ,"historicalpricedirective" ~:
    assertParseEqual (parseWithCtx nullctx historicalpricedirective "P 2004/05/01 XYZ $55.00\n") (HistoricalPrice (parsedate "2004/05/01") "XYZ" $ Mixed [dollars 55])

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
  ,"endtagdirective" ~: do
     assertParse (parseWithCtx nullctx endtagdirective "pop \n")

  ,"accountname" ~: do
    assertBool "accountname parses a normal accountname" (isRight $ parsewith accountname "a:b:c")
    assertBool "accountname rejects an empty inner component" (isLeft $ parsewith accountname "a::c")
    assertBool "accountname rejects an empty leading component" (isLeft $ parsewith accountname ":b:c")
    assertBool "accountname rejects an empty trailing component" (isLeft $ parsewith accountname "a:b:")

 ,"posting" ~: do
    assertParseEqual (parseWithCtx nullctx posting "  expenses:food:dining  $10.00   ; a: a a \n   ; b: b b \n")
                     (Posting False "expenses:food:dining" (Mixed [dollars 10]) "" RegularPosting [("a","a a "), ("b","b b ")] Nothing)
    assertBool "posting parses a quoted commodity with numbers"
                   (isRight $ parseWithCtx nullctx posting "  a  1 \"DE123\"\n")

  ,"amount" ~: do
     let -- | compare a parse result with a MixedAmount, showing the debug representation for clarity
         assertMixedAmountParse parseresult mixedamount =
             (either (const "parse error") showMixedAmountDebug parseresult) ~?= (showMixedAmountDebug mixedamount)
     assertMixedAmountParse (parseWithCtx nullctx amount "1 @ $2")
                            (Mixed [Amount unknown 1 (Just $ UnitPrice $ Mixed [Amount dollar{precision=0} 2 Nothing])])

  ,"leftsymbolamount" ~: do
    assertParseEqual (parseWithCtx nullctx leftsymbolamount "$1")
                     (Mixed [Amount Commodity {symbol="$",side=L,spaced=False,decimalpoint='.',precision=0,separator=',',separatorpositions=[]} 1 Nothing])
    assertParseEqual (parseWithCtx nullctx leftsymbolamount "$-1")
                     (Mixed [Amount Commodity {symbol="$",side=L,spaced=False,decimalpoint='.',precision=0,separator=',',separatorpositions=[]} (-1) Nothing])
    assertParseEqual (parseWithCtx nullctx leftsymbolamount "-$1")
                     (Mixed [Amount Commodity {symbol="$",side=L,spaced=False,decimalpoint='.',precision=0,separator=',',separatorpositions=[]} (-1) Nothing])

 ]]

entry1_str = unlines
 ["2007/01/28 coopportunity"
 ,"    expenses:food:groceries                   $47.18"
 ,"    assets:checking                          $-47.18"
 ,""
 ]

entry1 =
    txnTieKnot $ Transaction (parsedate "2007/01/28") Nothing False "" "coopportunity" "" []
     [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting [] Nothing, 
      Posting False "assets:checking" (Mixed [dollars (-47.18)]) "" RegularPosting [] Nothing] ""

