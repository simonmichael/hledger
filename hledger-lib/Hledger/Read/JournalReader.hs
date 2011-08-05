{-# LANGUAGE CPP #-}
{-|

A reader for hledger's (and c++ ledger's) journal file format.

From the ledger 2.5 manual:

@
The ledger file format is quite simple, but also very flexible. It supports
many options, though typically the user can ignore most of them. They are
summarized below.  The initial character of each line determines what the
line means, and how it should be interpreted. Allowable initial characters
are:

NUMBER      A line beginning with a number denotes an entry. It may be followed by any
            number of lines, each beginning with whitespace, to denote the entry’s account
            transactions. The format of the first line is:

                    DATE[=EDATE] [*|!] [(CODE)] DESC

            If ‘*’ appears after the date (with optional eﬀective date), it indicates the entry
            is “cleared”, which can mean whatever the user wants it t omean. If ‘!’ appears
            after the date, it indicates d the entry is “pending”; i.e., tentatively cleared from
            the user’s point of view, but not yet actually cleared. If a ‘CODE’ appears in
            parentheses, it may be used to indicate a check number, or the type of the
            transaction. Following these is the payee, or a description of the transaction.
            The format of each following transaction is:

                      ACCOUNT     AMOUNT    [; NOTE]

            The ‘ACCOUNT’ may be surrounded by parentheses if it is a virtual
            transactions, or square brackets if it is a virtual transactions that must
            balance. The ‘AMOUNT’ can be followed by a per-unit transaction cost,
            by specifying ‘ AMOUNT’, or a complete transaction cost with ‘\@ AMOUNT’.
            Lastly, the ‘NOTE’ may specify an actual and/or eﬀective date for the
            transaction by using the syntax ‘[ACTUAL_DATE]’ or ‘[=EFFECTIVE_DATE]’ or
            ‘[ACTUAL_DATE=EFFECtIVE_DATE]’.

=           An automated entry. A value expression must appear after the equal sign.
            After this initial line there should be a set of one or more transactions, just as
            if it were normal entry. If the amounts of the transactions have no commodity,
            they will be applied as modifiers to whichever real transaction is matched by
            the value expression.
 
~           A period entry. A period expression must appear after the tilde.
            After this initial line there should be a set of one or more transactions, just as
            if it were normal entry.

!           A line beginning with an exclamation mark denotes a command directive. It
            must be immediately followed by the command word. The supported commands
            are:

           ‘!include’
                        Include the stated ledger file.
           ‘!account’
                        The account name is given is taken to be the parent of all transac-
                        tions that follow, until ‘!end’ is seen.
           ‘!end’       Ends an account block.
 
;          A line beginning with a colon indicates a comment, and is ignored.
 
Y          If a line begins with a capital Y, it denotes the year used for all subsequent
           entries that give a date without a year. The year should appear immediately
           after the Y, for example: ‘Y2004’. This is useful at the beginning of a file, to
           specify the year for that file. If all entries specify a year, however, this command
           has no eﬀect.
           
 
P          Specifies a historical price for a commodity. These are usually found in a pricing
           history file (see the ‘-Q’ option). The syntax is:

                  P DATE SYMBOL PRICE

N SYMBOL   Indicates that pricing information is to be ignored for a given symbol, nor will
           quotes ever be downloaded for that symbol. Useful with a home currency, such
           as the dollar ($). It is recommended that these pricing options be set in the price
           database file, which defaults to ‘~/.pricedb’. The syntax for this command is:

                  N SYMBOL

        
D AMOUNT   Specifies the default commodity to use, by specifying an amount in the expected
           format. The entry command will use this commodity as the default when none
           other can be determined. This command may be used multiple times, to set
           the default flags for diﬀerent commodities; whichever is seen last is used as the
           default commodity. For example, to set US dollars as the default commodity,
           while also setting the thousands flag and decimal flag for that commodity, use:

                  D $1,000.00

C AMOUNT1 = AMOUNT2
           Specifies a commodity conversion, where the first amount is given to be equiv-
           alent to the second amount. The first amount should use the decimal precision
           desired during reporting:

                  C 1.00 Kb = 1024 bytes

i, o, b, h
           These four relate to timeclock support, which permits ledger to read timelog
           files. See the timeclock’s documentation for more info on the syntax of its
           timelog files.
@

-}

module Hledger.Read.JournalReader (
       emptyLine,
       journalAddFile,
       journalFile,
       ledgeraccountname,
       ledgerdatetime,
       ledgerDefaultYear,
       ledgerDirective,
       ledgerHistoricalPrice,
       reader,
       someamount,
       tests_Hledger_Read_JournalReader
)
where
import Control.Monad
import Control.Monad.Error
import Data.Char (isNumber)
import Data.List
import Data.List.Split (wordsBy)
import Data.Maybe
import Data.Time.Calendar
import Data.Time.LocalTime
import Safe (headDef)
import Test.HUnit
import Text.ParserCombinators.Parsec hiding (parse)
import Text.Printf

import Hledger.Data
import Hledger.Read.Utils
import Hledger.Utils
import Prelude hiding (readFile)
import Hledger.Utils.UTF8 (readFile)


-- let's get to it

reader :: Reader
reader = Reader format detect parse

format :: String
format = "journal"

-- | Does the given file path and data provide hledger's journal file format ?
detect :: FilePath -> String -> Bool
detect f _ = fileSuffix f == format

-- | Parse and post-process a "Journal" from hledger's journal file
-- format, or give an error.
parse :: FilePath -> String -> ErrorT String IO Journal
parse = parseJournalWith journalFile

-- | Top-level journal parser. Returns a single composite, I/O performing,
-- error-raising "JournalUpdate" (and final "JournalContext") which can be
-- applied to an empty journal to get the final result.
journalFile :: GenParser Char JournalContext (JournalUpdate,JournalContext)
journalFile = do
  journalupdates <- many journalItem
  eof
  finalctx <- getState
  return $ (juSequence journalupdates, finalctx)
    where 
      -- As all journal line types can be distinguished by the first
      -- character, excepting transactions versus empty (blank or
      -- comment-only) lines, can use choice w/o try
      journalItem = choice [ ledgerDirective
                           , liftM (return . addTransaction) ledgerTransaction
                           , liftM (return . addModifierTransaction) ledgerModifierTransaction
                           , liftM (return . addPeriodicTransaction) ledgerPeriodicTransaction
                           , liftM (return . addHistoricalPrice) ledgerHistoricalPrice
                           , emptyLine >> return (return id)
                           ] <?> "journal transaction or directive"

emptyLine :: GenParser Char JournalContext ()
emptyLine = do many spacenonewline
               optional $ (char ';' <?> "comment") >> many (noneOf "\n")
               newline
               return ()

ledgercomment :: GenParser Char JournalContext String
ledgercomment = do
  many1 $ char ';'
  many spacenonewline
  many (noneOf "\n")
  <?> "comment"

ledgercommentline :: GenParser Char JournalContext String
ledgercommentline = do
  many spacenonewline
  s <- ledgercomment
  optional newline
  eof
  return s
  <?> "comment"

ledgerDirective :: GenParser Char JournalContext JournalUpdate
ledgerDirective = do
  optional $ char '!'
  choice' [
    ledgerInclude
   ,ledgerAlias
   ,ledgerEndAliases
   ,ledgerAccountBegin
   ,ledgerAccountEnd
   ,ledgerTagDirective
   ,ledgerEndTagDirective
   ,ledgerDefaultYear
   ,ledgerDefaultCommodity
   ,ledgerCommodityConversion
   ,ledgerIgnoredPriceCommodity
   ]
  <?> "directive"

ledgerInclude :: GenParser Char JournalContext JournalUpdate
ledgerInclude = do
  string "include"
  many1 spacenonewline
  filename <- restofline
  outerState <- getState
  outerPos <- getPosition
  return $ do filepath <- expandPath outerPos filename
              txt <- readFileOrError outerPos filepath
              let inIncluded = show outerPos ++ " in included file " ++ show filename ++ ":\n"
              case runParser journalFile outerState filepath txt of
                Right (ju,_) -> juSequence [return $ journalAddFile (filepath,txt), ju] `catchError` (throwError . (inIncluded ++))
                Left err     -> throwError $ inIncluded ++ show err
      where readFileOrError pos fp =
                ErrorT $ liftM Right (readFile fp) `catch`
                  \err -> return $ Left $ printf "%s reading %s:\n%s" (show pos) fp (show err)

journalAddFile :: (FilePath,String) -> Journal -> Journal
journalAddFile f j@Journal{files=fs} = j{files=fs++[f]}

ledgerAccountBegin :: GenParser Char JournalContext JournalUpdate
ledgerAccountBegin = do
  string "account"
  many1 spacenonewline
  parent <- ledgeraccountname
  newline
  pushParentAccount parent
  return $ return id

ledgerAccountEnd :: GenParser Char JournalContext JournalUpdate
ledgerAccountEnd = do
  string "end"
  popParentAccount
  return (return id)

ledgerAlias :: GenParser Char JournalContext JournalUpdate
ledgerAlias = do
  string "alias"
  many1 spacenonewline
  orig <- many1 $ noneOf "="
  char '='
  alias <- restofline
  addAccountAlias (accountNameWithoutPostingType $ strip orig
                  ,accountNameWithoutPostingType $ strip alias)
  return $ return id

ledgerEndAliases :: GenParser Char JournalContext JournalUpdate
ledgerEndAliases = do
  string "end aliases"
  clearAccountAliases
  return (return id)

ledgerTagDirective :: GenParser Char JournalContext JournalUpdate
ledgerTagDirective = do
  string "tag" <?> "tag directive"
  many1 spacenonewline
  _ <- many1 nonspace
  restofline
  return $ return id

ledgerEndTagDirective :: GenParser Char JournalContext JournalUpdate
ledgerEndTagDirective = do
  (string "end tag" <|> string "pop") <?> "end tag or pop directive"
  restofline
  return $ return id

ledgerDefaultYear :: GenParser Char JournalContext JournalUpdate
ledgerDefaultYear = do
  char 'Y' <?> "default year"
  many spacenonewline
  y <- many1 digit
  let y' = read y
  failIfInvalidYear y
  setYear y'
  return $ return id

ledgerDefaultCommodity :: GenParser Char JournalContext JournalUpdate
ledgerDefaultCommodity = do
  char 'D' <?> "default commodity"
  many1 spacenonewline
  a <- someamount
  -- someamount always returns a MixedAmount containing one Amount, but let's be safe
  let as = amounts a
  when (not $ null as) $ setCommodity $ commodity $ head as
  restofline
  return $ return id

ledgerHistoricalPrice :: GenParser Char JournalContext HistoricalPrice
ledgerHistoricalPrice = do
  char 'P' <?> "historical price"
  many spacenonewline
  date <- try (do {LocalTime d _ <- ledgerdatetime; return d}) <|> ledgerdate -- a time is ignored
  many1 spacenonewline
  symbol <- commoditysymbol
  many spacenonewline
  price <- someamount
  restofline
  return $ HistoricalPrice date symbol price

ledgerIgnoredPriceCommodity :: GenParser Char JournalContext JournalUpdate
ledgerIgnoredPriceCommodity = do
  char 'N' <?> "ignored-price commodity"
  many1 spacenonewline
  commoditysymbol
  restofline
  return $ return id

ledgerCommodityConversion :: GenParser Char JournalContext JournalUpdate
ledgerCommodityConversion = do
  char 'C' <?> "commodity conversion"
  many1 spacenonewline
  someamount
  many spacenonewline
  char '='
  many spacenonewline
  someamount
  restofline
  return $ return id

ledgerModifierTransaction :: GenParser Char JournalContext ModifierTransaction
ledgerModifierTransaction = do
  char '=' <?> "modifier transaction"
  many spacenonewline
  valueexpr <- restofline
  postings <- ledgerpostings
  return $ ModifierTransaction valueexpr postings

ledgerPeriodicTransaction :: GenParser Char JournalContext PeriodicTransaction
ledgerPeriodicTransaction = do
  char '~' <?> "periodic transaction"
  many spacenonewline
  periodexpr <- restofline
  postings <- ledgerpostings
  return $ PeriodicTransaction periodexpr postings

-- | Parse a (possibly unbalanced) ledger transaction.
ledgerTransaction :: GenParser Char JournalContext Transaction
ledgerTransaction = do
  date <- ledgerdate <?> "transaction"
  edate <- optionMaybe (ledgereffectivedate date) <?> "effective date"
  status <- ledgerstatus <?> "cleared flag"
  code <- ledgercode <?> "transaction code"
  (description, comment) <-
      (do {many1 spacenonewline; d <- liftM rstrip (many (noneOf ";\n")); c <- ledgercomment <|> return ""; newline; return (d, c)} <|>
       do {many spacenonewline; c <- ledgercomment <|> return ""; newline; return ("", c)}
      ) <?> "description and/or comment"
  md <- try ledgermetadata <|> return []
  postings <- ledgerpostings
  return $ txnTieKnot $ Transaction date edate status code description comment md postings ""

ledgerdate :: GenParser Char JournalContext Day
ledgerdate = do
  -- hacky: try to ensure precise errors for invalid dates
  -- XXX reported error position is not too good
  -- pos <- getPosition
  datestr <- many1 $ choice' [digit, datesepchar]
  let dateparts = wordsBy (`elem` datesepchars) datestr
  currentyear <- getYear
  let [y,m,d] = case (dateparts,currentyear) of
                  ([m,d],Just y)  -> [show y,m,d]
                  ([_,_],Nothing) -> fail $ "partial date "++datestr++" found, but the current year is unknown"
                  _               -> dateparts
      maybedate = fromGregorianValid (read y) (read m) (read d)
  case maybedate of
    Nothing   -> fail $ "bad date: " ++ datestr
    Just date -> return date
  <?> "full or partial date"

ledgerdatetime :: GenParser Char JournalContext LocalTime
ledgerdatetime = do 
  day <- ledgerdate
  many1 spacenonewline
  h <- many1 digit
  char ':'
  m <- many1 digit
  s <- optionMaybe $ do
      char ':'
      many1 digit
  let tod = TimeOfDay (read h) (read m) (maybe 0 (fromIntegral.read) s)
  return $ LocalTime day tod

ledgereffectivedate :: Day -> GenParser Char JournalContext Day
ledgereffectivedate actualdate = do
  char '='
  -- kludgy way to use actual date for default year
  let withDefaultYear d p = do
        y <- getYear
        let (y',_,_) = toGregorian d in setYear y'
        r <- p
        when (isJust y) $ setYear $ fromJust y
        return r
  edate <- withDefaultYear actualdate ledgerdate
  return edate

ledgerstatus :: GenParser Char JournalContext Bool
ledgerstatus = try (do { many spacenonewline; char '*' <?> "status"; return True } ) <|> return False

ledgercode :: GenParser Char JournalContext String
ledgercode = try (do { many1 spacenonewline; char '(' <?> "code"; code <- anyChar `manyTill` char ')'; return code } ) <|> return ""

ledgermetadata :: GenParser Char JournalContext [(String,String)]
ledgermetadata = many ledgermetadataline

-- a comment line containing a metadata declaration, eg:
-- ; name: value
ledgermetadataline :: GenParser Char JournalContext (String,String)
ledgermetadataline = do
  many1 spacenonewline
  many1 $ char ';'
  many spacenonewline
  name <- many1 $ noneOf ": \t"
  char ':'
  many spacenonewline
  value <- many (noneOf "\n")
  optional newline
--  eof
  return (name,value)
  <?> "metadata line"

-- Parse the following whitespace-beginning lines as postings, posting metadata, and/or comments.
-- complicated to handle intermixed comment and metadata lines.. make me better ?
ledgerpostings :: GenParser Char JournalContext [Posting]
ledgerpostings = do
  ctx <- getState
  -- pass current position to the sub-parses for more useful errors
  pos <- getPosition
  ls <- many1 $ try linebeginningwithspaces
  let parses p = isRight . parseWithCtx ctx p
      postinglines = filter (not . (ledgercommentline `parses`)) ls
      -- group any metadata lines with the posting line above
      postinglinegroups :: [String] -> [String]
      postinglinegroups [] = []
      postinglinegroups (pline:ls) = (unlines $ pline:mdlines):postinglinegroups rest
          where (mdlines,rest) = span (ledgermetadataline `parses`) ls
      pstrs = postinglinegroups postinglines
  when (null pstrs) $ fail "no postings"
  return $ map (fromparse . parseWithCtx ctx (setPosition pos >> ledgerposting)) pstrs
  <?> "postings"
            
linebeginningwithspaces :: GenParser Char JournalContext String
linebeginningwithspaces = do
  sp <- many1 spacenonewline
  c <- nonspace
  cs <- restofline
  return $ sp ++ (c:cs) ++ "\n"

ledgerposting :: GenParser Char JournalContext Posting
ledgerposting = do
  many1 spacenonewline
  status <- ledgerstatus
  many spacenonewline
  account <- modifiedaccountname
  let (ptype, account') = (accountNamePostingType account, unbracket account)
  amount <- postingamount
  many spacenonewline
  comment <- ledgercomment <|> return ""
  newline
  md <- ledgermetadata
  return (Posting status account' amount comment ptype md Nothing)

-- Parse an account name, then apply any parent account prefix and/or account aliases currently in effect.
modifiedaccountname :: GenParser Char JournalContext AccountName
modifiedaccountname = do
  a <- ledgeraccountname
  prefix <- getParentAccount
  let prefixed = prefix `joinAccountNames` a
  aliases <- getAccountAliases
  return $ accountNameApplyAliases aliases prefixed

-- | Parse an account name. Account names may have single spaces inside
-- them, and are terminated by two or more spaces. They should have one or
-- more components of at least one character, separated by the account
-- separator char.
ledgeraccountname :: GenParser Char st AccountName
ledgeraccountname = do
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

-- | Parse an amount, with an optional left or right currency symbol and
-- optional price.
postingamount :: GenParser Char JournalContext MixedAmount
postingamount =
  try (do
        many1 spacenonewline
        someamount <|> return missingamt
      ) <|> return missingamt

someamount :: GenParser Char JournalContext MixedAmount
someamount = try leftsymbolamount <|> try rightsymbolamount <|> nosymbolamount 

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
                a <- someamount -- XXX can parse more prices ad infinitum, shouldn't
                return $ Just $ TotalPrice a)
           <|> (do
            many spacenonewline
            a <- someamount -- XXX can parse more prices ad infinitum, shouldn't
            return $ Just $ UnitPrice a))
         <|> return Nothing

-- gawd.. trying to parse a ledger number without error:

type Quantity = Double

-- -- | Parse a ledger-style numeric quantity and also return the number of
-- -- digits to the right of the decimal point and whether thousands are
-- -- separated by comma.
-- amountquantity :: GenParser Char JournalContext (Quantity, Int, Bool)
-- amountquantity = do
--   sign <- optionMaybe $ string "-"
--   (intwithcommas,frac) <- numberparts
--   let comma = ',' `elem` intwithcommas
--   let precision = length frac
--   -- read the actual value. We expect this read to never fail.
--   let int = filter (/= ',') intwithcommas
--   let int' = if null int then "0" else int
--   let frac' = if null frac then "0" else frac
--   let sign' = fromMaybe "" sign
--   let quantity = read $ sign'++int'++"."++frac'
--   return (quantity, precision, comma)
--   <?> "commodity quantity"

-- -- | parse the two strings of digits before and after a possible decimal
-- -- point.  The integer part may contain commas, or either part may be
-- -- empty, or there may be no point.
-- numberparts :: GenParser Char JournalContext (String,String)
-- numberparts = numberpartsstartingwithdigit <|> numberpartsstartingwithpoint

-- numberpartsstartingwithdigit :: GenParser Char JournalContext (String,String)
-- numberpartsstartingwithdigit = do
--   let digitorcomma = digit <|> char ','
--   first <- digit
--   rest <- many digitorcomma
--   frac <- try (do {char '.'; many digit}) <|> return ""
--   return (first:rest,frac)
                     
-- numberpartsstartingwithpoint :: GenParser Char JournalContext (String,String)
-- numberpartsstartingwithpoint = do
--   char '.'
--   frac <- many1 digit
--   return ("",frac)

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

tests_Hledger_Read_JournalReader = TestList [

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

   ,"ledgerTransaction" ~: do
    assertParseEqual (parseWithCtx nullctx ledgerTransaction entry1_str) entry1
    assertBool "ledgerTransaction should not parse just a date"
                   $ isLeft $ parseWithCtx nullctx ledgerTransaction "2009/1/1\n"
    assertBool "ledgerTransaction should require some postings"
                   $ isLeft $ parseWithCtx nullctx ledgerTransaction "2009/1/1 a\n"
    let t = parseWithCtx nullctx ledgerTransaction "2009/1/1 a ;comment\n b 1\n"
    assertBool "ledgerTransaction should not include a comment in the description"
                   $ either (const False) ((== "a") . tdescription) t

  ,"ledgerModifierTransaction" ~: do
     assertParse (parseWithCtx nullctx ledgerModifierTransaction "= (some value expr)\n some:postings  1\n")

  ,"ledgerPeriodicTransaction" ~: do
     assertParse (parseWithCtx nullctx ledgerPeriodicTransaction "~ (some period expr)\n some:postings  1\n")

  ,"ledgerDirective" ~: do
     assertParse (parseWithCtx nullctx ledgerDirective "!include /some/file.x\n")
     assertParse (parseWithCtx nullctx ledgerDirective "account some:account\n")
     assertParse (parseWithCtx nullctx (ledgerDirective >> ledgerDirective) "!account a\nend\n")

  ,"ledgercommentline" ~: do
     assertParse (parseWithCtx nullctx ledgercommentline "; some comment \n")
     assertParse (parseWithCtx nullctx ledgercommentline " \t; x\n")
     assertParse (parseWithCtx nullctx ledgercommentline ";x")

  ,"ledgerDefaultYear" ~: do
     assertParse (parseWithCtx nullctx ledgerDefaultYear "Y 2010\n")
     assertParse (parseWithCtx nullctx ledgerDefaultYear "Y 10001\n")

  ,"ledgerHistoricalPrice" ~:
    assertParseEqual (parseWithCtx nullctx ledgerHistoricalPrice "P 2004/05/01 XYZ $55.00\n") (HistoricalPrice (parsedate "2004/05/01") "XYZ" $ Mixed [dollars 55])

  ,"ledgerIgnoredPriceCommodity" ~: do
     assertParse (parseWithCtx nullctx ledgerIgnoredPriceCommodity "N $\n")

  ,"ledgerDefaultCommodity" ~: do
     assertParse (parseWithCtx nullctx ledgerDefaultCommodity "D $1,000.0\n")

  ,"ledgerCommodityConversion" ~: do
     assertParse (parseWithCtx nullctx ledgerCommodityConversion "C 1h = $50.00\n")

  ,"ledgerTagDirective" ~: do
     assertParse (parseWithCtx nullctx ledgerTagDirective "tag foo \n")

  ,"ledgerEndTagDirective" ~: do
     assertParse (parseWithCtx nullctx ledgerEndTagDirective "end tag \n")
  ,"ledgerEndTagDirective" ~: do
     assertParse (parseWithCtx nullctx ledgerEndTagDirective "pop \n")

  ,"ledgeraccountname" ~: do
    assertBool "ledgeraccountname parses a normal accountname" (isRight $ parsewith ledgeraccountname "a:b:c")
    assertBool "ledgeraccountname rejects an empty inner component" (isLeft $ parsewith ledgeraccountname "a::c")
    assertBool "ledgeraccountname rejects an empty leading component" (isLeft $ parsewith ledgeraccountname ":b:c")
    assertBool "ledgeraccountname rejects an empty trailing component" (isLeft $ parsewith ledgeraccountname "a:b:")

 ,"ledgerposting" ~: do
    assertParseEqual (parseWithCtx nullctx ledgerposting "  expenses:food:dining  $10.00\n")
                     (Posting False "expenses:food:dining" (Mixed [dollars 10]) "" RegularPosting [] Nothing)
    assertBool "ledgerposting parses a quoted commodity with numbers"
                   (isRight $ parseWithCtx nullctx ledgerposting "  a  1 \"DE123\"\n")

  ,"someamount" ~: do
     let -- | compare a parse result with a MixedAmount, showing the debug representation for clarity
         assertMixedAmountParse parseresult mixedamount =
             (either (const "parse error") showMixedAmountDebug parseresult) ~?= (showMixedAmountDebug mixedamount)
     assertMixedAmountParse (parseWithCtx nullctx someamount "1 @ $2")
                            (Mixed [Amount unknown 1 (Just $ UnitPrice $ Mixed [Amount dollar{precision=0} 2 Nothing])])

  ,"postingamount" ~: do
    assertParseEqual (parseWithCtx nullctx postingamount " $47.18") (Mixed [dollars 47.18])
    assertParseEqual (parseWithCtx nullctx postingamount " $1.")
                (Mixed [Amount Commodity {symbol="$",side=L,spaced=False,decimalpoint='.',precision=0,separator=',',separatorpositions=[]} 1 Nothing])
  ,"postingamount with unit price" ~: do
    assertParseEqual
     (parseWithCtx nullctx postingamount " $10 @ €0.5")
     (Mixed [Amount{commodity=dollar{precision=0},
                    quantity=10,
                    price=(Just $ UnitPrice $ Mixed [Amount{commodity=euro{precision=1},
                                                            quantity=0.5,
                                                            price=Nothing}])}])
  ,"postingamount with total price" ~: do
    assertParseEqual
     (parseWithCtx nullctx postingamount " $10 @@ €5")
     (Mixed [Amount{commodity=dollar{precision=0},
                    quantity=10,
                    price=(Just $ TotalPrice $ Mixed [Amount{commodity=euro{precision=0},
                                                             quantity=5,
                                                             price=Nothing}])}])

  ,"leftsymbolamount" ~: do
    assertParseEqual (parseWithCtx nullctx leftsymbolamount "$1")
                     (Mixed [Amount Commodity {symbol="$",side=L,spaced=False,decimalpoint='.',precision=0,separator=',',separatorpositions=[]} 1 Nothing])
    assertParseEqual (parseWithCtx nullctx leftsymbolamount "$-1")
                     (Mixed [Amount Commodity {symbol="$",side=L,spaced=False,decimalpoint='.',precision=0,separator=',',separatorpositions=[]} (-1) Nothing])
    assertParseEqual (parseWithCtx nullctx leftsymbolamount "-$1")
                     (Mixed [Amount Commodity {symbol="$",side=L,spaced=False,decimalpoint='.',precision=0,separator=',',separatorpositions=[]} (-1) Nothing])

 ]

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

