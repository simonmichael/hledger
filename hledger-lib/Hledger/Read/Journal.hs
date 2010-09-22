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

module Hledger.Read.Journal (
       tests_Journal,
       reader,
       journalFile,
       someamount,
       ledgeraccountname,
       ledgerExclamationDirective,
       ledgerHistoricalPrice,
       ledgerDefaultYear,
       emptyLine,
       ledgerdatetime,
)
where
import Control.Monad.Error (ErrorT(..), throwError, catchError)
import Data.List.Split (wordsBy)
import Text.ParserCombinators.Parsec hiding (parse)
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding (readFile, putStr, putStrLn, print, getContents)
import System.IO.UTF8
#endif
import Hledger.Data.Utils
import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Data.AccountName (accountNameFromComponents,accountNameComponents)
import Hledger.Data.Amount
import Hledger.Data.Transaction
import Hledger.Data.Posting
import Hledger.Data.Journal
import Hledger.Data.Commodity (dollars,dollar,unknown,nonsimplecommoditychars)
import Hledger.Read.Common


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
-- error-raising "JournalUpdate" which can be applied to an empty journal
-- to get the final result.
journalFile :: GenParser Char JournalContext JournalUpdate
journalFile = do items <- many journalItem
                 eof
                 return $ liftM (foldr (.) id) $ sequence items
    where 
      -- As all journal line types can be distinguished by the first
      -- character, excepting transactions versus empty (blank or
      -- comment-only) lines, can use choice w/o try
      journalItem = choice [ ledgerExclamationDirective
                          , liftM (return . addTransaction) ledgerTransaction
                          , liftM (return . addModifierTransaction) ledgerModifierTransaction
                          , liftM (return . addPeriodicTransaction) ledgerPeriodicTransaction
                          , liftM (return . addHistoricalPrice) ledgerHistoricalPrice
                          , ledgerDefaultYear
                          , ledgerIgnoredPriceCommodity
                          , ledgerTagDirective
                          , ledgerEndTagDirective
                          , emptyLine >> return (return id)
                          ] <?> "journal transaction or directive"

emptyLine :: GenParser Char st ()
emptyLine = do many spacenonewline
               optional $ (char ';' <?> "comment") >> many (noneOf "\n")
               newline
               return ()

ledgercomment :: GenParser Char st String
ledgercomment = do
  many1 $ char ';'
  many spacenonewline
  many (noneOf "\n")
  <?> "comment"

ledgercommentline :: GenParser Char st String
ledgercommentline = do
  many spacenonewline
  s <- ledgercomment
  optional newline
  eof
  return s
  <?> "comment"

ledgerExclamationDirective :: GenParser Char JournalContext JournalUpdate
ledgerExclamationDirective = do
  char '!' <?> "directive"
  directive <- many nonspace
  case directive of
    "include" -> ledgerInclude
    "account" -> ledgerAccountBegin
    "end"     -> ledgerAccountEnd
    _         -> mzero

ledgerInclude :: GenParser Char JournalContext JournalUpdate
ledgerInclude = do
  many1 spacenonewline
  filename <- restofline
  outerState <- getState
  outerPos <- getPosition
  return $ do filepath <- expandPath outerPos filename
              contents <- readFileOrError outerPos filepath
              let inIncluded = show outerPos ++ " in included file " ++ show filename ++ ":\n"
              case runParser journalFile outerState filepath contents of
                Right ju -> ju `catchError` (throwError . (inIncluded ++))
                Left err -> throwError $ inIncluded ++ show err
      where readFileOrError pos fp =
                ErrorT $ liftM Right (readFile fp) `catch`
                  \err -> return $ Left $ printf "%s reading %s:\n%s" (show pos) fp (show err)

ledgerAccountBegin :: GenParser Char JournalContext JournalUpdate
ledgerAccountBegin = do many1 spacenonewline
                        parent <- ledgeraccountname
                        newline
                        pushParentAccount parent
                        return $ return id

ledgerAccountEnd :: GenParser Char JournalContext JournalUpdate
ledgerAccountEnd = popParentAccount >> return (return id)

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

ledgerDefaultCommodity :: GenParser Char JournalContext JournalUpdate
ledgerDefaultCommodity = do
  char 'D' <?> "default commodity"
  many1 spacenonewline
  someamount
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

ledgerTagDirective :: GenParser Char JournalContext JournalUpdate
ledgerTagDirective = do
  string "tag" <?> "tag directive"
  many1 spacenonewline
  _ <- many1 nonspace
  restofline
  return $ return id

ledgerEndTagDirective :: GenParser Char JournalContext JournalUpdate
ledgerEndTagDirective = do
  string "end tag" <?> "end tag directive"
  restofline
  return $ return id

-- like ledgerAccountBegin, updates the JournalContext
ledgerDefaultYear :: GenParser Char JournalContext JournalUpdate
ledgerDefaultYear = do
  char 'Y' <?> "default year"
  many spacenonewline
  y <- many1 digit
  let y' = read y
  failIfInvalidYear y
  setYear y'
  return $ return id

-- | Try to parse a ledger entry. If we successfully parse an entry,
-- check it can be balanced, and fail if not.
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
  postings <- ledgerpostings
  let t = txnTieKnot $ Transaction date edate status code description comment postings ""
  case balanceTransaction t of
    Right t' -> return t'
    Left err -> fail err

ledgerdate :: GenParser Char JournalContext Day
ledgerdate = do
  -- hacky: try to ensure precise errors for invalid dates
  -- XXX reported error position is not too good
  -- pos <- getPosition
  datestr <- many1 $ choice' [digit, datesepchar]
  let dateparts = wordsBy (`elem` datesepchars) datestr
  case dateparts of
    [y,m,d] -> do
               failIfInvalidYear y
               failIfInvalidMonth m
               failIfInvalidDay d
               return $ fromGregorian (read y) (read m) (read d)
    [m,d]   -> do
               y <- getYear
               case y of Nothing -> fail "partial date found, but no default year specified"
                         Just y' -> do failIfInvalidYear $ show y'
                                       failIfInvalidMonth m
                                       failIfInvalidDay d
                                       return $ fromGregorian y' (read m) (read d)
    _       -> fail $ "bad date: " ++ datestr
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

ledgerstatus :: GenParser Char st Bool
ledgerstatus = try (do { many1 spacenonewline; char '*' <?> "status"; return True } ) <|> return False

ledgercode :: GenParser Char st String
ledgercode = try (do { many1 spacenonewline; char '(' <?> "code"; code <- anyChar `manyTill` char ')'; return code } ) <|> return ""

ledgerpostings :: GenParser Char JournalContext [Posting]
ledgerpostings = do
  -- complicated to handle intermixed comment lines.. please make me better.
  ctx <- getState
  let parses p = isRight . parseWithCtx ctx p
  -- parse the following non-comment whitespace-beginning lines as postings
  -- make sure the sub-parse starts from the current position, for useful errors
  pos <- getPosition
  ls <- many1 $ try linebeginningwithspaces
  let ls' = filter (not . (ledgercommentline `parses`)) ls
  when (null ls') $ fail "no postings"
  return $ map (fromparse . parseWithCtx ctx (setPosition pos >> ledgerposting)) ls'
  <?> "postings"

linebeginningwithspaces :: GenParser Char st String
linebeginningwithspaces = do
  sp <- many1 spacenonewline
  c <- nonspace
  cs <- restofline
  return $ sp ++ (c:cs) ++ "\n"

ledgerposting :: GenParser Char JournalContext Posting
ledgerposting = do
  many1 spacenonewline
  status <- ledgerstatus
  account <- transactionaccountname
  let (ptype, account') = (postingTypeFromAccountName account, unbracket account)
  amount <- postingamount
  many spacenonewline
  comment <- ledgercomment <|> return ""
  newline
  return (Posting status account' amount comment ptype Nothing)

-- qualify with the parent account from parsing context
transactionaccountname :: GenParser Char JournalContext AccountName
transactionaccountname = liftM2 (++) getParentAccount ledgeraccountname

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
postingamount :: GenParser Char st MixedAmount
postingamount =
  try (do
        many1 spacenonewline
        someamount <|> return missingamt
      ) <|> return missingamt

someamount :: GenParser Char st MixedAmount
someamount = try leftsymbolamount <|> try rightsymbolamount <|> nosymbolamount 

leftsymbolamount :: GenParser Char st MixedAmount
leftsymbolamount = do
  sign <- optionMaybe $ string "-"
  let applysign = if isJust sign then negate else id
  sym <- commoditysymbol 
  sp <- many spacenonewline
  (q,p,comma) <- amountquantity
  pri <- priceamount
  let c = Commodity {symbol=sym,side=L,spaced=not $ null sp,comma=comma,precision=p}
  return $ applysign $ Mixed [Amount c q pri]
  <?> "left-symbol amount"

rightsymbolamount :: GenParser Char st MixedAmount
rightsymbolamount = do
  (q,p,comma) <- amountquantity
  sp <- many spacenonewline
  sym <- commoditysymbol
  pri <- priceamount
  let c = Commodity {symbol=sym,side=R,spaced=not $ null sp,comma=comma,precision=p}
  return $ Mixed [Amount c q pri]
  <?> "right-symbol amount"

nosymbolamount :: GenParser Char st MixedAmount
nosymbolamount = do
  (q,p,comma) <- amountquantity
  pri <- priceamount
  let c = Commodity {symbol="",side=L,spaced=False,comma=comma,precision=p}
  return $ Mixed [Amount c q pri]
  <?> "no-symbol amount"

commoditysymbol :: GenParser Char st String
commoditysymbol = (quotedcommoditysymbol <|> simplecommoditysymbol) <?> "commodity symbol"

quotedcommoditysymbol :: GenParser Char st String
quotedcommoditysymbol = do
  char '"'
  s <- many1 $ noneOf ";\n\""
  char '"'
  return s

simplecommoditysymbol :: GenParser Char st String
simplecommoditysymbol = many1 (noneOf nonsimplecommoditychars)

priceamount :: GenParser Char st (Maybe MixedAmount)
priceamount =
    try (do
          many spacenonewline
          char '@'
          many spacenonewline
          a <- someamount -- XXX could parse more prices ad infinitum, shouldn't
          return $ Just a
          ) <|> return Nothing

-- gawd.. trying to parse a ledger number without error:

-- | Parse a ledger-style numeric quantity and also return the number of
-- digits to the right of the decimal point and whether thousands are
-- separated by comma.
amountquantity :: GenParser Char st (Double, Int, Bool)
amountquantity = do
  sign <- optionMaybe $ string "-"
  (intwithcommas,frac) <- numberparts
  let comma = ',' `elem` intwithcommas
  let precision = length frac
  -- read the actual value. We expect this read to never fail.
  let int = filter (/= ',') intwithcommas
  let int' = if null int then "0" else int
  let frac' = if null frac then "0" else frac
  let sign' = fromMaybe "" sign
  let quantity = read $ sign'++int'++"."++frac'
  return (quantity, precision, comma)
  <?> "commodity quantity"

-- | parse the two strings of digits before and after a possible decimal
-- point.  The integer part may contain commas, or either part may be
-- empty, or there may be no point.
numberparts :: GenParser Char st (String,String)
numberparts = numberpartsstartingwithdigit <|> numberpartsstartingwithpoint

numberpartsstartingwithdigit :: GenParser Char st (String,String)
numberpartsstartingwithdigit = do
  let digitorcomma = digit <|> char ','
  first <- digit
  rest <- many digitorcomma
  frac <- try (do {char '.'; many digit}) <|> return ""
  return (first:rest,frac)
                     
numberpartsstartingwithpoint :: GenParser Char st (String,String)
numberpartsstartingwithpoint = do
  char '.'
  frac <- many1 digit
  return ("",frac)
                     

tests_Journal = TestList [

   "ledgerTransaction" ~: do
    assertParseEqual (parseWithCtx emptyCtx ledgerTransaction entry1_str) entry1
    assertBool "ledgerTransaction should not parse just a date"
                   $ isLeft $ parseWithCtx emptyCtx ledgerTransaction "2009/1/1\n"
    assertBool "ledgerTransaction should require some postings"
                   $ isLeft $ parseWithCtx emptyCtx ledgerTransaction "2009/1/1 a\n"
    let t = parseWithCtx emptyCtx ledgerTransaction "2009/1/1 a ;comment\n b 1\n"
    assertBool "ledgerTransaction should not include a comment in the description"
                   $ either (const False) ((== "a") . tdescription) t

  ,"ledgerModifierTransaction" ~: do
     assertParse (parseWithCtx emptyCtx ledgerModifierTransaction "= (some value expr)\n some:postings  1\n")

  ,"ledgerPeriodicTransaction" ~: do
     assertParse (parseWithCtx emptyCtx ledgerPeriodicTransaction "~ (some period expr)\n some:postings  1\n")

  ,"ledgerExclamationDirective" ~: do
     assertParse (parseWithCtx emptyCtx ledgerExclamationDirective "!include /some/file.x\n")
     assertParse (parseWithCtx emptyCtx ledgerExclamationDirective "!account some:account\n")
     assertParse (parseWithCtx emptyCtx (ledgerExclamationDirective >> ledgerExclamationDirective) "!account a\n!end\n")

  ,"ledgercommentline" ~: do
     assertParse (parseWithCtx emptyCtx ledgercommentline "; some comment \n")
     assertParse (parseWithCtx emptyCtx ledgercommentline " \t; x\n")
     assertParse (parseWithCtx emptyCtx ledgercommentline ";x")

  ,"ledgerDefaultYear" ~: do
     assertParse (parseWithCtx emptyCtx ledgerDefaultYear "Y 2010\n")
     assertParse (parseWithCtx emptyCtx ledgerDefaultYear "Y 10001\n")

  ,"ledgerHistoricalPrice" ~:
    assertParseEqual (parseWithCtx emptyCtx ledgerHistoricalPrice "P 2004/05/01 XYZ $55.00\n") (HistoricalPrice (parsedate "2004/05/01") "XYZ" $ Mixed [dollars 55])

  ,"ledgerIgnoredPriceCommodity" ~: do
     assertParse (parseWithCtx emptyCtx ledgerIgnoredPriceCommodity "N $\n")

  ,"ledgerDefaultCommodity" ~: do
     assertParse (parseWithCtx emptyCtx ledgerDefaultCommodity "D $1,000.0\n")

  ,"ledgerCommodityConversion" ~: do
     assertParse (parseWithCtx emptyCtx ledgerCommodityConversion "C 1h = $50.00\n")

  ,"ledgerTagDirective" ~: do
     assertParse (parseWithCtx emptyCtx ledgerTagDirective "tag foo \n")

  ,"ledgerEndTagDirective" ~: do
     assertParse (parseWithCtx emptyCtx ledgerEndTagDirective "end tag \n")

  ,"ledgeraccountname" ~: do
    assertBool "ledgeraccountname parses a normal accountname" (isRight $ parsewith ledgeraccountname "a:b:c")
    assertBool "ledgeraccountname rejects an empty inner component" (isLeft $ parsewith ledgeraccountname "a::c")
    assertBool "ledgeraccountname rejects an empty leading component" (isLeft $ parsewith ledgeraccountname ":b:c")
    assertBool "ledgeraccountname rejects an empty trailing component" (isLeft $ parsewith ledgeraccountname "a:b:")

 ,"ledgerposting" ~: do
    assertParseEqual (parseWithCtx emptyCtx ledgerposting "  expenses:food:dining  $10.00\n") 
                     (Posting False "expenses:food:dining" (Mixed [dollars 10]) "" RegularPosting Nothing)
    assertBool "ledgerposting parses a quoted commodity with numbers"
                   (isRight $ parseWithCtx emptyCtx ledgerposting "  a  1 \"DE123\"\n")

  ,"someamount" ~: do
     let -- | compare a parse result with a MixedAmount, showing the debug representation for clarity
         assertMixedAmountParse parseresult mixedamount =
             (either (const "parse error") showMixedAmountDebug parseresult) ~?= (showMixedAmountDebug mixedamount)
     assertMixedAmountParse (parsewith someamount "1 @ $2")
                            (Mixed [Amount unknown 1 (Just $ Mixed [Amount dollar{precision=0} 2 Nothing])])

  ,"postingamount" ~: do
    assertParseEqual (parseWithCtx emptyCtx postingamount " $47.18") (Mixed [dollars 47.18])
    assertParseEqual (parseWithCtx emptyCtx postingamount " $1.")
                (Mixed [Amount Commodity {symbol="$",side=L,spaced=False,comma=False,precision=0} 1 Nothing])

  ,"leftsymbolamount" ~: do
    assertParseEqual (parseWithCtx emptyCtx leftsymbolamount "$1")
                     (Mixed [Amount Commodity {symbol="$",side=L,spaced=False,comma=False,precision=0} 1 Nothing])
    assertParseEqual (parseWithCtx emptyCtx leftsymbolamount "$-1")
                     (Mixed [Amount Commodity {symbol="$",side=L,spaced=False,comma=False,precision=0} (-1) Nothing])
    assertParseEqual (parseWithCtx emptyCtx leftsymbolamount "-$1")
                     (Mixed [Amount Commodity {symbol="$",side=L,spaced=False,comma=False,precision=0} (-1) Nothing])

 ]

entry1_str = unlines
 ["2007/01/28 coopportunity"
 ,"    expenses:food:groceries                   $47.18"
 ,"    assets:checking                          $-47.18"
 ,""
 ]

entry1 =
    txnTieKnot $ Transaction (parsedate "2007/01/28") Nothing False "" "coopportunity" ""
     [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting Nothing, 
      Posting False "assets:checking" (Mixed [dollars (-47.18)]) "" RegularPosting Nothing] ""

