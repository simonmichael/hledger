{-# LANGUAGE CPP #-}
{-|

Parsers for standard ledger and timelog files.

-}

module Ledger.Parse
where
import Control.Monad.Error (ErrorT(..), MonadIO, liftIO, throwError, catchError)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import System.Directory
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding (readFile, putStr, putStrLn, print, getContents)
import System.IO.UTF8
#endif
import Ledger.Utils
import Ledger.Types
import Ledger.Dates
import Ledger.AccountName (accountNameFromComponents,accountNameComponents)
import Ledger.Amount
import Ledger.Transaction
import Ledger.Posting
import Ledger.Journal
import System.FilePath(takeDirectory,combine)


-- utils

-- | Some context kept during parsing.
data LedgerFileCtx = Ctx {
      ctxYear     :: !(Maybe Integer)  -- ^ the default year most recently specified with Y
    , ctxCommod   :: !(Maybe String)   -- ^ I don't know
    , ctxAccount  :: ![String]         -- ^ the current stack of "container" accounts specified by !account
    } deriving (Read, Show)

emptyCtx :: LedgerFileCtx
emptyCtx = Ctx { ctxYear = Nothing, ctxCommod = Nothing, ctxAccount = [] }

-- containing accounts "nest" hierarchically

pushParentAccount :: String -> GenParser tok LedgerFileCtx ()
pushParentAccount parent = updateState addParentAccount
    where addParentAccount ctx0 = ctx0 { ctxAccount = normalize parent : ctxAccount ctx0 }
          normalize = (++ ":") 

popParentAccount :: GenParser tok LedgerFileCtx ()
popParentAccount = do ctx0 <- getState
                      case ctxAccount ctx0 of
                        [] -> unexpected "End of account block with no beginning"
                        (_:rest) -> setState $ ctx0 { ctxAccount = rest }

getParentAccount :: GenParser tok LedgerFileCtx String
getParentAccount = liftM (concat . reverse . ctxAccount) getState

setYear :: Integer -> GenParser tok LedgerFileCtx ()
setYear y = updateState (\ctx -> ctx{ctxYear=Just y})

getYear :: GenParser tok LedgerFileCtx (Maybe Integer)
getYear = liftM ctxYear getState

printParseError :: (Show a) => a -> IO ()
printParseError e = do putStr "ledger parse error at "; print e

-- let's get to it

parseLedgerFile :: LocalTime -> FilePath -> ErrorT String IO Journal
parseLedgerFile t "-" = liftIO getContents >>= parseLedger t "-"
parseLedgerFile t f   = liftIO (readFile f) >>= parseLedger t f

-- | Parses the contents of a ledger file, or gives an error.  Requires
-- the current (local) time to calculate any unfinished timelog sessions,
-- we pass it in for repeatability.
parseLedger :: LocalTime -> FilePath -> String -> ErrorT String IO Journal
parseLedger reftime inname intxt =
  case runParser ledgerFile emptyCtx inname intxt of
    Right m  -> liftM (journalConvertTimeLog reftime) $ m `ap` return nulljournal
    Left err -> throwError $ show err


ledgerFile :: GenParser Char LedgerFileCtx (ErrorT String IO (Journal -> Journal))
ledgerFile = do items <- many ledgerItem
                eof
                return $ liftM (foldr (.) id) $ sequence items
    where 
      -- As all ledger line types can be distinguished by the first
      -- character, excepting transactions versus empty (blank or
      -- comment-only) lines, can use choice w/o try
      ledgerItem = choice [ ledgerDirective
                          , liftM (return . addTransaction) ledgerTransaction
                          , liftM (return . addModifierTransaction) ledgerModifierTransaction
                          , liftM (return . addPeriodicTransaction) ledgerPeriodicTransaction
                          , liftM (return . addHistoricalPrice) ledgerHistoricalPrice
                          , ledgerDefaultYear
                          , emptyLine >> return (return id)
                          , liftM (return . addTimeLogEntry)  timelogentry
                          ]

ledgerDirective :: GenParser Char LedgerFileCtx (ErrorT String IO (Journal -> Journal))
ledgerDirective = do char '!' <?> "directive"
                     directive <- many nonspace
                     case directive of
                       "include" -> ledgerInclude
                       "account" -> ledgerAccountBegin
                       "end"     -> ledgerAccountEnd
                       _         -> mzero

ledgerInclude :: GenParser Char LedgerFileCtx (ErrorT String IO (Journal -> Journal))
ledgerInclude = do many1 spacenonewline
                   filename <- restofline
                   outerState <- getState
                   outerPos <- getPosition
                   let inIncluded = show outerPos ++ " in included file " ++ show filename ++ ":\n"
                   return $ do contents <- expandPath outerPos filename >>= readFileE outerPos
                               case runParser ledgerFile outerState filename contents of
                                 Right l   -> l `catchError` (throwError . (inIncluded ++))
                                 Left perr -> throwError $ inIncluded ++ show perr
    where readFileE outerPos filename = ErrorT $ liftM Right (readFile filename) `catch` leftError
              where leftError err = return $ Left $ currentPos ++ whileReading ++ show err
                    currentPos = show outerPos
                    whileReading = " reading " ++ show filename ++ ":\n"

expandPath :: (MonadIO m) => SourcePos -> FilePath -> m FilePath
expandPath pos fp = liftM mkRelative (expandHome fp)
  where
    mkRelative = combine (takeDirectory (sourceName pos))
    expandHome inname | "~/" `isPrefixOf` inname = do homedir <- liftIO getHomeDirectory
                                                      return $ homedir ++ drop 1 inname
                      | otherwise                = return inname

ledgerAccountBegin :: GenParser Char LedgerFileCtx (ErrorT String IO (Journal -> Journal))
ledgerAccountBegin = do many1 spacenonewline
                        parent <- ledgeraccountname
                        newline
                        pushParentAccount parent
                        return $ return id

ledgerAccountEnd :: GenParser Char LedgerFileCtx (ErrorT String IO (Journal -> Journal))
ledgerAccountEnd = popParentAccount >> return (return id)

-- parsers

-- | Parse a Journal from either a ledger file or a timelog file.
-- It tries first the timelog parser then the ledger parser; this means
-- parse errors for ledgers are useful while those for timelogs are not.

{-| Parse a ledger file. Here is the ledger grammar from the ledger 2.5 manual:

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

See "Tests" for sample data.
-}

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

ledgerModifierTransaction :: GenParser Char LedgerFileCtx ModifierTransaction
ledgerModifierTransaction = do
  char '=' <?> "modifier transaction"
  many spacenonewline
  valueexpr <- restofline
  postings <- ledgerpostings
  return $ ModifierTransaction valueexpr postings

ledgerPeriodicTransaction :: GenParser Char LedgerFileCtx PeriodicTransaction
ledgerPeriodicTransaction = do
  char '~' <?> "periodic transaction"
  many spacenonewline
  periodexpr <- restofline
  postings <- ledgerpostings
  return $ PeriodicTransaction periodexpr postings

ledgerHistoricalPrice :: GenParser Char LedgerFileCtx HistoricalPrice
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

-- like ledgerAccountBegin, updates the LedgerFileCtx
ledgerDefaultYear :: GenParser Char LedgerFileCtx (ErrorT String IO (Journal -> Journal))
ledgerDefaultYear = do
  char 'Y' <?> "default year"
  many spacenonewline
  y <- many1 digit
  let y' = read y
  guard (y' >= 1000)
  setYear y'
  return $ return id

-- | Try to parse a ledger entry. If we successfully parse an entry, ensure it is balanced,
-- and if we cannot, raise an error.
ledgerTransaction :: GenParser Char LedgerFileCtx Transaction
ledgerTransaction = do
  date <- ledgerdate <?> "transaction"
  edate <- try (ledgereffectivedate date <?> "effective date") <|> return Nothing
  status <- ledgerstatus
  code <- ledgercode
  description <- many1 spacenonewline >> liftM rstrip (many1 (noneOf ";\n") <?> "description")
  comment <- ledgercomment <|> return ""
  restofline
  postings <- ledgerpostings
  let t = txnTieKnot $ Transaction date edate status code description comment postings ""
  case balanceTransaction t of
    Right t' -> return t'
    Left err -> fail err

ledgerdate :: GenParser Char LedgerFileCtx Day
ledgerdate = (try ledgerfulldate <|> ledgerpartialdate) <?> "full or partial date"

ledgerfulldate :: GenParser Char LedgerFileCtx Day
ledgerfulldate = do
  (y,m,d) <- ymd
  return $ fromGregorian (read y) (read m) (read d)

-- | Match a partial M/D date in a ledger. Warning, this terminates the
-- program if it finds a match when there is no default year specified.
ledgerpartialdate :: GenParser Char LedgerFileCtx Day
ledgerpartialdate = do
  (_,m,d) <- md
  y <- getYear
  when (y==Nothing) $ fail "partial date found, but no default year specified"
  return $ fromGregorian (fromJust y) (read m) (read d)

ledgerdatetime :: GenParser Char LedgerFileCtx LocalTime
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

ledgereffectivedate :: Day -> GenParser Char LedgerFileCtx (Maybe Day)
ledgereffectivedate actualdate = do
  char '='
  -- kludgily use actual date for default year
  let withDefaultYear d p = do
        y <- getYear
        let (y',_,_) = toGregorian d in setYear y'
        r <- p
        when (isJust y) $ setYear $ fromJust y
        return r
  edate <- withDefaultYear actualdate ledgerdate
  return $ Just edate

ledgerstatus :: GenParser Char st Bool
ledgerstatus = try (do { many1 spacenonewline; char '*' <?> "status"; return True } ) <|> return False

ledgercode :: GenParser Char st String
ledgercode = try (do { many1 spacenonewline; char '(' <?> "code"; code <- anyChar `manyTill` char ')'; return code } ) <|> return ""

-- Complicated to handle intermixed comment lines.. please make me better.
ledgerpostings :: GenParser Char LedgerFileCtx [Posting]
ledgerpostings = do
  ctx <- getState
  let parses p = isRight . parseWithCtx ctx p
  ls <- many1 $ try linebeginningwithspaces
  let ls' = filter (not . (ledgercommentline `parses`)) ls
  guard (not $ null ls')
  return $ map (fromparse . parseWithCtx ctx ledgerposting) ls'
  <?> "postings"

linebeginningwithspaces :: GenParser Char st String
linebeginningwithspaces = do
  sp <- many1 spacenonewline
  c <- nonspace
  cs <- restofline
  return $ sp ++ (c:cs) ++ "\n"

ledgerposting :: GenParser Char LedgerFileCtx Posting
ledgerposting = do
  many1 spacenonewline
  status <- ledgerstatus
  account <- transactionaccountname
  let (ptype, account') = (postingTypeFromAccountName account, unbracket account)
  amount <- postingamount
  many spacenonewline
  comment <- ledgercomment <|> return ""
  restofline
  return (Posting status account' amount comment ptype Nothing)

-- Qualify with the parent account from parsing context
transactionaccountname :: GenParser Char LedgerFileCtx AccountName
transactionaccountname = liftM2 (++) getParentAccount ledgeraccountname

-- | Account names may have single spaces inside them, and are terminated
-- by two or more spaces. They should have one or more components of at
-- least one character, separated by the account separator char.

ledgeraccountname :: GenParser Char st String
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
  sym <- commoditysymbol 
  sp <- many spacenonewline
  (q,p,comma) <- amountquantity
  pri <- priceamount
  let c = Commodity {symbol=sym,side=L,spaced=not $ null sp,comma=comma,precision=p}
  return $ Mixed [Amount c q pri]
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
commoditysymbol = many1 (noneOf "-.0123456789;\n ") <?> "commodity symbol"

priceamount :: GenParser Char st (Maybe MixedAmount)
priceamount =
    try (do
          many spacenonewline
          char '@'
          many spacenonewline
          a <- someamount
          return $ Just a
          ) <|> return Nothing

-- gawd.. trying to parse a ledger number without error:

-- | parse a ledger-style numeric quantity and also return the number of
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
                     

{-| Parse a timelog entry. Here is the timelog grammar from timeclock.el 2.6:

@
A timelog contains data in the form of a single entry per line.
Each entry has the form:

  CODE YYYY/MM/DD HH:MM:SS [COMMENT]

CODE is one of: b, h, i, o or O.  COMMENT is optional when the code is
i, o or O.  The meanings of the codes are:

  b  Set the current time balance, or \"time debt\".  Useful when
     archiving old log data, when a debt must be carried forward.
     The COMMENT here is the number of seconds of debt.

  h  Set the required working time for the given day.  This must
     be the first entry for that day.  The COMMENT in this case is
     the number of hours in this workday.  Floating point amounts
     are allowed.

  i  Clock in.  The COMMENT in this case should be the name of the
     project worked on.

  o  Clock out.  COMMENT is unnecessary, but can be used to provide
     a description of how the period went, for example.

  O  Final clock out.  Whatever project was being worked on, it is
     now finished.  Useful for creating summary reports.
@

Example:

i 2007/03/10 12:26:00 hledger
o 2007/03/10 17:26:02

-}
timelogentry :: GenParser Char LedgerFileCtx TimeLogEntry
timelogentry = do
  code <- oneOf "bhioO"
  many1 spacenonewline
  datetime <- ledgerdatetime
  comment <- optionMaybe (many1 spacenonewline >> liftM2 (++) getParentAccount restofline)
  return $ TimeLogEntry (read [code]) datetime (fromMaybe "" comment)


-- misc parsing

-- | Parse a --display expression which is a simple date predicate, like
-- "d>[DATE]" or "d<=[DATE]", and return a posting-matching predicate.
datedisplayexpr :: GenParser Char st (Posting -> Bool)
datedisplayexpr = do
  char 'd'
  op <- compareop
  char '['
  (y,m,d) <- smartdate
  char ']'
  let date    = parsedate $ printf "%04s/%02s/%02s" y m d
      test op = return $ (`op` date) . postingDate
  case op of
    "<"  -> test (<)
    "<=" -> test (<=)
    "="  -> test (==)
    "==" -> test (==)
    ">=" -> test (>=)
    ">"  -> test (>)
    _    -> mzero

compareop = choice $ map (try . string) ["<=",">=","==","<","=",">"]

