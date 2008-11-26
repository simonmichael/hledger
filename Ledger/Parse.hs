{-|

Parsers for standard ledger and timelog files.

-}

module Ledger.Parse
where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Combinator
import qualified Text.ParserCombinators.Parsec.Token as P
import System.IO
import qualified Data.Map as Map
import Ledger.Utils
import Ledger.Types
import Ledger.Amount
import Ledger.Entry
import Ledger.Commodity
import Ledger.TimeLog
import Data.Time.LocalTime
import Data.Time.Calendar


-- utils

parseLedgerFile :: String -> IO (Either ParseError RawLedger)
parseLedgerFile "-" = fmap (parse ledgerfile "-") $ hGetContents stdin
parseLedgerFile f   = parseFromFile ledgerfile f
    
printParseError :: (Show a) => a -> IO ()
printParseError e = do putStr "ledger parse error at "; print e

-- set up token parsing, though we're not yet using these much
ledgerLanguageDef = LanguageDef {
   commentStart   = ""
   , commentEnd     = ""
   , commentLine    = ";"
   , nestedComments = False
   , identStart     = letter <|> char '_'
   , identLetter    = alphaNum <|> oneOf "_':"
   , opStart        = opLetter emptyDef
   , opLetter       = oneOf "!#$%&*+./<=>?@\\^|-~"
   , reservedOpNames= []
   , reservedNames  = []
   , caseSensitive  = False
   }
lexer      = P.makeTokenParser ledgerLanguageDef
whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
--symbol     = P.symbol lexer
natural    = P.natural lexer
parens     = P.parens lexer
semi       = P.semi lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer

-- parsers

-- | Parse a RawLedger from either a ledger file or a timelog file.
-- It tries first the timelog parser then the ledger parser; this means
-- parse errors for ledgers are useful while those for timelogs are not.
ledgerfile :: Parser RawLedger
ledgerfile = try ledgerfromtimelog <|> ledger

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
ledger :: Parser RawLedger
ledger = do
  -- we expect these to come first, unlike ledger
  modifier_entries <- many ledgermodifierentry
  periodic_entries <- many ledgerperiodicentry

  entries <- (many $ try ledgerentry) <?> "entry"
  final_comment_lines <- ledgernondatalines
  eof
  return $ RawLedger modifier_entries periodic_entries entries (unlines final_comment_lines)

ledgernondatalines :: Parser [String]
ledgernondatalines = many (try ledgerdirective <|> -- treat as comments
                           try commentline <|> 
                           blankline)

ledgerdirective :: Parser String
ledgerdirective = char '!' >> restofline <?> "directive"

blankline :: Parser String
blankline =
  do {s <- many1 spacenonewline; newline; return s} <|> 
  do {newline; return ""} <?> "blank line"

commentline :: Parser String
commentline = do
  many spacenonewline
  char ';' <?> "comment line"
  l <- restofline
  return $ ";" ++ l

ledgercomment :: Parser String
ledgercomment = 
    try (do
          char ';'
          many spacenonewline
          many (noneOf "\n")
        ) 
    <|> return "" <?> "comment"

ledgermodifierentry :: Parser ModifierEntry
ledgermodifierentry = do
  char '=' <?> "entry"
  many spacenonewline
  valueexpr <- restofline
  transactions <- ledgertransactions
  return (ModifierEntry valueexpr transactions)

ledgerperiodicentry :: Parser PeriodicEntry
ledgerperiodicentry = do
  char '~' <?> "entry"
  many spacenonewline
  periodexpr <- restofline
  transactions <- ledgertransactions
  return (PeriodicEntry periodexpr transactions)

ledgerentry :: Parser Entry
ledgerentry = do
  preceding <- ledgernondatalines
  date <- ledgerdate <?> "entry"
  status <- ledgerstatus
  code <- ledgercode
-- ledger treats entry comments as part of the description, we will too
--   desc <- many (noneOf ";\n") <?> "description"
--   let description = reverse $ dropWhile (==' ') $ reverse desc
  description <- many (noneOf "\n") <?> "description"
  comment <- ledgercomment
  restofline
  transactions <- ledgertransactions
  return $ balanceEntry $ Entry date status code description comment transactions (unlines preceding)

ledgerday :: Parser Day
ledgerday = do 
  y <- many1 digit
  char '/'
  m <- many1 digit
  char '/'
  d <- many1 digit
  many spacenonewline
  return (fromGregorian (read y) (read m) (read d))

ledgerdate :: Parser Date
ledgerdate = fmap mkDate ledgerday

ledgerdatetime :: Parser DateTime
ledgerdatetime = do 
  day <- ledgerday
  h <- many1 digit
  char ':'
  m <- many1 digit
  s <- optionMaybe $ do
      char ':'
      many1 digit
  many spacenonewline
  return (mkDateTime day (TimeOfDay (read h) (read m) (maybe 0 (fromIntegral.read) s)))


ledgerstatus :: Parser Bool
ledgerstatus = try (do { char '*'; many1 spacenonewline; return True } ) <|> return False

ledgercode :: Parser String
ledgercode = try (do { char '('; code <- anyChar `manyTill` char ')'; many1 spacenonewline; return code } ) <|> return ""

ledgertransactions :: Parser [RawTransaction]
ledgertransactions = 
    ((try virtualtransaction <|> try balancedvirtualtransaction <|> ledgertransaction) <?> "transaction") 
    `manyTill` (do {newline <?> "blank line"; return ()} <|> eof)

ledgertransaction :: Parser RawTransaction
ledgertransaction = do
  many1 spacenonewline
  account <- ledgeraccountname
  amount <- transactionamount
  many spacenonewline
  comment <- ledgercomment
  restofline
  return (RawTransaction account amount comment RegularTransaction)

virtualtransaction :: Parser RawTransaction
virtualtransaction = do
  many1 spacenonewline
  char '('
  account <- ledgeraccountname
  char ')'
  amount <- transactionamount
  many spacenonewline
  comment <- ledgercomment
  restofline
  return (RawTransaction account amount comment VirtualTransaction)

balancedvirtualtransaction :: Parser RawTransaction
balancedvirtualtransaction = do
  many1 spacenonewline
  char '['
  account <- ledgeraccountname
  char ']'
  amount <- transactionamount
  many spacenonewline
  comment <- ledgercomment
  restofline
  return (RawTransaction account amount comment BalancedVirtualTransaction)

-- | account names may have single spaces inside them, and are terminated by two or more spaces
ledgeraccountname :: Parser String
ledgeraccountname = do
    accountname <- many1 (accountnamechar <|> singlespace)
    return $ striptrailingspace accountname
    where 
      singlespace = try (do {spacenonewline; do {notFollowedBy spacenonewline; return ' '}})
      -- couldn't avoid consuming a final space sometimes, harmless
      striptrailingspace s = if last s == ' ' then init s else s

accountnamechar = notFollowedBy (oneOf "()[]") >> nonspace
    <?> "account name character (non-bracket, non-parenthesis, non-whitespace)"

transactionamount :: Parser MixedAmount
transactionamount =
  try (do
        many1 spacenonewline
        a <- someamount <|> return missingamt
        return a
      ) <|> return missingamt

someamount = try leftsymbolamount <|> try rightsymbolamount <|> nosymbolamount 

leftsymbolamount :: Parser MixedAmount
leftsymbolamount = do
  sym <- commoditysymbol 
  sp <- many spacenonewline
  (q,p,comma) <- amountquantity
  pri <- priceamount
  let c = Commodity {symbol=sym,side=L,spaced=not $ null sp,comma=comma,precision=p}
  return $ Mixed [Amount c q pri]
  <?> "left-symbol amount"

rightsymbolamount :: Parser MixedAmount
rightsymbolamount = do
  (q,p,comma) <- amountquantity
  sp <- many spacenonewline
  sym <- commoditysymbol
  pri <- priceamount
  let c = Commodity {symbol=sym,side=R,spaced=not $ null sp,comma=comma,precision=p}
  return $ Mixed [Amount c q pri]
  <?> "right-symbol amount"

nosymbolamount :: Parser MixedAmount
nosymbolamount = do
  (q,p,comma) <- amountquantity
  pri <- priceamount
  let c = Commodity {symbol="",side=L,spaced=False,comma=comma,precision=p}
  return $ Mixed [Amount c q pri]
  <?> "no-symbol amount"

commoditysymbol :: Parser String
commoditysymbol = many1 (noneOf "-.0123456789;\n ") <?> "commodity symbol"

priceamount :: Parser (Maybe MixedAmount)
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
amountquantity :: Parser (Double, Int, Bool)
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
numberparts :: Parser (String,String)
numberparts = numberpartsstartingwithdigit <|> numberpartsstartingwithpoint

numberpartsstartingwithdigit :: Parser (String,String)
numberpartsstartingwithdigit = do
  let digitorcomma = digit <|> char ','
  first <- digit
  rest <- many digitorcomma
  frac <- try (do {char '.'; many digit >>= return}) <|> return ""
  return (first:rest,frac)
                     
numberpartsstartingwithpoint :: Parser (String,String)
numberpartsstartingwithpoint = do
  char '.'
  frac <- many1 digit
  return ("",frac)
                     

spacenonewline :: Parser Char
spacenonewline = satisfy (\c -> c `elem` " \v\f\t")

restofline :: Parser String
restofline = anyChar `manyTill` newline

whiteSpace1 :: Parser ()
whiteSpace1 = do space; whiteSpace

nonspace = satisfy (not . isSpace)


{-| Parse a timelog file. Here is the timelog grammar, from timeclock.el 2.6:

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
timelog :: Parser TimeLog
timelog = do
  entries <- many timelogentry <?> "timelog entry"
  eof
  return $ TimeLog entries

timelogentry :: Parser TimeLogEntry
timelogentry = do
  many (commentline <|> blankline)
  code <- oneOf "bhioO"
  many1 spacenonewline
  datetime <- ledgerdatetime
  comment <- restofline
  return $ TimeLogEntry code datetime comment

ledgerfromtimelog :: Parser RawLedger
ledgerfromtimelog = do 
  tl <- timelog
  return $ ledgerFromTimeLog tl


-- misc parsing

{-| 
Parse a date in any of the formats allowed in ledger's period expressions,
and maybe some others:

> 2004
> 2004/10
> 2004/10/1
> 10/1
> 21
> october, oct
> yesterday, today, tomorrow
> (not yet) this/next/last week/day/month/quarter/year

Returns a FuzzyDate, to be converted to a full date later, in the IO
layer.  Note: assumes any text in the parse stream has been lowercased.
-}
smartdate :: Parser FuzzyDate
smartdate = do
  let dateparsers = [ymd, ym, md, y, d, month, mon, today', yesterday, tomorrow]
  (y,m,d) <- choice $ map try dateparsers
  return $ (y,m,d)

datesepchar = oneOf "/-."

ymd :: Parser FuzzyDate
ymd = do
  y <- many1 digit
  datesepchar
  m <- many1 digit
  guard (read m <= 12)
  datesepchar
  d <- many1 digit
  guard (read d <= 31)
  return (y,m,d)

ym :: Parser FuzzyDate
ym = do
  y <- many1 digit
  guard (read y > 12)
  datesepchar
  m <- many1 digit
  guard (read m <= 12)
  return (y,m,"1")

y :: Parser FuzzyDate
y = do
  y <- many1 digit
  guard (read y >= 1000)
  return (y,"1","1")

d :: Parser FuzzyDate
d = do
  d <- many1 digit
  guard (read d <= 31)
  return ("","",d)

md :: Parser FuzzyDate
md = do
  m <- many1 digit
  guard (read m <= 12)
  datesepchar
  d <- many1 digit
  guard (read d <= 31)
  return ("",m,d)

months = ["january","february","march","april","may","june",
          "july","august","september","october","november","december"]

mons = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]

month :: Parser FuzzyDate
month = do
  m <- choice $ map string months
  let i = maybe 0 (+1) $ (map toLower m) `elemIndex` months
  return ("",show i,"1")

mon :: Parser FuzzyDate
mon = do
  m <- choice $ map string mons
  let i = maybe 0 (+1) $ (map toLower m) `elemIndex` mons
  return ("",show i,"1")

today',yesterday,tomorrow :: Parser FuzzyDate
today'    = string "today"     >> return ("","","today")
yesterday = string "yesterday" >> return ("","","yesterday")
tomorrow  = string "tomorrow"  >> return ("","","tomorrow")

lastthisnextthing :: Parser FuzzyDate
lastthisnextthing = do
  r <- choice [
        string "last"
       ,string "this"
       ,string "next"
      ]
  many1 spacenonewline
  p <- choice [
        string "day"
       ,string "week"
       ,string "month"
       ,string "quarter"
       ,string "year"
      ]
  return ("",r,p)


type TransactionMatcher = Transaction -> Bool

-- | Parse a --display expression which is a simple date predicate, like
-- "d>[DATE]" or "d<=[DATE]", and return a transaction-matching predicate.
datedisplayexpr :: Parser TransactionMatcher
datedisplayexpr = do
  char 'd'
  op <- compareop
  char '['
  (y,m,d) <- smartdate
  char ']'
  let edate = parsedate $ printf "%04s/%02s/%02s" y m d
  let matcher = \(Transaction{date=tdate}) -> 
                  case op of
                    "<"  -> tdate <  edate
                    "<=" -> tdate <= edate
                    "="  -> tdate == edate
                    "==" -> tdate == edate -- just in case
                    ">=" -> tdate >= edate
                    ">"  -> tdate >  edate
  return matcher              

compareop = choice $ map (try . string) ["<=",">=","==","<","=",">"]
