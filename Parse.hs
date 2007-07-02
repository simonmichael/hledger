module Parse
where
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import System.IO

import Utils
import Models


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
symbol     = P.symbol lexer
natural    = P.natural lexer
parens     = P.parens lexer
semi       = P.semi lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer


ledgerfile :: Parser Ledger
ledgerfile = ledger <|> ledgerfromtimelog


-- standard ledger file parser
{-
Here's the ledger 2.5 grammar:
"The ledger ﬁle format is quite simple, but also very ﬂexible. It supports
many options, though typically the user can ignore most of them. They are
summarized below.  The initial character of each line determines what the
line means, and how it should be interpreted. Allowable initial characters
are:

NUMBER      A line beginning with a number denotes an entry. It may be followed by any
            number of lines, each beginning with whitespace, to denote the entry’s account
            transactions. The format of the ﬁrst line is:

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
            by specifying ‘ AMOUNT’, or a complete transaction cost with ‘@ AMOUNT’.
            Lastly, the ‘NOTE’ may specify an actual and/or eﬀective date for the
            transaction by using the syntax ‘[ACTUAL_DATE]’ or ‘[=EFFECTIVE_DATE]’ or
            ‘[ACTUAL_DATE=EFFECtIVE_DATE]’.

=           An automated entry. A value expression must appear after the equal sign.
            After this initial line there should be a set of one or more transactions, just as
            if it were normal entry. If the amounts of the transactions have no commodity,
            they will be applied as modiﬁers to whichever real transaction is matched by
            the value expression.
 
~           A period entry. A period expression must appear after the tilde.
            After this initial line there should be a set of one or more transactions, just as
            if it were normal entry.


!           A line beginning with an exclamation mark denotes a command directive. It
            must be immediately followed by the command word. The supported commands
            are:

           ‘!include’
                        Include the stated ledger ﬁle.
           ‘!account’
                        The account name is given is taken to be the parent of all transac-
                        tions that follow, until ‘!end’ is seen.
           ‘!end’       Ends an account block.
 
;          A line beginning with a colon indicates a comment, and is ignored.
 
Y          If a line begins with a capital Y, it denotes the year used for all subsequent
           entries that give a date without a year. The year should appear immediately
           after the Y, for example: ‘Y2004’. This is useful at the beginning of a ﬁle, to
           specify the year for that ﬁle. If all entries specify a year, however, this command
           has no eﬀect.
           
 
P          Speciﬁes a historical price for a commodity. These are usually found in a pricing
           history ﬁle (see the ‘-Q’ option). The syntax is:

                  P DATE SYMBOL PRICE

N SYMBOL   Indicates that pricing information is to be ignored for a given symbol, nor will
           quotes ever be downloaded for that symbol. Useful with a home currency, such
           as the dollar ($). It is recommended that these pricing options be set in the price
           database ﬁle, which defaults to ‘~/.pricedb’. The syntax for this command is:

                  N SYMBOL

        
D AMOUNT   Speciﬁes the default commodity to use, by specifying an amount in the expected
           format. The entry command will use this commodity as the default when none
           other can be determined. This command may be used multiple times, to set
           the default ﬂags for diﬀerent commodities; whichever is seen last is used as the
           default commodity. For example, to set US dollars as the default commodity,
           while also setting the thousands ﬂag and decimal ﬂag for that commodity, use:

                  D $1,000.00

C AMOUNT1 = AMOUNT2
           Speciﬁes a commodity conversion, where the ﬁrst amount is given to be equiv-
           alent to the second amount. The ﬁrst amount should use the decimal precision
           desired during reporting:

                  C 1.00 Kb = 1024 bytes

i, o, b, h
           These four relate to timeclock support, which permits ledger to read timelog
           ﬁles. See the timeclock’s documentation for more info on the syntax of its
           timelog ﬁles."
-}
-- parsec example: http://pandoc.googlecode.com/svn/trunk/src/Text/Pandoc/Readers/RST.hs
-- sample data in Tests.hs 

ledger :: Parser Ledger
ledger = do
  ledgernondatalines
  -- for now these must come first, unlike ledger
  modifier_entries <- many ledgermodifierentry
  periodic_entries <- many ledgerperiodicentry
  --
  entries <- (many ledgerentry) <?> "entry"
  eof
  return $ Ledger modifier_entries periodic_entries entries

ledgernondatalines :: Parser [String]
ledgernondatalines = many (ledgerdirective <|> ledgercomment <|> do {whiteSpace1; return []})

ledgercomment :: Parser String
ledgercomment = char ';' >> restofline <?> "comment"

ledgerdirective :: Parser String
ledgerdirective = char '!' >> restofline <?> "directive"

ledgermodifierentry :: Parser ModifierEntry
ledgermodifierentry = do
  char '=' <?> "entry"
  many spacenonewline
  valueexpr <- restofline
  transactions <- ledgertransactions
  ledgernondatalines
  return (ModifierEntry valueexpr transactions)

ledgerperiodicentry :: Parser PeriodicEntry
ledgerperiodicentry = do
  char '~' <?> "entry"
  many spacenonewline
  periodexpr <- restofline
  transactions <- ledgertransactions
  ledgernondatalines
  return (PeriodicEntry periodexpr transactions)

ledgerentry :: Parser Entry
ledgerentry = do
  date <- ledgerdate
  status <- ledgerstatus
  code <- ledgercode
  description <- anyChar `manyTill` ledgereol
  transactions <- ledgertransactions
  ledgernondatalines
  let entry = Entry date status code description transactions
  --let entry = Entry date status code description (map (\t -> t{tentry=entry}) transactions)
              
  return $ autofillEntry entry

ledgerdate :: Parser String
ledgerdate = do 
  y <- many1 digit
  char '/'
  m <- many1 digit
  char '/'
  d <- many1 digit
  many1 spacenonewline
  return $ printf "%04s/%02s/%02s" y m d

ledgerstatus :: Parser Bool
ledgerstatus = try (do { char '*'; many1 spacenonewline; return True } ) <|> return False

ledgercode :: Parser String
ledgercode = try (do { char '('; code <- anyChar `manyTill` char ')'; many1 spacenonewline; return code } ) <|> return ""

ledgertransactions :: Parser [Transaction]
ledgertransactions = (ledgertransaction <?> "transaction") `manyTill` (newline <?> "blank line")
                     -- => unlike ledger, we need to end the file with a blank line

ledgertransaction :: Parser Transaction
ledgertransaction = do
  many1 spacenonewline
  account <- ledgeraccount <?> "account"
  amount <- ledgeramount <?> "amount"
  many spacenonewline
  ledgereol
  many ledgercomment
  return (Transaction account amount)

-- account names may have single spaces in them, and are terminated by two or more spaces
ledgeraccount :: Parser String
ledgeraccount = many1 (alphaNum <|> char ':' <|> char '/' <|> char '_' <|> try (do {spacenonewline; do {notFollowedBy spacenonewline; return ' '}}))

ledgeramount :: Parser Amount
ledgeramount = try (do
                      many1 spacenonewline
                      currency <- many (noneOf "-.0123456789\n") <?> "currency"
                      quantity <- many1 (oneOf "-.,0123456789") <?> "quantity"
                      return (Amount (getcurrency currency) (read $ stripcommas quantity))
                   ) <|> 
                    return (Amount (Currency "AUTO" 0) 0)

stripcommas = filter (',' /=)

ledgereol :: Parser String
ledgereol = ledgercomment <|> do {newline; return []}  -- XXX problem, a transaction comment containing a digit fails

spacenonewline :: Parser Char
spacenonewline = satisfy (\c -> c `elem` " \v\f\t")

restofline :: Parser String
restofline = anyChar `manyTill` newline

whiteSpace1 :: Parser ()
whiteSpace1 = do space; whiteSpace


-- timelog file parser
{- 
timelog grammar, from timeclock.el 2.6

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

example:

i 2007/03/10 12:26:00 hledger
o 2007/03/10 17:26:02

-}

ledgerfromtimelog :: Parser Ledger
ledgerfromtimelog = do 
  tl <- timelog
  return $ ledgerFromTimeLog tl

timelog :: Parser TimeLog
timelog = do
  entries <- many timelogentry
  eof
  return $ TimeLog entries

timelogentry :: Parser TimeLogEntry
timelogentry = do
  code <- oneOf "bhioO"
  many1 spacenonewline
  date <- ledgerdate
  time <- many $ oneOf "0123456789:"
  let datetime = date ++ " " ++ time
  many spacenonewline
  comment <- restofline
  return $ TimeLogEntry code datetime comment



-- utils

parseError :: (Show a) => a -> IO ()
parseError e = do putStr "ledger parse error at "; print e

printParseResult :: Show v => Either ParseError v -> IO ()
printParseResult r = case r of Left e -> parseError e
                               Right v -> print v

parseLedgerFile :: String -> IO (Either ParseError Ledger)
parseLedgerFile "-" = fmap (parse ledgerfile "-") $ hGetContents stdin
parseLedgerFile f   = parseFromFile ledgerfile f
    
