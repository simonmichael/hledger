-- hledger - ledger-compatible money management utilities
-- GPLv3, (c) Simon Michael & contributors, 
-- ledger is at http://newartisans.com/ledger.html
-- here's the v2.5 grammar:
{-
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

import Debug.Trace
import Test.QuickCheck
import Test.HUnit

--import TildeExpand -- confuses my ghc 6.7
import System.Directory (getHomeDirectory)
import System.Environment (getEnv)
import Control.Exception (assert)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.Printf

-- sample data

sample_entry = "\
\2007/01/27 * joes diner\n\
\  expenses:food:dining                    $10.00\n\
\  expenses:gifts                          $10.00\n\
\  assets:checking                        $-20.00\n\
\\n" --"

sample_entry2 = "\
\2007/01/28 coopportunity\n\
\  expenses:food:groceries                 $47.18\n\
\  assets:checking\n\
\\n" --"

sample_entry3 = "\
\2007/01/01 * opening balance\n\
\    assets:cash                                $4.82\n\
\    equity:opening balances\n\
\\n\
\2007/01/01 * opening balance\n\
\    assets:cash                                $4.82\n\
\    equity:opening balances\n\
\\n\
\2007/01/28 coopportunity\n\
\  expenses:food:groceries                 $47.18\n\
\  assets:checking\n\
\\n" --"

sample_periodic_entry = "\
\~ monthly from 2007/2/2\n\
\  assets:saving            $200.00\n\
\  assets:checking\n\
\\n" --"

sample_periodic_entry2 = "\
\~ monthly from 2007/2/2\n\
\  assets:saving            $200.00         ;auto savings\n\
\  assets:checking\n\
\\n" --"

sample_periodic_entry3 = "\
\~ monthly from 2007/01/01\n\
\    assets:cash                                $4.82\n\
\    equity:opening balances\n\
\\n\
\~ monthly from 2007/01/01\n\
\    assets:cash                                $4.82\n\
\    equity:opening balances\n\
\\n" --"

sample_transaction  = "  expenses:food:dining  $10.00\n"

sample_transaction2 = "  assets:checking\n"

sample_ledger = "\
\\n\
\2007/01/27 * joes diner\n\
\  expenses:food:dining                    $10.00\n\
\  expenses:gifts                          $10.00\n\
\  assets:checking                        $-20.00\n\
\\n\
\\n\
\2007/01/28 coopportunity\n\
\  expenses:food:groceries                 $47.18\n\
\  assets:checking                        $-47.18\n\
\\n\
\" --"

sample_ledger2 = "\
\;comment\n\
\2007/01/27 * joes diner\n\
\  expenses:food:dining                    $10.00\n\
\  assets:checking                        $-47.18\n\
\\n" --"

sample_ledger3 = "\
\2007/01/27 * joes diner\n\
\  expenses:food:dining                    $10.00\n\
\;intra-entry comment\n\
\  assets:checking                        $-47.18\n\
\\n" --"

sample_ledger4 = "\
\!include \"somefile\"\n\
\2007/01/27 * joes diner\n\
\  expenses:food:dining                    $10.00\n\
\  assets:checking                        $-47.18\n\
\\n" --"

sample_ledger5 = ""

sample_ledger6 = "\
\~ monthly from 2007/1/21\n\
\    expenses:entertainment  $16.23        ;netflix\n\
\    assets:checking\n\
\\n\
\; 2007/01/01 * opening balance\n\
\;     assets:saving                            $200.04\n\
\;     equity:opening balances                         \n\
\\n" --"

-- a data model

data Ledger = Ledger {
                      modifier_entries :: [ModifierEntry],
                      periodic_entries :: [PeriodicEntry],
                      entries :: [Entry]
                     } deriving (Show, Eq)
data ModifierEntry = ModifierEntry { -- aka automated entry
                    valueexpr :: String,
                    m_transactions :: [Transaction]
                   } deriving (Eq)
data PeriodicEntry = PeriodicEntry {
                    periodexpr :: String,
                    p_transactions :: [Transaction]
                   } deriving (Eq)
data Entry = Entry {
                    date :: Date,
                    status :: Bool,
                    code :: String,
                    description :: String,
                    transactions :: [Transaction]
                   } deriving (Eq)
data Transaction = Transaction {
                                account :: Account,
                                amount :: Amount
                               } deriving (Eq)
data Amount = Amount {
                      currency :: String,
                      quantity :: Float
                     } deriving (Read, Eq)
type Date = String
type Account = String

-- ledger file parsing

-- set up token parsing, though we're not using it heavily yet
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

-- parsers
ledger = do
  ledgernondatalines
  -- for now these must come first, unlike ledger
  modifier_entries <- many ledgermodifierentry
  periodic_entries <- many ledgerperiodicentry
  --
  entries <- (many ledgerentry) <?> "entry"
  eof
  return (Ledger modifier_entries periodic_entries entries)

ledgernondatalines = many (ledgerdirective <|> ledgercomment <|> do {whiteSpace1; return []})

whiteSpace1 = do space; whiteSpace

restofline = anyChar `manyTill` newline

ledgercomment = char ';' >> restofline <?> "comment"

ledgerdirective = char '!' >> restofline <?> "directive"

ledgertransactions = (ledgertransaction <?> "transaction") `manyTill` (newline <?> "blank line")
                     -- => unlike ledger, we need to end the file with a blank line

ledgermodifierentry = do
  char '=' <?> "entry"
  many spacenonewline
  valueexpr <- restofline
  transactions <- ledgertransactions
  ledgernondatalines
  return (ModifierEntry valueexpr transactions)

ledgerperiodicentry = do
  char '~' <?> "entry"
  many spacenonewline
  periodexpr <- restofline
  transactions <- ledgertransactions
  ledgernondatalines
  return (PeriodicEntry periodexpr transactions)

ledgerentry = do
  date <- ledgerdate
  status <- ledgerstatus
  code <- ledgercode
  description <- anyChar `manyTill` ledgereol
  transactions <- ledgertransactions
  ledgernondatalines
  return (Entry date status code description transactions)

ledgerdate = do date <- many1 (digit <|> char '/'); many1 spacenonewline; return date

ledgerstatus = try (do { char '*'; many1 spacenonewline; return True } ) <|> return False

ledgercode = try (do { char '('; code <- anyChar `manyTill` char ')'; many1 spacenonewline; return code } ) <|> return ""

ledgertransaction = do
  many1 spacenonewline
  account <- ledgeraccount <?> "account"
  amount <- ledgeramount <?> "amount"
  many spacenonewline
  ledgereol
  many ledgercomment
  return (Transaction account amount)

-- account names may have single spaces in them, and are terminated by two or more spaces
ledgeraccount = many1 (alphaNum <|> char ':' <|> try (do {spacenonewline; do {notFollowedBy spacenonewline; return ' '}}))

ledgeramount = try (do
                      many1 spacenonewline
                      currency <- many (noneOf "-.0123456789\n") <?> "currency"
                      quantity <- many1 (oneOf "-.0123456789") <?> "quantity"
                      return (Amount currency (read quantity))
                   ) <|> 
                    return (Amount "" 0)

ledgereol = ledgercomment <|> do {newline; return []}

spacenonewline = satisfy (\c -> c `elem` " \v\f\t")

-- tests

test1 = TestCase (assertEqual "1==1" 1 1)
sometests = TestList [TestLabel "test1" test1]

tests = Test.HUnit.test [
              "test1" ~: "1==1" ~: 1 ~=? 1,
              "test2" ~: assertEqual "2==2" 2 2
             ]

prop_test1 = 1 == 1
prop2 = 1 == 1
      
test = do
  parseTest ledgertransaction sample_transaction
  parseTest ledgertransaction sample_transaction2
  parseTest ledgerentry sample_entry
  parseTest ledgerentry sample_entry2
  parseTest ledgerentry sample_entry3
  parseTest ledgerperiodicentry sample_periodic_entry
  parseTest ledgerperiodicentry sample_periodic_entry2
  parseTest ledgerperiodicentry sample_periodic_entry3
  parseTest ledger sample_ledger
  parseTest ledger sample_ledger2
  parseTest ledger sample_ledger3
  parseTest ledger sample_ledger4
  parseTest ledger sample_ledger5
  parseTest ledger sample_ledger6
  parseTest ledger sample_periodic_entry
  parseTest ledger sample_periodic_entry2
  parseMyLedgerFile >>= printParseResult
  return ()
--   assert_ $ amount t1 == 8.50
--   putStrLn "ok"
--     where assert_ e = assert e return ()             

printParseResult r =
    case r of
      Left err -> do putStr "ledger parse error at "; print err
      Right x  -> do print x

-- ok, what can we do with it ?

parseMyLedgerFile = do
  ledgerFile >>= parseFromFile ledger >>= return
    where 
      ledgerFile = do
        filepath <- getEnv "LEDGER" `catch` \_ -> return "ledger.dat"
        -- don't know how to accomplish this great feat
        --ledger_file <- tildeExpand filepath
        let ledger_file = filepath
        return ledger_file

showLedger l = "Ledger has\n"
               ++ (showModifierEntries $ modifier_entries l)
               ++ (showPeriodicEntries $ periodic_entries l)
               ++ (showEntries $ entries l)

showModifierEntries [] = ""
showModifierEntries es =
    (show n) ++ " modifier " ++ (inflectEntries n) ++ ":\n" ++ unlines (map show es)
            where n = length es

showPeriodicEntries [] = ""
showPeriodicEntries es =
    (show n) ++ " periodic " ++ (inflectEntries n) ++ ":\n" ++ unlines (map show es)
            where n = length es

showEntries [] = ""
showEntries es =
    (show n) ++ " " ++ (inflectEntries n) ++ ":\n" ++ unlines (map show es)
            where n = length es

inflectEntries 1 = "entry"
inflectEntries _ = "entries"

instance Show ModifierEntry where 
    show e = "= " ++ (valueexpr e) ++ "\n" ++ unlines (map show (m_transactions e))

instance Show PeriodicEntry where 
    show e = "~ " ++ (periodexpr e) ++ "\n" ++ unlines (map show (p_transactions e))

instance Show Entry where 
    show e = date e ++ " " ++ s ++ c ++ d ++ "\n" ++ unlines (map show (transactions e))
        where 
          d = description e
          s = case (status e) of {True -> "* "; False -> ""}
          c = case (length(code e) > 0) of {True -> (code e ++ " "); False -> ""}

instance Show Transaction where 
    show t = printf "    %-40s  %20.2s" (take 40 $ account t) (show $ amount t)

instance Show Amount where show a = (currency a) ++ (show $ quantity a)

r = register
register = do 
  p <- parseMyLedgerFile
  case p of
    Left err -> do putStr "ledger parse error at "; print err
    Right l  -> putStr $ showLedger l

main = do register
