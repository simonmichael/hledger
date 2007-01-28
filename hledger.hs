-- hledger - ledger-like money management utilities
-- GPLv3, (c) Simon Michael & contributors, 
-- ledger is at http://newartisans.com/ledger.html

import Debug.Trace
import System.Directory (getHomeDirectory)
import System.Environment (getEnv)
import Control.Exception (assert)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
--import TildeExpand -- confuses my ghc 6.7

-- sample data

sample_entry = "\
\2007/01/27 * joes diner\n\
\  expenses:food:dining                    $10.00\n\
\  expenses:gifts                          $10.00\n\
\  assets:checking                        $-20.00\n\
\\n\" --"

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

-- a data model

data Ledger = Ledger {
                      modifier_entries :: [ModifierEntry],
                      periodic_entries :: [PeriodicEntry],
                      entries :: [Entry]
                     } deriving (Show, Eq)
data Entry = Entry {
                    date :: Date,
                    status :: Bool,
                    code :: String,
                    description :: String,
                    transactions :: [Transaction]
                   } deriving (Show, Eq)
data ModifierEntry = ModifierEntry {
                    valueexpr :: String,
                    m_transactions :: [Transaction]
                   } deriving (Show, Eq)
data PeriodicEntry = PeriodicEntry {
                    periodexpr :: String,
                    p_transactions :: [Transaction]
                   } deriving (Show, Eq)
data Transaction = Transaction {
                                account :: Account,
                                amount :: Amount
                               } deriving (Show, Eq)
data Amount = Amount {
                      currency :: String,
                      quantity :: Float
                     } deriving (Read, Show, Eq)
type Date = String
type Account = String

-- ledger file parsing
-- struggling.. easier with a token parser ?

ledgerLanguageDef = LanguageDef {
   commentStart   = ""
   , commentEnd     = ""
   , commentLine    = ";"
   , nestedComments = False
   , identStart     = letter <|> char '_'
   , identLetter    = alphaNum <|> oneOf "_':"
   , opStart        = opLetter emptyDef
   , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
   , reservedOpNames= []
   , reservedNames  = []
   , caseSensitive  = False
   }

lexer  = P.makeTokenParser ledgerLanguageDef
whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer
natural    = P.natural lexer
parens     = P.parens lexer
semi       = P.semi lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer




ledger = do
  ledgernondatalines
  -- unlike ledger these must be first for now
  modifier_entries <- many ledgermodifierentry
  periodic_entries <- many ledgerperiodicentry
  --
  entries <- (many ledgerentry) <?> "entry"
  eof
  return (Ledger modifier_entries periodic_entries entries)

ledgernondatalines = many (ledgercomment <|> ledgerdirective <|> do {space; return []})
--ledgernondatalines = many (ledgerdirective <|> do {whiteSpace; return []})

ledgercomment = char ';' >> anyChar `manyTill` newline <?> "comment"

ledgerdirective = char '!' >> anyChar `manyTill` newline <?> "directive"

ledgermodifierentry = do
  ledgernondatalines
  char '=' <?> "entry"
  valueexpr <- anyChar `manyTill` newline
  transactions <- (ledgertransaction <?> "transaction") `manyTill` (newline <?> "blank line")
  spaces
  return (ModifierEntry valueexpr transactions)

ledgerperiodicentry = do
  ledgernondatalines
  char '~' <?> "entry"
  periodexpr <- anyChar `manyTill` newline
  transactions <- (ledgertransaction <?> "transaction") `manyTill` (newline <?> "blank line")
  spaces
  return (PeriodicEntry periodexpr transactions)

ledgerentry = do
  ledgernondatalines
  date <- ledgerdate
  many1 spacenonewline
  status <- ledgerstatus
  code <- ledgercode
  description <- anyChar `manyTill` ledgereol
  transactions <- (ledgertransaction <?> "transaction") `manyTill` (newline <?> "blank line")
                  -- unlike ledger, we need the file to end with a blank line
  spaces
  return (Entry date status code description transactions)

ledgerdate = many1 (digit <|> char '/')

ledgerstatus = try (do { char '*'; many1 spacenonewline; return True } ) <|> return False

ledgercode = try (do { char '('; code <- anyChar `manyTill` char ')'; return code } ) <|> return ""

ledgertransaction = do
  many (ledgercomment <|> ledgerdirective)
  many1 spacenonewline
  account <- ledgeraccount <?> "account"
  amount <- ledgeramount <?> "amount"
  many spacenonewline
  ledgereol
  return (Transaction account amount)

--ledgeraccount = many1 (alphaNum <|> char ':')
ledgeraccount = many1 (alphaNum <|> char ':' <|> try (do {spacenonewline; do {notFollowedBy spacenonewline; return ' '}}))

--twoormorespaces = do spacenonewline; many1 spacenonewline

ledgeramount = try (do
                      many1 spacenonewline --twoormorespaces
                      currency <- many (noneOf "-.0123456789\n") <?> "currency"
                      quantity <- many1 (oneOf "-.0123456789") <?> "quantity"
                      return (Amount currency (read quantity))
                   ) <|> 
                    return (Amount "" 0) -- change later to balance the entry

ledgereol = ledgercomment <|> do {newline; return []}

spacenonewline = satisfy (\c -> c `elem` " \v\f\t")

-- run tests

parseMyLedgerFile = do
  fname <- ledgerFilePath
  parsed <- parseFromFile ledger fname
  return parsed
    where
      ledgerFilePath = do
                      filepath <- getEnv "LEDGER" `catch` \_ -> return "ledger.dat"
                      -- don't know how to accomplish this great feat
                      --ledger_file <- tildeExpand filepath
                      let ledger_file = filepath
                      return ledger_file

main = do
  showParseResult (parse ledgertransaction "" sample_transaction)
  showParseResult (parse ledgertransaction "" sample_transaction2)
  showParseResult (parse ledgerentry "" sample_entry)
  showParseResult (parse ledgerentry "" sample_entry2)
  showParseResult (parse ledgerentry "" sample_entry3)
  showParseResult (parse ledgerperiodicentry "" sample_periodic_entry)
  showParseResult (parse ledgerperiodicentry "" sample_periodic_entry2)
  showParseResult (parse ledgerperiodicentry "" sample_periodic_entry3)
  showParseResult (parse ledger "" sample_ledger)
  showParseResult (parse ledger "" sample_ledger2)
  showParseResult (parse ledger "" sample_ledger3)
  showParseResult (parse ledger "" sample_ledger4)
  showParseResult (parse ledger "" sample_ledger5)
  showParseResult (parse ledger "" sample_periodic_entry)
  showParseResult (parse ledger "" sample_periodic_entry2)
  parseMyLedgerFile >>= showParseResult 
    where
      showParseResult r =
          case r of
            Left err -> do putStr "ledger parse error at "; print err
            Right x  -> print x

      

--   assert_ $ amount t1 == 8.50
--   putStrLn "ok"
--     where assert_ e = assert e return ()             
