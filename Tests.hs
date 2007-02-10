
module Tests
where

import Text.ParserCombinators.Parsec
import Test.QuickCheck
import Test.HUnit
-- trying to make "*Tests> test" work
-- hiding (test)
--import qualified Test.HUnit (Test.HUnit.test)

import Options
import Types
import Parse

-- sample data

transaction1_str  = "  expenses:food:dining  $10.00\n"

transaction1 = Transaction "expenses:food:dining" (Amount "$" 10)

entry1_str = "\
\2007/01/28 coopportunity\n\
\  expenses:food:groceries                 $47.18\n\
\  assets:checking\n\
\\n" --"

entry1 =
    (Entry "2007/01/28" False "" "coopportunity" 
               [Transaction "expenses:food:groceries" (Amount "$" 47.18), 
                Transaction "assets:checking" (Amount "$" (-47.18))])

entry2_str = "\
\2007/01/27 * joes diner\n\
\  expenses:food:dining                    $10.00\n\
\  expenses:gifts                          $10.00\n\
\  assets:checking                        $-20.00\n\
\\n" --"

entry3_str = "\
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

periodic_entry1_str = "\
\~ monthly from 2007/2/2\n\
\  assets:saving            $200.00\n\
\  assets:checking\n\
\\n" --"

periodic_entry2_str = "\
\~ monthly from 2007/2/2\n\
\  assets:saving            $200.00         ;auto savings\n\
\  assets:checking\n\
\\n" --"

periodic_entry3_str = "\
\~ monthly from 2007/01/01\n\
\    assets:cash                                $4.82\n\
\    equity:opening balances\n\
\\n\
\~ monthly from 2007/01/01\n\
\    assets:cash                                $4.82\n\
\    equity:opening balances\n\
\\n" --"

ledger_str = "\
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

ledger2_str = "\
\;comment\n\
\2007/01/27 * joes diner\n\
\  expenses:food:dining                    $10.00\n\
\  assets:checking                        $-47.18\n\
\\n" --"

ledger3_str = "\
\2007/01/27 * joes diner\n\
\  expenses:food:dining                    $10.00\n\
\;intra-entry comment\n\
\  assets:checking                        $-47.18\n\
\\n" --"

ledger4_str = "\
\!include \"somefile\"\n\
\2007/01/27 * joes diner\n\
\  expenses:food:dining                    $10.00\n\
\  assets:checking                        $-47.18\n\
\\n" --"

ledger5_str = ""

ledger6_str = "\
\~ monthly from 2007/1/21\n\
\    expenses:entertainment  $16.23        ;netflix\n\
\    assets:checking\n\
\\n\
\; 2007/01/01 * opening balance\n\
\;     assets:saving                            $200.04\n\
\;     equity:opening balances                         \n\
\\n" --"

ledger7_str = "\
\2007/01/01 * opening balance\n\
\    assets:cash                                $4.82\n\
\    equity:opening balances                         \n\
\\n\
\2007/01/02 * ayres suites\n\
\    expenses:vacation                        $179.92\n\
\    assets:checking                                 \n\
\\n\
\2007/01/02 * auto transfer to savings\n\
\    assets:saving                            $200.00\n\
\    assets:checking                                 \n\
\\n\
\2007/01/03 * poquito mas\n\
\    expenses:food:dining                       $4.82\n\
\    assets:cash                                     \n\
\\n\
\2007/01/03 * verizon\n\
\    expenses:phone                            $95.11\n\
\    assets:checking                                 \n\
\\n\
\2007/01/03 * discover\n\
\    liabilities:credit cards:discover         $80.00\n\
\    assets:checking                                 \n\
\\n\
\2007/01/04 * blue cross\n\
\    expenses:health:insurance                 $90.00\n\
\    assets:checking                                 \n\
\\n\
\2007/01/05 * village market liquor\n\
\    expenses:food:dining                       $6.48\n\
\    assets:checking                                 \n\
\\n" --"

ledger7 = Ledger [] [] 
          [
           Entry {
                  date="2007/01/01", status=False, code="*", description="opening balance",
                  transactions=[
                                Transaction {account="assets:cash", 
                                             amount=Amount {currency="$", quantity=4.82}},
                                Transaction {account="equity:opening balances", 
                                             amount=Amount {currency="$", quantity=(-4.82)}}
                               ]
                 },
           Entry {
                  date="2007/02/01", status=False, code="*", description="ayres suites",
                  transactions=[
                                Transaction {account="expenses:vacation", 
                                             amount=Amount {currency="$", quantity=179.92}},
                                Transaction {account="assets:checking", 
                                             amount=Amount {currency="$", quantity=(-179.92)}}
                               ]
                 }
          ]

-- 2007/01/02 * auto transfer to savings
--     assets:saving                            $200.00
--     assets:checking

-- 2007/01/03 * poquito mas
--     expenses:food:dining                       $4.82
--     assets:cash

-- 2007/01/03 * verizon
--     expenses:phone                            $95.11
--     assets:checking

-- 2007/01/03 * discover
--     liabilities:credit cards:discover         $80.00
--     assets:checking

-- 2007/01/04 * blue cross
--     expenses:health:insurance                 $90.00
--     assets:checking

-- 2007/01/05 * village market liquor
--     expenses:food:dining                       $6.48
--     assets:checking

-- utils

assertParseEqual :: (Show a, Eq a) => a -> (Either ParseError a) -> Assertion
assertParseEqual expected parsed =
    case parsed of
      Left e -> parseError e
      Right v  -> assertEqual " " expected v

assertEqual' e a = assertEqual "" e a

parse' p ts = parse p "" ts

-- hunit tests

--   parseTest ledgerentry entry2_str
--   parseTest ledgerentry entry3_str
--   parseTest ledgerperiodicentry periodic_entry1_str
--   parseTest ledgerperiodicentry periodic_entry2_str
--   parseTest ledgerperiodicentry periodic_entry3_str
--   parseTest ledger ledger_str
--   parseTest ledger ledger2_str
--   parseTest ledger ledger3_str
--   parseTest ledger ledger4_str
--   parseTest ledger ledger5_str
--   parseTest ledger ledger6_str
--   parseTest ledger periodic_entry1_str
--   parseTest ledger periodic_entry2_str
--   parseLedgerFile ledgerFilePath >>= printParseResult

test_parse_ledgertransaction :: Assertion
test_parse_ledgertransaction =
    assertParseEqual transaction1 (parse' ledgertransaction transaction1_str)      

test_parse_ledgerentry =
    assertParseEqual entry1 (parse' ledgerentry entry1_str)

test_autofill_entry = 
    assertEqual'
      (Amount "$" (-47.18))
      (amount $ last $ transactions $ autofill entry1)

tests = TestList [
                   t "test_parse_ledgertransaction" test_parse_ledgertransaction
                 , t "test_parse_ledgerentry" test_parse_ledgerentry
                 , t "test_autofill_entry" test_autofill_entry
                 ] 
    where t label fn = TestLabel label $ TestCase fn

tests2 = Test.HUnit.test [
                          "test1" ~: assertEqual "2 equals 2" 2 2
                              ]

-- quickcheck properties

prop1 = 1 == 1
--prop_test_parse_ledgertransaction =
--     (Transaction "expenses:food:dining" (Amount "$" 10)) == 
--     (parse' ledgertransaction transaction_str))

props = [
         prop1
        ]