module Tests where

import Text.ParserCombinators.Parsec
import Test.QuickCheck
import Test.HUnit

import Options
import Types
import Parse

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

-- utils

assertParseEqual :: (Show a, Eq a) => a -> (Either ParseError a) -> Assertion
assertParseEqual expected parsed =
    case parsed of
      Left e -> parseError e
      Right v  -> assertEqual " " expected v

assertEqual' e a = assertEqual "" e a

parse' p ts = parse p "" ts

    
-- hunit tests

test_parse_ledgertransaction :: Assertion
test_parse_ledgertransaction =
    assertParseEqual
      (Transaction "expenses:food:dining" (Amount "$" 10))
      (parse' ledgertransaction sample_transaction)

entry2 =
    (Entry "2007/01/28" False "" "coopportunity" 
               [Transaction "expenses:food:groceries" (Amount "$" 47.18), 
                Transaction "assets:checking" (Amount "" 0)])

test_parse_ledgerentry =
  assertParseEqual entry2 (parse' ledgerentry sample_entry2)

test_show_entry =
  assertEqual'
    "2007/01/28 coopportunity\n    expenses:food:groceries                                 $47.18\n    assets:checking                                            0.0\n"
    (show entry2)


hunittests = TestList [
                       test "test_parse_ledgertransaction" test_parse_ledgertransaction
                      , test "test_parse_ledgerentry" test_parse_ledgerentry
                      , test "test_show_entry" test_show_entry
                      ] 
    where test label fn = TestLabel label $ TestCase fn

hunittests2 = Test.HUnit.test [
                               "test1" ~: assertEqual "2 equals 2" 2 2
                              ]

-- quickcheck properties

prop1 = 1 == 1

--prop_test_parse_ledgertransaction =
--     (Transaction "expenses:food:dining" (Amount "$" 10)) == 
--     (parse' ledgertransaction sample_transaction))
-- how ?

-- commands

test :: IO ()      
test = do
  runTestTT hunittests
  runTestTT hunittests2
  quickCheck prop1
  parseTest ledgertransaction sample_transaction2
  parseTest ledgerentry sample_entry2
--   parseTest ledgerentry sample_entry3
--   parseTest ledgerperiodicentry sample_periodic_entry
--   parseTest ledgerperiodicentry sample_periodic_entry2
--   parseTest ledgerperiodicentry sample_periodic_entry3
--   parseTest ledger sample_ledger
--   parseTest ledger sample_ledger2
--   parseTest ledger sample_ledger3
--   parseTest ledger sample_ledger4
--   parseTest ledger sample_ledger5
--   parseTest ledger sample_ledger6
--   parseTest ledger sample_periodic_entry
--   parseTest ledger sample_periodic_entry2
--   parseLedgerFile ledgerFilePath >>= printParseResult
