module Tests where

import Test.QuickCheck
import Test.HUnit
import Text.ParserCombinators.Parsec
--import Control.Exception (assert)

import Parse
import Options

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

-- hunit tests

test1 = TestCase (assertEqual "1==1" 1 1)
sometests = TestList [TestLabel "test1" test1]

tests = Test.HUnit.test [
              "test1" ~: "1==1" ~: 1 ~=? 1,
              "test2" ~: assertEqual "2==2" 2 2
             ]

-- quickcheck tests

prop_test1 = 1 == 1
prop2 = 1 == 1

-- commands

test :: IO ()      
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
  parseLedgerFile ledgerFile >>= printParseResult
  return ()
--   assert_ $ amount t1 == 8.50
--   putStrLn "ok"
--     where assert_ e = assert e return ()             

