
module Tests
where
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

import Options
import Models
import Parse
import Utils

-- sample data

transaction1_str  = "  expenses:food:dining  $10.00\n"

transaction1 = Transaction "expenses:food:dining" (dollars 10)

entry1_str = "\
\2007/01/28 coopportunity\n\
\  expenses:food:groceries                 $47.18\n\
\  assets:checking\n\
\\n" --"

entry1 =
    (Entry "2007/01/28" False "" "coopportunity" 
               [Transaction "expenses:food:groceries" (Amount (getcurrency "$") 47.18), 
                Transaction "assets:checking" (Amount (getcurrency "$") (-47.18))])

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

ledger1_str = "\
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
\2007/01/01 * opening balance\n\
\    income:interest                                $-4.82\n\
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

l = ledger7
ledger7 = RawLedger
          [] 
          [] 
          [
           Entry {
                  edate="2007/01/01", estatus=False, ecode="*", edescription="opening balance",
                  etransactions=[
                                Transaction {taccount="assets:cash", 
                                             tamount=Amount {currency=(getcurrency "$"), quantity=4.82}},
                                Transaction {taccount="equity:opening balances", 
                                             tamount=Amount {currency=(getcurrency "$"), quantity=(-4.82)}}
                               ]
                 }
          ,
           Entry {
                  edate="2007/02/01", estatus=False, ecode="*", edescription="ayres suites",
                  etransactions=[
                                Transaction {taccount="expenses:vacation", 
                                             tamount=Amount {currency=(getcurrency "$"), quantity=179.92}},
                                Transaction {taccount="assets:checking", 
                                             tamount=Amount {currency=(getcurrency "$"), quantity=(-179.92)}}
                               ]
                 }
          ,
           Entry {
                  edate="2007/01/02", estatus=False, ecode="*", edescription="auto transfer to savings",
                  etransactions=[
                                Transaction {taccount="assets:saving", 
                                             tamount=Amount {currency=(getcurrency "$"), quantity=200}},
                                Transaction {taccount="assets:checking", 
                                             tamount=Amount {currency=(getcurrency "$"), quantity=(-200)}}
                               ]
                 }
          ,
           Entry {
                  edate="2007/01/03", estatus=False, ecode="*", edescription="poquito mas",
                  etransactions=[
                                Transaction {taccount="expenses:food:dining", 
                                             tamount=Amount {currency=(getcurrency "$"), quantity=4.82}},
                                Transaction {taccount="assets:cash", 
                                             tamount=Amount {currency=(getcurrency "$"), quantity=(-4.82)}}
                               ]
                 }
          ,
           Entry {
                  edate="2007/01/03", estatus=False, ecode="*", edescription="verizon",
                  etransactions=[
                                Transaction {taccount="expenses:phone", 
                                             tamount=Amount {currency=(getcurrency "$"), quantity=95.11}},
                                Transaction {taccount="assets:checking", 
                                             tamount=Amount {currency=(getcurrency "$"), quantity=(-95.11)}}
                               ]
                 }
          ,
           Entry {
                  edate="2007/01/03", estatus=False, ecode="*", edescription="discover",
                  etransactions=[
                                Transaction {taccount="liabilities:credit cards:discover", 
                                             tamount=Amount {currency=(getcurrency "$"), quantity=80}},
                                Transaction {taccount="assets:checking", 
                                             tamount=Amount {currency=(getcurrency "$"), quantity=(-80)}}
                               ]
                 }
          ]

timelogentry1_str  = "i 2007/03/11 16:19:00 hledger\n"
timelogentry1 = TimeLogEntry 'i' "2007/03/11 16:19:00" "hledger"

timelogentry2_str  = "o 2007/03/11 16:30:00\n"
timelogentry2 = TimeLogEntry 'o' "2007/03/11 16:30:00" ""

timelog1_str = concat [
                timelogentry1_str,
                timelogentry2_str
               ]
timelog1 = TimeLog [
            timelogentry1,
            timelogentry2
           ]


-- utils

assertEqual' e a = assertEqual "" e a

parse' p ts = parse p "" ts

assertParseEqual :: (Show a, Eq a) => a -> (Either ParseError a) -> Assertion
assertParseEqual expected parsed =
    case parsed of
      Left e -> parseError e
      Right v -> assertEqual " " expected v

parseEquals :: Eq a => (Either ParseError a) -> a -> Bool
parseEquals parsed other =
    case parsed of
      Left e -> False
      Right v -> v == other

-- hunit tests

tests = runTestTT $ test [
         test_ledgertransaction
        , test_ledgerentry
        , test_autofillEntry
        , test_expandAccountNames
        , test_ledgerAccountNames
        , test_cacheLedger
        , 2 @=? 2
        ]

test_ledgertransaction :: Assertion
test_ledgertransaction =
    assertParseEqual transaction1 (parse' ledgertransaction transaction1_str)      

test_ledgerentry =
    assertParseEqual entry1 (parse' ledgerentry entry1_str)

test_autofillEntry = 
    assertEqual'
    (Amount (getcurrency "$") (-47.18))
    (tamount $ last $ etransactions $ autofillEntry entry1)

test_expandAccountNames =
    assertEqual'
    ["assets","assets:cash","assets:checking","expenses","expenses:vacation"]
    (expandAccountNames ["assets:cash","assets:checking","expenses:vacation"])

test_ledgerAccountNames =
    assertEqual'
    ["assets","assets:cash","assets:checking","assets:saving","equity","equity:opening balances",
    "expenses","expenses:food","expenses:food:dining","expenses:phone","expenses:vacation",
     "liabilities","liabilities:credit cards","liabilities:credit cards:discover"]
    (rawLedgerAccountNames ledger7)

test_cacheLedger =
    assertEqual' 14 (length $ Map.keys $ accounts $ cacheLedger ledger7)
    

-- quickcheck properties

props = mapM quickCheck
    [
     parse' ledgertransaction transaction1_str `parseEquals`
     (Transaction "expenses:food:dining" (Amount (getcurrency "$") 10))
    ,
     rawLedgerAccountNames ledger7 == 
     ["assets","assets:cash","assets:checking","assets:saving","equity",
      "equity:opening balances","expenses","expenses:food","expenses:food:dining",
      "expenses:phone","expenses:vacation","liabilities","liabilities:credit cards",
      "liabilities:credit cards:discover"]
    ,
     parseLedgerPatternArgs [] == ([],[])
    ,parseLedgerPatternArgs ["a"] == (["a"],[])
    ,parseLedgerPatternArgs ["a","b"] == (["a","b"],[])
    ,parseLedgerPatternArgs ["a","b","--"] == (["a","b"],[])
    ,parseLedgerPatternArgs ["a","b","--","c","b"] == (["a","b"],["c","b"])
    ,parseLedgerPatternArgs ["--","c"] == ([],["c"])
    ,parseLedgerPatternArgs ["--"] == ([],[])
    ,parse' timelogentry timelogentry1_str `parseEquals` timelogentry1
    ,parse' timelogentry timelogentry2_str `parseEquals` timelogentry2
    ,parse' timelog timelog1_str `parseEquals` timelog1
    ]


