module Tests
where
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Test.HUnit
import Ledger
import Utils
import Options
import BalanceCommand
import PrintCommand
import RegisterCommand


-- import Test.QuickCheck
-- quickcheck = mapM quickCheck ([
--         ] :: [Bool])

runtests = runTestTT alltests

alltests = concattests [
            tests
           ,accounttests
           ,accountnametests
           ,amounttests
           ,balancecommandtests
           ,currencytests
           ,entrytests
           ,ledgertests
           ,parsertests
           ,printcommandtests
           ,rawledgertests
           ,rawtransactiontests
           ,registercommandtests
           ,timelogtests
           ]
    where
      concattests = foldr (\(TestList as) (TestList bs) -> TestList (as ++ bs)) (TestList []) 

tests = 
 TestList
 [
         "display dollar amount" ~: show (dollars 1) ~?= "$1.00"
         
        ,"display time amount" ~: show (hours 1) ~?= "1.0h"

        ,"amount precision"   ~: do
           let a1 = Amount (getcurrency "$") 1.23 1
           let a2 = Amount (getcurrency "$") (-1.23) 2
           let a3 = Amount (getcurrency "$") (-1.23) 3
           assertequal (Amount (getcurrency "$") 0 1) (a1 + a2)
           assertequal (Amount (getcurrency "$") 0 1) (a1 + a3)
           assertequal (Amount (getcurrency "$") (-2.46) 2) (a2 + a3)
           assertequal (Amount (getcurrency "$") (-2.46) 3) (a3 + a3)
           -- sum adds 0, with Amount fromIntegral's default precision of 2
           assertequal (Amount (getcurrency "$") 0 1) (sum [a1,a2])
           assertequal (Amount (getcurrency "$") (-2.46) 2) (sum [a2,a3])
           assertequal (Amount (getcurrency "$") (-2.46) 2) (sum [a3,a3])

        ,"ledgertransaction"  ~: do
           assertparseequal rawtransaction1 (parsewith ledgertransaction rawtransaction1_str)

        ,"ledgerentry"        ~: do
           assertparseequal entry1 (parsewith ledgerentry entry1_str)
                            
        ,"autofillEntry"      ~: do
           assertequal
            (Amount (getcurrency "$") (-47.18) 2)
            (tamount $ last $ etransactions $ autofillEntry entry1)

        ,"punctuatethousands"      ~: punctuatethousands "" @?= ""
        ,"punctuatethousands"      ~: punctuatethousands "1234567.8901" @?= "1,234,567.8901"
        ,"punctuatethousands"      ~: punctuatethousands "-100" @?= "-100"

        ,"expandAccountNames" ~: do
        assertequal
         ["assets","assets:cash","assets:checking","expenses","expenses:vacation"]
         (expandAccountNames ["assets:cash","assets:checking","expenses:vacation"])

        ,"ledgerAccountNames" ~: do
        assertequal
         ["assets","assets:cash","assets:checking","assets:saving","equity","equity:opening balances",
          "expenses","expenses:food","expenses:food:dining","expenses:phone","expenses:vacation",
          "liabilities","liabilities:credit cards","liabilities:credit cards:discover"]
         (accountnames ledger7)

        ,"cacheLedger"        ~: do
        assertequal 15 (length $ Map.keys $ accounts $ cacheLedger wildcard rawledger7 )

        ,"showLedgerAccounts" ~: do
        assertequal 4 (length $ lines $ showLedgerAccountBalances ledger7 1)

        ,"ledgeramount"       ~: do
        assertparseequal (Amount (getcurrency "$") 47.18 2) (parsewith ledgeramount " $47.18")
        assertparseequal (Amount (getcurrency "$") 1 0) (parsewith ledgeramount " $1.")

        ,"pruneBoringBranches" ~: do
           atree <- liftM (ledgerAccountTree 99) $ ledgerfromfile "sample.ledger"
           assertequal 13 (length $ flatten $ atree)
           assertequal 12 (length $ flatten $ pruneBoringBranches $ atree)
 ]

balancecommandtests = 
 TestList 
 [
  "simple balance report" ~: do
    l <- ledgerfromfile "sample.ledger"
    assertequal
     "                 $-1  assets\n\
     \                  $2  expenses\n\
     \                 $-2  income\n\
     \                  $1  liabilities\n\
     \" --"
     (balancereport [] [] l)
  ,

  "balance report with showsubs" ~: do
    l <- ledgerfromfile "sample.ledger"
    assertequal
     "                 $-1  assets\n\
     \                 $-2    cash\n\
     \                  $1    saving\n\
     \                  $2  expenses\n\
     \                  $1    food\n\
     \                  $1    supplies\n\
     \                 $-2  income\n\
     \                 $-1    gifts\n\
     \                 $-1    salary\n\
     \                  $1  liabilities:debts\n\
     \" --"
     (balancereport [ShowSubs] [] l)
  ,

  "balance report with account pattern" ~: do
    rl <- rawledgerfromfile "sample.ledger"
    let l = cacheLedger (mkRegex "o") $ filterRawLedgerEntries "" "" wildcard rl
    assertequal
     "                  $1  expenses:food\n\
     \                 $-2  income\n\
     \--------------------\n\
     \                 $-1\n\
     \" --"
     (balancereport [] ["o"] l)
  ,

  "balance report with account pattern and showsubs" ~: do
    rl <- rawledgerfromfile "sample.ledger"
    let l = cacheLedger (mkRegex "o") $ filterRawLedgerEntries "" "" wildcard rl
    assertequal
     "                  $1  expenses:food\n\
     \                 $-2  income\n\
     \                 $-1    gifts\n\
     \                 $-1    salary\n\
     \--------------------\n\
     \                 $-1\n\
     \" --"
     (balancereport [ShowSubs] ["o"] l)
 ]

-- | Assert a parsed thing equals some expected thing, or print a parse error.
assertparseequal :: (Show a, Eq a) => a -> (Either ParseError a) -> Assertion
assertparseequal expected parsed = either printParseError (assertequal expected) parsed


-- test data

rawtransaction1_str  = "  expenses:food:dining  $10.00\n"

rawtransaction1 = RawTransaction "expenses:food:dining" (dollars 10) ""

entry1_str = "\
\2007/01/28 coopportunity\n\
\  expenses:food:groceries                 $47.18\n\
\  assets:checking\n\
\\n" --"

entry1 =
    (Entry "2007/01/28" False "" "coopportunity" ""
     [RawTransaction "expenses:food:groceries" (Amount (getcurrency "$") 47.18 2) "", 
      RawTransaction "assets:checking" (Amount (getcurrency "$") (-47.18) 2) ""] "")


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

rawledger7 = RawLedger
          [] 
          [] 
          [
           Entry {
             edate="2007/01/01", 
             estatus=False, 
             ecode="*", 
             edescription="opening balance", 
             ecomment="",
             etransactions=[
              RawTransaction {
                taccount="assets:cash", 
                tamount=Amount {currency=(getcurrency "$"), quantity=4.82, precision=2},
                tcomment=""
              },
              RawTransaction {
                taccount="equity:opening balances", 
                tamount=Amount {currency=(getcurrency "$"), quantity=(-4.82), precision=2},
                tcomment=""
              }
             ],
             epreceding_comment_lines=""
           }
          ,
           Entry {
             edate="2007/02/01", 
             estatus=False, 
             ecode="*", 
             edescription="ayres suites", 
             ecomment="",
             etransactions=[
              RawTransaction {
                taccount="expenses:vacation", 
                tamount=Amount {currency=(getcurrency "$"), quantity=179.92, precision=2},
                tcomment=""
              },
              RawTransaction {
                taccount="assets:checking", 
                tamount=Amount {currency=(getcurrency "$"), quantity=(-179.92), precision=2},
                tcomment=""
              }
             ],
             epreceding_comment_lines=""
           }
          ,
           Entry {
             edate="2007/01/02", 
             estatus=False, 
             ecode="*", 
             edescription="auto transfer to savings", 
             ecomment="",
             etransactions=[
              RawTransaction {
                taccount="assets:saving", 
                tamount=Amount {currency=(getcurrency "$"), quantity=200, precision=2},
                tcomment=""
              },
              RawTransaction {
                taccount="assets:checking", 
                tamount=Amount {currency=(getcurrency "$"), quantity=(-200), precision=2},
                tcomment=""
              }
             ],
             epreceding_comment_lines=""
           }
          ,
           Entry {
             edate="2007/01/03", 
             estatus=False, 
             ecode="*", 
             edescription="poquito mas", 
             ecomment="",
             etransactions=[
              RawTransaction {
                taccount="expenses:food:dining", 
                tamount=Amount {currency=(getcurrency "$"), quantity=4.82, precision=2},
                tcomment=""
              },
              RawTransaction {
                taccount="assets:cash", 
                tamount=Amount {currency=(getcurrency "$"), quantity=(-4.82), precision=2},
                tcomment=""
              }
             ],
             epreceding_comment_lines=""
           }
          ,
           Entry {
             edate="2007/01/03", 
             estatus=False, 
             ecode="*", 
             edescription="verizon", 
             ecomment="",
             etransactions=[
              RawTransaction {
                taccount="expenses:phone", 
                tamount=Amount {currency=(getcurrency "$"), quantity=95.11, precision=2},
                tcomment=""
              },
              RawTransaction {
                taccount="assets:checking", 
                tamount=Amount {currency=(getcurrency "$"), quantity=(-95.11), precision=2},
                tcomment=""
              }
             ],
             epreceding_comment_lines=""
           }
          ,
           Entry {
             edate="2007/01/03", 
             estatus=False, 
             ecode="*", 
             edescription="discover", 
             ecomment="",
             etransactions=[
              RawTransaction {
                taccount="liabilities:credit cards:discover", 
                tamount=Amount {currency=(getcurrency "$"), quantity=80, precision=2},
                tcomment=""
              },
              RawTransaction {
                taccount="assets:checking", 
                tamount=Amount {currency=(getcurrency "$"), quantity=(-80), precision=2},
                tcomment=""
              }
             ],
             epreceding_comment_lines=""
           }
          ]
          ""

ledger7 = cacheLedger wildcard rawledger7 

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

