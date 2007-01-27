-- ledger-compatible money management tools
-- (c) 2007 Simon Michael & contributors, released under GPL v3 or later

import Control.Exception (assert)

-- data model
type Date = String
type Account = String
type Money = Float
-- a transaction records a movement of money between two accounts
data Transaction = Transaction {
                                date :: Date,
                                account :: Account,       -- debit this
                                other_account :: Account, -- credit this
                                description :: String,
                                amount :: Money
                               }

-- sample data
t1 = Transaction "2007-01-01" "checking" "food" "joe's diner" 8.50

-- tests
main = do
  assert_ $ amount t1 == 8.50
  putStrLn "ok"
    where assert_ e = assert e return ()             
