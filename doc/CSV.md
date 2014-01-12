---
title: hledger How to read CSV files
---

# How to read CSV files

Here's a quick example of [converting a CSV file](MANUAL.html#csv-files).

Say we have downloaded `checking.csv` from a bank for the first time:

    "Date","Note","Amount"
    "2012/3/22","DEPOSIT","50.00"
    "2012/3/23","TRANSFER TO SAVINGS","-10.00"

We could create `checking.csv.rules` containing:

    account1 assets:bank:checking
    skip     1
    fields   date, description, amount
    currency $

    if ~ SAVINGS
     account2 assets:bank:savings

This says:
"always use assets:bank:checking as the first account;
ignore the first line;
use the first, second and third CSV fields as the entry date, description and amount respectively;
always prepend $ to the amount value;
if the CSV record contains 'SAVINGS', use assets:bank:savings as the second account".
[CSV files](MANUAL.html#csv-files) in the manual describes the syntax.

Now hledger can read this CSV file:

    $ hledger -f checking.csv print
    using conversion rules file checking.csv.rules
    2012/03/22 DEPOSIT
        income:unknown             $-50.00
        assets:bank:checking        $50.00

    2012/03/23 TRANSFER TO SAVINGS
        assets:bank:savings         $10.00
        assets:bank:checking       $-10.00

We might save this output as `checking.journal`, and/or merge it (manually) into the main journal file.

