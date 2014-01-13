# How to read CSV files

Here's a quick example of [converting a CSV file](MANUAL.html#csv-files).

Say we have downloaded `checking.csv` from a bank for the first time:

    "Date","Note","Amount"
    "2012/3/22","DEPOSIT","50.00"
    "2012/3/23","TRANSFER TO SAVINGS","-10.00"

We could create `checking.csv.rules` containing:

	# skip the first CSV line (headings)
	skip 1

	# use the first three fields in each CSV record as transaction date, description and amount respectively
	fields   date, description, amount

	# prepend $ to CSV amounts
	currency $

	# always set the first account to assets:bank:checking
	account1 assets:bank:checking

	# if the CSV record contains ‘SAVINGS’, set the second account to assets:bank:savings
	# (if not set, it will be expenses:unknown or income:unknown)
	if ~ SAVINGS
	 account2 assets:bank:savings

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

