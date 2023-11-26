## codes

List the codes seen in transactions, in the order parsed.

_FLAGS

This command prints the value of each transaction's code field, in the
order transactions were parsed. The transaction code is an optional
value written in parentheses between the date and description, often
used to store a cheque number, order number or similar.

Transactions aren't required to have a code, and missing or empty codes
will not be shown by default. With the `-E`/`--empty` flag, they will
be printed as blank lines.

You can add a query to select a subset of transactions.

Examples:

```journal
2022/1/1 (123) Supermarket   
 Food       $5.00
 Checking    

2022/1/2 (124) Post Office
 Postage    $8.32
 Checking

2022/1/3 Supermarket
 Food      $11.23
 Checking 

2022/1/4 (126) Post Office
 Postage    $3.21
 Checking
```

```cli
$ hledger codes
123
124
126
```

```cli
$ hledger codes -E
123
124

126
```

