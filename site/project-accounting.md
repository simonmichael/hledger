# Project accounting

Some ways to track small business/freelancer activity - orders, budgets, invoices, payments..

## Accrual method

Revenue is declared when work is performed:

```journal
; budget:* - virtual accounts tracking what customers have committed
; to pay for various things. Should not go below 0.
2017/10/30 Order from CUSTOMER (order id) 
    (budget:CUSTOMER:PROJECT_ID:pos1)                       1000
    (budget:CUSTOMER:PROJECT_ID:pos2)                       3000
     
; some work was done on pos1 and pos2, invoice for it.
; Using accrual accounting method
; (revenue is declared when work is done, ~= when invoiced)
2017/10/31 Invoice (invoice id) - (PROJECT_ID)
    (budget:CUSTOMER:PROJECT_ID:pos1)                       -500  ; update project budget
    (budget:CUSTOMER:PROJECT_ID:pos2)                      -1000
    assets:receivable:CUSTOMER:PROJECT_ID:pos1               500
    assets:receivable:CUSTOMER:PROJECT_ID:pos2              1000
    revenues:CUSTOMER
    (liabilities:tax:federal)                               -150  ; note tax due, eg 15% of revenue

; a customer payment is received
2017/11/15 Payment for INVOICE_ID
    assets:receivable:CUSTOMER:PROJECT_ID:pos1              -500
    assets:receivable:CUSTOMER:PROJECT_ID:pos2             -1000
    assets:bank:checking

; make a tax payment
2018/4/15 Pay taxes due from 2017
    liabilities:tax:federal                                 5000
    assets:bank:checking
```

## Cash method

Revenue is declared when payment is received:

```journal
2017/10/30 Order from CUSTOMER (order id) 
    (budget:CUSTOMER:PROJECT_ID:pos1)                       1000
    (budget:CUSTOMER:PROJECT_ID:pos2)                       3000

; record an invoice sent. Not a real transaction in cash accounting,
; but we can balance it with the project budget as shown:
2017/10/31 Invoice (invoice id) - (PROJECT_ID)
    budget:CUSTOMER:PROJECT_ID:pos1                         -500
    assets:receivable:CUSTOMER:PROJECT_ID:pos1               500
    budget:CUSTOMER:PROJECT_ID:pos2                        -1000
    assets:receivable:CUSTOMER:PROJECT_ID:pos2              1000

; receive payment. Cash basis, so revenue declared here.
2017/11/15 Payment for INVOICE_ID
    (assets:receivable:CUSTOMER:PROJECT_ID:pos1)            -500
    (assets:receivable:CUSTOMER:PROJECT_ID:pos2)           -1000
    revenues:CUSTOMER                                      -1500
    (liabilities:tax:federal)                               -150  ; note tax due, eg 15% of revenue
    assets:bank:checking

; make a tax payment
2018/4/15 Pay taxes due from 2017
    liabilities:tax:federal                                 5000
    assets:bank:checking
```
