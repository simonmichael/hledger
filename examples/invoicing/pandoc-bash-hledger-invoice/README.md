Scripts adapted from a real-world setup, not guaranteed to be current or working.

Example: 

Show invoice preview:
```
$ ./invoice abinvoice.tmpl.md client.ab.dev 200
---
papersize: letter
margin-left: 20mm
margin-right: 25mm
margin-top: 20mm
margin-bottom: 20mm
...

![](logo.jpg){ width=100mm }\
Joe Consultant | +1 (111) 111 1111 | joe@example.com | 500 Done Dr. #1, Work Ville, CA 10000, USA

Carl Client\
AB Inc.\
PO Box 11111\
CA 20000\

December 4, 2021

# Invoice 202111cw

| Description                                 |   Rate |    Qty |   Total |
|:--------------------------------------------|-------:|-------:|--------:|
| Systems reliability engineering             | $ 1111 |        | $  1111 |
| On-call monitoring & tech support           | $ 2222 |        | $  2222 |
| Contractor/vendor management                | $  333 |        | $   333 |
| Custom SW development & maintenance (Nov)   | $  444 |   2.00 | $   888 |
| Reimbursable expenses (Nov)                 |        |        | $   200 |
| &nbsp;                                      |        |        |         |
| Total due                                   |        |        | $  4754 |
|                                             |        |        |         |


Terms: Now due. Your business is appreciated, thank you!
```

Generate markdown and PDF invoices and sample journal entries:
```
$ ./invoice abinvoice.tmpl.md client.ab.dev 200 --md --pdf --txn
wrote abinvoice202111.md
Loading pages (1/6)
Counting pages (2/6)                                               
Resolving links (4/6)                                                       
Loading headers and footers (5/6)                                           
Printing pages (6/6)
Done                                                                      
wrote abinvoice202111.pdf

--------------------------------------------------------------------------------

2021-12-04 (202111) abinvoice | invoice $ 4754
    (assets:receivable:abinvoice:consulting)      $ 4554 ; Nov hourly & Dec fixed fees
    ;(assets:receivable:abinvoice:reimbursement)  $  200 ; Nov reimbursable expenses

; 2021-12-04 (202111) abinvoice | payment
;     ; receive full amount of invoice
;     assets:bank:checking                $ 4754
;     assets:receivable:abinvoice:reimbursement    $-  200
;     assets:receivable:abinvoice:consulting       $- 4554 = ./invoice
;     ; recognise revenue (cash accounting)
;     (revenues:abinvoice)                         $- 4554
;     ; estimate tax due, tax-saved-on:  ?, TODO:
;     (liabilities:tax:us:2021)              $-1275  ; 28%
;     (liabilities:tax:st:2021)              $-364  ;  8%
;     ; Total tax:                               $1639  ; 36%
;     ; Post-tax income:                         $2915

; 2021-12-04 save estimated tax from abinvoice 202111, received 2021-12-04
;     assets:bank:checking               $-1639
;     assets:bank:savings:tax:us:2021     $1275
;     assets:bank:savings:tax:st:2021     $364

```
