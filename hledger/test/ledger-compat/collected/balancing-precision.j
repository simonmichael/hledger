; In this journal, $'s precision is 2 in txn1, 4 in txn2, and 4 globally.
; Ledger checks transaction balancedness using local precisions only,
; so it accepts txn1's $-0.00045312 imbalance.

2022-01-01 txn1
    expenses                                 AAA 989.02 @ $1.123456  ; $1111.12045312
    checking                                  $-1111.12

2022-01-02 txn2
    expenses                                      $0.1234
    checking
