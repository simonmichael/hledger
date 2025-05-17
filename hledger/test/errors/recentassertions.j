#!/usr/bin/env -S hledger check recentassertions -f

account a
commodity ""
payee last balance assertion
payee good
payee bad1
payee bad2

2022-01-01 last balance assertion
  a  0 = 0

2022-01-08 good
  a  0

# More than 7 days after latest balance assertion, fails this check.
2022-01-09 bad1
  a  0

2022-01-10 bad2
  a  0
