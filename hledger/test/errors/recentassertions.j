#!/usr/bin/env -S hledger check recentassertions -f
# Postings more than 7 days after latest balance assertion.

2022-01-01 *
  a  0 = 0

2022-01-08 good
  a  0

2022-01-09 bad1
  a  0

2022-01-10 bad2
  a  0
